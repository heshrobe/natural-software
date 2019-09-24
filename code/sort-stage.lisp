;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;; Design Question: Why does this follow the symbolic tokens, rather than just structural
;;; elements (dataflow controlflow links entry-points branches joins initial-inputs final-outputs, etc.)
;;; The symbolic-tokens trace the actual dataflow and are useful for code-generation and in particular
;;; for determining whether to bind a new variable to the result of a computation (more than one consumer)
;;; but they can miss elements of temporal dependency.

;;; Design note:
;;; We want every task interface to precede every task in its implementation
;;; this is easily done by having upstream tasks add the abstact-task of the parent
;;; The abstract tasks must also get queued which is done by adding them to the consumers
;;; of all the input tokens that arrive at the interface.
;;; Another note:
;;; A state source that also has a state sync
;;; has to somehow make anything downstream from it also be downstream from
;;; the state sink.

;;; A Bug: Consider a primitive-enumerator
;;; i.e .something like an index-enumerator (Task A)
;;; Suppose that one of the downstream tasks of Task A (Task C) has an input
;;; that comes from an task (Task B) that isn't ordered with respect to Task A
;;; (i.e. Task B is neither upstream nor downstream from Task A).
;;;
;;; There are two issues if Task B is ordered after Task A but before
;;; the end of Task A's downstream tasks (it would have to be before the end
;;; of Task A's downstream tasks, since it's upstream of Task C).
;;; 
;;; 1) Task A will stop gobbling tasks when it hits Task B because
;;;    Task A isn't in the upstream-tasks of Task B 
;;;     (this happens in the array normalization example)
;;; 2) If Task C has a side-effect it will be executed on each iteration of
;;;    the loop, which isn't semantically correct.
;;; So even if there's a cheap fix to issue 1, issue 2 is fundamental.
;;;
;;; Therefore during the sort, we need to make Task B an upstream task of Task A
;;; To find Task b, look at all downstream tasks of Task A (Task C).
;;; Ask if Task C has an upstream task Task B that isn't downstream from Task A
;;; If so, add Task B to the upstream tasks of Task A given to the topological sorter.
;;; Note that this will include Task A and any upstream tasks of Task A, so you can
;;; filter out those that are upstream of others in the set.
;;; (note: the upstream stream tasks given to the sorter are the immediate upstream
;;;  tasks, not all upstream tasks).  I'm pretty sure that upstream-tasks is only
;;; called once by the sorter, so we can define an :around method for upstream tasks
;;; on primitive-enumerator that does the search described above and adds all such
;;; tasks to the result returned by the normal methoed.
;;;
;;; As a result we need to calculate all-upstream-tasks and all-downstream-tasks 
;;; before the topological sort.  This is a standard dynamic-programming
;;; recursive calculation where the condition for not recurring is when 
;;; the guy hit either has no immediate upstream tasks or has all-upstream-tasks
;;; calculated (downstream is exactly the same).
;;; 

;;; Note: The other times we sub-gobble are
;;;  1) When creating a variable binding
;;;  2) When creating an internal function
;;;  not clear if these have the same issue

(defgeneric upstream-tasks (task))
(defgeneric downstream-tasks (task))
(defgeneric all-upstream-tasks (task))
(defgeneric all-downstream-tasks (task))

;;; Fix: that upstream-tasks and downstream-tasks need to be
;;; symmetric, but the use of find-indirectly-forced-predecessors gives
;;; you precessors that you aren't successor off

(defun topological-sort-sub-tasks (top-level-task)
  (let* ((initial-tasks (cons top-level-task 
			      (append (find-initial-inputs (selected-implementation top-level-task)) 
				      (find-global-initial-tasks top-level-task))))
	 (all-tasks nil))
    (setq all-tasks (compute-all-upstream-and-downstream-tasks top-level-task))
    (loop for task in all-tasks
        unless (eql task top-level-task)
        do (pushnew top-level-task  (all-upstream-tasks task)))
    (let ((branch-ht (build-branch-basic-blocks all-tasks)))
      (loop for task in all-tasks 
	  when (typep task 'branch)
	  do (find-indirectly-forced-predecessors task branch-ht)))
    (let ((sorted-guys (topological-sort initial-tasks #'upstream-tasks #'downstream-tasks)))
      ;; (format *trace-output* "~%All tasks ~a ~%Length ~a" sorted-guys (length sorted-guys))
      sorted-guys)))

(defun compute-all-upstream-and-downstream-tasks (top-level-task)
  (let ((initial-tasks (cons top-level-task 
			     (append (find-initial-inputs (selected-implementation top-level-task)) 
				     (find-global-initial-tasks top-level-task))))
	(all-tasks nil)
	(upstream-already-done (make-hash-table))
	(downstream-already-done (make-hash-table)))
    (labels ((do-upstream (task)
	       (cond
		((gethash task upstream-already-done)
		 (all-upstream-tasks task))
		((null (upstream-tasks task)) nil)
		(t (setf (all-upstream-tasks task) (upstream-tasks task))
		   (loop for next-task in (upstream-tasks task)
		       do (loop for another-task in (do-upstream next-task)
			      do (pushnew another-task (all-upstream-tasks task))))
		   (setf (gethash task upstream-already-done) t)
		   (all-upstream-tasks task))))
	     (do-downstream (task)
	       (cond
		((gethash task downstream-already-done)
		 (all-downstream-tasks task))
		((null (downstream-tasks task)) nil)
		(t (setf (all-downstream-tasks task) (downstream-tasks task))
		   (loop for next-task in (downstream-tasks task)
		       do (loop for another-task in (do-downstream next-task)
			      do (pushnew another-task (all-downstream-tasks task))))
		   (setf (gethash task downstream-already-done) t)
		   (all-downstream-tasks task))))
	     (pursue (task)
	       (unless (member task all-tasks)
		 (push task all-tasks)
		 (do-upstream task)
		 (do-downstream task)
		 (loop for next in (downstream-tasks task)
		     do (pursue next)))))
      (loop for task in initial-tasks
	  do (pursue task))
      all-tasks)))

;;; What this is about:
;;; consider the code along one path downstream from a branch B.  Some action A along this path
;;; might take an input that is produced by some component C that isn't downstream from B.
;;; You want to make sure that that predecessor doesn't get generated inside the branch since it doesn't
;;; depend on the branch condition.  The way to do this is to make the branching task (i.e. the supererior of B)
;;; be downstream from C.
;;;            C      --------
;;;            |      |      |
;;;            |      --------
;;;            |      | B |  |
;;;            |      ________
;;;            \   /
;;;               A
;;;
;;; One special case is an enumerator where if you didn't lift C out of the branch's basic block
;;; it would occur on each iteration (and if it has side effects that would be completely wrong).
;;; So what you need to do is for each branch B trace downstream until the path terminates either at a
;;; join or a dead end.  For everything on that path e.g. A, you have to check its immediate predecessors C
;;; and see if any of the are not downstream from B.  If so you add B's parent (the branching task)
;;; to the successors of C.
;;; One minor complication is that as you trace down the sucessors of B you might encounter another branch B'
;;; if so you need to then trace its basic block and then return to tracing B's.

(defmethod build-branch-basic-blocks (all-tasks)
  (let ((blocks-ht (make-hash-table)))
    (labels 
	((follow-this-branch (branch)
	   ;; We collect the block starting at this branch
	   ;; and ending at either a join or a dead-end
	   ;; If it's a join we return that
	   (let ((answer )
		 (downstream-guys nil) 
		 (terminator nil))
	     (cond
	      ((setq answer (gethash branch blocks-ht))
	       (setq downstream-guys (first answer)
		     terminator (second answer)))
	      (t
		 (loop for component in (downstream-tasks branch)
		     do (pushnew component downstream-guys)
		     if (is-a-real-join component)
		     do (pushnew component downstream-guys) (setq terminator component)
		     else do (multiple-value-bind (his-downstream-guys his-terminator)
				 (follow-this-component component :for-branch t)
			       (setq downstream-guys (union his-downstream-guys downstream-guys))
			       (when (typep his-terminator 'join)
				 (setq terminator his-terminator))))
		 (setf (gethash branch blocks-ht) (list downstream-guys terminator))))
	     (values downstream-guys terminator)))
	 (is-a-real-join (component)
	   (and (typep component 'join) (typep (superior component) 'primitive-joining-task)))
	 (follow-this-component (component &key (for-branch t))
	   (let ((downstream-guys (list component)) (terminator nil))
	     (cond 
	      ((and for-branch (is-a-real-join component))
	       (setq  terminator component))
	      ((typep component 'branch)
	       ;; gobble the branch's basic block and then keep	       
	       ;; going with everything after it
	       (multiple-value-bind (his-downstream-guys his-terminator) 
		   (follow-this-branch component)
		 (setq downstream-guys (union his-downstream-guys downstream-guys))
		 ;; If the branch ended at a join, rather than a dead end
		 ;; then we need to continue the tracing from the join
		 (when his-terminator
		   (multiple-value-bind (his-downstream-guys his-terminator) (follow-this-component his-terminator :for-branch for-branch)
		     (setq downstream-guys (union his-downstream-guys downstream-guys)
			   terminator his-terminator)))))
	      (t (loop for component in (downstream-tasks component)
		     do (multiple-value-bind (his-downstream-guys his-terminator) (follow-this-component component :for-branch for-branch) 
			  (setq downstream-guys (union downstream-guys his-downstream-guys))
			  (when his-terminator (setq terminator his-terminator))
			  ))))
	     (values downstream-guys terminator))))
      (loop for task in all-tasks
	  when (typep task 'branch)
	  do (follow-this-branch task))
      blocks-ht)))

;;; For each task downstream from the branch (but still in its basic block)
;;; Look at each immediate predecessor and if it doesn't depend on the branch
;;; Make it a predecessor of the Branch's Branching-task
(defmethod find-indirectly-forced-predecessors ((task branch) branch-ht)
  (let ((branching-task (superior task))
	(answers nil))
    (loop for downstream-task in (first (gethash task branch-ht))
	do (loop for downstream-predecessor in (upstream-tasks downstream-task)
	       unless (or (eql downstream-predecessor branching-task)
			  (and (typep downstream-predecessor 'state-source)
			       (not (eql (getf (properties downstream-predecessor) :locality) :local)))
			  (member downstream-predecessor (all-downstream-tasks branching-task)))
	       do (pushnew downstream-predecessor answers)))
    (loop for answer in answers
	do (pushnew branching-task (cached-downstream-tasks answer))
	   (pushnew answer (cached-upstream-tasks branching-task)))
    answers)
    )
      
;;;;;; This works because once the tasks are sorted it's guaranteed
;;;;;; that anything upstream of a task has been processed first and
;;;;;; therefore that guy's list of all-upstream-tasks is already complete
;;;;;; so all we have to do is union all of those and add in the immediate upstream tasks
;;;(defun compute-upstream-tasks (sorted-tasks)
;;;  (loop for this-task in sorted-tasks
;;;      do (loop for immediate-upstream-task in (upstream-tasks this-task)
;;;             do (pushnew immediate-upstream-task (all-upstream-tasks this-task))
;;;                (loop for super-task in (all-upstream-tasks immediate-upstream-task)
;;;                    do (pushnew super-task (all-upstream-tasks this-task))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Proxy tasks for tasks downstream from a join
;;;   When you hit a join there are two options
;;;   1) You want to merge the two flows and then just continue with one stream
;;;      If there are values being merged this would amount to binding new variables
;;;      to the values in a pattern like (let ((x (if <...> <branch-1-result> <branch-2-result>))) ...)
;;;      If there are no values, then just just do (if <...> <stuff-1> <stuff-2>) ...
;;;   2) The branching can't be merged like above.  For example, each branch enuerates a stream
;;;      of values, there's really no way to play the game as above.  You could pass in a funarg
;;;      which each branch calls on its enumeration-variable.  Or you could just inline everything downstream
;;;      in each branch.  Or there might not be any values bind but just want to inline the code from the downstream
;;;      in each branch.
;;;   For the moment every case I have is case 2.
;;;   We'll indicate that by the absence of some of the :join-values property on the join.
;;;
;;;   The propagation stage will completely ignore this.
;;;   The topological sort will create a "proxy" for each downstream task for each join when it encounters the join.
;;;   The proxy really just needs to be a data-structure that includes the join and the real task.
;;;   Given that it can figure out how to "dejoin the values" that are passed on in the propagation stage.
;;;   The downstream task of a proxy is a proxy for the downstream task of the enclosed real task.
;;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod upstream-tasks :around (task)
  (or (cached-upstream-tasks task)
      (let ((answer (call-next-method)))
	(setf (cached-upstream-tasks task) answer)
	answer)))

(defmethod downstream-tasks :around (task)
  (or (cached-downstream-tasks task)
      (let ((answer (call-next-method)))
	(setf (cached-downstream-tasks task) answer)
	answer)))

(defclass proxy-task (up-down-cache-mixin)
  ((join :accessor join :initarg :join :initform nil)
   (real-task :accessor real-task :initarg :real-task :initform nil)
   ;; these are proxies or joins
   (downstream-stuff :accessor downstream-stuff :initform nil)
   (upstream-stuff :accessor upstream-stuff :initform nil)
   ))

(defmethod print-object ((proxy proxy-task) stream)
  (format stream "#<pr ~a ~a" (real-task proxy) (join proxy)))

(defgeneric is-branch? (thing))
(defmethod is-branch? ((thing t)) nil)
(defmethod is-branch? ((branch branch-mixin)) t)
(defmethod is-branch? ((proxy proxy-task)) (is-branch? (real-task proxy)))

(defgeneric is-join? (thing))
(defmethod is-join? ((thing t)) nil)
(defmethod is-join? ((join join-mixin)) t)
(defmethod is-join? ((proxy proxy-task)) (is-join? (real-task proxy)))

(defmethod superior ((proxy proxy-task)) (superior (real-task proxy)))
(defmethod task-type ((proxy proxy-task)) (task-type (real-task proxy)))
(defmethod substituted-branch-condition ((proxy proxy-task)) (substituted-branch-condition (real-task proxy)))

;;; This keeps you from duplicating the proxies if
;;; this guy has more than 1 upstream task.

(defmethod downstream-tasks ((proxy proxy-task))
  (let ((proxies (downstream-stuff proxy))
	(real-task (real-task proxy))
	(join (join proxy)))
    (when (null proxies)
      (setf (proxies real-task) nil)
      (loop for dtask in (downstream-tasks real-task)
	  for new-proxy = (make-instance 'proxy-task :join join :real-task dtask)
	  do (push proxy (proxies real-task))
	     (push proxy (upstream-stuff new-proxy))
	     (push new-proxy proxies)
	  finally (setf (downstream-stuff proxy) proxies)))
      proxies)
  )

(defmethod upstream-tasks ((proxy proxy-task))
  (upstream-stuff proxy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion-tasks
;;;
;;; We need to guarantee that all sub-tasks of the implementation come after the task's beginning
;;;  So we make all components of the task be among its downstream tasks (maybe only initial tasks are required)
;;;  and we make the task be in the list of upstream tasks for each of its components (again maybe only initial are required)
;;;
;;; Now Consider a task with an implementation that has one or more outgoing control flows to one or more
;;; other tasks, but no dataflows to those tasks.
;;; We need to make sure that the downstream task(s) come after all components of the task.
;;; The problem is that the sub-components don't have anything to point to that represents the end of 
;;; whole task. There are two approaches:
;;; 1) Track the downstream tasks and make them be in the downstream list of all components of the parent
;;;    and also make all the sub-taks in the upstream-tasks of those downstream
;;;
;;; 2) Introduce a pseudo task to representation the completion of the main task
;;; It has to be in the downstream tasks of all final tasks inside the implementation
;;; and the task's (final or all) subtasks be in the upstream tasks of the surrogate
;;; task and we make the surrogate task be in the upstream tasks of all tasks to which
;;; the original task has control flows.
;;; The advantage of 2 is that we have a single thing to do this.
;;; 
;;; How do branching-tasks and completion-tasks interact?
;;;         This should be simpler since:
;;;         The implementation of a branching task must have exit points corresponding to 
;;;         each branch.  The downstream task of the exit-point is the branch (and the upstream
;;;         task of the exit point is the branch).  And the downstream task of the branch
;;;         are the real downstream tasks.  Because the branch doesn't have inputs,
;;;
;;; There can occasionally be confusion when for example an accumulator which is a joining task
;;; has one join path that returns a value (typically the exit path) and another which doesn't
;;; To make sure that this other path isn't regarded as a path which operates by side-effect and
;;; therefore has no outputs, we introduced a special task-type, path-end to make it clear
;;; that there are really no tasks downstream from there
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass completion-task (up-down-cache-mixin)
  ((real-task :initarg :real-task :accessor real-task)
   ;; need this at the end of sorting stage
   ))

(defmethod task-type ((task completion-task)) 'completion-pseudo-task)

(defmethod initialize-instance :after ((tc completion-task) &key real-task &allow-other-keys)
  (setf (completion-task real-task) tc))

(defmethod get-completion-task ((task has-successors-mixin))
  (unless (is-top-level-task? task)
    (or (completion-task task)
	(make-instance 'completion-task :real-task task))))

;;; downstream tasks are all the tasks downstream from
;;; the real-task via control flows and data flows

(defmethod downstream-tasks ((tc completion-task))
  (let ((real-task (real-task tc)))
    (all-data-and-control-targets real-task)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful sub-routines
;;;
;;; Written to avoid duplicates in the answer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod all-dataflow-sources ((task has-input-ports-mixin))
  (let ((answer nil))
    (loop for input in (inputs task)
	for dflows = (incoming-flows input)
	do (loop for dflow in dflows
	       do (pushnew (task (input dflow)) answer)))
    answer))

(defmethod all-control-flow-sources ((task has-predecessors-mixin))
  (let ((answer nil))
    (loop for cflow in (incoming-control-flows task)
	do (pushnew (predecessor cflow) answer))
    answer))

(defmethod all-data-and-control-sources ((task input-side-mixin))
  (let ((answer nil))
    (loop for thing in (all-dataflow-sources task)
	do (pushnew thing answer))
    (loop for thing in (all-control-flow-sources task)
	do (pushnew thing answer))
    answer))

(defmethod all-dataflow-targets ((task has-output-ports-mixin))
  (let ((answer nil))
    (loop for output in (outputs task)
	for dflows = (outgoing-flows output)
	do (loop for dflow in dflows
	       do (pushnew (task (output dflow)) answer)))
    answer))

(defmethod all-control-flow-targets ((task has-successors-mixin))
  (let ((answer nil))
    (loop for cflow in (outgoing-control-flows task)
	do (Pushnew (successor cflow) answer))
    answer))

(defmethod all-data-and-control-targets ((task output-side-mixin))
  (let ((answer nil))
    (loop for target in (all-dataflow-targets task) do (pushnew target answer))
    (loop for target in (all-control-flow-targets task) do (pushnew target answer))
    answer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finding initial and final tasks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Default is t which is always safe
;;; Notice that this default case
;;; includes final-outputs and exit-points
;;; because they aren't in any of the other cases
(defmethod is-final-task? ((task t)) t)

;;; A join is never final since the joining-task it's part of
;;; is always downstream from it.
(defmethod is-final-task? ((task join-mixin)) nil)

;;; A dead-end is a final task
(defmethod is-final-task? ((task output-side-mixin))
  (and (null (outgoing-control-flows task))
       (loop for port in (outputs task)
	   always (null (outgoing-flows port)))))

(defmethod is-final-task? ((task has-branches-mixin))
  (loop for branch in (branches task)
      always (is-final-task? branch)))

(defmethod find-final-tasks ((ct composite-task) &optional no-path-ends)
  (let ((answer nil))
    (loop for task in (children ct)
	when (and (is-final-task? task)
		  (or (null no-path-ends)
		      (not (typep task 'path-end))))
	do (pushnew task answer))
    answer))

;;; Default is t which is always safe
;;; Notice that this default case
;;; includes initial-inputs and entry-points
;;; and constants (and allocations ?)
;;; because they aren't in any of the other cases
(defmethod is-initial-task? ((task t)) t)

;;; A branch is never initial since the branching-task it's part of
;;; is always upstream from it.
(defmethod is-initial-task? ((task branch-mixin)) nil)

;;; A something with no inputs or incoming control flows  is an initial task
(defmethod is-initial-task? ((task input-side-mixin))
  (and (null (incoming-control-flows task))
       (loop for port in (inputs task)
	   always (null (incoming-flows port)))))

(defmethod is-initial-task? ((task has-joins-mixin))
  (loop for join in (joins task)
      always (is-initial-task? join)))

(defmethod find-initial-tasks ((ct composite-task))
  (let ((answer nil))
    (loop for task in (children ct)
	when (is-initial-task? task)
	do (pushnew task answer))
    answer))

(defmethod corresponding-parent-task ((task has-superior-mixin))
  (let ((parent (superior task)))
    (when parent 
      (let ((abstract-task (abstract-task parent)))
	(when abstract-task
	  abstract-task)))))

(defmethod corresponding-parent-task ((task branch))
  ;; the superior is the branching task
  (corresponding-parent-task (superior task)))

(defmethod is-top-level-task? ((task branch)) nil)

(defmethod is-top-level-task? ((task join)) nil)

(defmethod is-top-level-task? ((task has-superior-mixin))
  (null (corresponding-parent-task task)))

(defmethod top-level-task ((task has-superior-mixin))
  (loop for current = task then parent
      for parent = (corresponding-parent-task current)
      when (null parent)
      return current))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Upstream and downstream tasks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod upstream-tasks ((task t)) nil)

;;; Branching-tasks don't have completion tasks
;;; The branches act as the completion tasks

;;; The upstream tasks are all the final sub-tasks
;;; of the original task
(defmethod upstream-tasks ((ct completion-task))
  (let ((real-task (real-task ct)))
    ;; this check should be redundant since none of 
    ;; these ever create a completion task
    (if (or (primitive? real-task) (typep real-task 'has-branches-mixin) (dont-expand real-task))
        (list real-task)
      (let ((implementation (selected-implementation real-task)))
        (find-final-tasks implementation 'no-path-ends)))))

(defmethod upstream-tasks :around ((task core-task-mixin))
  (cond ((is-global-initial-task? task) (list (top-level-task task)))
	((is-top-level-task? task) nil)
	(t (call-next-method))))

;;; At the moment this is just constant-values
;;; but should we fix up the way allocates work
;;; it would include them as well
;;; Right now allocates are just normal tnow handle completion tasks.  asks with a
;;; specific task-type of 'allocate
;;; Constant values are also initial-object-mixin, so this is redundant
(defmethod upstream-tasks ((task local-element-mixin))
  ;; Note that if it's global the around task above will get called
  ;; rather than thin
  (list (corresponding-parent-task task)))

(defmethod upstream-tasks ((task initial-input))
  (list (corresponding-parent-task task)))

(defmethod upstream-tasks ((task input-side-mixin))
  (if (is-initial-task? task)
      ;; if it's an initial task then there are no incoming links
      ;; so the parent is the predecessro
      (list (corresponding-parent-task task))
    ;; otherwise get the predecessors and for each
    ;; collect its completion task if it's not a branch or something opaque
    (let ((answer nil))
      (loop for predecessor in (all-data-and-control-sources task)
	  if (or (typep predecessor 'branch) (not (typep predecessor 'task-interface-mixin))
		 (primitive? predecessor) (dont-expand predecessor))
	  do (pushnew predecessor answer)
	  else do (pushnew (get-completion-task predecessor) answer))
      answer)
    ))

;;; For a branch of a primitive branching task
;;; it's the branching task
;;; Otherwise it's the corresponding exit-point in the implementation
(defmethod upstream-tasks ((branch branch)) 
  (let ((branching-task (superior branch)))
    (if (or (getf (properties branching-task) :dont-expand)
	    (typep branching-task 'primitive-branching-task))
	(list branching-task)
      (list (corresponding-exit-point branch)))))

;;; for primitive-joining-tasks, it's all the joins
;;; which are input-side-mixin so normal methods apply to them
;;; For non-primitive we use normal compound-task methods
(defmethod upstream-tasks ((task primitive-joining-task))
  (joins task))

(defmethod upstream-tasks ((task joining-task))
  (if (getf (properties task) 'dont-expand)
      (joins task)
    (call-next-method)))

(defmethod upstream-tasks ((ep entry-point))
  (list (corresponding-join ep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Downstream Tasks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod downstream-tasks ((task t)) nil)

;;; A path end is, well the end of the path
(defmethod downstream-tasks ((task path-end)) nil)

(defmethod downstream-tasks :around ((task task-interface-mixin))
  (let ((answer nil))
    (cond
     ((or (primitive? task) (dont-expand task))
      ;; chase its outgoing pointers
      (setq answer (call-next-method)))
     ((is-top-level-task? task)
      (loop for sub-task in (find-global-initial-tasks task)
	  do (pushnew sub-task answer))))
    ;; if it's a final task then add the completion task
    ;; of the parent task
    (when (is-final-task? task)
      ;; First of all, if he's a final task within a sub-plan
      ;; then the completion task of the parent depends on him
      (let* ((parent-task (corresponding-parent-task task)))
	(when (and parent-task (not (typep parent-task 'branching-task)))
	  ;; Abstract task is the envelope corresponding to
	  ;; his parent.  That could be a branching task
	  ;; for which get-completion-task would fail!!!!
	  (let ((completion-task (get-completion-task parent-task)))
	    (when completion-task
	      (pushnew completion-task answer))))))
    (unless (or (primitive? task) (dont-expand task))
      ;; so it has an implementation and we need to include all it's
      ;; initial tasks except for those that are non-local constants
      (loop for thing in (find-local-initial-tasks (selected-implementation task))
          do (pushnew thing answer)))
    answer))

(defmethod downstream-tasks ((fo final-object-mixin))
  (let* ((abstract-task (corresponding-parent-task fo))
	 (completion-task (when abstract-task (get-completion-task abstract-task))))
    (when completion-task
      (list completion-task))))

;;; If the exit-point has values, those are already propagated down to the real
;;; successors.  So we're really only handling the linking up to the parents branch.

(defmethod downstream-tasks ((ep exit-point))
  (list (corresponding-branch ep)))
 
(defmethod downstream-tasks ((task output-side-mixin))
  ;; everything it flows to
  (let ((answer (all-data-and-control-targets task)))
    ;; plus if it's a final task the completion task of the parent
    (when (and (is-final-task? task) 
	       ;; a final branch is one that doesn't go anywhere
	       (not (typep task 'branch)))
      (let* ((abstract-task (corresponding-parent-task task))
	     (completion-task (when abstract-task (get-completion-task abstract-task))))
	(when completion-task
	  (pushnew completion-task answer))))
    answer))

;;; Fix: Need to integrate this with the stuff below
;;; right now there are two methods for this and the 
;;; other one comes later
;;;(defmethod downstream-tasks ((join join))
;;;  (let ((joining-task (superior join)))
;;;    (cond
;;;     ((typep joining-task 'primitive-joining-task)
;;;      (let ((proxies (downstream-proxies join)))
;;;	(when (null proxies)
;;;	  (loop for task in (downstream-tasks joining-task)
;;;	      for proxy = (make-instance 'proxy-task :join join :real-task task)
;;;	      do (push join (upstream-stuff proxy))
;;;		 (push proxy proxies)
;;;	      finally (setf (downstream-proxies join) proxies)))
;;;	proxies))
;;;     (t (break "Need to handle joins on non primitive tasks")))))

(defmethod downstream-tasks ((task join))
  (let ((joining-task (superior task)))
    (if (or (typep joining-task 'primitive-joining-task)
	    (getf (properties joining-task) 'dont-expand))
	(list joining-task)
      (list (corresponding-entry-point task)))))

(defmethod downstream-tasks ((task primitive-branching-task)) (branches task))

(defmethod downstream-tasks ((task branching-task))
  (if (getf (properties task) :dont-expand)
    ;; A branching task that isn't expanded is generally
    ;; a stuff for a recursive call.  It isn't clear in the generic case
    ;; what to do, so we dispatch.
      (downstream-tasks-for-opaque-brancher task (task-type task))
    (call-next-method)))

;;; The vanilla case: do nothing
(defmethod downstream-tasks-for-opaque-brancher ((task has-branches-mixin) (task-type t))
  (branches task))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For checking that the downstream upstream relationships are built correctly
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consistency-check-design (design)
  (let ((failures nil))
    (map-over-implementation-hierarchy (abstract-task design)
				       #'(lambda (thing)
					   (print thing)
					   (unless (consistency-check thing)
					     (push thing failures)
					     (princ 'failed))))
    failures))

(defun consistency-check (thing)
  (and (loop for u in (upstream-tasks thing)
	   always (member thing (downstream-tasks u)))
       (loop for d in (downstream-tasks thing)
	   always (member thing (upstream-tasks d)))))