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

(defgeneric upstream-tasks (task))
(defgeneric downstream-tasks (task))

(defun topological-sort-sub-tasks (top-level-task)
  (let* ((initial-tasks (cons top-level-task (append (find-initial-inputs (selected-implementation top-level-task)) (find-global-initial-tasks top-level-task))))
         (sorted-tasks (topological-sort initial-tasks #'upstream-tasks #'downstream-tasks)))
    (loop for task in initial-tasks 
        unless (eql task top-level-task)
        do (setf (all-upstream-tasks task) (list top-level-task)))
    (compute-upstream-tasks sorted-tasks)
    sorted-tasks))

;;; This works because once the tasks are sorted it's guaranteed
;;; that anything upstream of a task has been processed first and
;;; therefore that guy's list of all-upstream-tasks is already complete
;;; so all we have to do is union all of those and add in the immediate upstream tasks
(defun compute-upstream-tasks (sorted-tasks)
  (loop for this-task in sorted-tasks
      do (loop for immediate-upstream-task in (upstream-tasks this-task)
             do (pushnew immediate-upstream-task (all-upstream-tasks this-task))
                (loop for super-task in (all-upstream-tasks immediate-upstream-task)
                    do (pushnew super-task (all-upstream-tasks this-task))))))

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

(defclass proxy-task ()
  ((join :accessor join :initarg :join :initform nil)
   (real-task :accessor real-task :initarg :real-task :initform nil)
   ;; these are proxies or joins
   (downstream-stuff :accessor downstream-stuff :initform nil)
   (upstream-stuff :accessor upstream-stuff :initform nil)
   (all-upstream-tasks :accessor all-upstream-tasks :initform nil)
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

(defmethod downstream-tasks ((join join))
  (let ((joining-task (superior join)))
    (cond
     ((typep joining-task 'primitive-joining-task)
      (let ((proxies (downstream-proxies join)))
	(when (null proxies)
	  (loop for task in (downstream-tasks joining-task)
	      for proxy = (make-instance 'proxy-task :join join :real-task task)
	      do (push join (upstream-stuff proxy))
		 (push proxy proxies)
	      finally (setf (downstream-proxies join) proxies)))
	proxies))
     (t (break "Need to handle joins on non primitive tasks")))))

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
;;; Consider a task with an implementation that has one or more outgoing control flows to one or more
;;; other tasks.
;;; We need to guarantee that all sub-tasks of the implementation come after the task's beginning
;;; and that the downstream task(s) come after all components of the task.
;;; The problem is that the sub-components don't have anything to point that represents the end of 
;;; whole task.
;;;  We make all components of the first task be among its downstream tasks
;;; and we make the task be in the list of upstream tasks for each of its (initial or all) components
;;;
;;; We also create a surrogate-task to represent the completion of the task.
;;; It has to be in the downstream tasks of all final tasks inside the implementation
;;; and the task's (final or all) subtasks be in the upstream tasks of the surrogate
;;; task and we make the surrogate task be in the upstream tasks of all tasks to which
;;; the original task has control flows.
;;; TO FiX: How do branching-tasks and completion-tasks interact?
;;;         This should be simpler since:
;;;         The implementation of a branching task must have exit points corresponding to 
;;;         each branch.  The downstream task of the exit-point is the branch (and the upstream
;;;         task of the exit point is the branch).  And the downstream task of the branch
;;;         are the real downstream tasks.  Because the branch doesn't have inputs,
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass completion-task ()
  ((real-task :initarg :real-task :accessor real-task)
   ;; need this at the end of sorting stage
   (all-upstream-tasks :initform nil :accessor all-upstream-tasks))
  )

(defmethod task-type ((task completion-task)) 'completion-pseudo-task)

(defmethod initialize-instance :after ((tc completion-task) &key real-task &allow-other-keys)
  (setf (completion-task real-task) tc))

(defmethod get-completion-task ((task has-successors-mixin))
  (or (completion-task task)
      (make-instance 'completion-task :real-task task)))

;;; downstream tasks are all the tasks downstream from
;;; the real-task via control flows
(defmethod downstream-tasks ((tc completion-task))
  (let ((real-task (real-task tc)))
    (loop for cflow in (outgoing-control-flows real-task)
        collect (successor cflow))))

;;; The upstream tasks are all the final sub-tasks
;;; of the original task
(defmethod upstream-tasks ((ct completion-task))
  (let ((real-task (real-task ct)))
    (if (dont-expand real-task)
        (list real-task)
      (let ((implementation (selected-implementation real-task)))
        (find-final-tasks implementation)))))

(defmethod find-final-tasks ((ct composite-task))
  (loop for task in (children ct)
      when (is-final-task? task)
      collect task))

(defmethod is-final-task? ((task t)) t)

;;; A join is never final since the joining-task it's part of
;;; is always downstream from it.
(defmethod is-final-task? ((task join-mixin)) nil)

(defmethod is-final-task? ((task output-side-mixin))
  (and (null (outgoing-control-flows task))
       (loop for port in (outputs task)
	   always (null (outgoing-flows port)))))

(defmethod is-final-task? ((task has-branches-mixin))
  (loop for branch in (branches task)
      always (is-final-task? branch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Upstream and downstream tasks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is what you do if you want to treat the task as transparent
;;; Eventually we'll need to have a choice about that to deal with reccusive
;;; definitions

(defmethod upstream-tasks ((task t)) nil)

(defmethod upstream-tasks ((task input-side-mixin))
  (let ((answer nil))
    (loop for input in (inputs task)
        for token = (symbolic-token input)
        for producer = (producer token)
        unless (eql producer task) ;; (or (eql producer task) (not (task-is-real-computation producer)))
        do (pushnew producer answer)
           ;; link back to final output in the implementation
           )
    ;; for each input see if the output feeding it
    ;; is linked to a final output in its implemenation
    ;; and if so build dependency on the final outputs
    ;; incoming control-flows
    (loop for input in (inputs task)
        do (loop for dflow in (incoming-flows input)
               for source-port = (input dflow)
               for corresponing-port = (corresponding-port source-port)
               when corresponing-port
                    ;; note that the corresponding port is actually a final output
                    ;; not a port !!!!predecessoer
               do (Push corresponing-port answer)))
    ;; Notice that we don't try to make a completion task for a branch
    ;; below.  The branch works as the predecessor.
    (loop for cflow in (incoming-control-flows task)
        for predecessor = (predecessor cflow)
        if (or (typep predecessor 'branch) (primitive? predecessor) (dont-expand predecessor))
        do  (pushnew predecessor answer)
        else do (pushnew (get-completion-task predecessor) answer))
    ;; every component is downstream from the envelope
    ;; containing it
    (let ((parent (superior task)))
      (when parent 
        (let ((abstract-task (abstract-task parent)))
          (when abstract-task
            (pushnew abstract-task answer)))))  
    answer))

;;; At the moment this is just constant-values
;;; but should we fix up the way allocates work
;;; it would include them as well
;;; Right now allocates are just normal tasks with a
;;; specific task-type of 'allocate
(defmethod upstream-tasks ((task local-element-mixin))
  (when (local? task) 
    (let ((superior (superior task)))
      (when superior (list (abstract-task superior))))))

#|

(defmethod parents-control-flow-predecessors ((task t)) nil)

(defmethod parents-control-flow-predecessors ((task has-superior-mixin))
  (let ((parent (superior task)))
    (when parent 
      (let ((abstract-task (abstract-task parent)))
        (when abstract-task
          (loop for cflow in (incoming-control-flows abstract-task)
              collect (predecessor cflow)))))))
|#
        

;;; If the parent task is opaque then this is correct.
;;; However, if it's not then the upstream task
;;; is the corresponding exit-point

(defmethod upstream-tasks ((branch branch)) 
  (let ((branching-task (superior branch)))
    (if (or (getf (properties branching-task) :dont-expand)
	    (typep branching-task 'primitive-branching-task))
	(list branching-task)
      (list (corresponding-exit-point branch)))))

;;; for joining-tasks, they're never generated

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Downstream Tasks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod downstream-tasks ((task t)) nil)

(defmethod downstream-tasks ((fo final-output))
  (let ((corresponding-port (corresponding-port fo)))
    (when corresponding-port
      (let ((token (symbolic-token corresponding-port)))
	(consumers token)))))


(defmethod downstream-tasks ((task output-side-mixin))
  (let ((answer nil))
    (loop for output in (outputs task)
        for token = (symbolic-token output)
        when token
        do (loop for consumer in (consumers token)
               do (pushnew consumer answer)))
    (loop for cflow in (outgoing-control-flows task)
        do (pushnew (successor cflow) answer))
    answer))

(defmethod downstream-tasks :around ((task has-successors-mixin))
  (let ((answer (call-next-method))
        (cflows (outgoing-control-flows task)))
    ;; If there's a outgoing control flow 
    ;; that actually points at something
    ;; and this guy has an implementation
    ;; then his completion task need to be a successor
    (when (and (selected-implementation task)
	       (loop for cflow in cflows thereis (successor cflow)))
      (push (get-completion-task task) answer))
    answer))      

;;; for joins see above

(defmethod downstream-tasks ((task has-branches-mixin))
  (if (getf (properties task) :dont-expand)
      (downstream-tasks-for-opaque-brancher task (task-type task))
    ;; A branching task that isn't expanded is generally
    ;; a stuff for a recursive call.  It isn't clear in the generic case
    ;; what to do, so we dispatch.
    (branches task)))

;;; The vanilla case: do nothing
(defmethod downstream-tasks-for-opaque-brancher ((task has-branches-mixin) (task-type t))
  (branches task))

 (defmethod downstream-tasks :around ((task task-interface-mixin))
  (let ((answer nil))
    ;; First of all, if he's a final task within a sub-plan
    ;; then the completion task of the parent depends on him
    (let* ((parent (superior task))
           (abstract-task (when parent (abstract-task parent))))
      (when (and abstract-task
                 (member task (find-final-tasks parent)))
	;; Fix:  Abstract task is the envelope corresponding to
	;; his parent.  That could be a branching task
	;; for which get-completion-task would fail!!!!
        (push (get-completion-task abstract-task) answer)))
    (cond 
     ((or (primitive? task) (dont-expand task))
      ;; just get the obvious downstream tasks
      (loop for thing in (call-next-method) do (pushnew thing answer)))
     ((or (getf (properties task) :introduce-labels)
          (getf (properties task) :local-bindings))
      (let ((implementation (selected-implementation task)))
        (loop for input in (inputs task)
            for corresponding-input = (find-corresponding-input-port implementation (name input))
            for token = (symbolic-token corresponding-input)
            for consumers = (consumers token)
            do (loop for c in consumers do (Pushnew c answer)))))
     (t ;; constants and allocations 
      (loop for thing in (find-local-initial-tasks (selected-implementation task))
          do (pushnew thing answer))
      ;; This is here just to be sure, causes
      ;; extra requeuing doesn't seem to be needed
      (loop for thing in (children (selected-implementation task)) 
          unless (typep thing 'initial-input)
          do (pushnew thing answer))
      (loop for thing in (call-next-method)
          do (pushnew thing answer))
      ;; I guess this is to get the internal tasks
      ;; that consume the inputs
      ;; The problem is that this also gets similar guys
      ;; in other components that also use this input
      ;; This only causes extra requeuing not improper
      ;; ordering, I think.
      (loop for input-port in (inputs task)
          for input-token = (symbolic-token input-port)
          do (loop for thing in (consumers input-token)
                 do (pushnew thing answer)))))
    answer))


;;; If the exit-point has values, those are already propagated down to the real
;;; successors.  So we're really only handling the linking up to the parents branch.
(defmethod downstream-tasks ((task exit-point))
  (list (corresponding-branch task)))


(defun consistency-check-design (design)
  (map-over-implementation-hierarchy (abstract-task design)
				     #'(lambda (thing)
					 (print thing)
					 (unless (consistency-check thing)
					   (princ 'failed)))))

(defun consistency-check (thing)
  (and (loop for u in (upstream-tasks thing)
	   always (member thing (downstream-tasks u)))
       (loop for d in (downstream-tasks thing)
	   always (member thing (upstream-tasks d)))))