;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translating to actual code
;;;  First supporting routines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     
(defun find-initial-inputs (design)
  (loop for component in (children design)
      when (typep component 'initial-input)
      collect component))

(defun find-constants (design)
  (loop for component in (children design)
      when (typep component 'constant-value)
      collect component)
  )

(defun find-final-outputs (design)
  (loop for component in (children design)
	when (typep component 'final-output)
      collect component))

(defun find-normal-components (design)
  (loop for component in (children design)
      unless (or (typep component 'initial-input)
		 (typep component 'constant-value)
		 (typep component 'final-output))
	collect component))

(defun build-global-state-source-initializations (state-source-alist language)
  (loop for (task locality) in state-source-alist
      unless locality
      collect (state-source-initialization task language)))

(defun build-local-state-source-initializations (task language)
  (loop for (task locality) in (find-all-state-sources task)
      when (eql locality :local)
      collect (state-source-initialization task language)))

(defmethod state-source-initialization ((source state-source) language)
  (let* ((input (first (inputs source)))
	 (input-token (symbolic-token input))
	 (input-value (value-to-use input-token language))
	 (input-type (type-constraint input-token))
	 (output (first (outputs source)))
	 (output-token (symbolic-token output))
	 (variable (value output-token)))
    (construct-state-source-initialization variable input-value input-type language)
    ))

(defmethod construct-state-source-initialization (variable input-value input-type (language (eql :lisp)))
  (declare (ignore input-type))
  (list variable input-value))

(defmethod construct-state-source-initialization (variable input-value input-type (language (eql :java)))
  (binding-code-for variable input-type input-value nil language))

(defmethod find-all-state-sources ((task task-interface-mixin))
  (let ((answer nil))
    (flet ((do-one-level (sub-task)
	     (when (typep sub-task 'state-source)
	       (let ((parent-interface (abstract-task (superior sub-task)))
		     (locality (getf (properties sub-task) :locality)))
		 (when (and (superior parent-interface) (eql locality :local))
		   (setf (getf (properties parent-interface) :local-bindings) t))
		 (push (list sub-task locality) answer)))))
      (map-over-implementation-hierarchy task #'do-one-level))
    answer))

(defun find-state-sources (design)
  (loop for component in (children design)
      when (typep component 'state-source)
      collect component))

(defun find-state-sinks (design)
  (loop for component in (children design)
	when (typep component 'state-sink)
      collect component))

;;; There are three different versions here
;;; One finds only local initial-tasks
;;; One finds only global initial-tasks
;;; One finds all initial-tasks
;;; It would be nice to collapse into a single version
;;; but map-over-selected-implementation doesn't
;;; allow extrat arguments to be passed, so 3 versions for now
(defmethod find-local-initial-tasks ((task composite-task))
  (loop for task in (find-initial-tasks task)
      for task-type = (task-type task)
      when (or (and (eql task-type 'allocate) (getf (properties task) :local))
	       (and (eql task-type 'constant) (local? task))
	       (eql task-type 'input))
      collect task))

(defmethod is-global-initial-task? ((task core-task-mixin))
  (or (and (eql (task-type task) 'allocate) (null (inputs task)) (not (getf (properties task) :local)))
      (and (eql (task-type task) 'constant) (not (local? task)))))

(defmethod find-global-initial-tasks ((task composite-task))
  (loop for task in (children task)
      when (is-global-initial-task? task)
      collect task))

(defmethod find-global-initial-tasks ((task task-interface-mixin))
  (let ((answers nil))
    (flet ((process-one-level (sub-task history implementation)
             (declare (ignore sub-task history))
             (when implementation
               (let ((initial-tasks (find-global-initial-tasks implementation)))
                 (loop for initial-task in initial-tasks
                     do (push initial-task answers))))))
      (map-over-selected-implementation task #'process-one-level))
    answers))

(defmethod find-all-initial-tasks ((task composite-task))
  (loop for task in (children task)
      when (or 
            ;; an allocate can have inputs, it's only top level
            ;; when it doesn't
            (and (eql (task-type task) 'allocate) (null (inputs task)))
            (eql (task-type task) 'constant))
      collect task))

(defmethod find-all-initial-tasks ((task task-interface-mixin))
  (let ((answers nil))
    (flet ((process-one-level (sub-task history implementation)
             (declare (ignore sub-task history))
             (when implementation
               (let ((initial-tasks (find-all-initial-tasks implementation)))
                 (loop for initial-task in initial-tasks
                     do (push initial-task answers))))))
      (map-over-selected-implementation task #'process-one-level))
    answers))

(defmethod port-name ((input initial-input))
  (name (first (outputs input))))

(defmethod port-name ((constant constant-value))
  (name (first (outputs constant))))

(defmethod port-name ((output final-output))
  (name (first (inputs output))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating code
;;;  Three stages:
;;;    1) Symbolic simulation propagates symbolic values (tokens) and links
;;;       up producers and consumers through the tokens.  This is language independent
;;;    2) Code generation.  Topologically sorts the tasks by the precedence relation built
;;;       in the first stage.  Generates a syntax tree by stepping through this sorted list.
;;;       Some tasks are groupers themselves (e.g. branches, enumerators) that take over
;;;       control of the queue temporarily.  Many methods are langauge neutral, but the
;;;       leaves of the call tree are language dependent
;;;    3) Pretty print the syntax tree into source code.  For Lisp this is just #'pprint
;;;       For java its a tree walk with dispatch handlers for each node type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *non-implementable-built-ins* 
    '(take put truncate index-enumerator list-enumerator vector-length 
      vector-push vector-accessor allocate constant-value 
      state-source state-sink print))

(defun is-primitive-for-simulation (task)
  (member (task-type task) *non-implementable-built-ins*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Symbolic Tokens for Symbolic Execution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Every call to make-token for a value token must set:
;;;     Producer: What task created the token
;;;     Value: The symbolic value
;;;     Type: The type constraint on the value
;;;     Port: The port of the producer that this value is put into
;;; Every call to a control-flow token must set:
;;;     Producer


(defmacro make-token (type &rest plist)
  `(make-instance ',type
     ,@plist))

;;; This is mixed into all tokens
;;; Then we specialize into control and value tokens
(defclass symbolic-token-mixin ()
  ((producer :accessor producer :initarg :producer :initform nil)
   (branch-dependencies :initform nil :initarg :branch-dependencies :accessor branch-dependencies)
   (inputs :initarg :inputs :initform nil :accessor inputs)
   (consumers :Initform nil :initarg :consumers :accessor consumers)
   )
  )

;;; Control Tokens
(defclass control-token-mixin (symbolic-token-mixin)
  ()
  )

(defclass simple-control-token (control-token-mixin)
  ()
  )

(defclass branch-control-token  (control-token-mixin)
  ((branch :initarg :branch :accessor branch)
   (branch-condition :initarg :branch-condition :accessor branch-condition))
  )

(defclass join-control-token (control-token-mixin)
  ((branch :initarg :branch :accessor branch))
  )

(defclass entry-point-control-token (control-token-mixin)
  ()
  )

;;; Value carrying tokens

(defclass value-token-mixin (symbolic-token-mixin)
  ((value :accessor value :initarg :value)
   (port :initarg :port :initform nil :accessor port)
   ;; I'm not sure this is even necessary.  In what way is it different
   ;; from the type-constaint of the port?
   (type :accessor type-constraint :initarg :type)
   (form :Initform nil :Initarg :form :accessor form))  
  )

;;; The output of a normal compute box
;;; (e.g. vector-length)
(defclass normal-output (value-token-mixin)
  ()
  )

(defclass truncate-normal-output (normal-output)
  ()
  )

(defclass temporal-sequence-output-value (normal-output)
  ((input-token :initarg :input-token :accessor input-token))
  )

(defclass stream-output-value (normal-output)
  ((input-token :initarg :input-token :accessor input-token))
  )

;;; The output of a join
;;; this would typicall have code that's a conditional
(defclass join-symbolic-value (value-token-mixin)
  ((joined-values :initform nil :initarg :joined-values :accessor joined-values))
  )

;;; The output of an allocate statement
;;; typical code might be (make-array ...)
(defclass allocate-symbolic-value-token (value-token-mixin)
  ()
  )

;;; The value of a state-variable
(defclass state-variable-value-token (value-token-mixin)
  ()
  )

;;; A constant value
(defclass constant-value-token (value-token-mixin)
  ()
  )

(defclass bound-variable-mixin ()
  (
   ;; Apparently this instance variable serves no purpose given that every bound-variable
   ;; mixin is also a value-token-mixin and that includes a value
   ;; instance-variable
   ;; (variable-name :accessor variable-name :initarg :variable-name)
   )
  )


(defclass top-level-argument (bound-variable-mixin value-token-mixin)
  ()
  )

(defclass state-variable-initialization (value-token-mixin bound-variable-mixin)
  ((initializer :accessor initializer :initarg :initializer)
   (code :accessor code :initarg :code))
  )

(defclass stream-processor-surrogate-output (value-token-mixin bound-variable-mixin)
  ()
  )

;;; Making bound-variable-mixin come first causes the value-to-use
;;; to always use the value rather than the form of this token.
(defclass enumerator-symbolic-value-mixin (bound-variable-mixin value-token-mixin)
  ()
  )

(defclass index-enumerator-symbolic-value (enumerator-symbolic-value-mixin)
  ((lower-bound :initarg :lower-bound :accessor lower-bound)
   (upper-bound :initarg :upper-bound :accessor upper-bound))
  )

(defclass range-symbolic-value (value-token-mixin)
  ((lower-bound :accessor lower-bound :initarg :lower-bound)
   (upper-bound :accessor upper-bound :initarg :upper-bound))
  )

(defclass list-enumerator-symbolic-value (enumerator-symbolic-value-mixin)
  ((the-list :initarg :the-list :accessor the-list))
  )

(defgeneric fill-in-token (token associated-task)
  )

(defmethod initialize-instance :after ((token symbolic-token-mixin) &rest ignore &key &allow-other-keys)
  (let ((task (producer token)))
    (fill-in-token token task)))

(defmethod fill-in-token ((token t) (task t))
  (values)
  )

(defmethod fill-in-token (token (task input-side-mixin)) 
  (let ((input-tokens (loop for input in (inputs task) for token = (symbolic-token input) when token collect token))
	(branch-dependencies (task-input-branch-dependencies task)))
    (setf (inputs token) input-tokens
	  (branch-dependencies token) branch-dependencies)
    ))

(defmethod fill-in-token :before ((token value-token-mixin) (task t))
  (setf (value token) (simplified-form (value token)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Code Generation after building implementation Map
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-code (top-level-task &optional (build-map t) (language :lisp))
  (if build-map
      (produce-implementation-map top-level-task)
    (initialize-design top-level-task))
  (let* ((top-level-arguments (inputs top-level-task))
         (initial-tasks (find-all-initial-tasks top-level-task))
         (inputs (inputs top-level-task))
         (input-1 (first inputs))
         (outputs (outputs top-level-task))
         (output (first outputs))
         (streaming? (is-a-simple-stream-processor top-level-task))
         (implementation (selected-implementation top-level-task))
         (state-source-alist (find-all-state-sources top-level-task))
         (generation-controller (generation-controller (superior top-level-task)))
         )
    (declare (ignore implementation))
    (when (is-a-simple-stream-processor top-level-task)
      ;; Hack: the order of pushes matters here
      (pushnew output top-level-arguments)
      (pushnew input-1 top-level-arguments)
      )
    (propagation-stage top-level-task initial-tasks output streaming?)
    (destructuring-bind (caller . *all-tasks*) (topological-sort-sub-tasks top-level-task)
      (declare (special *all-tasks*))
      (let ((the-code (gobble-code language caller)))
        (code-for-top-level-task top-level-task top-level-arguments  
                                 the-code language
                                 ;; This form is here because you can only get this initializations after code
                                 ;; geneation has gone through (to establish all the form slots in the tokens)
                                 :state-variable-code (build-global-state-source-initializations state-source-alist language)
                                 :streaming? streaming?
                                 :generation-controller generation-controller)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is the main driver
;;; You repeatedly gobble up tasks and collect their corresponding form
;;; Some forms are groupers which themselves run the same loop and then
;;; wrap the sequence up in a binding form (e.g. a loop generator does this)
;;; Normally you just keep gobbling until you hit a dead-end, i.e. something
;;; with no downstream tasks.
;;; However, any combining task (I.e. a branch) does have downstream tasks
;;; It gobbles until it hits a dead-end and then sets *last-guy-was-a-terminator*
;;; to T to say that it's finished eating stuff and so there's nothing further down
;;; for the guy above it to gobble.
;;;
;;; A Normal task with only one consumer returns the special token *defer-token*
;;; saying that it's going to be inlined by its one consumer
;;; It also calculates its corresponding computation form and stores that in its "form" slot
;;; However, if it has multiple consumers it turns into a let binding
;;; This binds the "value" slot to the computed form and then runs a gobble loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *all-tasks* nil "The queue of tasks for code generation")

;;; If I'm a sub-gobbler, I set this in my gobbler that invoked me
;;; to say that I ran out, so it's a dead end A branching task, which
;;; sub-gobbles for each branch should reset this after each branch,
;;; but that's actually not necessary since both branches are setting
;;; this in the parent of the branching task and by the time it sees
;;; it, it's true that the branching task has exhausted the stream.
(defvar *last-guy-was-a-terminator* nil)

(defvar *defer-token* (gensym "DEFER"))

;;; Tasks are ordered by precedence. 
;;; Each tasks knows all of its upstream tasks
;;; So when processing for a caller just process forms
;;; until you hit one that doesn't have you as a member
;;; of it's all-upstream-tasks list.
;;; Caller has already been popped from *all-tasks*

(defun gobble-code (language &optional caller)
  (loop until (null *all-tasks*)
      while (member caller (all-upstream-tasks (first *all-tasks*)))
      for next-task = (pop *all-tasks*)
      for next-form = (form-for next-task (task-type next-task) language)
      unless (eql next-form *defer-token*)
      collect next-form into forms
      finally 
              (return forms))
        )

;;; just for debugging
;;; All the tracing is making this unreadable
; (defvar *gobble-indent* 0)
; (defvar *gobble-trace* nil)

; (defun gobble-code (language &optional caller)
;   (let ((answers nil)
; 	(number-of-answers 0)
; 	(*gobble-indent* (+ *gobble-indent* 4)))    
;     (let ((*last-guy-was-a-terminator* nil))
;       (when *gobble-trace*
; 	(format *error-output* "~%~vtStarting for ~a" *gobble-indent* caller))
;       (flet ((gobble-one ()
					; 	       (loop with answer = nil
; 		   until *last-guy-was-a-terminator*
; 		   for next-task = (pop *all-tasks*)
; 		   for next-form = nil
; 		   do (when *gobble-trace*
; 			(format *error-output* "~%~vtProcessing ~a" (+ *gobble-indent* 2) next-task))
; 		   do (setq next-form (form-for next-task (task-type next-task) language))
; 		   unless (eql next-form *defer-token*)
; 		   do (push next-form answer)
; 		   when (null (downstream-tasks next-task))
; 		   do (when *gobble-trace*
; 			(format *error-output* "~%~vtSpiked by for ~a" *gobble-indent* next-task))
; 		      (setq *last-guy-was-a-terminator* t)
; 		   finally (return (nreverse answer)))))
; 	(loop while *all-tasks*
; 	    for next-answer = (gobble-one)
; 	    when (not (null next-answer))
; 	    do (setq answers (nconc next-answer answers))
; 	       (incf number-of-answers)
; 	    if (and caller *all-tasks* (member caller (upstream-tasks (first *all-tasks*))))
; 	       ;; we're going to gobble again, reset the local flag
; 	    do (setq *last-guy-was-a-terminator* nil)
; 	    else do (return))))
;     ;; signal my caller that I ran out of stuff
;     (setq *last-guy-was-a-terminator* t)
;     (when *gobble-trace*
;       (format *error-output* "~%~vtEnding for ~a ~a" *gobble-indent* caller answers))
;     answers
;     ))

(defmacro sub-gobble ((variable language &optional caller)
		      &body body)
  `(let ((,variable (gobble-code ,language ,caller)))
       ,@body))

(defun is-a-simple-stream-processor (top-level-task)
  (when (typep top-level-task 'output-side-mixin)
    (let ((inputs (inputs top-level-task))
	  (outputs (outputs top-level-task)))
      (and inputs
	   outputs
	   (null (rest inputs))
	   (null (rest outputs))
	   (let ((inputs-type-constraints (port-type-constraints (first inputs)))
		 (outputs-type-constraints (port-type-constraints (first outputs))))
	     (and (null (rest inputs-type-constraints))
		  (listp (first inputs-type-constraints))
		  (eql (first (first inputs-type-constraints)) 'stream)
		  (null (rest outputs-type-constraints))
		  (listp (first outputs-type-constraints))
		  (eql (first (first outputs-type-constraints)) 'stream)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning up a design so you can regenerate code
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric initialize-design (task)
  (:method-combination progn))

(defmethod initialize-design progn ((task simulation-support-mixin))
  (setf (my-code task) nil))

(defmethod initialize-design progn ((task input-side-mixin))
  (loop for port in (inputs task)
      do (setf (symbolic-token port) nil))
  (setf (n-inputs-provided task) 0
	(ready-to-run task) nil))

(defmethod initialize-design progn ((task output-side-mixin))
  (loop for port in (outputs task)
      do (setf (symbolic-token port) nil))
  (setf (n-outputs-provided task) 0)
  (loop for cflow in (outgoing-control-flows task) 
      do (disarm-control-flow cflow))
  )

(defmethod initialize-design progn ((task has-branches-mixin))
  (loop for branch in (branches task)
      do (initialize-design branch)))

(defmethod initialize-design progn ((task has-joins-mixin))
  (loop for join in (joins task)
      do (initialize-design join)))

(defmethod initialize-design progn ((task implementation))
  (loop for component in (children task)
      do (initialize-design component)))

(defmethod initialize-design progn ((task task-interface-mixin))
  (let ((selected-implementation (selected-implementation task)))
    (when selected-implementation
      (initialize-design selected-implementation))))

(defun map-over-implementation-hierarchy (task function)
  (labels ((do-one-task (sub-task)
	     (funcall function sub-task)
	     (let ((implementation (find-selected-implementation-of sub-task)))
	       (when implementation
		 (loop for child in (children implementation)
		     do (do-one-task child))))))
    (do-one-task task)))

(defun map-upward-through-implementation-hierarchy (task function)
  (labels ((do-one-level (implementation)
	     (loop for child in (children implementation)
		 do (funcall function child))
	     (when (typep implementation 'implementation)
	       (let ((abstract-task (abstract-task implementation)))
		 (when abstract-task
		   (let ((implementation (superior abstract-task)))
		     (when implementation (do-one-level implementation))))))))
    (do-one-level (superior task))))

(defun find-component-above (initial-task predicate)
  (map-upward-through-implementation-hierarchy
   initial-task
   #'(lambda (task)
       (when (funcall predicate task)
	 (return-from find-component-above task)))))

(defun find-component-above-named (initial-task name)
  (find-component-above 
   initial-task
   #'(lambda (task) (eql (name task) name))
   ))

(defun find-ultimate-superior (initial-task)
  (find-component-above
   initial-task
   #'(lambda (task)
       (not (typep (superior task) 'implementation)))))

(defun find-sub-components-of-type (top-level task-type)
  (let ((answers nil))
    (flet  ((check-for-type-and-collect (task)
	      (when (eql (task-type task) task-type)
		(push task answers))))
      (map-over-implementation-hierarchy top-level #'check-for-type-and-collect))
    answers))

(defun affect-all-subcomponents-of-type (top-level task-type action)
  (let ((targets (find-sub-components-of-type top-level task-type)))
    (loop for target in targets
	do (funcall action target))))

(defun test (top-level task-type)
  (flet ((add-a-port (target)
	   (remove-port 'input 'path-so-far target)
	   (let ((port (add-port 'input 'path-so-far target)))
	     (add-port-type-description port '(list json-key)))
	   ))
    (affect-all-subcomponents-of-type top-level task-type #'add-a-port)))

