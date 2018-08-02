;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pass 1, symbolic simulation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note: Right now the propagation stage is computing code for state variable initializations 
;;; This shouldn't be necessary if we start off code generation with all initial tasks (i.e. those without
;;; any inputs) then these will flow into the state variable source task and the right thing will happen.
;;; E.g. an allocation is an initial task.  It flows into the state source which binds it (state sources bind
;;; everything).  Similarly for constants, or more arbitrary computations.

;; For generating unique names
(defvar *variable-counter-table* nil)

(defun next-instance-of-variable (variable-name)
  (when (listp variable-name)
    (setq variable-name (first variable-name)))
  (let ((number (gethash variable-name *variable-counter-table*)))
    (unless number
      (setq number 0))
    (incf number)
    (setf (gethash variable-name *variable-counter-table*) number)
    (intern (format nil "~a-~d" variable-name number))))

(defun propagation-stage (top-level-task initial-tasks output streaming?)
  (let ((*variable-counter-table* (make-hash-table)))
    (propagate-initial-inputs (inputs top-level-task) top-level-task)
    (propagate-initial-tasks initial-tasks)
    (when streaming?
      (let* ((output-name (name output))
             (implementation (selected-implementation top-level-task))
             (corresponding-input-port (find-corresponding-input-port implementation output-name))
             (type  (port-type-constraint output))
             (symbolic-token (make-token stream-processor-surrogate-output
                               :producer top-level-task
                               :port corresponding-input-port
                               :value output-name
                               :type type
                               )))	  
        (set-port-symbolic-token corresponding-input-port symbolic-token)))
    (values initial-tasks)
    ))

(defun propagate-initial-inputs (inputs top-level-task)
  (loop for input in inputs
      for input-name = (name input)
      for type = (port-type-constraint input)
      for symbolic-token = (make-token top-level-argument
                             :producer top-level-task
                             :port input
                             :type type
                             :value input-name
                             :variable-name input-name)
      do (set-port-symbolic-token input symbolic-token)))

(defun propagate-initial-tasks (initial-tasks)
  (loop for task in initial-tasks do (propagate task (task-type task))))


;;; This is apparently dead code?
#|
(defgeneric initialization-code-for (type task source language))

(defmethod initialization-code-for ((type (eql 'constant)) initializing-task (source t) (language t))
  (values (value initializing-task) t)
  )

(defmethod initialization-code-for ((type (eql 'allocate)) initializing-task (source t) (language t))
  (let* ((port (first (outputs initializing-task)))
         (type-constraint (port-type-constraint port))
         (type-constraint-definition (is-definition-for type-constraint)))
    (multiple-value-bind (allocation-code allocation-code-exists) (allocation-code type-constraint language)
      (multiple-value-bind (definition-allocation-code definition-allocation-code-exists) (allocation-code type-constraint-definition language)
        (values (or allocation-code definition-allocation-code)
                (or allocation-code-exists definition-allocation-code-exists))))))

(defmethod initialization-code-for ((type t) (initializing-task core-task-mixin) (source t) (language t))
  (loop for output in (outputs initializing-task)
      do (loop for outgoing-flow in (outgoing-flows output)
	     for destination-port = (output outgoing-flow)
	     for destination-task = (task destination-port)
	     when (eql destination-task source)
	     do (let ((token (symbolic-token output)))
		  (when token
		    (return-from initialization-code-for (values (form token) t)))))))

|#

(defmethod program-outputs ((task task-interface))
  (loop for output in (outputs task)
      for token = (symbolic-token output)
      collect (or (form token) (value token))))

(defmethod program-outputs ((task branching-task))
  (loop for branch in (branches task)
      append (loop for output in (outputs branch)
                 for token = (symbolic-token output)
                 collect (value token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Propagating through ports
;;;  specific port-value-arrived methods in each area of concern
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric port-value-arrived (port port-direction task value))

(defmethod set-port-symbolic-token ((p port) value)
  (setf (symbolic-token p) value)
  ;; what happens depends on the direction of the port
  ;; and the type of the parent 
  (port-value-arrived p (direction p) (task p) value))

(defun simplified-form (expression)
  (ask `[viewpoint-equivalence ,expression ?equivalent]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from simplified-form (copy-object-if-necessary ?equivalent))))
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  There are 5 steps in the processing of each component:
;;;  1) Notice that an input has arrived
;;;  2) Notice all inputs are present and that all control flows are armed
;;;     In which case the task is ready to run
;;;  3) Run the task
;;;     For primitive? = T tasks there are specific propagate methods
;;;     For tasks with implementations, propagate inputs down to the internals of the implementation
;;;  5) Noticing that an output has arrived from simulation of an implementation
;;;  4) Noticing that all outputs are present and propagating the output values forward
;;;     For implementations this means lifting the outputs from the implementation to the interface
;;;     For both types it then means moving the outputs of the interface forward on dataflows
;;;      and when all outputs are ready arming the control-flows.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Input Side Handling
;;;
;;; Note: A Join is of type input-side-mixin, so this will trigger when all inputs
;;;       and control flows of the join are present, call Run-Task on the join
;;;       The distinctive features of joins are all handled there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric consumes-input? (task task-type input))

(defmethod consumes-input? ((task core-task-mixin) (task-type t) (value t))
  t)

;;; for a task-interface normally it just passes the token to
;;; its initial inputs so it doesn't consume the input.
;;; However, if it's primitive, marked not to expand, or 
;;; is going to be introduced as an internal function
;;; then it does consume the inputs

(defmethod consumes-input? ((task task-interface-mixin) (task-type t) (value t))
  (or (primitive? task)
      (dont-expand task)
      (getf (properties task) :introduce-labels)
      (getf (Properties task) :local-bindings)))


;;; Initial object are entry-points, initial-inputs and constants
;;; none of these consume the token
(defmethod consumes-input? ((task initial-object-mixin) (task-type t) (value t)) nil)
;;; Final objects are exit-points, final-outputs and state-sinks
;;; the first two just pass the token up to the parent
(defmethod consumes-input? ((task final-object-mixin) (task-type t) (value t)) nil) 
;;; a State sink consumes the input
(defmethod consumes-input? ((task state-sink) (task-type t) (value t)) t)

(defmethod consumes-input? ((task task-interface-mixin) (task-type (eql 'take)) (value value-token-mixin)) nil)
(defmethod consumes-input? ((task task-interface-mixin) (task-type (eql 'take)) (value temporal-sequence-output-value))  t)
(defmethod consumes-input? ((task task-interface-mixin) (task-type (eql 'take)) (value join-symbolic-value))  t)

(defmethod consumes-input? ((task task-interface-mixin) (task-type (eql 'put)) (value t))
  (let* ((sequence-port (port-named 'input 'sequence task))
	 (sequence-type (port-type-constraint sequence-port)))
    (if (and (listp sequence-type) (eql (first sequence-type) 'temporal-sequence))
	nil
      t)))

;;; Note: The Entry-Point and Final-Output classes mix in core-task-mixin, so this applies to its input ports
(defmethod port-value-arrived ((p port) (direction (eql 'input)) (task core-task-mixin) value)
  ;; (when (task-is-real-computation task)
  (when (consumes-input? task (task-type task) value) (push task (consumers value)))
  (input-provided task p))

(defmethod disarm-control-flow ((cflow control-flow))
  (setf (armer cflow) nil
	(arming-token cflow) nil))

(defmethod arm-control-flow ((cflow control-flow) value &optional setter)
  (let ((already-armed? (arming-token cflow)))
    (when (and value already-armed?)
      (error "Arming already armed control flow ~a" cflow))
    (setf (arming-token cflow) value)
    (when value
      (setf (armer cflow) setter)
      (notice-control-flow-armed (successor cflow)))))

;;; Note: The Exit-Point and Final-Output classes mix in input-side-mixin and core-task-mixin
;;; So as values propagate to it, it will do the two things
;;; below and then decide that it's ready to run
;;; So for output handling of an implementation that terminates
;;; in branches we just need to wait until the branch is ready
;;; to run and then propagate its values upward, triggering the
;;; corresponding Branch to complete execution.
;;; For normal-tasks we can let each final-output do its run-task method
;;; to propagate a value upward.

(defmethod input-provided ((task input-side-mixin) (p port))
  (incf (n-inputs-provided task))
  (check-if-task-is-ready task)
  (when (ready-to-run task)
    (run-task task)))

(defmethod notice-control-flow-armed ((task input-side-mixin))
  (check-if-task-is-ready task)
  (when (ready-to-run task)
    (run-task task)))

(defmethod check-if-task-is-ready ((task input-side-mixin))
  (when (and (= (n-inputs-provided task) (n-inputs task))
             (loop for cflow in (incoming-control-flows task)
                 always (arming-token cflow)))
    (setf (ready-to-run task) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Running the task
;;;  there are some special cases here:
;;;  1) The task is primitive, it has no implementation
;;;     In this case, there has to be a propogate method that carries out the simulation
;;;     propagate takes a second argument, the task-type which it dispatches on
;;;  2) The task is a Join (not a joining-task, no input ever arrives to the joining-task, only to it's brqnches)
;;;     There are now two cases:
;;;     a) The parent of the join is primitive, in which case you just move the values to their corresponding output ports
;;;     b) The parent of the join is not primitive, in which case you find the corresponding entry-point in the implementation
;;;        and propagate values down to the corresponding output-port of the entry-point
;;;  3) The task has an implementation
;;;      In chich case you propagate the values down to the output-ports of the corresponding initial-input
;;;  4) The task is a branching-task
;;;     There are two cases:
;;;     1) The task is primitive.  
;;;        In which case you do each branch in turn, arming control flows and collect the code for each
;;;           there will be no output ports on the branches, because a primitive branch just branches.  If it
;;;           wanted to compute outputs it would have an implementation to do so
;;;        Then you build the appropriate Cond (when, if, ...)
;;;     2) The task has an implementation
;;;        In this case, you treat things normally
;;;        inside the implementation somewhere there will be a primitive branching-task which will deal with the
;;;        splitting of control-flow.
;;;        On its output side it will have exit-points that match up with the output branches
;;;           and their inputs will match up with the outputs of the branch.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ToDo: if the task-type of state-source and state-sink were state-source and state-sink, not state
;;; then we wouldn't need these two methods

(defmethod run-task :around ((task simulation-support-mixin))
  (setf (execution-started task) t)
  (call-next-method)
  (setf (execution-finished task) t)
  )


(defmethod run-task ((task state-source))
  (propagate task 'state-source))

(defmethod run-task ((task state-sink))
  (propagate task 'state-sink))

(defmethod run-task :around ((task task-interface-mixin))
  (if (primitive? task)
      (propagate task (task-type task))
    (call-next-method)))

(defmethod run-task ((task primitive-joining-task))
  (propagate task (task-type task)))

(defmethod run-task ((task primitive-branching-task))
  (propagate task (task-type task)))

;;; A Path-end really is the end, do nothing
(defmethod run-task ((task path-end)) ())


;;; so we only get here if it's supposed to have an implementation
;;; this would include branching-tasks because they just propagate
;;; values to the implementation like anything else.
;;; They propagate values up to the interface in a slightly different manner
;;; Branching tasks propagate up from ports of exit-points
;;; Other tasks with implementations propagate up from the port of a final-value

(defmethod run-task ((task task-interface-mixin))
  (let ((selected-implementation (selected-implementation task)))
    (cond
     ;; I don't think we'll get here anymore because of the :around method above
     ;; that's assuming the primitive? is true for anything that is-primitive-for-simulation
     ((is-primitive-for-simulation task)
      (break "I thought that you couldn't get here")
      (propagate task (task-type task)))
     ;; the task isn't really
     ;; primitive, but we want to treat it as such.  Either because we
     ;; want to generate an external call rather than inlining or
     ;; because it's a recursive invocation
     ((dont-expand task)
      (propagate task (task-type task)))
     (t
      (propagate-values-to-selected-implementation task selected-implementation)
      ;; There's something like this on the propagate method for task-interface
      ;; but in this case we never call propagate
      (when (and (null (outputs task))
		 (not (typep task 'branching-task)))
        (arm-task-control-flows task))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Moving values into the implmentation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I'm just going to pass the token down, not create a new one as was done before.
;;; When the token arrives at the parent task, it's added as a consumer as well.
;;; In the topological sort we'll make every child have its abstract task as a predecessor.

;;;(defmethod propagate-values-to-selected-implementation ((task input-side-mixin) implementation)
;;;  (loop for input-port in (inputs task)
;;;      for input-token = (symbolic-token input-port)
;;;      for name = (name input-port)
;;;      for corresponding-port = (find-corresponding-input-port implementation name)
;;;      do (set-port-symbolic-token corresponding-port input-token)))

(defmethod propagate-values-to-selected-implementation ((task input-side-mixin) implementation)
  (let* ((real-task (typecase task (join (superior task)) (otherwise task)))
         (real-implementation (typecase implementation (entry-point (superior implementation)) (otherwise implementation)))
         ;; If this is going to become an internal function
         ;; it's treated differently.  It introduces a new bound variable
         ;; to propagate to its child.  Otherwise we just push
         ;; the token along.
         ;; Similarly if it has local bindings we want the task to show up
         ;; in the code-generation stream.
         (has-local-bindings (when (typep task 'has-properties-mixin) (getf (properties task) :local-bindings)))
         (introduce-internal-function  (when (typep task 'has-properties-mixin) (getf (properties real-task) :introduce-labels))))
    (loop for input-port in (inputs task)
        for name = (name input-port)
        for type = (port-type-constraint input-port)
        for token = (symbolic-token input-port)
        for value = (value token)
        for corresponding-port = (find-corresponding-input-port implementation name)
        for initial-input = (task corresponding-port)
        for viewpoint-to-apply = (viewpoint-to-apply real-task real-implementation name)
        when viewpoint-to-apply 
        do (setf (value token) (list viewpoint-to-apply value)
                 ;; here the form slot is used to hold the original value
                 (form token) value)
        unless corresponding-port do (error "No corresponding port ~a ~a" task name)
        do (setf (corresponding-port corresponding-port) input-port
                 (corresponding-port input-port) initial-input)
           (cond
            (has-local-bindings
             ;; see comment above
             (let ((symbolic-token (make-token normal-output
                                     :producer task
                                     :port input-port
                                     :type type
                                     :value value
                                     )))
               (set-port-symbolic-token corresponding-port symbolic-token)))
            (introduce-internal-function
             ;; see comment above
             (let ((symbolic-token (make-token top-level-argument
                                     :producer task
                                     :port input-port
                                     :type type
                                     :value name
                                     :variable-name name
                                     )))
               (set-port-symbolic-token corresponding-port symbolic-token)))
            (t ;; just push the token along unchanged
             (set-port-symbolic-token corresponding-port token))))))

(defmethod propagate-values-to-selected-implementation :before ((task join) (implementation entry-point))
  (setf (corresponding-join implementation) task)
  )

;;; ?choices isn't being set?'
(defun viewpoint-to-apply (task implementation port-name)
  (ask `[and [selected-implementation ,implementation ,task ?choices]
	     [viewpoint-applied ,task ,port-name ?viewpoint ?verb ?object ?type-constraint]
	     ]
       #'(lambda (just)
	   (declare (ignore just))
	   (loop for (choice-type name) in ?choices
	       when (and (eql choice-type 'viewpoint)
			 (eql name ?viewpoint))
	       do (return-from viewpoint-to-apply ?viewpoint))))
  nil)

;;; Here there are two methods: 
;;;  One for a normal implementation where you look for Initial-inputs
;;;  Another for an entry point where you seach its output ports
(defmethod find-corresponding-input-port ((implementation implementation) name)
  (loop for thing in (children implementation)
      when (and (or (typep thing 'initial-input) (typep thing 'entry-point))
		(eql (port-name thing) name))
      return (first (outputs thing))))

(defmethod find-corresponding-input-port ((entry-point entry-point) name)
  (port-named 'output name entry-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Joining Tasks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod run-task ((join join))
  (let ((task (superior join)))
    (if (primitive? task)
	;; only run this guys when all the joins
	;; are good to go.
	(when (loop for join in (joins task)
		  always (ready-to-run join))
	  (run-primitive-joining-task task join))
      (run-compound-joining-task task join))))

;;; The properties of a joining-task has a field :corresponding-branch
;;; whose value is a symbol.  Defcliche doesn't (yet) turn this into a reference
;;; to actual branching-task, so we do that here.
(defmethod link-up-corresponding-branch ((task has-joins-mixin))
  (let* ((branch-name (getf (properties task) :corresponding-branch))
         (superior (superior task))
         (branching-task (child-named superior branch-name)))
    (setf (corresponding-branch task) branching-task)))

(defmethod run-compound-joining-task ((task joining-task) (join join))
  (link-up-corresponding-branch task)
  (let* ((implementation (selected-implementation task))
         (join-name (name join))
         (corresponding-entry-point (loop for component in (children implementation)
                                        when (and (typep component 'entry-point)
                                                  (eql (join-name component) join-name))
                                        return (values component))))
    (cond
     ((not (null corresponding-entry-point))
      (setf (corresponding-entry-point join) corresponding-entry-point)
      (propagate-values-to-selected-implementation join corresponding-entry-point)
      (when (null (outputs corresponding-entry-point))
        ;; if there aren't any outputs of the entry-point, nothing will cause
        ;; it to wake up and propagate its control flows
        (arm-task-control-flows corresponding-entry-point)))
     (t
      (error "No entry point corresponds to ~a of ~a" join task)))))

;; For each value should we figure out the branch dependencies now or wait to code generation time?
;; FixMe: So it appears that we need to deal with this finally
;;  when propagating values 
;; Needs to arm control flows as well
(defmethod run-primitive-joining-task ((task primitive-joining-task) (join join))
  (link-up-corresponding-branch task)
  (let ((joins (joins task))
	(outputs (outputs task)))
    (when (loop for join in (joins task) always (ready-to-run join))
      ;; build a symbolic value for each output that tracks each of the joined inputs
      (if outputs
	  (loop for output in outputs
	      for name = (name output)
			 ;; This should include the token not the value
			 ;; that way later on we can do (value-to-use the-token)
	      for corresponding-input-values = (loop for join in joins
						   for corresponding-input-port = (port-named 'input name join)
						   for his-token = (symbolic-token corresponding-input-port)
						   collect (cons join his-token))
	      for symbolic-output = (make-token join-symbolic-value
				      :port output
				      :producer task
				      ;; Fix Me: This is a total hack based on 
				      ;; the assumption that all values are the same
				      :value corresponding-input-values
				      :joined-values corresponding-input-values
				      :type (port-type-constraint output)
				      )
	      do (set-port-symbolic-token output symbolic-output))
	;; if there were outputs setting all of them would cause the 
	;; the task-has-completed-execution method to run which would arm
	;; the control-flows.  But if there are no outputs
	;; that wouldn't happen so this is required.
	(arm-task-control-flows task))
      )))

(defun dejoined-value (form active-joins)
  (labels ((one-level (sub-form)
	     (cond
	      ((atom sub-form) sub-form)
	      ((and (consp (first sub-form)) (typep (first (first sub-form)) 'join))
	       (loop for join in active-joins
		   for the-guy = (cdr (assoc join sub-form))
		   when the-guy return (one-level the-guy)))
	      (t (loop for thing in sub-form
		     collect (one-level thing))))))
    (one-level form)))

(defmethod task-input-branch-dependencies ((task input-side-mixin))
  (let ((dependencies nil))
    (loop for input-port in (inputs task)
        for token = (symbolic-token input-port)
        when token 
        do (loop for branch in (branch-dependencies token)
               do (pushnew branch dependencies)))
    (loop for cflow in (incoming-control-flows task)
        for armer = (arming-token cflow)
        when armer
        do (loop for branch in (branch-dependencies armer)
               do (pushnew branch dependencies)))
    dependencies))

(defmethod task-input-branch-dependencies ((entry-point entry-point))
  (let ((corresponding-join (corresponding-join entry-point)))
    (task-input-branch-dependencies corresponding-join)))

(defmethod task-input-branch-dependencies ((initial-input initial-input))
  (values)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Primitive Propagation Methods
;;;
;;;  Each "Propagate" step corresponds to executing a PRIMITIVE component of the flow graph
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric propagate (task task-type))

(defmethod propagate ((task state-source) (type (eql 'state-source)))
  (let* ((state-variable-name (state-name task))
         (input-port (first (inputs task)))
         (input-token (symbolic-token input-port))
         (output-port (first (outputs task)))
         (output-token (make-token state-variable-value-token
                         :producer task
                         :port output-port
                         :value state-variable-name
                         :type (type-constraint input-token))))
    (set-port-symbolic-token output-port output-token)))


(defmethod propagate ((task state-sink) (type (eql 'state-sink)))
  (let* ((state-variable-name (state-name task)) 
         (input-port (first (inputs task)))
         (input-token (symbolic-token input-port))
         (output-port (first (outputs task)))
         (output-token (make-token state-variable-value-token
                         :producer task
                         :port output-port
                         :value state-variable-name
                         :type (type-constraint input-token))))
    (set-port-symbolic-token output-port output-token)))

(defmethod propagate ((task constant-value) (type (eql 'constant)))
  (let* ((value (value task))
         (type (type task))
         (output-port (first (outputs task)))
         (output-token (make-token constant-value-token
                         :producer task
                         :port output-port
                         :value value
                         :type type)))
    (set-port-symbolic-token output-port output-token)))
		      
;;; Note: This is apparently redundant with the task-has-completed-execution method
;;; on output-side-mixin which also propagates control flow
;;; BUT: If the task has no outputs, then that method never runs
;;; so we need to do this.
;;;
;;; Note task-interface isn't the same as task-interface-mixin
;;; Compound Branches and Joins include task-interface-mixin
;;; but won't run this method.
;;; Primitive branches and joins don't even include task-interface-mixin
;;; so they don't run this.
(defmethod propagate :after ((task task-interface) (type t))
  (when (null (outputs task))
    (arm-task-control-flows task)))

(defmethod multiple-users? ((port port))
  (not (null (rest (outgoing-flows port)))))

;;; All allocations in the current set of cliche's flow into 
;;; exactly one consumer, but that's just an accident.
;;; So these should introduce a bound variable and defer
;;; the form creation until code generation time

(defmethod propagate ((task task-interface) (type (eql 'allocate)))
  (let* ((output (first (outputs task)))
         (type (port-type-constraint output))
         (variable-name (next-instance-of-variable type))
         (new-value (make-token allocate-symbolic-value-token
                      :port output
                      :producer task
                      :type type
                      :value variable-name
                      )))
    (set-port-symbolic-token output new-value)))

;;; Fix: This still seems like a bad hack
;;; Should the modeling include an "Allocation" primitive
(defun allocation-code (type language)
  (when (listp type) (setq type (first type)))
  (ask `[allocation-code ,type ?code ,language]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from allocation-code (values ?code t)))))

(defmethod propagate ((task task-interface) (type (eql 'add-to-set)))
  (let* ((the-set-port (port-named 'input 'the-set task))
	 (the-set-token (symbolic-token the-set-port))
	 (the-set (value the-set-token))
	 (updated-set-port (port-named 'output 'the-set task))
	 (symbolic-value (make-token normal-output
			   :producer task
			   :port updated-set-port
			   :type (port-type-constraint the-set-port)
			   :value the-set
			   )))
    (set-port-symbolic-token updated-set-port symbolic-value)))

(defmethod propagate ((task task-interface) (type (eql 'vector-push)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector (value vector-token))
	 (output (first (outputs task)))
	 (symbolic-value (make-token normal-output
			   :producer task
			   :port output
			   :type (port-type-constraint output)
			   :value vector
			   )))
    (set-port-symbolic-token output symbolic-value)))

;;; Put is an enqueue if the output is a stream
;;; If the output is a sequence there's no code emitted
;;; But in either case we have to package up the output 
;;; with the correct typing information.
;;; It takes a value as input
;;; And its output is the stream or sequence (or possibly a join that connects to the stream or sequence)

(defmethod propagate ((task task-interface) (type (eql 'put)))
  (let* ((data-input (port-named 'input 'data task))
	 (data-input-token (symbolic-token data-input))
	 (data-input-type (type-constraint data-input-token))
	 (data-input-form (form data-input-token))	 
	 (data-input-value (value data-input-token))
	 ;; (abstract-statement? (is-abstract-statement? data-input-form))
	 (output-value (or data-input-form data-input-value))
	 (stream-input (port-named 'input 'sequence task))
	 (stream-input-token (symbolic-token stream-input))
	 (stream-input-value (value stream-input-token))
	 (stream-input-type-constraint (port-type-constraint stream-input))
	 (output (first (outputs task)))
	 (output-is-stream? (is-of-type? stream-input-type-constraint 'stream))
	 (output-token (if output-is-stream? 
			   (make-token normal-output
			     :producer task
			     :port output
			     :type `(stream ,data-input-type)
			     :value stream-input-value)
			 (make-token temporal-sequence-output-value
			   :producer task
			   :port output
			   :type `(temporal-sequence ,data-input-type)
			   :value output-value
			   :input-token data-input-token)
			 )))
    (set-port-symbolic-token output output-token)))

;;; This appears to be dead code at the moment
;;;(defun put-port-destination (port)
;;;  (labels ((follow-to-next-port (port)
;;;	     (if (null (outgoing-flows port))
;;;		 (name port)
;;;	       (let* ((dataflow (first (outgoing-flows port)))
;;;		      (the-next-guys-port (output dataflow))
;;;		      (his-name (name the-next-guys-port))
;;;		      (his-owner (task the-next-guys-port)))
;;;		 (typecase his-owner
;;;		   (exit-point his-name)
;;;		   (final-output (let* ((his-parent (abstract-task (superior his-owner)))
;;;					(corrsponding-port (port-named 'output his-name his-parent)))
;;;				   (follow-to-next-port corrsponding-port)))
;;;		   (join (let* ((joining-task (superior his-owner))
;;;				(his-output (port-named 'output his-name joining-task)))
;;;			   (follow-to-next-port his-output)))
;;;		   (otherwise (error "Put not connected to something reasonable ~a" his-owner)))))))
;;;    (follow-to-next-port port)))

			  
;;; Take can be getting data from either an enumeration
;;; or a real stream.
;;; In either case, the guy it's getting it from has created a bound
;;; variable.  So we just pass that along
;;; During code generation we have to distinguish
;;; the two cases.  For a sequence we need to emit a dequeue and bind
;;; it if there are multiple-users.

(defmethod detemporalize-token ((token t)) (values token nil))
(defmethod detemporalize-token ((token temporal-sequence-output-value)) (values (input-token token) nil))
(defmethod detemporalize-token ((token enumerator-symbolic-value-mixin)) (values (enumeration-token token) nil))
;;; this may be completely wrong
(defmethod detemporalize-token ((token stream-output-value)) (values (input-token token) nil))
(defmethod detemporalize-token ((input-token join-symbolic-value))
  (values
   (make-token join-symbolic-value
     :value (loop for (join . token) in (value input-token)
		collect (cons join (detemporalize-token token))))
   t))



(defmethod propagate ((task task-interface) (type (eql 'take)))
  (let* ((input (first (inputs task)))
	 (input-token (symbolic-token input))
	 (output (first (outputs task))))
    ;; If the token is of type temporal-sequence-output-value
    ;; then it's the result of a put into a temporal sequence
    ;; which we're now pulling out, i.e. it's a noop.  In that case,
    ;; just pass the token on.
    ;; if the token comes from an enumerator there's nothing to do
    ;; if from a sequence then we need to create a new variable
    ;; (or a dequeue form)
    (multiple-value-bind (new-token need-updating) (detemporalize-token input-token)
      (when need-updating
	(setf (producer new-token) task
	      (port new-token) output))      
      (set-port-symbolic-token output new-token)
    )))

(defmethod propagate ((task task-interface) (type (eql 'truncate)))
  (let* ((input (first (inputs task)))
         (input-token (symbolic-token input))
         (output (first (outputs task)))
         (input-type (type-constraint input-token)))
    ;; if the token comes from an enumerator there's nothing to do
    ;; if from a sequence then we need to create a new variable
    ;; (or a dequeue form)
    (let ((source (producer input-token)))
      (when (typep source 'primitive-enumerator)
        (setf (abnormal-exit? source) t)))
    (let ((new-variable (make-token truncate-normal-output
                          :value (value input-token)
                          :port output
                          :producer task
                          ;; The input type is either (sequence <xxx.) or (stream <xxx>)
                          ;; so this gives us xxx
                          :type input-type
                          )))
      (set-port-symbolic-token output new-variable))
    ))

(defmethod propagate ((task task-interface) (type (eql 'range-constructor)))
  (let* ((lower-bound-port (port-named 'input 'lower-bound task))
	 (lower-bound-token (symbolic-token lower-bound-port))
	 (lower-bound-value (value lower-bound-token))
	 (upper-bound-port (port-named 'input 'upper-bound task))
	 (upper-bound-token (symbolic-token upper-bound-port))
	 (upper-bound-value (value upper-bound-token))
	 (output-port (port-named 'output 'the-range task))
	 (output-token (make-token range-symbolic-value
			 :value 'index
			 :port output-port
			 :producer task
			 :lower-bound lower-bound-value
			 :upper-bound upper-bound-value))
	 )
    (set-port-symbolic-token output-port output-token)
    ))

(defmethod propagate ((task task-interface) (type (eql 'place-constructor)))
  (let* ((container-port (port-named 'input 'container task))
	 (container-token (symbolic-token container-port))
	 (container-value (value container-token))
	 (container-type (type-constraint container-token))
	 (offset-port (port-named 'input 'offset task))
	 (offset-token (symbolic-token offset-port))
	 (offset-value (value offset-token))
	 (offset-type (type-constraint offset-token))
	 (output-port (port-named 'output 'the-place task))
	 (output-token (make-token place-symbolic-value
			 :value 'place	;??
			 :port output-port
			 :producer task
			 :offset offset-value
			 :type `(place ,container-type ,offset-type)
			 :container container-value))
	 )
    (set-port-symbolic-token output-port output-token)
    ))



;;; Note: These methods only run on the primitive ENUMERATOR branching task
;;; ENUMERATOR is a primitive generator so it should branch just as complex generators do.
;;; So this has to propagate a value on the MORE branch and collect that code
;;; the it has to propagate on the EMPTY branch (where there are no ports)
;;; and if there's anything that comes back, introduce a FINALLY clause

(defmethod propagate ((task primitive-enumerator) (type (eql 'index-enumerator)))
  (let* ((input (port-named 'input 'the-set task))
         (input-value-token (symbolic-token input))
         (lower-bound (lower-bound input-value-token))
         (upper-bound (upper-bound input-value-token))
         ;; (multiple-consumers (rest (consumers input-value-token)))
         ;; (input-value (if multiple-consumers (value input-value-token) (form input-value-token)))
         (more-branch (branch-named 'more task))
         (empty-branch (branch-named 'empty task))
         ;; maybe this should fetch the branch by name?
         (output (first (outputs more-branch)))
         (new-index (next-instance-of-variable 'index))
	 (enumeration-token (make-token normal-output
			:Producer more-branch
			:port output
			:type 'integer
			:value new-index))
         (output-value (make-token index-enumerator-symbolic-value
                         :producer more-branch
                         :port output
                         :type '(temporal-sequence integer)
                         :lower-bound lower-bound
                         :upper-bound upper-bound
                         :value new-index
			 :enumeration-token enumeration-token
                         )))
    ;; Doing this will trigger the task-has-completed execution method
    ;; on the branch which will arm the control flows.
    (set-port-symbolic-token output output-value)
    ;; because no outputs on this branch task-has-completed-execution won't get
    ;; called so
    ;; you must call arm-task-control-flows
    (arm-task-control-flows empty-branch)
    ))

(defmethod propagate ((task primitive-enumerator) (type (eql 'list-enumerator)))
  (let* ((input (first (inputs task)))
         (input-type-constraint (port-type-constraint input))
         (more-branch (branch-named 'more task))
         (empty-branch (branch-named 'empty task))
         ;; maybe this should fetch the branch by name?
         (output (first (outputs more-branch)))
         (new-element (next-instance-of-variable 'list-element))
         (output-value (make-token list-enumerator-symbolic-value
                         :producer more-branch 
                         :port output
                         :value new-element
                         ;; this should be a sequence of 
                         ;; the type of the list-elements
                         :type `(temporal-sequence ,(if (listp input-type-constraint) (second input-type-constraint) input-type-constraint))
                         )))
    ;; see comment in the previous message
    (set-port-symbolic-token output output-value)
    (arm-task-control-flows empty-branch)
    ))

(defmethod propagate ((task task-interface) (type (eql 'add))) (propagate-numeric-op task 'sum))
(defmethod propagate ((task task-interface) (type (eql 'subtract))) (propagate-numeric-op task 'difference))
(defmethod propagate ((task task-interface) (type (eql 'multiply))) (propagate-numeric-op task 'proudct))
(defmethod propagate ((task task-interface) (type (eql 'divide))) (propagate-numeric-op task 'quotient))
(defmethod propagate ((task task-interface) (type (eql 'count))) (propagate-numeric-op task 'count))

(defun propagate-numeric-op (task output-name)
  (let* ((input-1 (port-named 'input 'i-1 task))
	 (output-port (port-named 'output output-name task))
	 (output-value (next-instance-of-variable output-name))
	 (output-value (make-token normal-output
			 :producer task
			 :port output-port
			 :type (port-type-constraint input-1)
			 :value output-value
			 )))
    (set-port-symbolic-token output-port output-value)
    ))

;;; This is the code for a vanilla primitive branching task
;;; that simply makes a decision and doesn't produce outputs
;;; The idea here is that we'll propagate down all branches
;;; Each branch will add itself to the token so that we 
;;; can tell which control path we're on
;;;

(defmethod propagate :before ((task has-branches-mixin) type-of-branching-task)
  (declare (ignore type-of-branching-task))
  ;; it's harmless but doing this for a :dont-expand task is probably meaningless
  (when (or (getf (properties task) :dont-expand) (typep task 'primitive-branching-task))
    (let ((sublist nil))
      (loop for input in (inputs task)
          for name = (name input)
          for token = (symbolic-token input)
          do (push (cons name token) sublist))
      (loop for branch in (branches task)
          do (setf (substituted-branch-condition branch) (sublis sublist (branch-condition branch))))
      )))


;;; The default method for a test (anything other than a test needs its own methods)
;;; just arms the control flows
(defmethod propagate ((task primitive-branching-task) (type t))
  (loop for branch in (branches task)
      for outputs = (outputs branch)
      if outputs
      do (loop for output-port in outputs
             for output-type = (port-type-constraint output-port)
             for new-variable = (if (listp output-type) 
                                    (next-instance-of-variable (second output-type))
                                  (next-instance-of-variable output-type))
             for output-token = (make-token normal-output
                                  :producer branch
                                  :port output-port
                                  :type output-type
                                  :value new-variable)
             do (set-port-symbolic-token output-port output-token))
         ;; if there are no outputs then the task completion won't get trigger
         ;; and the control flows won't get armed.
      else do (arm-task-control-flows branch)))
  

;;; Type-splits output the input again on each branch so that we can 
;;; specialize the type.  So we need to propagate the token.
;;; This will cause each branch to run its task-has-completed-exeecution method
;;; which will trigger the control flows to arm
(defmethod propagate ((task primitive-branching-task) (type (eql 'type-split)))
  (let* ((input-port (first (inputs task)))
         (input-token (symbolic-token input-port))
         (input-value (value input-token)))
    (loop for branch in (branches task)
        for output-port = (first (outputs branch))
        for type-description = (port-type-constraint output-port)
        for value-token = (make-token normal-output
                            :producer branch
                            :port output-port
                            :type type-description
                            :value input-value)
        do (set-port-symbolic-token output-port value-token))))

(defmethod propagate ((task task-interface) (type (eql 'vector-length)))
  (let* ((output (first (outputs task)))
	 (new-variable (next-instance-of-variable 'length))
	 (token (make-token normal-output
		  :producer task
		  :port output
		  :type 'integer
		  :value new-variable
		  )))
      (set-port-symbolic-token output token)
      ))

;;; FixMe: Simplifying (element ...) should turn to aref if it's not otherwise
;;; simplified
(defmethod propagate ((task task-interface) (type (eql 'vector-accessor)))
  (let* ((output-port (port-named 'output 'the-element task))
	 (output-type (port-type-constraint output-port))
	 (output (first (outputs task)))
	 ;; (multiple-users (not (null (rest (outgoing-flows output)))))
	 (new-variable (next-instance-of-variable output-type))
	 (token (make-token normal-output
		  :producer task
		  :port output
		  :type output-type
		  :value new-variable
		  )))
    (set-port-symbolic-token output token)))


(defmethod propagate ((task task-interface) (type (eql 'print)))
  ;; nothing to do, no outputs
  (values))


;;; This is a default method for a simple 1-input 1-output
;;; primitive such as left, right
(defmethod propagate ((task task-interface) (type t))
  (let ((input-ports (inputs task))
	(output-ports (outputs task)))
    (when (or (null input-ports)
	      (and (rest input-ports) output-ports)
	      ;; (null output-ports)
	      (rest output-ports))
      (break "You shouldn't be able to get here, probably missing a more specific propagate method"))
    (let* ((output-port (first output-ports)))
      (when output-port
	(let* ((output-type (port-type-constraint output-port))
	       (new-variable (next-instance-of-variable (if (listp output-type) (second output-type) output-type)))
	       (output-token (make-token normal-output
			       :producer task
			       :port output-port
			       :type output-type
			       :value new-variable)))
	  (set-port-symbolic-token output-port output-token))))))

(defmethod propagate ((task branching-task) (type t))
  ;; Probably for a :dont-expand task we don't want to do
  ;; this.  More specific versions would understand what the 
  ;; output ports would mean, but for the generic case that's undefined
  (loop for branch in (branches task)
      for outputs = (outputs branch)
      if outputs
      do (loop for output-port in outputs
	     for output-type = (port-type-constraint output-port)
	     for new-variable = (if (listp output-type) 
				    (next-instance-of-variable (second output-type))
				  (next-instance-of-variable output-type))
	     for output-token = (make-token normal-output
				  :producer task
				  :port output-port
				  :type output-type
				  :value new-variable)
	     do (set-port-symbolic-token output-port output-token))
	 ;; need to do this because if there are no outputs the branch is never triggered into 
	 ;; the "ready to run" state
      else do (arm-task-control-flows branch)
	 ))
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lift up values from final-outputs and Exit-Points of the implementation to the abstract interface
;;; 
;;; When all the inputs to a final-output or Exit-Point are present
;;; Run-task will get called on the final-output or exit-point
;;; Each will propagate the values up to the parent-interface (or its branch for exit-points)
;;; Which will then notice that they have completed execution
;;; Note: A non-branching task whose implementation has no final-outputs will never finish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod run-task ((exit-point exit-point))
  (let ((task (superior exit-point))
	(branch-name (branch-name exit-point)))
    (typecase task
      (implementation
       (let* ((abstract-task (abstract-task task))
	      (branch (branch-named branch-name abstract-task)))
	 (setf (corresponding-branch exit-point) branch
	       (corresponding-exit-point branch) exit-point)
	 (loop for exit-point-port in (inputs exit-point)
	     for his-token = (symbolic-token exit-point-port)
	     for port-name = (name exit-point-port)
	     for branch-port = (port-named 'output port-name branch)
			       ;; it's possible that this guy produces outputs that the 
			       ;; abstract tasks doesn't care about.
	     when branch-port
	     do (set-port-symbolic-token branch-port his-token))
	 ;; If the branch doesn't have any outputs, then he'll never
	 ;; trigger and propagate through his control-flows.  This
	 ;; tells him that all 0 of his outputs have arrived.
	 (when (null (inputs exit-point))
	   (arm-task-control-flows branch))))
      (otherwise
       (error "Running an exit point in something that isn't an implementation")
       ))))

(defmethod find-corresponding-output ((final-output final-output))
  (let* ((task (superior final-output))
	 (port (first (inputs final-output)))
	 (port-name (name port)))
    (typecase task
      (implementation
       (let* ((abstract-task (abstract-task task))
	      (parent-port (port-named 'output port-name abstract-task)))
	 (setf (corresponding-port final-output) parent-port
	       (corresponding-port parent-port) final-output)
	 parent-port))
      (otherwise
       ))))

(defmethod run-task ((final-output final-output))
  (let* ((task (superior final-output))
         (port (first (inputs final-output)))
         (port-name (name port))
         (value-token (symbolic-token port)))
    (typecase task
      (implementation
       ;; this links up the final output to its corresponding output port
       ;; linking is bidirectional.
       (find-corresponding-output final-output)
       (let* ((abstract-task (abstract-task task))
              (parent-port (port-named 'output port-name abstract-task)))
         ;; make sure that the final output shows up as a downstream
         ;; task of the token.  It in turn will point at the parent port.
         (push final-output (consumers value-token))
         (set-port-symbolic-token parent-port value-token)))
      (otherwise
       ))))

;;; Move output values along dataflow links
(defmethod port-value-arrived ((p port) (direction (eql 'output)) (task output-side-mixin) value)
  (declare (ignore value))
  (incf (n-outputs-provided task))
  (when (> (n-outputs-provided task) (n-outputs task)) 
    (break "More outputs provided than exist ~a ~d ~d" task (n-outputs-provided task) (n-outputs-provided task)))
  (when (eql (n-outputs-provided task) (n-outputs task))
    (task-has-completed-execution task)))

;;; Note since this is on output-side-mixin, it's not
;;; applicable to any branching task (primitive or compound)
;;; BUT it would be applicable to the branches themselves
;;; if the branches have outputs.  
;;; But if the task has no outputs, then this will never get
;;; called and the task will have to trigger arm-task-control-flows itself.
(defmethod task-has-completed-execution ((task output-side-mixin))
  (loop for p in (outputs task)
      for token = (symbolic-token p)
      do (loop for dataflow in (outgoing-flows p)
	     for next-port = (output dataflow)
	     do (set-port-symbolic-token next-port token)))
  ;; this lets the arming of control flow tokens
  ;; to be generic and use method over-rides
  (arm-task-control-flows task)
  )

(defmethod arm-task-control-flows ((task output-side-mixin))
    (loop for control-flow in (outgoing-control-flows task)
      for token = (make-token simple-control-token
		    :producer task)
	do (arm-control-flow control-flow token task)))

