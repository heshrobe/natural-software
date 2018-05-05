;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

 (in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Synthesizing implementations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The idea is that this proposes a refinement
;;; and the refinement is a procedure
;;; The procedure will return three values:
;;;   Applicable: T or NIl.  If nil the other values are irrelevant
;;;   Subgoals: Further things to achieve if approach is to succeed
;;;   A design in which some (or all) of the work has been achieved

(define-predicate plan-for (type thing arguments partial-plan choices-so-far my-choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate possible-implementation (task plan plan-name choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate selected-implementation (task plan choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate choice-behind-plan (plan choice) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; Says that in mapping to the implementation, we applied a viewpoint to the named port.
;;; Which means that in propagating values down during symbolic execution, we should add the viewpoint in 
(define-predicate viewpoint-applied (parent port-name viewpoint-name action object type-constraint) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;; for example for generate row-view this would be [terms-for-viewpint generate row-view traverse row_wise]
(define-predicate terms-for-viewpoint (viewpoint-name action subordinate-verb how) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; This tells you how to create an object of the object type
;;; Note: For right now, I'm ignoring arguments that conditon
;;; allocation (e.g. what size vector you want)
(define-predicate allocation-code (object-type code) (ltms:ltms-predicate-model))
(define-predicate reason-for (implementation parent choice reason) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;;
;;; Contact points for cliche's that are intended to merge into another design
;;; The specific example is path accumulation being merged into tree traversal
;;; Here a port description is a keyword list (:component :branch :join :direction :name) where branch and join
;;; are both optional and mutually exclusive
(define-predicate contact-point (cliche-name Port-description) (ltms:ltms-predicate-model))
(define-predicate initializer-for-merge (cliche-name initializer-name) (ltms:ltms-predicate-model))
      
(defun build-recursive-implementation (top-level-procedure design-editor)
  (let ((dip (design-in-progress design-editor)))
    (unwind-protect
	(let ((plan-names nil))
	  (labels ((do-next-level (procedure)
		     ;; clean out any stale information
		     (when (and (typep procedure 'task-interface-mixin)
				(not (primitive? procedure))
				(not (is-primitive-for-simulation procedure)))
		       (untell `[possible-implementation ? ,procedure ? ?])
		       (untell `[selected-implementation ? ,procedure ?])
		       (setf (implementations procedure) nil)
		       (setf (selected-implementation procedure) nil)
		       (unless (dont-expand procedure)
			 (find-implementations-for procedure
						   #'(lambda (plan choices)
						       (destructuring-bind (plan-name . args) plan
							 (multiple-value-bind (implementation plan-name)
							     (implement-plan design-editor procedure plan-name (copy-object-if-necessary args) choices)
							   (push (list implementation plan-name choices) plan-names)
							   (loop for child in (children implementation)
							       do (do-next-level child))))))))))
	    (do-next-level top-level-procedure))
	  plan-names)
    (switch-focus design-editor dip))))

(defun find-implementations-for (procedure continuation)
  (let* ((task-type (task-type procedure))
	 (args (canonical-arguments-for task-type procedure))
	 (there-is-a-plan nil))
    ;; (format *error-output* "~%Looking for ~a ~a ~a" task-type procedure args)
    (ask `[plan-for ,task-type ,procedure ,args ?plan nil ?choices]
	 #'(lambda (just)
	     (declare (ignore just))
	     ;; (format *error-output* "~%Found ~a" ?plan)
	     (setq there-is-a-plan t)
	     (funcall continuation ?plan ?choices)))
    (when (null there-is-a-plan)
      (format *error-output* "~%no plan for ~a" procedure))
    ))

;;; ToDo: Generalize from (stream <x>) to more general 
;;; form including (sequence <x>)
(defun canonical-form-for (type-expression)
  (cond 
   ((symbolp type-expression)
    (or (definition-for type-expression) type-expression))
   ((is-of-type? (first type-expression) 'container)
    `(,(first type-expression) ,@(loop for thing in (rest type-expression) collect (canonical-form-for thing))))
   (t ;;; Example for making a transducer
    type-expression)))

(defun definition-for (symbol)
  (ask `[definition ,symbol ?def]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from definition-for ?def)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Plan Language
;;;
;;; initial-input: name port-name port-type
;;; final-output: name port-name port-type
;;; Constant: A source of a constant value, e.g. True, 1, 'foo
;;; state: direction name port-name port-type
;;; component: component-type component-name . property plist
;;; control-flow source destination-component-name
;;; dataflow: (source-component-name source-port-name) (destination-component-name destination-port-name)
;;; plan: plan-name steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun implement-plan (design-editor parent plan-name args choices)
  (let* ((unique-plan-name (intern (gensymbol (name parent) "IMPLEMENTATION")))
	 (new-design (get-new-design unique-plan-name design-editor :class 'implementation :abstract-task parent)))
    (apply plan-name new-design args)
    (tell `[possible-implementation ,new-design ,parent ,plan-name ,choices])
    (push new-design (implementations parent))
    (loop for choice in choices
	do (tell `[choice-behind-plan ,new-design ,choice]))
    (values new-design plan-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Knowledge about refinements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-viewpoint? (type)
  (ask `[can-be-viewed-as ? ? ,(first type)]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from is-viewpoint? t))
       :do-backward-rules nil)
  nil)

(defun is-abstract-statement? (form)
  (cond
   ((atom form) nil)
   (t
    (ask `[abstract-operator ,(car form)]
	 #'(lambda (just)
	     (declare (ignore just))
	     (return-from is-abstract-statement? t))
	 :do-backward-rules nil)
    nil)))
			    
(defrule compound-source-viewed-as (:backward)
  :then [can-be-viewed-as (?input-type . ?input-parameters) ?output-type ?viewpoint]
  :if [can-be-viewed-as ?input-type ?output-type ?viewpoint]
  )

(defrule property-through-viewpoint (:backward)
  :then [property-value-of ?thing ?property-name ?value]
  :if [viewpoint-equivalence (?property-name ?thing) ?value]
  )

(defrule data-type-property-explicit (:backward)
  :then [property-value-of ?thing element-type ?value]
  :if [and (listp ?thing)
	   (is-a-type? (first ?thing))
	   (unify ?value (second ?thing))])

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

(defun find-state-sources (design)
  (loop for component in (children design)
      when (typep component 'state-source)
      collect component))

(defun find-state-source-initialization (state-source)
  (let* ((input-port (first (inputs state-source)))
	 (dataflow (when input-port (first (incoming-flows input-port))))
	 (initializing-port (when dataflow (input dataflow))))
    (when initializing-port
      (values
       (task initializing-port)
       dataflow))))

(defun find-state-sinks (design)
  (loop for component in (children design)
	when (typep component 'state-sink)
      collect component))

;;; This is dead code I thinkz
;;;(defun find-take-operations (design)
;;;  (loop for c in (children design)
;;;      when (eql (task-type c) 'take)
;;;      collect (let* ((input-port (first (inputs c)))
;;;		     (dataflow (first (incoming-flows input-port)))
;;;		     (source-port (input dataflow)))
;;;		(list c dataflow (task source-port)))))

(defmethod port-name ((input initial-input))
  (name (first (outputs input))))

(defmethod port-name ((constant constant-value))
  (name (first (outputs constant))))

(defmethod port-name ((output final-output))
  (name (first (inputs output))))

(defclass implementation-map ()
  ((input-map :accessor input-map :initarg :input-map)
   (output-map :accessor output-map :initarg :output-map)
   (state-sources :accessor state-sources :initarg :state-sources)
   (state-sinks :accessor state-sinks :initarg :state-sinks))
  )

;;; ToDo: This looks like dead code at the moment
;;; but it might be a cleaner way of doing things
;;; that what's done below.  Although it would need to 
;;; be updated to be aware of non-primitive branching and joining-tasks
;;; and their entry-points and exit-points
(defun build-implementation-map (design implementation)
  (let ((input-ports (inputs design))
	(output-ports (outputs design))
	(initial-inputs (find-initial-inputs implementation))
	(final-outputs (find-final-outputs implementation)))
    (make-instance 'implementation-map
      :input-map (loop for input-port in input-ports
		     for name = (name input-port)
		     for corresponding-input = (find name initial-inputs :key #'port-name)
		     collect (cons input-port corresponding-input))
      :output-map (loop for output-port in output-ports
		      for name = (name output-port)
		      for corresponding-output = (find name final-outputs :key #'port-name)
		      collect (cons output-port corresponding-output))
      :state-sources (find-state-sources implementation)
      :state-sinks (find-state-sinks implementation))))


(defun find-possible-implementations-of (design)
  (let ((answers nil))
    (ask `[possible-implementation ?implementation ,design ?name ?choices]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push (list ?implementation ?name ?choices) answers)))
    answers))

(defun find-selected-implementation-of (design)
  (ask `[selected-implementation ?implementation ,design ?choices]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from find-selected-implementation-of
	     ?implementation))))
		 
(defparameter *talk-to-me-about-implementations* nil)

(defun produce-implementation-map (design)
  (labels ((do-next-level (sub-design)
	     (typecase sub-design
	       (input-side-mixin (setf (n-inputs-provided sub-design) 0))
	       (joining-task (loop for join in (joins sub-design) do (setf (n-inputs-provided join) 0))))
	     (typecase sub-design
	       (output-side-mixin (setf (n-outputs-provided sub-design) 0))
	       (branching-task (loop for branch in (branches sub-design) do (setf (n-outputs-provided branch) 0))))
	     (unless (or (not (typep sub-design 'task-interface-mixin))
			 (dont-expand sub-design)
			 (primitive? sub-design))
	       (let ((possible-implementations (find-possible-implementations-of sub-design))
		     (selected-implementation (find-selected-implementation-of sub-design)))
		 (cond
		  ((null possible-implementations)
		   (error "No implementation for ~a" sub-design))
		  ((not (null selected-implementation)))
		  ((null (rest possible-implementations))
		   (setq selected-implementation (first (first possible-implementations))))
		  (t (setq selected-implementation 
		       (if *talk-to-me-about-implementations*
			   (choose-selected-implementation
			    sub-design (name sub-design) possible-implementations)
			 (first (first possible-implementations))
			 ))))
		 (when (null selected-implementation)
		   (error "No selected implementation for ~a" sub-design))
		 (let ((choices nil))
		   (ask `[possible-implementation ,selected-implementation ,sub-design ? ?choices]
			#'(lambda (just)
			    (declare (ignore just))
			    (setq choices ?choices)))			
		   (setf (selected-implementation sub-design) selected-implementation)
		   (tell `[selected-implementation ,selected-implementation ,sub-design ,choices]))
	       (loop for child in (children selected-implementation)
		   do (do-next-level child))))))
    (do-next-level design)))

(defun find-state-initializations (task)
  (let ((answers nil))
    (flet ((process-one-level (sub-task history implementation)
	     (declare (ignore sub-task history))
	     (when implementation
	       (let ((state-sources (find-state-sources implementation)))
		 (loop for source in state-sources
		     for source-name = (state-name source)
		     for init = (find-state-source-initialization source)
		     for locality = (getf (properties source) :locality)
		     do (multiple-value-bind (code code-exists)  (when init (initialization-code-for (task-type init) init source))
			  (push (list source source-name code locality code-exists) answers)))))))
      (map-over-selected-implementation task #'process-one-level))
    answers))

(defmethod initialization-code-for ((type (eql 'constant)) initializing-task (source t))
  (values (value initializing-task) t)
  )

(defmethod initialization-code-for ((type (eql 'allocate)) initializing-task (source t))
  (let* ((port (first (outputs initializing-task)))
	 (type-constraint (port-type-constraint port))
	 (type-constraint-definition (is-definition-for type-constraint)))
    (multiple-value-bind (allocation-code allocation-code-exists) (allocation-code type-constraint)
      (multiple-value-bind (definition-allocation-code definition-allocation-code-exists) (allocation-code type-constraint-definition)
	(values (or allocation-code definition-allocation-code)
		(or allocation-code-exists definition-allocation-code-exists))))))

(defmethod initialization-code-for ((type t) (initializing-task core-task-mixin) (source t))
  (loop for output in (outputs initializing-task)
      do (loop for outgoing-flow in (outgoing-flows output)
	     for destination-port = (output outgoing-flow)
	     for destination-task = (task destination-port)
	     when (eql destination-task source)
	     do (let ((token (symbolic-token output)))
		  (when token
		    (return-from initialization-code-for (values (form token) t)))))))

(defun find-choice (possible-implementation)
  (ask `[and [possible-implementation ,possible-implementation ?task ? ?choices]
	     [viewpoint-applied ?task ? ?choice ?action ?object ?type-contraint]
	     [terms-for-viewpoint ?choice ?action ?verb ?verb-modifier]]
       #'(lambda (just)
	   (declare (ignore just))
	   (when (member ?choice ?choices :key #'second)
	     (return-from find-choice (list ?choice ?action ?object ?type-contraint ?verb ?verb-modifier))))))

(defun find-definition-for-type-constraint (type-constraint)
  (ask `[definition ?definition ,type-constraint]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from find-definition-for-type-constraint ?definition)))
  nil)

(defun find-best-description-of-constraint (type-constraint)
  (let* ((more-abstract-constraint (find-definition-for-type-constraint type-constraint))
	 (owner (or more-abstract-constraint (if (listp type-constraint) (first type-constraint) type-constraint))))
    owner))

(defun map-over-selected-implementation (task function)
  (labels ((do-one-task (sub-task history)
	     (let ((implementation (find-selected-implementation-of sub-task)))
	       (when implementation
		 (funcall function sub-task history implementation)
		 (loop for child in (children implementation)
		     do (do-one-task child (list* implementation sub-task history)))))))
    (do-one-task task nil)))

(defun print-it (task history implementation)
  (format t "~% ~a <-- ~a<-- ~{~a~^ <--~}" 
	  (name implementation)
	  (name task)
	  (mapcar #'name history)))

;;; ToDo: seems to be dead code
(defun same-state-variable (state-source state-sink)
  (and (eql (superior state-source) (superior state-sink))
       (eql (name (first (outputs state-source)))
	    (name (first (inputs state-sink))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Symbolic Simulation for code generation
;;;   Each level produces its code which it returns by setting special variable *generated-code*
;;;   Each level binds this so that it gets the code below it and can then examine and jigger that code
;;;   Each level then returns its code by setting *generated-code*
;;;   Macro with-generated-code manages this
;;;   
;;;  There are a variety of steps:
;;;   Propagating values from ports through data flows to downstream ports
;;;   Notifying tasks that a value has arrived
;;;   Simulating the task when all its inputs have arrived
;;;    Either propagate down to the implementation
;;;    Or it's a primitive type with dedicated code
;;;
;;; First, Some Preliminaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *variable-counter-table* nil)

(defun next-instance-of-variable (variable-name)
  (let ((number (gethash variable-name *variable-counter-table*)))
    (unless number
      (setq number 0))
    (incf number)
    (setf (gethash variable-name *variable-counter-table*) number)
    (intern (format nil "~a-~d" variable-name number))))

(defparameter *non-implementable-built-ins* 
    '(take put truncate index-enumerator list-enumerator vector-length vector-push vector-accessor allocate constant-value state-source state-sink print))

(defun is-primitive-for-simulation (task)
  (member (task-type task) *non-implementable-built-ins*))

;;;(defvar *generated-code* nil)
;;;
;;;;;; How this works:
;;;;;; We are returning the downstream code that results after our execution
;;;;;; It is returned in most-recent-first order.
;;;;;; We then stick in front of that our own code, preserving most-recent-first order
;;;(defmacro with-generated-code ((sub-code-variable) stuff-to-do &body code-generation)
;;;  (let ((declarations nil))
;;;    (loop for form = (first stuff-to-do)
;;;	for function = (first form)
;;;	while (eql function 'declare)
;;;	do (push form declarations)
;;;	   (pop stuff-to-do))
;;;    `(let ((,sub-code-variable nil))
;;;       ,@(reverse declarations)
;;;       (let ((*generated-code* nil))
;;;	 ,@stuff-to-do
;;;	 (setq ,sub-code-variable *generated-code*))
;;;       (setq *generated-code* (append ,@code-generation *generated-code*)))))



;;;(defvar *propagation-trail* nil "A stack of ports and control flows that have been set (armed) during propagation")
;;;
;;;;;; for the moment the entries are either a task or a control-flow
;;;;;; i.e. we're not trying to remove values from ports (particularly output-ports)
;;;(defun unwind-propagation-stack ()
;;;  (loop for entry in *propagation-trail*
;;;      do (typecase entry
;;;	   (input-side-mixin (decf (n-inputs-provided entry)))
;;;	   (control-flow (setf (armed? entry) nil)))))
;;;
;;;(defmacro with-propagation-state (&body body)
;;;  `(let ((*propagation-trail* nil))
;;;     (unwind-protect (progn ,@body)
;;;       (unwind-propagation-stack))))


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
   (code :initform nil :accessor code :initarg :code)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tokens for pseudo operations:
;;;  initial-input, final-output, entry-point, exit-point
;;; 
;;; Actually I'd prefer not to have these at all
;;; But the initial input can have a viewpoint applied
;;; so we build a new token with the appropriate information
;;; filled in.  (Maybe we could just modify the original token?
;;; 
;;; Each of these keep back-pointers to the original token
;;; so the when they arrive at an actual computation step
;;; we can figure out who really produced the input and
;;; update its consumers field
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; None of these are used anymore
;;; The value of an initial input
;;;(Defclass initial-input-value-token (value-token-mixin)
;;;  ((corresponding-token :initarg :corresponding-token :accessor corresponding-token :initform nil))
;;;  )
;;;
;;;(defclass final-output-value-token (value-token-mixin)
;;;  ((corresponding-token :initarg :corresponding-token :accessor corresponding-token :initform nil))
;;;  )
;;;
;;;;;; This isn't used at the moment
;;;(defclass entry-point-value-token (value-token-mixin)
;;;  ((corresponding-token :initarg :corresponding-token :accessor corresponding-token :initform nil))
;;;  )
;;;
;;;(defclass exit-point-value-token (value-token-mixin)
;;;  ((corresponding-token :initarg :corresponding-token :accessor corresponding-token :initform nil))
;;;  )


(defclass bound-variable-mixin ()
  ((variable-name :accessor variable-name :initarg :variable-name))
  )


(defclass top-level-argument (value-token-mixin bound-variable-mixin)
  ()
  )

(defclass state-variable-initialization (value-token-mixin bound-variable-mixin)
  ((initializer :accessor initializer :initarg :initializer)
   (code :accessor code :initarg :code))
  )

(defclass stream-processor-surrogate-output (value-token-mixin bound-variable-mixin)
  ()
  )


(defclass enumerator-symbolic-value-mixin (value-token-mixin bound-variable-mixin)
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

;;; This was used to compute the consumers field
;;; But that's now being done as the flow proceeds.
;;;(defmethod fill-in-token ((token value-token-mixin) (task output-side-mixin))
;;;  (let* ((owning-port (port token))
;;;	 (consumers (when owning-port 
;;;		      (loop for flow in (outgoing-flows owning-port)
;;;			  for destination-port = (Output flow)
;;;			  collect destination-port))))
;;;    (setf (consumers token) consumers))
;;;  )

;;; these are here to tell you whether a task actually does something
;;; or is just part of the plumbing between abstract tasks and their
;;; implementations
;;;
;;; The ideas is that during symbolic simulation whenever a token arrives
;;; at a real computation, we add that computation to the "consumers" field
;;; of the token.  So if the consumers field has more than one element then
;;; we probably want to bind it to a variable before sending it on.
;;; Futhermore the network of tokens and their consumers is the actual flow
;;; graph of the computation. Each token is bi-directionally linked 
;;; to its producer (producer slot in the token, in the task the ports and their token slots) 
;;; and to its consumers (consumers slots and the same for the task).

(defmethod task-is-real-computation ((task final-object-mixin)) nil)
;;; this is a subclass of final object, but actually consumes a value
(defmethod task-is-real-computation ((task state-sink)) t)
(defmethod task-is-real-computation ((task state-source)) nil)
(defmethod task-is-real-computation ((task constant-value)) nil)
(defmethod task-is-real-computation ((task initial-object-mixin)) nil)
(defmethod task-is-real-computation ((task primitive-branching-task)) t)
(defmethod task-is-real-computation ((task branching-task)) (dont-expand task))
(defmethod task-is-real-computation ((task primitive-joining-task)) t)
(defmethod task-is-real-computation ((task joining-task)) nil)
(defmethod task-is-real-computation ((task join)) (task-is-real-computation (superior task)))
(defmethod task-is-real-computation ((task branch)) (task-is-real-computation (superior task)))

(defmethod task-is-real-computation ((task task-interface)) 
  (cond ;; ((non-real-temporal-sequence-task task (task-type task)) nil)
	((not (null (member (task-type task) *non-implementable-built-ins*))) t)
	((null (selected-implementation task)) t)))

;;; These are actually dead code.  The only caller is the commented out first clause
;;; in the COND above.
(defmethod non-real-temporal-sequence-task ((task task-interface) (type t)) nil)

(defmethod non-real-temporal-sequence-task ((task task-interface) (type (eql 'take)))
  (let* ((input-port (port-named 'input 'sequence-data task))
	 (input-type (port-type-constraint input-port)))
    (and (listp input-type)
	 (eql (first input-type) 'temporal-sequence))
    ))

(defmethod non-real-temporal-sequence-task ((task task-interface) (type (eql 'put)))
  (let* ((input-port (port-named 'input 'sequence task))
	 (input-type (port-type-constraint input-port)))
    (and (listp input-type)
	 (eql (first input-type) 'temporal-sequence))
    ))

(defmethod non-real-temporal-sequence-task ((task task-interface) (type (eql 'truncate)))
  t)
	


(defun all-downstream-computations (token)
  (let ((computations nil))
    (labels ((do-a-token (token)
	       ;; (format t "~%Doing token ~a" token)
	       (let* ((port (port token))
		      (task (task port)))
		 (if (eql (direction port) 'output)
		     (loop for consumer in (consumers token)
			 do (do-a-task consumer))
		   (do-a-task task))))
	     (do-a-task (task)
	       (unless (member task computations)
		 (format t "~%Doing task ~a" task)
		 (push task computations)
		 (loop for output-port in (outputs task)
		     for token = (symbolic-token output-port)
		     do (do-a-token token)))))
      (do-a-token token))
    computations))

(defun sort-computations (tasks)
  (flet ((precedes (task1 task2)
	   (loop for output-port in (outputs task1)
	       for token = (symbolic-token output-port)
	       for consumers = (consumers token)
	       when (member task2 consumers)
	       return t)
	   nil))
  (sort tasks 
	#'(lambda (task1 task2) (precedes task2 task1)))))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric code-for (task task-type language))

(defvar *completed-tasks* nil)

(defun generate-code (top-level-task &optional (build-map t) (language :lisp))
  (if build-map
      (produce-implementation-map top-level-task)
    (initialize-design top-level-task))
  (let* ((top-level-arguments (loop for input-port in (inputs top-level-task) collect (name input-port)))
	 (state-variable-map nil)	;maps state sources and sinks to variable names
	 (state-variable-code nil)
	 (constants (find-constants (selected-implementation top-level-task)))
	 (inputs (inputs top-level-task))
	 (input-1 (first inputs))
	 (outputs (outputs top-level-task))
	 (output (first outputs))
	 (streaming? (is-a-simple-stream-processor top-level-task)))
    (when (is-a-simple-stream-processor top-level-task)
      ;; Hack: the order of pushes matters here
      (pushnew (name output) top-level-arguments)
      (pushnew (name input-1) top-level-arguments)
      )
    (let ((*variable-counter-table* (make-hash-table))
	  ;; (*generated-code* nil)
	  )
      (loop for constant in constants do (propagate constant 'constant))
      (loop for input in (inputs top-level-task)
	  for input-name = (name input)
	  for type = (port-type-constraint input)
	  for symbolic-token = (make-token top-level-argument
				 :producer top-level-task
				 :port input
				 :type type
				 :value input-name
				 :variable-name input-name
				 )
	  do (set-port-symbolic-token input symbolic-token))
      (loop for (source source-name code locality code-exists) in (Find-state-initializations top-level-task)
	do (push (list source source-name) state-variable-map)
	when (and code-exists (or (not (eql locality 'local)) (eql (abstract-task (superior source)) top-level-task)))
	do (push (list source-name code) state-variable-code))
      (loop for (source source-name code) in state-variable-map
	  for initializer-port = (first (inputs source))
	  for symbolic-token = (make-token state-variable-initialization
				 :producer top-level-task
				 :port initializer-port
				 :value source-name
				 :type (port-type-constraint initializer-port)
				 :initializer source
				 :code code
				 )
	  ;; do (format *error-output* "~%Initializing ~a to ~a" initializer-port symbolic-token)
	  do (set-port-symbolic-token initializer-port symbolic-token))
      ;; (format *error-output* "source ports done")
      ;; This is a special purpose hack for a simple-stream-processor
      ;; In that case, the output is actually an input that we side-effect
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
      ;; (format *error-output* "surrogate hack done")
      
      ;; (format *error-output* "input ports done")
      (let* ((*completed-tasks* nil)
	     (the-code (code-for top-level-task (task-type top-level-task) language)))
	(values
	 (code-for-top-level-task top-level-task top-level-arguments the-code language
				      :state-variable-code state-variable-code
				      :streaming? streaming?)
	 state-variable-map)))))

(defmethod code-for-top-level-task (top-level-task top-level-arguments the-code (language (eql :lisp))
				    &key  state-variable-code streaming?)
  (let ((name (name top-level-task))
	(outputs (program-outputs top-level-task)))
    (If state-variable-code
	`(defun ,name ,top-level-arguments
	   (let ,state-variable-code
	     ,@the-code
	     ,@(unless streaming?
		 `(,@outputs))))
	`(defun ,name ,top-level-arguments
	   ,@the-code
	   ,@(unless streaming?
	       `(,@outputs)))
	)))

(defmethod code-for-top-level-task (top-level-task top-level-arguments the-code (language (eql :java))
				    &key state-variable-code streaming?)
  ;; Fix: Will eventually get used  
  (declare (ignore state-variable-code streaming? top-level-arguments))
  (let ((name (name top-level-task))
	(outputs (program-outputs top-level-task)))
    ;; Fix: Will eventually get used
    (declare (ignore outputs))
    `(:class-definition :visible? t 
			:name ,name
			:methods ((:name main :visible? t :static? t :result |void|
					       :args (|String| \[\] |args|)
					       :body ((:application |DoIt|)))
				  (:name |DoIt| :visible? nil :static? t :result |void| 
					  :args nil
					  :body ,the-code)))))

(defmethod pretty-print-for-language (code stream (language (eql :java)))
  (let ((type (first code)))
    (jpp type (cdr code) stream 0))
  )


; public class HelloWorld {

;     public static void main (String[] args) {
;         // Prints "Hello, World" to the terminal window.
;         System.out.println("Hello, World");
;     }

; }

(defmethod jpp ((type (eql :class-definition)) code stream indent)
  (destructuring-bind (&key visible? name methods variables) code
    ;; for now ignore class variables
    (declare (ignore variables))
    (format stream "~%~vt~:[private~;public~] class ~a {"
	    indent
	    visible?
	    name)
    (loop for method in methods do (jpp :method method stream (+ indent 3)))
    (format stream "~%~vt}" indent)
    ))

;;; MAIN :VISIBLE? T :STATIC? T :RESULT |void| :ARGS (|String| [] |args|) :BODY ((|DoIt| NIL))
(defmethod jpp ((type (eql :method)) code stream indent)
  (destructuring-bind (&key name visible? static? result args body) code
    (format stream "~%~vt ~:[private~;public~] ~:[~;static~] ~a ~a ~:[()~;~a~] {"
	    indent visible? static? result name (not (null args)) args)
    (loop for (key . stuff) in body do (jpp key stuff stream (+ indent 3)))
    (format stream "~%~vt}" indent)))

;;; Operation . arguments e.g. (:application System.out.println "Hello, World" "Mumble")
(defmethod jpp ((type (eql :application)) code stream indent)
  (format stream "~%~vt" indent)
  (format stream "~:a(" (first code))
  (loop for token on (rest code) 
      do (jpp-dispatch (first token) stream 0)
      when (rest token) do (format stream ", "))
  (format stream ");")
  )

;;; This is the template for loop with both normal and abnormal exits
; abnormal: {
;     for (i = 0; i < a.length; i++)
; 	{if (abnormal-exit-test)
; 		{
; 		    abnormal-exit-code;
; 		    break abnormal;
; 		}
; 	}
;     normal-exit-code;
; }
					; code-after-for-loop
;;; At the moment, I don't think I know whether there's an abnormal exit
;;; so I'll generate this template always until that gets fixed.

(defun jpp-dispatch (form stream indent) (jpp (first form) (rest form) stream indent))

(defmethod jpp ((type (eql :for)) code stream indent)
  (format stream "~%~vt" indent)
  (destructuring-bind (loop-variable &key init refresh terminate body exit-code abnormal-exit?) code
    (when abnormal-exit?
      (format stream "~%~vtabnormal-exit:{" (incf indent 3)))
    (format stream "~%~vtfor (" indent)
    (jpp-dispatch loop-variable stream indent)
    (format stream " = ")
    (jpp-dispatch init stream 0)
    (format stream "; " stream)
    (jpp-dispatch terminate stream 0)
    (format stream "; " stream) 
    (jpp-dispatch refresh stream 0)
    (format stream ")")
    (format stream "~%~vt{" (+ indent 3))
    (loop for stuff in body do (jpp-dispatch stuff stream (+ indent 6)))
    (format stream "~%~vt}" (+ indent 3))
    (loop for stuff in exit-code do (jpp-dispatch stuff stream indent))
    (when abnormal-exit?
      (format stream "~%~vt}" indent))
    )
  )

;;; this is something like a + b which doesn't start a new line
;;; usually inside an assignment or a for

(defmethod jpp ((type (eql :binary-op)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (operation operand-1 operand-2) code
    (jpp-dispatch operand-1 stream 0)
    (format stream " ~a " operation)
    (jpp-dispatch operand-2 stream 0)
    ))

(defmethod jpp ((type (eql :binary-comparison)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (operation operand-1 operand-2) code
    (jpp-dispatch operand-1 stream 0)
    (format stream " ~a " operation)
    (jpp-dispatch operand-2 stream 0)
    ))
  
(defmethod jpp ((type (eql :assignment)) code stream indent)
  (destructuring-bind (&key variable right-hand-side) code
    (format stream "~%~vt~:a = " indent variable)
    (destructuring-bind (expression-type expression) right-hand-side
      (jpp expression-type expression stream 0)
      )
    ))

(defmethod jpp ((type (eql :constant)) constant stream indent)
  (declare (ignore indent))
  (let ((the-value (first constant)))
    (format stream "~:[~:a~;~s~]" (stringp the-value) the-value)
    ))

(defmethod jpp ((type (eql :variable)) identifier stream indent)
  (declare (ignore indent))
  (let ((the-variable  (first identifier)))
    (format stream "~:[~:a~;~s~]" (stringp the-variable) the-variable)
    ))

(defparameter for-test '(:for (:variable |i|) :init (:constant 0) 
			 :refresh (:binary-op + (:variable |i|) (:constant 1))
			 :terminate (:binary-comparison < (:variable |i|) (:constant 10))
			 :body ((:application |System.out.println| (:variable |i|) (:constant 10)))
			 :exit-code ((:application |System.out.println| (:constant "Done")))
			 :abnormal-exit? t))

(defmethod program-outputs ((task task-interface))
  (loop for output in (outputs task)
      for token = (symbolic-token output)
      collect (value token)))

(defmethod program-outputs ((task branching-task))
  (loop for branch in (branches task)
      append (loop for output in (outputs branch)
		 for token = (symbolic-token output)
		 collect (value token))))

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

;;; Note: The Entry-Point and Final-Output classes mix in core-task-mixin, so this applies to its input ports
(defmethod port-value-arrived ((p port) (direction (eql 'input)) (task core-task-mixin) value)
  (when (task-is-real-computation task)
    (push task (consumers value)))
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
      (propagate-values-to-selected-implementation task selected-implementation)))))

(defmethod run-task ((join join))
  (let ((task (superior join)))
    (if (primitive? task)
	;; only run this guys when all the joins
	;; are good to go.
	(when (loop for join in (joins task)
		  always (ready-to-run join))
	  (run-primitive-joining-task task join))
      (run-compound-joining-task task join))))

(defmethod run-compound-joining-task ((task joining-task) (join join))
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
	(task-has-completed-execution corresponding-entry-point)))
     (t
      (error "No entry point corresponds to ~a of ~a" join task)))))


;; For each value should we figure out the branch dependencies now or wait to code generation time?
;; FixMe: So it appears that we need to deal with this finally
;;  when propagating values 
(defmethod run-primitive-joining-task ((task primitive-joining-task) (join join))
  (let ((joins (joins task)))
    (when (loop for join in (joins task) always (ready-to-run join))
      ;; build a symbolic value or each output that tracks each of the joined inputs
      (loop for output in (outputs task)
	  for name = (name output)
	  for corresponding-input-values = (loop for join in joins
					       for corresponding-input-port = (port-named 'input name join)
					       for his-token = (symbolic-token corresponding-input-port)
					       for his-value = (value his-token)
					       collect (cons join his-value))
	  for symbolic-output = (make-token join-symbolic-value
				  :port output
				  :producer task
				  ;; Fix Me: This is a total hack based on 
				  ;; the assumption that all values are the same
				  :value corresponding-input-values
				  :joined-values corresponding-input-values
				  :type (port-type-constraint output)
				  )
	  do (set-port-symbolic-token output symbolic-output)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Supporting routines for getting tasks started
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ToDo: There's code above that builds an input map.  Can that data-structure be used here?
;;; Note that here task could be a join and implementation its corresponding entry-point
(defmethod propagate-values-to-selected-implementation ((task input-side-mixin) implementation)
  (let* ((real-task (typecase task (join (superior task)) (otherwise task)))
	 (real-implementation (typecase implementation (entry-point (superior implementation)) (otherwise implementation)))
	 ;; I'm not sure why this was here
	 (labels?  (getf (properties real-task) :introduce-labels)))
    (loop for input-port in (inputs task)
	for name = (name input-port)
	for type = (port-type-constraint input-port)
	for token = (symbolic-token input-port)
	for value = (value token)
	for corresponding-port = (find-corresponding-input-port implementation name)
	for viewpoint-to-apply = (viewpoint-to-apply real-task real-implementation name)
	when viewpoint-to-apply 
	do (setf (value token) (list viewpoint-to-apply value)
		 ;; here the form slot is used to hold the original value
		 (form token) value)
	unless corresponding-port do (error "No corresponding port ~a ~a" task name)
	if labels?
	   ;; see comment above
	do (let ((symbolic-token (make-token top-level-argument
				   :producer task
				   :port input-port
				   :type type
				   :value name
				   :variable-name name
				   )))
	     (set-port-symbolic-token corresponding-port symbolic-token))
	else do (set-port-symbolic-token corresponding-port token))))

(defmethod propagate-values-to-selected-implementation :before ((task join) (implementation entry-point))
  (setf (corresponding-join implementation) task)
  )

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

;;; Here there are two methods: 
;;;  One for a normal implementation where you look for Initial-inputs
;;;  Another for an entry point where you seach its output ports
(defmethod find-corresponding-input-port ((implementation implementation) name)
  (loop for thing in (children implementation)
      when (and (typep thing 'initial-input)
		(eql (port-name thing) name))
      return (first (outputs thing))))

(defmethod find-corresponding-input-port ((entry-point entry-point) name)
  (port-named 'output name entry-point))

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
  (values)
  )

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
  (unless (outputs task)
    (task-has-completed-execution task)))

(defmethod multiple-users? ((port port))
  (not (null (rest (outgoing-flows port)))))

(defmethod propagate ((task task-interface) (type (eql 'allocate)))
  (let* ((output (first (outputs task)))
	 (type (port-type-constraint output))
	 (allocation-code (allocation-code type))
	 (new-value (make-token allocate-symbolic-value-token
		      :port output
		      :producer task
		      :type type
		      :value allocation-code
		      :code allocation-code)))
    (set-port-symbolic-token output new-value)))

(defun allocation-code (type)
  (when (listp type) (setq type (first type)))
  (ask `[allocation-code ,type ?code]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from allocation-code (values ?code t)))))

(defmethod propagate ((task task-interface) (type (eql 'add-to-set)))
  (let* ((the-set-port (port-named 'input 'the-set task))
	 (the-set-token (symbolic-token the-set-port))
	 (the-set (value the-set-token))
	 (the-element-port (port-named 'input 'the-element task))
	 (the-element-token (symbolic-token the-element-port))
	 (the-element (value the-element-token))
	 (updated-set-port (port-named 'output 'the-set task))
	 (symbolic-value (make-token normal-output
			   :producer task
			   :port updated-set-port
			   :type (port-type-constraint the-set-port)
			   :value the-set
			   :code (if (getf (properties task) :unique?)
				     `(adjoin ,the-element ,the-set)
				   `(cons ,the-element ,the-set))
			   )))
    (set-port-symbolic-token updated-set-port symbolic-value)))

(defmethod propagate ((task task-interface) (type (eql 'vector-push)))
  (let* ((new-data-port (port-named 'input 'new-data task))
	 (new-data-token (symbolic-token new-data-port))
	 (new-data (value new-data-token))
	 (vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector (value vector-token))
	 (output (first (outputs task)))
	 (symbolic-value (make-token normal-output
			   :form `(vector-push ,new-data ,vector)
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
			     :form `(enqueue ,data-input-value ,stream-input-value)
			     :type `(stream ,data-input-type)
			     :value stream-input-value)
			 (make-token temporal-sequence-output-value
			   :producer task
			   :port output
			   :form data-input-form
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
;;; I either case, the guy it's getting it from has created a bound
;;; variable.  So we just pass that along
;;; During code generation we have to distinguish
;;; the two cases.  For a sequence we need to emit a dequeue and bind
;;; it if there are multiple-users.

(defmethod propagate ((task task-interface) (type (eql 'take)))
  (let* ((input (first (inputs task)))
	 (input-token (symbolic-token input))
	 (input-tokens-input-token (when (typep input-token 'temporal-sequence-output-value) (input-token input-token)))
	 (output (first (outputs task)))
	 (input-type (type-constraint input-token))
	 (sequence-type (first input-type))
	 (sequence-element-type (second input-type)))
    ;; if the token comes from an enumerator there's nothing to do
    ;; if from a sequence then we need to create a new variable
    ;; (or a dequeue form)
    (let ((new-variable (make-token normal-output
			  :value (if (eql sequence-type 'stream)
				     ;; in this case we're always going to emit a loop
				     ;; so we need a new-variable
				     sequence-element-type
				   (value input-token))
			  :port output
			  :producer (if (typep input-token 'temporal-sequence-output-value) 
					(producer input-tokens-input-token)
					task)
			  ;; The input type is either (sequence <xxx.) or (stream <xxx>)
			  ;; so this gives us xxx
			  :type (second input-type)
			  )))
      (set-port-symbolic-token output new-variable))
    ))

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
	 (multiple-consumers (rest (consumers input-value-token)))
	 (input-value (if multiple-consumers (value input-value-token) (form input-value-token)))
	 (more-branch (branch-named 'more task))
	 (empty-branch (branch-named 'empty task))
	 ;; maybe this should fetch the branch by name?
	 (output (first (outputs more-branch)))
	 (new-index (next-instance-of-variable 'index))
	 (output-value (make-token index-enumerator-symbolic-value
			 :producer task
			 :port output
			 :type '(temporal-sequence integer)
			 :lower-bound lower-bound
			 :upper-bound upper-bound
			 :value new-index
			 )))
    ;; Doing this will trigger the task-has-completed execution method
    ;; on the branch which will arm the control flows.
    (set-port-symbolic-token output output-value)
    ;; so doing this is now going to double arm the control flows
;;;    (loop for cflow in (outgoing-control-flows more-branch) 
;;;	for arming-token = (make-token branch-control-token
;;;			     :producer task
;;;			     :branch more-branch
;;;			     :branch-condition (branch-condition more-branch)
;;;			     )
;;;	do (arm-control-flow cflow arming-token task))
    ;; because no outputs on this branch
    ;; you must call task-has-completed-execution
    (task-has-completed-execution empty-branch)
    ))

(defmethod propagate ((task primitive-branching-task) (type (eql 'list-enumerator)))
  (let* ((input (first (inputs task)))
	 (input-type-constraint (port-type-constraint input))
	 (input-value (symbolic-token input))
	 (more-branch (branch-named 'more task))
	 (empty-branch (branch-named 'empty task))
	 ;; maybe this should fetch the branch by name?
	 (output (first (outputs more-branch)))
	 (new-element (next-instance-of-variable 'list-element))
	 (output-value (make-token list-enumerator-symbolic-value
			 :producer task 
			 :port output
			 :value new-element
			 ;; this should be a sequence of 
			 ;; the type of the list-elements
			 :type `(temporal-sequence ,(if (listp input-type-constraint) (second input-type-constraint) input-type-constraint))
			 :the-list input-value
			 )))
    ;; see comment in the previous message
    (set-port-symbolic-token output output-value)
;;;    (loop for cflow in (outgoing-control-flows more-branch) 
;;;	for arming-token = (make-token branch-control-token
;;;			     :branch more-branch
;;;			     :branch-condition (branch-condition more-branch)
;;;			     :producer task
;;;			     )
;;;	do (arm-control-flow cflow arming-token task))
    (task-has-completed-execution empty-branch)
    ))


;;; This is the code for a vanilla primitive branching task
;;; that simply makes a decision and doesn't produce outputs
;;; The idea here is that we'll propagate down all branches
;;; Each branch will add itself to the token so that we 
;;; can tell which control path we're on
;;;
;;; Fix Sometime: Need to propagate to branch outputs !!!
;;; For the moment fixed in more specific methods where needed.
;;; The assumption is that the branches have no outputs so
;;; nothing will trigger the branches' task-has-completed-execution method
;;; so we do the equivalent here.

(defmethod propagate :before ((task primitive-branching-task) type-of-branching-task)
  (declare (ignore type-of-branching-task))
  (let ((sublist nil))
    (loop for input in (inputs task)
	for name = (name input)
	for token = (symbolic-token input)
	for value = (value token)
	do (push (cons name value) sublist))
    (loop for branch in (branches task)
	do (setf (substituted-branch-condition branch) (sublis sublist (branch-condition branch))))
	   ))

;;; The default method for a test (anything other than a test needs its own methods)
;;; just arms the control flows
(defmethod propagate ((task primitive-branching-task) (type-of-branching-task t))
  (loop for branch in (branches task)
      do (task-has-completed-execution branch)))

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
			    :producer task
			    :port output-port
			    :type type-description
			    :value input-value)
	do (set-port-symbolic-token output-port value-token))))

(defmethod propagate ((task task-interface) (type (eql 'vector-length)))
  (let* ((output (first (outputs task)))
	 (multiple-consumers (not (null (rest (outgoing-flows output)))))
	 (vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector-value (value vector-token))
	 (new-variable (next-instance-of-variable 'length))
	 (form (simplified-form `(length ,vector-value)))
	 (token (make-token normal-output
		  :producer task
		  :port output
		  :type 'integer
		  :form form
		  :value (cond ((is-abstract-statement? form) new-variable)
			       (multiple-consumers new-variable)
			       (t form))
		  )))
      (set-port-symbolic-token output token)
      ))

;;; FixMe: Simplifying (element ...) should turn to aref if it's not otherwise
;;; simplified
(defmethod propagate ((task task-interface) (type (eql 'vector-accessor)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector-value (value vector-token))
	 (vector-port-type-constraint (port-type-constraint vector-port))
	 (output-type (if (listp vector-port-type-constraint) (second vector-port-type-constraint) vector-port-type-constraint))
	 (index-port (port-named 'input 'index task))
	 (index-value (value (symbolic-token index-port)))
	 (output (first (outputs task)))
	 ;; (multiple-users (not (null (rest (outgoing-flows output)))))
	 (new-variable (next-instance-of-variable output-type))
	 (unsimplified-form `(element ,vector-value ,index-value))
	 (form (simplified-form unsimplified-form))
	 (token (make-token normal-output
		  :producer task
		  :port output
		  :type output-type
		  :value new-variable
		  :form (if (equal form unsimplified-form) (subst 'aref 'element unsimplified-form) form)
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
	      (rest input-ports)
	      ;; (null output-ports)
	      (rest output-ports))
      (break "You shouldn't be able to get here, probably missing a more specific propagate method"))
    (let* ((input-port (first input-ports))
	   (input-token (symbolic-token input-port))
	   (input-value (value input-token))
	   (task-type (task-type task))
	   (output-port (first output-ports)))
      (when output-port
	(let* ((output-type (port-type-constraint output-port))
	       (new-variable (next-instance-of-variable (if (listp output-type) (second output-type) output-type)))
	       (output-token (make-token normal-output
			       :producer task
			       :port output-port
			       :type output-type
			       :value new-variable
			       :form `(,task-type ,input-value))))
	  (set-port-symbolic-token output-port output-token))))))

(defmethod procedure-name-for-component ((task task-interface) (task-type t) input-values)
  `(,task-type ,@input-values))

(defmethod procedure-name-for-component ((task branching-task) (task-type (eql 'tree-traverse)) input-values)
  (let ((name (getf (properties task) :recursive-call-name)))
    `(,name ,@input-values)))

(defmethod propagate ((task branching-task) (type t))
  (let ((task-type (task-type task))
	(input-values (loop for input-port in (inputs task) 
			  for token = (symbolic-token input-port)
			  for value = (value token)
			  for code = (code token)
			  collect (or code value))))
    (loop for branch in (branches task)
      do (loop for output-port in (outputs branch)
	     for output-type = (port-type-constraint output-port)
	     for new-variable = (if (listp output-type) 
				    (next-instance-of-variable (second output-type))
				  (next-instance-of-variable output-type))
	     for output-token = (make-token normal-output
					    :producer task
					    :port output-port
					    :type output-type
					    :value new-variable
					    :form (procedure-name-for-component task task-type input-values))
	     do (set-port-symbolic-token output-port output-token)))))
				  
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
	 (setf (corresponding-branch exit-point) branch)
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
	   (task-has-completed-execution branch))))
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
	 parent-port))
      (otherwise
       ))))

;;; FixMe: Needs to make a token
(defmethod run-task ((final-output final-output))
  (let* ((task (superior final-output))
	 (port (first (inputs final-output)))
	 (port-name (name port))
	 (value-token (symbolic-token port)))
    (typecase task
      (implementation
       (let* ((abstract-task (abstract-task task))
	      (parent-port (port-named 'output port-name abstract-task)))
	 (set-port-symbolic-token parent-port value-token)))
      (otherwise
       ))))

;;; Move output values along dataflow links
(defmethod port-value-arrived ((p port) (direction (eql 'output)) (task output-side-mixin) value)
  (declare (ignore value))
  (incf (n-outputs-provided task))
  (when (eql (n-outputs-provided task) (n-outputs task))
    (task-has-completed-execution task)))

;;; Note since this is on output-side-mixin, it's not
;;; applicable to any branching task (primitive or compound)
;;; BUT it would be applicable to the branches themselves
;;; if the branches have outputs
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I think this is dead code
(defun all-downstream-tasks (top-level-task)
  (let ((answers nil)
	(state-initializations (mapcar #'car (find-state-initializations top-level-task)))
	(inputs (loop for input in (inputs top-level-task)
		    collect (symbolic-token input))))
    (labels ((do-one (thing)
	       (typecase thing
		 (symbolic-token-mixin
		  (loop for consumer in (consumers thing)
		      do (pushnew consumer answers)
			 (do-one consumer)))
		 (output-side-mixin
		  (loop for output in (outputs thing)
		      for token = (symbolic-token output)
		      do (do-one token)))
		 (initial-input
		  (do-one (symbolic-token (first (outputs thing)))))
		 (primitive-branching-task
		     (loop for branch in (branches thing)
			 do (do-one branch)))
		 (state-sink)
		 (join
		    (let ((superior (superior thing)))
		      (do-one superior))))))
      (loop for thing in state-initializations do (do-one thing))
      (loop for thing in inputs do (do-one thing)))
    answers))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating code after symbolic execution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is for non-primitive tasks
;;; The primitive ones all have their own methods
;;; Compound joining tasks will need a method that overrides this one

(defmethod code-for :around ((task t) (task-type t) (language t))
  (let* ((upstream-tasks (upstream-tasks task))
	 (entry-condition (and (loop for task in upstream-tasks
				   always (or (typep task 'constant-value) (typep task 'state-source) (member task *completed-tasks*)))
			       (not (member task *completed-tasks*))))
	 (not-completed (loop for task in (upstream-tasks task) when (not (member task *completed-tasks*)) collect task))
	 )
    (declare (ignorable not-completed))
    ;; (when not-completed (format *error-output* "~%Not completed ~a for ~a" not-completed task))
    (when entry-condition
      (push task *completed-tasks*)
      ;; (format *error-output* "~%Entering task ~a type ~a" task task-type )
      (let ((answer (call-next-method)))
	;; (format *error-output* "~%Finishing task ~a type ~a ~%  --> ~a" task task-type answer)
	answer))))

(defmethod code-for :around ((task task-interface-mixin) (task-type t) (language (eql :lisp)))
  (let ((answer (call-next-method))
	(introduce-labels (getf (properties task) :introduce-labels))
	(state-variable-code nil)
	)
    ;; only create the binding if we're a sub-task
    ;; a top level task has a design (a composite-task, but not an implementation)
    ;; as its superior
    (when (typep (superior task) 'implementation)
      (setq state-variable-code
	(loop for (source source-name code locality code-exists) in (Find-state-initializations task)
	    when (and code-exists (eql locality 'local) (eql (abstract-task (superior source)) task))
	    collect (list source-name code))))
    ;; (format *error-output* "~%Finishing task ~a type ~a ~%  --> ~a" task task-type answer)
    ;; FixMe: This is lisp specific but is in a method for all languages.
    (when introduce-labels
      (setq answer
	`((labels ((,introduce-labels ,(loop for port in (inputs task) collect (name port))
		     ,@answer))
	    (,introduce-labels ,@(loop for input in (inputs task)
					 for token = (symbolic-token input)
					 for value = (simplified-form (value token))
					 collect value))
	    ))))
    (when state-variable-code
      (setq answer `((let ,state-variable-code 
		       ,@answer))))
    answer))


(defmethod code-for (task task-type language)
  (break "Why am I generating code for ~a ~a ~a" task task-type language)
  (values nil))

(defmethod code-for ((task state-source)  (task-type (eql 'state)) (language (eql :lisp)))
  nil)

(defmethod code-for ((task task-interface-mixin) (task-type (eql 'print)) (language (eql :lisp)))
  (let ((input-values (loop for input-port in (inputs task)
			  for input-token = (symbolic-token input-port)
			  for input-value = (value input-token)
			  collect input-value)))
    `((print ,@input-values))))

(defmethod code-for ((task task-interface-mixin) (task-type (eql 'print)) (language (eql :java)))
  (let ((input-values (loop for input-port in (inputs task)
			  for input-token = (symbolic-token input-port)
			  for input-value = (value input-token)
			  collect input-value)))
    `((:application |System.out.println| ,@input-values))))

(defmethod code-for ((task task-interface-mixin) (task-type t) (language t))
  (if (dont-expand task)
      (generate-opaque-task-code task language)
    (let ((implementation (selected-implementation task)))
      (code-for implementation task language))))

(defmethod generate-opaque-task-code ((task task-interface) (language :lisp))
  (let* ((output-port (first (outputs task))) 
	 (output-token (symbolic-token output-port))
	 (output-form (form output-token))
	 (output-variable (value output-token))
	 (output-consumers (consumers output-token))
	 (downstream-code (downstream-code output-port language)))
    (if (rest output-consumers)
	`((let ((,output-variable ,output-form))
	    ,@downstream-code))
      (list output-form))))


(defmethod generate-opaque-task-code ((task has-branches-mixin) language)
  ;; a special case for generators with an exit branch
  (let* ((branches (branches task))
	 (more-branch (find 'more branches :key #'name))
	 (empty-branch (find 'empty branches :key #'name)))
    (cond 
     ((and (= (length branches) 2) more-branch empty-branch)
      (let* ((more-output-port (first (outputs more-branch)))
	     (more-output-token (symbolic-token more-output-port))
	     (output-variable (value more-output-token))
	     (more-output-consumers (consumers more-output-token))
	     (output-form (form more-output-token))
	     (downstream-code (downstream-code more-output-port language)))
	(if (rest more-output-consumers)
	    `((let ((,output-variable ,output-form))
		,@downstream-code))
	  (list output-form))
	)))))


(defmethod code-for ((task implementation) (task-type t) (language t))
  (loop for sub-task in (children task)
      when (or (typep sub-task 'initial-input)
	       (typep sub-task 'constant-value))
      append (code-for sub-task (task-type sub-task) language)))

;;; here go all the specific method for final-output entry-point exit-point, etc.

(defmethod code-for ((task initial-input) (task-type (eql 'input)) (language t))
  (let* ((port (first (outputs task))))
    (loop for dataflow in (outgoing-flows port)
	for output-port = (output dataflow)
	for task = (task output-port)
	append (code-for task (task-type task) language))))

;;; CHECK:
;;; This is here to make the hello world program work.
;;; Need to make sure it doesn't screw up anything else
(defmethod code-for ((task constant-value) (task-type (eql 'constant)) (language t))
  (let* ((port (first (outputs task))))
    (loop for dataflow in (outgoing-flows port)
	for output-port = (output dataflow)
	for task = (task output-port)
	append (code-for task (task-type task) language))))

;;; Fix?: This seems wrong, shouldn't it be the code for the corresponding port
(defmethod code-for ((final-output final-output) (task-type (eql 'output)) (language t))
  (let* ((task (superior final-output)))
    (typecase task
      (implementation
       (let* ((abstract-task (abstract-task task)))
	 (downstream-code abstract-task language)))
      (otherwise (break "why am I here"))
       )))

;;; Fix me:
;;; If you're actually using the values then this has to be done differently
;;; but I'm not yet doing that, so this should do.
;;; Also rather than making a let to wrap around the downstream code
;;; You could substitute the values (have to do alpha reduction, i.e. code walk and avoid capturing variables)
;;; but for a quick hack I'm just doing 
(defvar *active-joins* nil)

(defmethod code-for ((task join) (task-type (eql 'join)) (language t))
  (let ((superior (superior task)))
    (if (primitive? superior)
	(let ((*completed-tasks* (cons superior *completed-tasks*))
		(*active-joins* (cons task *active-joins*)))
	    (let ((downstream-code (downstream-code superior language)))
	      (when downstream-code
		(dejoined-value downstream-code *active-joins*))))
      (let ((corresponding-entry-point (corresponding-entry-point task)))
	(code-for corresponding-entry-point (task-type corresponding-entry-point) language)
	))))

;;;(defmethod code-for ((task join) (task-type (eql 'join)) (language t))
;;;  (let ((superior (superior task)))
;;;    (if (primitive? superior)
;;;	(let* ((parents-output-tokens (outputs superior))
;;;	       (binding-list (loop for input-port in (inputs task) 
;;;				 for input-token = (symbolic-token input-port)
;;;				 for input-variable = (value input-token)
;;;				 for port-name = (name input-port)
;;;				 for output-port = (find port-name parents-output-tokens :key #'name)
;;;				 for output-token = (symbolic-token output-port)
;;;				 for output-variable = (value output-token)
;;;				 collect (cons output-variable input-variable))))
;;;	  ;; (format *error-output* "~%Doing join ~a of ~a" task superior)
;;;	  (let ((*completed-tasks* (cons superior *completed-tasks*)))
;;;	    (let ((downstream-code (downstream-code superior language)))
;;;	      (when downstream-code
;;;		(sublis binding-list downstream-code)))))
;;;      (let ((corresponding-entry-point (corresponding-entry-point task)))
;;;	(code-for corresponding-entry-point (task-type corresponding-entry-point) language)
;;;	))))

;;; FixMe: Branch condition stuff isn't handled uniformly at all
(defmethod code-for ((task primitive-branching-task) (task-type t) (language t))
  ;; (format *error-output* "~%Coming through primitive branching code ~a" task)
  (let ((branches-code (loop for branch in (branches task)
			   for condition = (substituted-branch-condition branch)
			   for his-code = (code-for branch (task-type branch) language)
			   when his-code 
			   collect (cons condition his-code))))
    (if (rest branches-code)
	`((cond ,@branches-code))
      (let ((only-pair (first branches-code)))
	`((when ,(first only-pair) ,@(rest only-pair)))))))

(defmethod code-for ((entry-point entry-point) (task-type (eql 'entry-point)) (language t))
  (downstream-code entry-point language))

(defmethod code-for ((exit-point exit-point) (task-type t) (language t))
  (let* ((corresponding-branch (corresponding-branch exit-point)))
    (code-for corresponding-branch (task-type corresponding-branch) language)
    ))

(defmethod code-for ((branch branch) (task-type (eql 'branch)) (language t))
  (downstream-code branch language))

(defmethod downstream-code ((output-port port) language)
  (loop for dataflow in (outgoing-flows output-port)
      for port = (output dataflow)
      for task = (task port)
      append (code-for task (task-type task) language)))

(defmethod downstream-code ((task core-task-mixin) language)
  (append
   (loop for output in (outputs task)
       append (loop for dflow in (outgoing-flows output)
		  for next-task = (task (output dflow))
		  append (code-for next-task (task-type next-task) language)))
   (loop for cflow in (outgoing-control-flows task)
	 for next-task = (successor cflow)
	 append (code-for next-task (task-type next-task) language))
   ))

(defmethod downstream-code :around ((task primitive-joining-task) (language t))
  ;; (format *error-output* "~%Entering ~a my-code ~a" task (my-code task))
  (when (null (my-code task))
    (setf (my-code task) (call-next-method)))
  ;; (format *error-output* "~%Exiting ~a my-code ~a" task (my-code task))
  (my-code task)
  )


(defmethod code-for ((task task-interface) (task-type (eql 'take)) (language (eql :lisp)))
  (let* ((input-port (port-named 'input 'sequence-data task))
	 (input-token (symbolic-token input-port))
	 (input-variable (value input-token))
	 (sequence-type (first (type-constraint input-token)))
	 (output-port (first (outputs task)))
	 (output-token (symbolic-token output-port))
	 (output-variable (value output-token))
	 (downstream-code (downstream-code task language)))
    (case sequence-type
      (stream
       ;; the variable might have been converted to viewpoint of something
       ;; to we need this check
       ;; Check: Can this issue occur elsewhere?
       `((loop for ,(if (is-viewpoint? output-variable) (form output-token) output-variable) = (dequeue ,input-variable)
	     do ,@downstream-code)))
      (t downstream-code)
      )))

(defmethod code-for ((task task-interface) (task-type (eql 'truncate)) (language (eql :lisp)))
  (let* ((input-port (port-named 'input 'data task))
	 (input-token (symbolic-token input-port))
	 (input-variable (value input-token))
	 (return-value (getf (properties task) :return-value?)))
    (if return-value
	`((return ,input-variable))
      `((return))
      )))

(defmethod code-for ((task task-interface) (task-type (eql 'truncate)) (language (eql :java)))
  (let* ((input-port (port-named 'input 'data task))
	 (input-token (symbolic-token input-port))
	 (input-variable (value input-token))
	 (return-value (getf (properties task) :return-value?)))
    (declare (ignore return-value input-variable))
    `(:break abnormal-exit)))

(defmethod code-for ((task task-interface) (task-type (eql 'put)) (language (eql :lisp)))
  (let* ((output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (output-form (form output-token))
	 (downstream-code (downstream-code task language))
	 )
    ;; probably the output token should encode whether 
    ;; the put actually does something in a better way than this
    (if (and output-form (eql (first output-form) 'enqueue))
	(cons output-form downstream-code)
      downstream-code)))

(defmethod code-for ((task task-interface) (task-type (eql 'vector-accessor)) (language (eql :lisp)))
  (let* ((output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (value (value output-token))
	 (multiple-users? (rest (consumers output-token)))
	 (form (form output-token))
	 (abstract-statement? (is-abstract-statement? form)))
    (if (and (not abstract-statement?) multiple-users?)
	`((let ((,value ,form))
	    ,@(downstream-code task language)))
      (downstream-code output language))
    ))

(defmethod code-for ((task task-interface) (task-type (eql 'vector-length)) (language (eql :lisp)))
  (let* ((input (first (inputs task)))
	 (input-value-token (symbolic-token input))
	 (input-value (value input-value-token))
	 (output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (output-token-value (value output-token))
	 (multiple-consumers (rest (consumers output-token)))
	 (downstream-code (downstream-code task language)))
    (if multiple-consumers
	`((let ((,output-token-value ,(simplified-form `(length ,input-value))))
	    ,@downstream-code))
      downstream-code)))

(defmethod code-for ((task task-interface) (type (eql 'vector-push)) (language (eql :lisp)))
  (let* ((output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (form (form output-token))
	 (downstream-code (downstream-code task language)))
    (cons form downstream-code)))

(defmethod code-for ((task task-interface) (type (eql 'range-constructor)) (language t))
  (downstream-code task language)
  )

(defmethod code-for ((task primitive-enumerator) (task-type (eql 'index-enumerator)) (language (eql :lisp)))
  (let* ((more-branch (branch-named 'more task))
	 (more-output-port (first (outputs more-branch)))
	 (more-output-port-token (symbolic-token more-output-port))
	 (index (value more-output-port-token))
	 (lower-bound (lower-bound more-output-port-token))
	 (upper-bound (upper-bound more-output-port-token))
	 (more-branch-code (code-for more-branch (task-type more-branch) language))
	 (empty-branch (branch-named 'empty task))
	 (empty-branch-code (code-for empty-branch (task-type empty-branch) language)))
    `((loop for ,index from , lower-bound below ,upper-bound
	      do ,@more-branch-code
		 ,@(when empty-branch-code `(finally ,@empty-branch-code))))
    ))

;;; (loop-variable &key init refresh terminate body)
(defmethod code-for ((task primitive-enumerator) (task-type (eql 'index-enumerator)) (language (eql :java)))
  (let* ((more-branch (branch-named 'more task))
	 (more-output-port (first (outputs more-branch)))
	 (more-output-port-token (symbolic-token more-output-port))
	 (index (value more-output-port-token))
	 (lower-bound (lower-bound more-output-port-token))
	 (upper-bound (upper-bound more-output-port-token))
	 (more-branch-code (code-for more-branch (task-type more-branch) language))
	 (empty-branch (branch-named 'empty task))
	 (empty-branch-code (code-for empty-branch (task-type empty-branch) language)))
    `(:for (:variable ,index)
	   :init (:constant 0)
	   :refresh (:binary-op + (:variable ,index) (:constant 1))
	   :terminate (:binary-comparison < (:variable ,index) (:constant ,upper-bound))
	   :body ,more-branch-code
	   :exit-code ,empty-branch-code
	   :abnormal-exit? (abnormal-exit? task)
	   )))

(defmethod code-for ((task primitive-enumerator) (task-type (eql 'list-enumerator)) (language (eql :lisp)))
  (let* ((more-branch (branch-named 'more task))
	 (more-output-port (first (outputs more-branch)))
	 (more-output-port-token (symbolic-token more-output-port))
	 (element (value more-output-port-token))
	 (the-list-token (the-list more-output-port-token))
	 (the-list (or (form the-list-token) (value the-list-token)))
	 (more-branch-code (code-for more-branch (task-type more-branch) language))
	 (empty-branch (branch-named 'empty task))
	 (empty-branch-code (code-for empty-branch (task-type empty-branch) language)))
    `((loop for ,element in ,the-list
	      do ,@more-branch-code
		 ,@(when empty-branch-code `(finally ,@empty-branch-code))))
    ))

(defmethod code-for ((task task-interface-mixin) (task-type (eql 'allocate)) (language (eql :lisp)))
  ;; the only thing that should be downstream is a state sink
  (downstream-code task language)
  )

(defmethod code-for ((task state-sink) (task-type (eql 'state)) (language (eql :lisp)))
  (let* ((state-name (state-name task))
	 (new-value-port (first (inputs task)))
	 (new-value-token (symbolic-token new-value-port))
	 ; Fix: Kludge because unsure whether it should be code or form
	 (new-value-code (or (form new-value-token) (code new-value-token) (value new-value-token))))
    `(,(simplified-form `(setq ,state-name ,new-value-code)))))

(defmethod code-for ((task task-interface) (type (eql 'add-to-set)) (language (eql :lisp)))
  (let* ((the-set-port (port-named 'input 'the-set task))
	 (the-set-token (symbolic-token the-set-port))
	 (the-set (value the-set-token))
	 (the-element-port (port-named 'input 'the-element task))
	 (the-element-token (symbolic-token the-element-port))
	 (the-element (value the-element-token)))
    (declare (ignore the-element the-set))
    `(;; (pushnew ,the-element ,the-set)
      ,@(downstream-code (first (outputs task)) language)
      )))

(defmethod code-for ((task task-interface) (type (eql 'left)) (language (eql :lisp)))
  (let* ((the-left-port (first (outputs task)))
	 (the-left-token (symbolic-token the-left-port))
	 (the-left (value the-left-token))
	 (left-form (form the-left-token))
	 (downstream-code (downstream-code task language)))
    ;; Fix: Should use viewpoints or something more principled than this
    (when downstream-code
      (if (rest (consumers the-left-token))
	  `((let ((,the-left ,(simplified-form (subst 'first 'left left-form))))
	      ,@downstream-code))
	downstream-code))))
  

(defmethod code-for ((task task-interface) (type (eql 'right)) (language (eql :lisp)))
  (let* ((the-right-port (first (outputs task)))
	 (the-right-token (symbolic-token the-right-port))
	 (the-right (value the-right-token))
	 (right-form (form the-right-token))
	 (downstream-code (downstream-code task language)))
    ;; Fix: Should use viewpoints or something more principled than this
    (when downstream-code
      (if (rest (consumers the-right-token))
	  `((let ((,the-right ,(simplified-form (subst 'rest 'right right-form))))
	      ,@downstream-code))
	downstream-code))))

(defmethod code-for ((task task-interface) (type (eql 'first)) (language (eql :lisp)))
  (let* ((the-first-port (first (outputs task)))
	 (the-first-token (symbolic-token the-first-port))
	 (the-first (value the-first-token))
	 (first-form (form the-first-token))
	 (downstream-code (downstream-code task language)))
    ;; Fix: Should use viewpoints or something more principled than this
    (when downstream-code
      (if (rest (consumers the-first-token))
	  `((let ((,the-first ,(simplified-form first-form)))
	      ,@downstream-code))
	downstream-code))))

(defmethod code-for ((task task-interface) (type (eql 'rest)) (language (eql :lisp)))
  (let* ((the-rest-port (first (outputs task)))
	 (the-rest-token (symbolic-token the-rest-port))
	 (the-rest (value the-rest-token))
	 (rest-form (form the-rest-token))
	 (downstream-code (downstream-code task language)))
    ;; Fix: Should use viewpoints or something more principled than this
    (when downstream-code
      (if (rest (consumers the-rest-token))
	  `((let ((,the-rest ,(simplified-form rest-form)))
	      ,@downstream-code))
	downstream-code))))
  

	 
;;; This is what you do if you want to treat the task as transparent
;;; Eventually we'll need to have a choice about that to deal with reccusive
;;; definitions

(defmethod upstream-tasks ((task t))
  nil)

(defmethod upstream-tasks ((task input-side-mixin))
  (let ((answer nil))
    (loop for input in (inputs task)
	for token = (symbolic-token input)
	for producer = (producer token)
	unless (or (eql producer task) (not (task-is-real-computation producer)))
	do (pushnew producer answer))
    (loop for cflow in (incoming-control-flows task)
	do (pushnew (armer cflow) answer))
    answer))

(defmethod upstream-tasks ((task joining-task))
  (joins task))

(defmethod downstream-tasks ((task t))
  nil)

(defmethod downstream-tasks ((task output-side-mixin))
  (let ((answer nil))
    (loop for output in (outputs task)
	for token = (symbolic-token output)
	do (loop for consumer in (consumers token)
	       do (pushnew consumer answer)))
    (loop for cflow in (outgoing-control-flows task)
	do (pushnew task answer))
    answer))

(defmethod downstream-tasks ((task has-branches-mixin))
  (branches task))


;;; Apparently dead code
;;;(defmethod initial-methods ((task implementation))
;;;  (loop for child-task in (children task)
;;;      when (and (loop for input in (inputs child-task)
;;;		    for his-token = (symbolic-token input)
;;;		    always (or (typep his-token 'initial-input-value-token)
;;;			       (typep his-token 'state-variable-value-token)
;;;			       ))
;;;		(null (incoming-control-flows child-task)))
;;;      collect child-task))

		


	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversation about choices
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun explain-purpose (what-im-trying-to-do to-what &optional qualifiers-of-what singular?)
  ;; in case it's something like (stream pixel))
  (when (listp to-what)
    (setq to-what (second to-what)))
  (let* ((generator (make-instance 'question-generator))
	 (me (intern-entity generator "I" :definite? 'null))
	 (try (intern-verb generator "try" :tense '|present| :progressive t))	 
	 (thing-to-do (intern-verb generator (string what-im-trying-to-do) :tense '|to|))
	 (the-objects (intern-entity generator (string (if singular? (find-singular-of-noun to-what) (find-plural-of-noun to-what)))
				     :singular? singular?))
	 (object-qualifier (when qualifiers-of-what (intern-entity generator (string qualifiers-of-what) :definite? t)))
	 )
    (create-main-clause generator me try thing-to-do :is-question? nil :subject-is-wh? nil :is-imperative? nil)
    (intern-and-add-texp generator (list me thing-to-do the-objects))
    (when qualifiers-of-what (create-possesive generator the-objects object-qualifier))
    generator))

;;; here the better text would be "I can generate the pixels by traversing the image row first"

(defun terms-for-describing-viewpoint (action viewpoint)
  (ask `[terms-for-viewpoint ,viewpoint ,action ?verb ?modifier]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from terms-for-describing-viewpoint 
	     (values ?verb ?modifier))))
  nil)

(defun describe-an-option (what-im-trying-to-do to-what verb modifier &optional also?)
  (when (listp to-what)
    (setq to-what (second to-what)))
  (let* ((generator (make-instance 'question-generator))
	   (me (intern-entity generator "I" :definite? 'null))
	   (thing-to-do (intern-verb generator (string what-im-trying-to-do) :modality '|can|))
	   (the-objects (intern-entity generator (string (find-plural-of-noun to-what)) :singular? nil :definite? t))
	   (aux-verb (intern-new-instance generator (string verb)))
	   (how-to-aux-verb (intern-entity generator (string-downcase (string modifier)) :definite? 'null)))
      (intern-and-add-texp generator (list me thing-to-do the-objects))
      (intern-and-add-texp generator (list me 
			  aux-verb
			  how-to-aux-verb))
      (when also? 
	(add-verb-modifier generator thing-to-do '|has_modifier| '|also|))
      (intern-and-add-texp generator (list thing-to-do (intern-new-instance generator '|has_method|) aux-verb))
      generator))

(defun describe-selected-option (what-im-trying-to-do to-what verb modifier)
  (when (listp to-what)
    (setq to-what (second to-what)))
  (let* ((generator (make-instance 'question-generator))
	 (you (intern-entity generator "You" :definite? 'null))
	 (chose (intern-verb generator "choose" :tense '|past|))
	 (thing-to-do (intern-verb generator (string what-im-trying-to-do)))
	 (the-objects (intern-entity generator (string (find-plural-of-noun to-what)) :singular? nil :definite? t))
	 (aux-verb (intern-new-instance generator (string verb)))
	 (how-to-aux-verb (intern-entity generator (string-downcase (string modifier)) :definite? 'null))
	 (has-method (intern-new-instance generator "has_method")))
    (create-main-clause generator you chose has-method :is-question? nil :subject-is-wh? nil :is-imperative? nil)
    (intern-and-add-texp generator (list you thing-to-do the-objects))
    (intern-and-add-texp generator (list you 
					 aux-verb
					 how-to-aux-verb))
    (intern-and-add-texp generator (list thing-to-do has-method aux-verb))
    (push (current-parse generator) (all-parses generator))
    generator))



 
(defmethod full-symbolic-description ((i implementation))
  (ask `[possible-implementation ,i ?parent ?method ?choices]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from full-symbolic-description
	     (list 'implementation :method ?method :choices ?choices :task (full-symbolic-description ?parent)))))
  nil)

(defmethod full-symbolic-description ((task task-interface-mixin))
  (list 'task 
	:name (name task)
	:parent (full-symbolic-description (superior task))))

(defmethod full-symbolic-description ((task composite-task))
  ;; note this is for composite tasks that aren't implemenetations
  (list 'task :name (name task)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Composition of cliches
;;;
;;;
;;; General idea:
;;;  (Ignore name clashes for the moment)
;;;  1) Build an instantiated version of the cliche in a new parent by
;;;  calling the cliche' function with appropriate arguments
;;;  2) Identify all inputs (initial-inputs and entry-points) 
;;;     add and also add inputs to the corresponding
;;;     interface.  If the interface has joins then add ports to the appropriate
;;;     join and also add port to the corresponding entry point.
;;;  3) Same for all outputs (final-outpus, exit-points).
;;;  4) Remove the components from the temporary superior and then 
;;;     Add the components to the real destination.
;;;     and reconnect the data and control flows as necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-composition-request ((parser core-parser-mixin) main interface cliche-name)
  (declare (ignore main))
  (let* ((cliche (convert-start-string-to-lisp-atom cliche-name)))
    (when (fboundp cliche)
      (compose cliche (composition-args cliche interface) interface))))

(defun compose (cliche-name args interface)
  (let* ((implementation (selected-implementation interface))
	 (temporary-parent (make-instance 'composite-task :name 'temp))
	 (initializer (find-cliche-initializer cliche-name)))
    (apply cliche-name temporary-parent args)
    (let ((inputs (find-initial-inputs temporary-parent))
	  (outputs (find-final-outputs temporary-parent))
	  (components (find-normal-components temporary-parent)))
      (find-or-merge-initial-inputs inputs temporary-parent implementation interface)
      (find-or-merge-outputs outputs temporary-parent implementation interface)
      (find-or-merge-components components temporary-parent implementation)
      (connect-contact-points cliche-name implementation inputs outputs)
      ;; at this point nothing internal is newly created, so we don't need to rewire anything
      ;; (wire-data-flows input-map output-map component-map temporary-parent implementation)
      ;; (wire-control-flows input-map output-map component-map temporary-parent implementation)
      ;; 
      ;; Fixme:  '(json-path) is a total-hack and doesn't need to be
      ;; because "args" is the composition-args which contains what we need
      ;; Futhermore, we might want to add something to build a take
      (when initializer (apply initializer (superior interface) (name interface) args)))))

(defun find-or-merge-initial-inputs (initial-inputs temporary-parent implementation interface)
  (loop with joining-task? = (typep interface 'has-joins-mixin)
      for initial-input in initial-inputs
      for port = (first (outputs initial-input))
      for name = (name port)
      for type-constraint = (port-type-constraint port)
      for appropriate-join-name = (when joining-task? (ask-which-join-to-use interface name))
      do (flet ((add-input (task)
		  (let* ((appropriate-join (when joining-task? (join-named appropriate-join-name task)))
			 (new-port (add-port 'input name (or appropriate-join task))))
		    (add-port-type-description new-port type-constraint))))
	   (affect-all-subcomponents-of-type interface (task-type interface) #'add-input))
	 (cond ((not joining-task?)
		(reparent-child temporary-parent implementation initial-input))
	       (t (let ((entry-point (loop for child in (children implementation)
					  when (and (typep child 'entry-point)
						    (eql (join-name child) appropriate-join-name))
					  return child)))
		    (reparent-port 'output initial-input entry-point port))))))


(defun find-or-merge-outputs (final-outputs temporary-parent implementation interface)
  (loop with branching-task? = (typep interface 'has-branches-mixin)
      for final-output in final-outputs
      for port = (first (inputs final-output))
      for name = (name port)
      for type-constraint = (port-type-constraint port)
      for appropriate-branch-name = (when branching-task? 
				      (if (is-of-type? (task-type interface) 'enumerator)
					  'more
				      (ask-which-branch-to-use interface name)))
      do (flet ((add-output (task)
		  (let* ((appropriate-branch (when branching-task? (branch-named appropriate-branch-name task)))
			 (new-port (add-port 'output name (or appropriate-branch task))))
		    (add-port-type-description new-port type-constraint))))
	   (affect-all-subcomponents-of-type interface (task-type interface) #'add-output))
	 (cond ((not branching-task?)
		(reparent-child temporary-parent implementation final-output))
	       (t (let ((exit-point (loop for child in (children implementation)
					when (and (typep child 'exit-point)
						  (eql (branch-name child) appropriate-branch-name))
					return child)))
		    (reparent-port 'input final-output exit-point port))))))

;;; Everything internal is just copied
(defun find-or-merge-components (components temporary-parent implementation)
  (loop for component in components
      do (reparent-child temporary-parent implementation component))
  )

(defun connect-contact-points (cliche-name design new-input-ports new-output-ports)
  (let ((contact-points (find-contact-points cliche-name design)))
    (multiple-value-bind (input-ports output-ports) (all-ports design)
      (loop for port in contact-points
	  for direction = (direction port)
	  for type-constraint = (port-type-constraint port)
	  do (case direction
	       (input
		(let ((potential-matches (loop for (source-port . source-type-constraint) in output-ports
					     when (and (is-of-type? source-type-constraint type-constraint)
						       (not (member source-port new-output-ports))
						       (not (member source-port contact-points))
						       (not (typep (task port) 'final-output))
						       (not (typep (task port) 'exit-point)))
					     collect source-port)))
		  (if potential-matches
		      (if (null (rest potential-matches))
			(add-dataflow (first potential-matches) port)
			(break "Ambiguous source for contact ~a ~a" potential-matches port))
		    (break "No possible contact points for ~a ~a" port type-constraint))))
	       (output 
		(let ((potential-matches (loop for (target-port . target-type-constraint) in input-ports
					     when (and (is-of-type? type-constraint target-type-constraint)
						       (null (incoming-flows target-port))
						       (not (member target-port new-input-ports))
						       (not (member target-port contact-points))
						       (not (typep (task port) 'initial-input))
						       (not (typep (task port) 'entry-point)))
					     collect target-port)))
		  (if potential-matches
		    (if (null (rest potential-matches))
			(add-dataflow port (first potential-matches))
		      (break "Ambiguous target for contact ~a ~a" potential-matches port))
		    (break "No possible matches for ~a ~a" port type-constraint)))))))))
			 
			 
(defun all-ports (design)
  (let ((input-ports nil)
	(output-ports nil))
    (flet ((collect-inputs (component)
	     (loop for port in (inputs component)
		 do (push (cons port (port-type-constraint port)) input-ports)))
	   (collect-outputs (component)
	     (loop for port in (outputs component)
		 do (push (cons port (port-type-constraint port)) output-ports))))  
    (loop for component in (children design)
	when (typep component 'input-side-mixin) 
	do (collect-inputs component) 
	when (Typep component 'output-side-mixin)
	do (collect-outputs component)
	when (typep component 'has-branches-mixin)
	do (loop for branch in (branches component)
	       do (collect-outputs branch))
	when (typep component 'has-joins-mixin)
	do (loop for join in (joins component)
	       do (collect-outputs join))))
    (values input-ports output-ports)))

(defun find-contact-points (cliche-name design)
  (let ((port-descriptions nil))	
    (ask `[contact-point ,cliche-name ?point]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push ?point port-descriptions)))
    (let ((ports nil))
      (loop for port-description in port-descriptions
	  do (destructuring-bind (&key branch join component name direction) port-description
	       (let ((component (child-named design component)))
		 (when branch (setq component (branch-named branch component)))
		 (when join (setq component (join-named join component)))
		 (push (port-named direction name component) ports))))
      ports)))

(defun find-cliche-initializer (cliche-name)
  (ask `[initializer-for-merge ,cliche-name ?init-name]
       #'(lambda (just)
	   (declare (ignore just))
	   (return-from find-cliche-initializer ?init-name))))


(defun non-terminal-of-tree-type (tree-type)
  (ask `[subtype ?sub-type ,tree-type]
       #'(lambda (just)
	   (declare (ignore just))
	   (ask [is-of-type ?sub-type tree-non-terminal-node]
		#'(lambda (just)
		    (declare (ignore just))
		    (return-from non-terminal-of-tree-type ?sub-type)))))
  nil)

(defun terminal-of-tree-type (tree-type)
  (ask `[subtype ?sub-type ,tree-type]
       #'(lambda (just)
	   (declare (ignore just))
	   (ask [is-of-type ?sub-type tree-terminal-node]
		#'(lambda (just)
		    (declare (ignore just))
		    (return-from terminal-of-tree-type ?sub-type)))))
  nil)

(defmethod handle-connect-request ((parser core-parser-mixin) main from-what to-what)
  (declare (ignore main))
  (let* ((the-port (first (outputs from-what)))
	 (his-type (port-type-constraint the-port))
	 (name (if (listp his-type) (second his-type) his-type)))
    (connect-port-to-task the-port to-what name)))

(defun connect-port-to-task (port task-name &optional (name (name port)))
  (let* ((port-task (task port))
	 (task (find-component-above-named port-task task-name))
	 (final-level (superior task))
	 (port-type-constraint (port-type-constraint port))
	 )
    (let ((final-source-port (lift-output-to-level port final-leveL name))
	  (final-destination-port (add-port 'input name task)))
      (tell `[port-type-constraint ,final-destination-port ,port-type-constraint])
      (make-dataflow 'output name (task final-source-port)
		     'input name task))))

(defun lift-output-to-level (port destination-task name)
  (let* ((task (task port))
	 (port-name (name port))
	 (type (port-type-constraint port))
	 (destination-level (superior destination-task)))
    (labels ((do-one-level (implementation)
	       (let ((final-output (add-final-output implementation name name type)))
		 (make-dataflow 'output port-name task
				'input name final-output))
	       (let* ((task (abstract-task implementation))
		      (superior (superior task))
		      (added-port (add-port 'output name task)))
		 (tell `[port-type-constraint ,added-port ,type])
		 (cond
		  ((or (not (typep  superior 'implementation)) (eq superior destination-level))
		   (do-one-level superior))
		  (t (return-from lift-output-to-level (values added-port))))
		  )))
     (do-one-level (superior task)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The planning macros:
;;; 
;;; Defcliche defines a parameterized cliche' a set of components, dataflows and controlflows
;;;
;;; Defreduction is a piece of planning knowledge, reducing higher level goals to lower level ones
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun process-plan-query-description (plist)
    (pop plist)
    (let ((type (getf plist :type))
	  (instance (or (getf plist :instance) (make-logic-variable-maker)))
	  (parameters (getf plist :parameters))
	  (previous-choices (getf plist :previous-choices))
	  (new-choices (getf plist :new-choices))
	  (sub-plan (or (getf plist :sub-plan) (make-logic-variable-maker))))
      (when (and (null previous-choices) (null new-choices))
	(setq previous-choices (make-logic-variable-maker)
	      new-choices previous-choices))
      `(predication-maker '(plan-for ,type ,instance ,parameters ,sub-plan ,previous-choices ,new-choices))))
  
  (defun make-logic-variable-maker (&optional (name "?ANONYMOUS"))
    (unless (char-equal (aref name 0) #\?)
      (setq name (concatenate 'string "?" name)))
    (setq name (string-upcase name))
    `(logic-variable-maker ,(intern (gensym name))))
  )


(defmacro defreduction (reduction-name (type arguments) &key the-instance reduce-to prerequisites previous-choices new-choices actions subplans the-plan)
  (flet ((make-anonymous-variable ()
	   (make-logic-variable-maker "ANONYMOUS")))
    (unless the-instance (setq the-instance (make-anonymous-variable)))
    (when (and (null previous-choices) (null new-choices))
      (setq previous-choices (make-anonymous-variable)
	    new-choices previous-choices))
    (unless the-plan (setq the-plan (make-logic-variable-maker "plan"))))
  (let ((processed-subplans (loop for subplan in subplans collect (process-plan-query-description subplan))))
    `(defrule ,reduction-name (:backward)
       :then (predication-maker '(plan-for ,type ,the-instance ,arguments ,the-plan ,previous-choices ,new-choices))
       :if (predication-maker 
	    '(and 
	      ,@prerequisites
	      ,@processed-subplans
	      ,@(when reduce-to `((unify ,the-plan ,(typecase reduce-to 
						      (logic-variable-maker reduce-to)
						      (symbol reduce-to)
						      (list `(list ',(first reduce-to) ,@(rest reduce-to)))))))
	      ,@actions
	      ))
       )))

(defun parameter-names (type)
  (let ((names nil))
    (ask `[has-property ,type ?property]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push ?property names)))
    names))

(defun port-names (type direction)
  (let ((names nil))
    (ask `[has-port ,type ,direction ?name]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push ?name names)))
    names))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Newer version of defcliche
;;;
;;; Language Definition:
;;;   Initial-input:  (:name <name> :type <Type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Final-output:   (:name <name> :type <type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Entry-point:    (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   Exit-point:     (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   State-element   (:direction <source/sink> :name <name> :port-name <port-name> :port-type <port-type>)
;;;   Component:      (:name <name> :type <type> ,@[<propterty-keyword <property-value> ... ])
;;;   Constant:       (:name <name> :type <type> :value <value>)
;;;   Dataflow:       ((:component <component-name> :port <port-name> (optional :branch <branch-name>)
;;;                    (:component <component-name> :port <port-name> (optional :branch <branch-name>)))
;;;   Control-flow:   ((:component <component-name> (optional :branch <branch-name>))
;;;                    (:component <component-name> (optional :branch <join-name>)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel)

(defun process-bindings (binding-set)
  (loop for (name form) in binding-set
      collect (list (devariablize name 'normal) (devariablize form 'raw)))
  )

(defun process-initial-input-description (form parent-name)
  (destructuring-bind (&key name type ((:port port-name)) ((:branch branch-name))) form
    (when (null port-name) (setq port-name name))
    `(add-initial-input ,parent-name ,(devariablize name) ,(devariablize port-name) ,(devariablize type 'normal) ,(devariablize branch-name))))

(defun process-final-output-description (form parent-name)
  (destructuring-bind (&key name type ((:port port-name)) ((:branch branch-name))) form
    (when (null port-name) (setq port-name name))
    `(add-final-output ,parent-name ,(devariablize name) ,(devariablize port-name) ,(devariablize type 'normal) ,(devariablize branch-name))))

(defun process-entry-point-description (form parent-name)
  (destructuring-bind (&key name ports) form
    (let ((ports (loop for entry in ports
		     collect (destructuring-bind (&key name type) entry 
			       (list name type)))))
      `(add-entry-point ,parent-name ,(devariablize name) ,(devariablize name) ,(devariablize ports 'normal)))))

(defun process-exit-point-description (form parent-name)
  (destructuring-bind (&key name ports) form
    (let ((ports (loop for entry in ports
			   collect (destructuring-bind (&key name type) entry 
				     (list name type)))))
      `(add-exit-point ,parent-name  ,(devariablize name) ,(devariablize name) ,(devariablize ports 'normal)))))

(defun process-state-element-description (form parent-name)
  (destructuring-bind (&key direction name port-name port-type locality ((:state state-name))) form
    (unless state-name (setq state-name name))
    (ecase direction
      (source `(add-state-source ,parent-name ,(devariablize name) ,(devariablize port-name) ,(devariablize port-type 'normal) ,(devariablize state-name) 
				 ',locality))
      (sink `(add-state-sink ,parent-name ,(devariablize name) ,(devariablize port-name) ,(devariablize port-type 'normal) ,(devariablize state-name))))))

(defun process-constant-description (form parent-name)
  (destructuring-bind (&key name type value) form
    `(add-constant ,parent-name ,(devariablize name) ,(devariablize type) ,(devariablize value))))


(defun process-component-description (form parent-name)
  (destructuring-bind (&rest plist &key type name &allow-other-keys) form
    (with-keywords-removed (property-plist plist '(:name :type))
      `(create-task ,(devariablize name) ,(devariablize type) ,parent-name ,@(devariablize property-plist 'top-level)))))

(defun process-control-flow-description (form parent-name)
  (destructuring-bind ((&key ((:component source-component-name)) ((:branch source-branch-name)))
		       (&key ((:component destination-component-name)) ((:branch destination-join-name))))
      form
    (let ((destination-component (if (null destination-join-name)
				     `(child-named ,parent-name ,(devariablize destination-component-name))
				   `(join-named ,(devariablize destination-join-name) (child-named ,parent-name ,(devariablize destination-component-name)))))
	  (source-component (if (null source-branch-name)
				`(child-named ,parent-name ,(devariablize source-component-name))
			      `(branch-named ,(devariablize source-branch-name) (child-named ,parent-name ,(devariablize source-component-name))))))
      `(make-control-flow ,source-component ,destination-component))
      ))

(defun process-dataflow-description (form parent-name)
  (destructuring-bind ((&key ((:component source-component-name)) ((:port source-port-name)) ((:branch source-branch-name)))
		       (&key ((:component destination-component-name)) ((:port destination-port-name)) ((:branch destination-branch-name))))
      form
    (cond 
     ((null source-branch-name)
      (setq source-component-name `(child-named ,parent-name ,(devariablize source-component-name))
	    source-port-name (devariablize source-port-name)))
     (t (setq source-component-name `(branch-named ,(devariablize source-branch-name) (child-named ,parent-name ,(devariablize source-component-name)))
	      source-port-name (devariablize source-port-name))))
    (cond
     ((null destination-branch-name)
      (setq destination-component-name `(child-named ,parent-name ,(devariablize destination-component-name))
	    destination-port-name (devariablize destination-port-name)))
     (t (setq destination-component-name `(join-named ,(devariablize destination-branch-name) (child-named ,parent-name ,(devariablize destination-component-name)))
	      destination-port-name (devariablize destination-port-name))))
    `(make-dataflow 'output ,source-port-name ,source-component-name
		    'input ,destination-port-name ,destination-component-name)))

(defun process-contact-point-description (cliche-name contact-point-description)
  `(tell [contact-point ,cliche-name ,contact-point-description]))

(defun devariablize (token &optional (mode 'top-level)) 
  (flet ((variable-from-lv (token) (intern (subseq (string (second token)) 1))))
    (typecase token
      (null nil)
      (logic-variable-maker (variable-from-lv token))
      (number token)
      (list (ecase mode
	      (raw (loop for thing in token collect (devariablize thing mode)))
	      (top-level (loop for thing in token collect (devariablize thing 'normal)))
	      (normal `(list ,@(loop for thing in token collect (devariablize thing 'normal))))))
      (keyword token)
      (symbol (if (eql mode 'raw) token `',token)))))

(defmacro defcliche (plan-name args &key components dataflows control-flows 
					 initial-inputs final-outputs
					 entry-points exit-points state-elements
					 contact-points initializer
					 bindings
					 declarations
					 constants
					 )
  (let ((parent-name (gensym)))
    `(eval-when (:load-toplevel :execute :compile-toplevel)
       (defun ,plan-name (,parent-name ,@(mapcar #'devariablize args))
	 (declare (ignorable ,@(mapcar #'devariablize args)))
	 (declare ,@declarations)
	 (let* ,(process-bindings bindings)
	 ,@(loop for input in initial-inputs collect (process-initial-input-description input parent-name))
	 ,@(loop for output in final-outputs collect (process-final-output-description output parent-name))
	 ,@(loop for entry-point in entry-points collect (process-entry-point-description entry-point parent-name))
	 ,@(loop for exit-point in exit-points collect (process-exit-point-description exit-point parent-name))
	 ,@ (loop for constant in constants collect (process-constant-description constant parent-name))
	 ,@(loop for state-element in state-elements collect (process-state-element-description state-element parent-name))
	 ,@(loop for component in components collect (process-component-description component parent-name))
	 ,@(loop for dataflow in dataflows collect (process-dataflow-description dataflow parent-name))
	 ,@(loop for cflow in control-flows collect (process-control-flow-description cflow parent-name)))
	 )
       ,@(loop for contact-point in contact-points collect (process-contact-point-description plan-name contact-point))
       (tell `[initializer-for-merge ,',plan-name ,',initializer])
       )
    ))
)

(defun sub-types (type)
  (let ((answer nil))
    (ask `[subtype ?sub ,type]
	 #'(lambda (just)
	     (declare (ignore just))
	     (pushnew ?sub answer)))
    answer))





(defmethod find-ports-producing-type (type (design implementation))
  (loop for task in (children design)
      append (find-ports-producing-type type task)))

(defmethod find-ports-producing-type (type (task branching-task))
  (loop for branch in (branches task)
      append (loop for port in (outputs branch)
		 when (equal (port-type-constraint port) type)
		 collect (list task branch port))))

(defmethod find-ports-producing-type (type (task output-side-mixin))
  (loop for port in (outputs task)
      when (equal (port-type-constraint port) type)
      collect (list task port)))

(defmethod find-ports-producing-type ((type t) (task t)) nil)

(defmethod find-ports-consuming-type (type (design implementation))
  (loop for task in (children design)
      append (find-ports-consuming-type type task)))

(defmethod find-ports-consuming-type (type (task joining-task))
  (loop for join in (joins task)
      append (loop for port in (inputs join)
		 when (equal (port-type-constraint port) type)
		 collect (list task join port))))

(defmethod find-ports-consuming-type (type (task input-side-mixin))
  (loop for port in (inputs task)
      when (equal (port-type-constraint port) type)
      collect (list task port)))

(defmethod find-ports-consuming-type ((type t) (task t)) nil)