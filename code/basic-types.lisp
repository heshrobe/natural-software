;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic computer design objects
;;;   The original version of this code is in /Research-projects/Oasis/delay-sim/code/
;;;   That was probably more complicated than I'll need for this particular hack
;;;   So this is a reduced version of that code.  Added complexity can be found at the source
;;;   The code in delay sim was an event based simulator
;;;   all that infrastructure is being remove
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :natsoft)

;;;; basic classes 
;;; parent child path-name task-type

(defclass has-task-type-mixin ()
  ((task-type :initform nil :initarg :task-type :accessor task-type)))

(defclass has-name-mixin ()
  ((name :initarg :name :accessor name)))

(defclass has-superior-mixin ()
  ((superior :initarg :superior :accessor superior :initform nil)))

(defmethod initialize-instance :after ((task has-superior-mixin) &rest stuff)
  (declare (ignore stuff))
  (let ((parent (superior task)))
    (when parent
      (add-child parent task))))

(defclass has-pathname-mixin (has-superior-mixin has-name-mixin)
  ())

(defclass has-display-tick-mixin ()
  ((display-output-tick :initform 0 :accessor display-output-tick))
  )

(defmethod path-name ((task has-pathname-mixin) &optional relative-to)
  ;; the relative-to optional is to make this compatible with 
  ;; Joshua which provides such an option in its object-model
  (with-slots (name superior) task	
    (if (or (null superior) (eql superior relative-to))
	(list name)
      (cons name (path-name superior)))))

(defmethod print-object ((task has-pathname-mixin) stream)
  (format stream "#<~a " (common-lisp:type-of task))
  (print-pathname task stream)
  (format stream ">"))

(defmethod print-pathname ((task has-pathname-mixin) stream &optional relative-to)
  (format stream "~{~a~^.~}" (path-name task relative-to))
  )



;;; Logically this wouldn't appear until refinement, since these fields are only
;;; used there.  However, I want this stuff mixed into any element that's part of the simulation
;;; so it's hear.

(defclass simulation-support-mixin ()
  ((my-code :accessor my-code :initform nil)
   (ready-to-run :initform nil :accessor ready-to-run)
   (execution-started :initform nil :accessor execution-started)
   (execution-finished :initform nil :accessor execution-finished)
   ))

;;; During code generation something downstream from a join
;;; gets turned into a proxy of the real-task (a virtual copy)
;;; This is the backpointer to those proxies
(defclass has-proxies-mixin ()
  ((proxies :initform nil :accessor proxies))
  )

;;; this mixin is used by the sort-stage of the code generator but
;;; it's mixed into a bunch of core things in this file

(defclass up-down-cache-mixin ()
  ((cached-upstream-tasks :initform nil :accessor cached-upstream-tasks)
   (cached-downstream-tasks :initform nil :accessor cached-downstream-tasks)
   (all-upstream-tasks :accessor all-upstream-tasks :initform nil)
   (all-downstream-tasks :accessor all-downstream-tasks :initform nil)))

;;; A basic building block
(defclass core-task-mixin (has-proxies-mixin has-display-tick-mixin
			   has-pathname-mixin has-task-type-mixin simulation-support-mixin 
			   up-down-cache-mixin)
  ((selected? :initform nil :accessor selected?)
   (output-record :initform nil :accessor output-record)
   )
  )

;;;; mixin for composite objects

(defclass has-children-mixin ()
  ((children :initarg :children :accessor children :initform nil)
   (executable-children :initform nil :accessor executable-children)
   (body-instantiated :initform nil :reader body-instantiated?)))

(defmethod add-child ((parent has-children-mixin) (child has-superior-mixin))
  (setf (superior child) parent)
  (setf (children parent)
    (nconc (children parent) (list child)))
  )

(defmethod remove-child ((parent has-children-mixin) (child has-superior-mixin))
  (setf (superior child) nil)
  (setf (children parent) (remove child (children parent)))
  (loop for port in (inputs child)
      do (remove-port (direction port) (name port)  child))
  (loop for port in (outputs child)
      do (remove-port (direction port) (name port)  child))
  (remove-incoming-control-flows child)
  (remove-outgoing-control-flows child)
  (untell `[component-of ,parent ,child])
  (untell `[type-of ,child ?])
  (untell `[property-value-of ,child ?name ?value])
  )

(defmethod reparent-child ((old-parent has-children-mixin) (new-parent has-children-mixin) (child has-superior-mixin))
  (setf (superior child) new-parent)
  (setf (children old-parent) (remove child (children old-parent)))
  (push child (children new-parent))
  (untell `[component-of ,old-parent ,child])
  (tell `[component-of ,new-parent ,child])
  )

(defmethod child-named ((parent has-children-mixin) (name symbol))
  (find name (children parent)
	:key #'name))

(defmethod kill ((parent has-children-mixin))
  (loop for child in (children parent)
      do (remove-child parent child)))



;;;; basic classes for positioning in task network
;;;  predecessors, successors control-flow

(defclass control-flow ()
  ((predecessor :initarg :predecessor :accessor predecessor :initform nil)
   (successor :initarg :successor :accessor successor :initform nil)
   (arming-token :initform nil :accessor arming-token)
   (armer :initform nil :accessor armer)
   ))

(defclass has-predecessors-mixin ()
  ((incoming-control-flows :initform nil :accessor incoming-control-flows))) 

(defmethod add-predecessor ((task has-predecessors-mixin) (the-flow control-flow))
  (push the-flow (incoming-control-flows task))
  the-flow)

(defmethod remove-predecessor ((task has-predecessors-mixin) (the-flow control-flow))
  (setf (incoming-control-flows task) (remove the-flow (incoming-control-flows task))))

(defclass has-successors-mixin ()
  ((outgoing-control-flows :initform nil :accessor outgoing-control-flows)
   (completion-task :initform nil :accessor completion-task)))

(defmethod add-successor ((task has-successors-mixin) (the-flow control-flow))
  (push the-flow (outgoing-control-flows task))
  the-flow)

(defmethod remove-successor ((task has-successors-mixin) (the-flow control-flow))
  (setf (outgoing-control-flows task) (remove the-flow (outgoing-control-flows task))))

;;; control flow

(defmethod make-control-flow ((input has-successors-mixin) (output has-predecessors-mixin))
  (let ((the-flow (make-instance 'control-flow :predecessor input :successor output)))
    (add-predecessor output the-flow)
    (add-successor input the-flow)
    ;; (controlled-tell `[controlflow-from ,the-flow ,input ,output])
    (tell `[controlflow-from ,the-flow ,input ,output])
    the-flow))

(defmethod make-control-flow ((input cons) (output has-predecessors-mixin))
  (destructuring-bind (branch-name input-task) input
    (let ((the-branch (branch-named branch-name input-task)))
      (make-control-flow the-branch output))))

(defmethod make-control-flow ((input has-successors-mixin) (output cons))
  (destructuring-bind (join-name output-task) output
    (let ((the-join (join-named join-name output-task)))
      (make-control-flow input the-join))))

(defmethod remove-control-flow ((c control-flow))
  (let ((predecessor (predecessor c))
	(successor (successor c)))
    (tell `[controlflow-from ,c ,predecessor ,successor])
    (remove-predecessor successor c)
    (remove-successor predecessor c)))

(defmethod control-flow-from ((from has-successors-mixin) (to has-predecessors-mixin))
  (loop for flow in (outgoing-control-flows from)
      for target = (successor flow)
      when (eql to target)
      return flow))




;;;; basic classes dealing with storage of values
;;; ports and guys that have them.

(defgeneric add-port (direction name module))
(defgeneric port-named (direction name module))

(defclass could-be-problematic-mixin ()
  ((is-problematic? :initform nil :accessor is-problematic?)))

(defclass port (has-name-mixin could-be-problematic-mixin)
  ((task :initarg :task :accessor task)
   (direction :initarg :direction :accessor direction)
   (type-constraint-string :initform nil :accessor type-constraint-string)
   ;; (value :initarg :value :accessor value)
   (incoming-flows :initarg :incoming-flows :accessor incoming-flows :initform nil)
   (outgoing-flows :initarg :outgoing-flows :accessor outgoing-flows :initform nil)
   ;; This is for symbolic evaluation
   (symbolic-token :initform nil :accessor symbolic-token)
   ;; for an input port of an interface this would be the initial-input in the implementation
   ;; for an output port of an interface this would be the final-output in the implementation
   (corresponding-port :initform nil :accessor corresponding-port)
   ))

;;; Return the most specific type constraint
(defmethod port-type-constraint ((p port))
  (let ((answers nil))
    (ask `[port-type-constraint ,p ?type]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push ?type answers)))
    (sort answers #'is-of-type?)
  (first answers)))

(defmethod type-constraint-string :around ((p port))
  (let ((answer (call-next-method)))
    (unless answer
      (let ((type-constraint (port-type-constraint p)))
	(when type-constraint
	  (setq answer (format nil "~a" type-constraint)))))
    (setf (type-constraint-string p) answer)
    answer))

(defmethod remove-dataflows ((port port))
  (loop for flow in (incoming-flows port)
      do (remove-dataflow flow))
  (loop for flow in (outgoing-flows port)
      do (remove-dataflow flow)))

(defclass has-input-ports-mixin ()
    ((inputs :initarg :inputs :accessor inputs :initform nil)))

(defmethod port-named ((direction (eql 'input)) name (module has-input-ports-mixin))
  (find name (inputs module) :key #'name))

(defmethod add-port :around (direction name (module core-task-mixin))
  (let ((existing-port (port-named direction name module)))
    (or existing-port
	(let ((the-new-port (call-next-method)))
	  ;; (controlled-tell `[port-of ,module ,the-new-port] :justification :premise)
	(values the-new-port (tell `[port-of ,module ,the-new-port] :justification :premise))))))

(defmethod add-port ((direction (eql 'input)) name (module has-input-ports-mixin))
  (let ((the-port (make-instance 'port 
		    :task module 
		    :direction direction 
		    :name name )))
    (setf (inputs module) (nconc (inputs module) (list the-port)))
    the-port))

(defmethod add-port-type-description ((port port) (constraint t))
  (tell `[port-type-constraint ,port ,constraint]))

(defmethod remove-port-type-descriptions ((port port))
  (setf (type-constraint-string port) nil)
  (untell `[port-type-constraint ,port ?]))

(defmethod remove-port :around (direction name module)
  (declare (ignore direction name))
  (let ((the-port (call-next-method)))
    (when the-port
      (untell `[port-of ,module ,the-port])
      (untell `[port-type-constraint ,the-port ?]))))

(defmethod remove-port ((direction (eql 'input)) name (module has-input-ports-mixin))
  (let ((the-port (port-named direction name module)))
    (when the-port
      (remove-dataflows the-port)
      (setf (inputs module) (remove the-port (inputs module))))
    the-port))
      
(defclass has-output-ports-mixin ()
  ((outputs :initarg :outputs :accessor outputs :initform nil)))

(defmethod port-named ((directon (eql 'output)) name (module has-output-ports-mixin))
  (find name (outputs module) :key #'name))

(defmethod add-port ((direction (eql 'output)) name (module has-output-ports-mixin))
  (let ((the-port (make-instance 'port 
		    :task module 
		    :direction direction 
		    :name name)))
    (setf (outputs module) (nconc (outputs module) (list the-port)))
    the-port))

(defmethod remove-port ((direction (eql 'output)) name (module has-output-ports-mixin))
  (let ((the-port (port-named direction name module)))
    (when the-port
      (remove-dataflows the-port)
      (setf (outputs module) (remove the-port (outputs module))))
    the-port))

(defmethod print-object ((p port) stream)
  (format stream "#<port ~a ~a" (direction p) (name p))
  (let ((task (task p)))
    (when task
      (write-char #\. stream)
      (print-pathname task stream)))
  (format stream ">"))



;;;; dataflows

(defclass dataflow (could-be-problematic-mixin)
  ((input :initarg :input :accessor input)
   (output :initarg :output :accessor output))
  )

(defmethod print-object ((flow dataflow) stream)
  (format stream "#<dataflow ~a ~a>" (input flow) (output flow))
  )

;;; Notice that this doesn't supoprt output-to-output or input-to-input
;;; dataflows that would be present when connecting to next higher
;;; level of the deisgn
(defmethod make-dataflow (idirection input-name input-task
                          odirection output-name output-task)
  (let ((iport (port-named idirection input-name input-task))
        (oport (port-named odirection output-name output-task)))
    (unless iport (error "No port named ~a of ~a" input-name input-task))
    (unless oport (error "No port named ~a of ~a" output-name output-task))
    (add-dataflow iport oport)))

(defmethod add-dataflow ((input-port port) (output-port port))
  (let ((dataflow (make-instance 'dataflow :input input-port :output output-port)))
    (push dataflow (outgoing-flows input-port))
    (push dataflow (incoming-flows output-port))
    ;; (controlled-tell `[dataflow-from ,dataflow ,input-port ,output-port] :justification :premise)
    (tell `[dataflow-from ,dataflow ,input-port ,output-port] :justification :premise)))

(defmethod remove-dataflow ((flow dataflow))
  (let ((input-port (input flow))
	(output-port (output flow)))
    (setf (outgoing-flows input-port) (remove flow (outgoing-flows input-port))
	  (incoming-flows output-port) (remove flow (incoming-flows output-port)))
    (untell `[dataflow-from ,flow ,input-port ,output-port])
    ))

(defmethod dataflow-from ((from port) (to port))
  (loop for flow in (outgoing-flows from)
      for target = (output flow)
      when (eql target to)
      return flow))



;;;; Conditional Execution

(defclass has-branches-mixin ()
    ((branches :initform nil :initarg :branches :accessor branches))
  )

(defmethod branch-named (name (task has-branches-mixin))
  (find name (branches task) :key #'name))

(defmethod remove-child :after ((parent has-children-mixin) (child has-branches-mixin))
  (loop for branch in (branches child)
      do (loop for port in (outputs branch)
	     do (remove-port (direction port) (name port) branch))
	 (remove-outgoing-control-flows branch)))

(defclass branch-mixin (output-side-mixin)
  ((condition :initarg :condition :accessor branch-condition)
   (substituted-branch-condition :accessor substituted-branch-condition :initform nil)
   (corresponding-exit-point :initform nil :accessor corresponding-exit-point :initarg :corresponding-exit-point))
  )

(defclass branch (branch-mixin core-task-mixin)
    ())

;;; Assumption: Add-child here is being called by 
;;; an interface constructor that is over-riding the parent's constructor
(defmethod add-child :around ((parent has-branches-mixin) (the-branch branch-mixin))
  (let* ((name (name the-branch))
	 (existing-branch (branch-named name parent)))
    (when existing-branch (remove-child parent existing-branch))
    (call-next-method)))

(defmethod add-child ((parent has-branches-mixin) (the-branch branch-mixin))
  (setf (branches parent) (append (branches parent) (list the-branch)))
  (values the-branch
	  ;; (controlled-tell `[branch-of ,parent ,the-branch] :justification :premise)
	  (tell `[branch-of ,parent ,the-branch] :justification :premise)))

(defmethod remove-child ((parent has-branches-mixin) (the-branch branch-mixin))
  (setf (branches parent) (delete the-branch (branches parent)))
  (untell `[branch-of ,parent ,the-branch])
  )



;;; useful building bloxks

;;; default methods
(defmethod inputs ((task core-task-mixin)) nil)
(defmethod outputs ((task core-task-mixin)) nil)
(defmethod is-target-of-branch ((task core-task-mixin)) nil)

;;; These fields are only here to support simulation

(defclass input-side-mixin (has-predecessors-mixin has-input-ports-mixin)
  ((n-inputs-provided :initform 0 :accessor n-inputs-provided)
   (n-inputs :initform 0 :accessor n-inputs)
   ))

(defmethod remove-incoming-control-flows ((task core-task-mixin)) (values))

(defmethod remove-incoming-control-flows ((task input-side-mixin))
  (loop for cf in (incoming-control-flows task)
      do (remove-control-flow cf)))

(defmethod remove-outgoing-control-flows ((task core-task-mixin)) (values))

(defmethod add-port :before ((direction (eql 'input)) name (module input-side-mixin))
  (declare (ignore name))
  (incf (n-inputs module)))

(defmethod remove-port :before ((direction (eql 'input)) name (module input-side-mixin))
  (declare (ignore name))
  (decf (n-inputs module)))

(defclass output-side-mixin (has-successors-mixin has-output-ports-mixin)
  ( ;;; idea is that as our implemenation accumulates final
   ;;; outputs we delay execution until all the final outputs
   ;;; are available.  This forces execution to go completely 
   ;;; through at one level before it continues a level up.
   (n-outputs-provided :initform 0 :accessor n-outputs-provided)
   (n-outputs :initform 0 :accessor n-outputs)
   ))

(defmethod remove-outgoing-control-flows ((task output-side-mixin))
  (loop for cf in (outgoing-control-flows task)
	do (remove-control-flow cf)))

(defmethod add-port :before ((direction (eql 'output)) name (module output-side-mixin))
  (declare (ignore name))
  (incf (n-outputs module)))

(defmethod remove-port :before ((direction (eql 'output)) name (module output-side-mixin))
  (declare (ignore name))
  (decf (n-outputs module)))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Joins
;;; Rejoining after branches
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass has-joins-mixin ()
  ((joins :initform nil :initarg :joins :accessor joins)
   (corresponding-branch :Initform nil :accessor corresponding-branch)
   (join-to-execute :initform nil :accessor join-to-execute)))

(defmethod join-named (name (task has-joins-mixin))
  (find name (joins task) :key #'name))

(defmethod remove-incoming-control-flows ((task has-joins-mixin))
  (loop for join in (joins task)
      do (remove-incoming-control-flows join)))

(defmethod remove-child :after ((parent has-children-mixin) (child has-joins-mixin))
  (loop for join in (joins child)
      do (loop for port in (inputs join)
	     do (remove-port (direction port) (name port) join))
	 (remove-incoming-control-flows join)))


(defclass join-mixin (input-side-mixin)
  ())

(defclass join (join-mixin core-task-mixin)
  ((corresponding-entry-point :initform nil :accessor corresponding-entry-point)
   (downstream-proxies :initform nil :accessor downstream-proxies))
  )

(defmethod add-child :around ((parent has-joins-mixin) (the-join join-mixin))
  (let* ((name (name the-join))
	 (existing-join (join-named name parent)))
    (or existing-join
	(call-next-method))))

(defmethod add-child ((parent has-joins-mixin) (the-join join-mixin))
  (push the-join (joins parent))
  (values the-join 
	  ;;(controlled-tell `[join-of , parent ,the-join] :justification :premise)
	  (tell `[join-of , parent ,the-join] :justification :premise))
  )

(defmethod remove-child ((parent has-joins-mixin) (the-join join-mixin))
  (setf (joins parent) (delete the-join (joins parent)))
  (untell `[join-of ,parent ,the-join])
  )



(defclass has-properties-mixin ()
  ((properties :initarg :properties :initform nil :accessor properties)))

;;;;  useful instantiated classes


(defclass basic-task (has-properties-mixin
                      output-side-mixin
                      input-side-mixin
                      core-task-mixin)
  ()
  )

(defmethod find-dataflow ((source-module basic-task) (source-port-name symbol) (destination-module basic-task) (destination-port-name symbol))
  (let ((real-source-port (port-named 'output source-port-name source-module))
	(real-destination-port (port-named 'input destination-port-name destination-module)))
    (find-dataflow source-module real-source-port destination-module real-destination-port)))

(defmethod find-dataflow ((source-module basic-task) (source-port port) (destination-module basic-task) (destination-port port))
  (let ((outgoing-flows (outgoing-flows source-port)))
    (loop for flow in outgoing-flows
	for output = (output flow)
	when (eql output destination-port)
	return flow)))

(defgeneric selected-implementation (component))
(defmethod selected-implementation ((thing t)) nil)

(defclass task-interface-mixin ()
  ((primitive? :accessor primitive? :initform nil :initarg :primitive?)
   (dont-expand :accessor dont-expand :initform nil :Initarg :dont-expand)
   (implementations :initform nil :accessor implementations)
   (selected-implementation :initform nil :accessor selected-implementation))) 

;;; An abstract interface
;;; This interface can be simulated by computing values for the 
;;; output ports in terms of the input ports
;;; To know when to run, we keep track of how many inputs have been
;;; provided and fire once they're all there

;;; We might want to make a distinction in the classes
;;; between tasks that have implementations and tasks
;;; that have a behavioral description but aren't implementable
;;; (and how is that different from the primtive/composite distinction?)

(defclass task-interface (task-interface-mixin basic-task)
  ())

;;; An abstract interface plus its implementation
(defclass composite-task (has-children-mixin basic-task)
  ())

;;; This is intended to be a composite task that's an (one of possibly many) implementions of a
;;; a primtive task.  Not completely clear what the difference is
(defclass implementation
    (composite-task)
  ;; who I'm implementing
  ((abstract-task :initarg :abstract-task :accessor abstract-task :initform nil)
   (generation-controller :Initarg :generation-controller :initform nil :accessor generation-controller)
   )
  )


(defclass branching-task (has-branches-mixin
			  task-interface-mixin
			  input-side-mixin
			  core-task-mixin
			  has-properties-mixin)
  ()
  )

(defclass primitive-branching-task (has-branches-mixin
				    input-side-mixin
				    core-task-mixin
				    has-properties-mixin)
  ()
  )

(defclass primitive-enumerator (primitive-branching-task)
  ((abnormal-exit? :accessor abnormal-exit? :initform nil))
  )

(defmethod primitive? ((task primitive-branching-task)) t)

(defclass joining-task (has-joins-mixin 
			task-interface-mixin
			output-side-mixin
			core-task-mixin
			has-properties-mixin)
  ())

(defclass primitive-joining-task (has-joins-mixin 
				  task-interface-mixin
				  output-side-mixin
				  core-task-mixin
				  has-properties-mixin)
  ()
  )

(defmethod primitive? ((task primitive-joining-task)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial Inputs and Final Outputs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
;;; get-counter-number appears to be dead code
(defvar *module-to-io-counter-hash-table* (make-hash-table))


(defun get-counter-number (module type)
  (let ((entry (gethash module *module-to-io-counter-hash-table*)))
    (unless entry
      (setq entry (list 0 0)))
    (let ((answer (case type (input (incf (first entry))) (output (incf (second entry))))))
      (setf (gethash module *module-to-io-counter-hash-table*) entry)
      answer)))
|#


(defclass initial-object-mixin ()
  ())

(defmethod incoming-control-flows ((task initial-object-mixin)) nil)
(defmethod is-initial-task ((input initial-object-mixin)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass local-element-mixin ()
  ((local? :accessor local? :initarg :local :initform nil))
  )

(defclass constant-mixin ()
  ((type :accessor type :initarg :type)
   (value :accessor value :initarg :value))
  )

(defclass constant-value (local-element-mixin constant-mixin initial-object-mixin output-side-mixin core-task-mixin)
  ()
  )

(defmethod add-constant ((parent has-children-mixin) name type value local?)
  (let ((the-constant (make-instance 'constant-value :value value :type type :name name :superior parent :task-type 'constant :local local?)))
    (multiple-value-bind (the-port assertion) (add-port 'output name the-constant)
      (declare (ignore assertion))
      (tell `[component-of ,parent ,the-constant])
      (tell `[port-type-constraint ,the-port ,type]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial Inputs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass initial-input (initial-object-mixin output-side-mixin core-task-mixin)
  ((branch-name :initform nil :initarg :branch-name :accessor branch-name)
   ;; the input port in the interface that corresponds to this initial input
   (corresponding-port :accessor corresponding-port :initform nil)
   ))

(defmethod add-initial-input ((parent has-children-mixin) name output-name output-type &optional branch-name)
  (let* ((initial-input (make-instance 'initial-input :name name :superior parent :task-type 'input)))
    (multiple-value-bind (the-port assertion) (add-port 'output output-name initial-input)
      (declare (ignore assertion)) 
      (when branch-name
	(setf (branch-name initial-input) branch-name)
	(tell `[branch-name-of ,initial-input ,branch-name]))
      (tell `[component-of ,parent ,initial-input])    
      (tell `[port-type-constraint ,the-port ,output-type])
      initial-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Final Outputs
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass final-object-mixin ()
  ((branch-name :initform nil :initarg :branch-name :accessor branch-name)))

(defmethod task-component-children ((output final-object-mixin)) 
  nil)

(defmethod branch-name ((thing core-task-mixin)) nil)
(defmethod join-name ((thing core-task-mixin)) nil)

;;; Has-successors allows you to have a control flow from a final output
(defclass final-output (has-successors-mixin final-object-mixin input-side-mixin core-task-mixin)
  ;; the output port in the interface that corresponds to this final output
  ((corresponding-port :accessor corresponding-port :initform nil)
   ))

(defmethod add-final-output ((parent has-children-mixin) name input-name input-type &optional branch-name)
  (let* ((final-output (make-instance 'final-output :name name :superior parent :task-type 'output)))
    (multiple-value-bind (the-port assertion) (add-port 'input input-name final-output)
      (declare (ignore assertion))
      (tell `[component-of ,parent ,final-output])
      (tell `[port-type-constraint ,the-port ,input-type])
      (when branch-name
	(setf (branch-name final-output) branch-name)
	(tell `[branch-name-of ,final-output ,branch-name]))
      final-output
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  State sources and sinks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass has-state-variable-mixin ()
  ((state-name :accessor state-name :initarg :state-name))
  )

;;; This has an output so that the updated state can flow to other tasks
(defclass state-sink (has-state-variable-mixin input-side-mixin output-side-mixin
		      has-predecessors-mixin has-successors-mixin core-task-mixin has-properties-mixin)
  ())

(defmethod add-state-sink ((parent has-children-mixin) name input-name input-type &optional state-name)
  (unless state-name (setq state-name name))
  (let* ((state-variable (make-instance 'state-sink :name name :superior parent :task-type 'state :state-name state-name))
	 (output-name (intern (format nil "~a-~a" 'updated input-name))))
    (tell `[component-of ,parent ,state-variable])    
    (multiple-value-bind (the-port assertion) (add-port 'input input-name state-variable)
      (declare (ignore assertion))
      (tell `[port-type-constraint ,the-port ,input-type]))
    (multiple-value-bind (the-port assertion) (add-port 'output output-name state-variable)
      (declare (ignore assertion))
      (tell `[port-type-constraint ,the-port ,input-type]))))


;;; This has an input so that you can initialize the state
(defclass state-source (has-state-variable-mixin input-side-mixin output-side-mixin 
			has-predecessors-mixin has-successors-mixin core-task-mixin has-properties-mixin)
  ())

(defmethod add-state-source ((parent has-children-mixin) name output-name output-type &optional state-name locality)
  (unless state-name (setq state-name name))
  (let* ((state-variable (make-instance 'state-source :name name :superior parent :task-type 'state :state-name state-name))
	 (input-name (intern (format nil "~a-~a" 'initial output-name))))
    (setf (getf (properties state-variable) :locality) locality)
    (tell `[component-of ,parent ,state-variable])    
    (multiple-value-bind (the-port assertion) (add-port 'input input-name state-variable)
      (declare (ignore assertion))
      (tell `[port-type-constraint ,the-port ,output-type]))
    (multiple-value-bind (the-port assertion) (add-port 'output output-name state-variable)
      (declare (ignore assertion))
      (tell `[port-type-constraint ,the-port ,output-type]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Entry-and Exit- points
;;;
;;;    These exist so that we can abstract branching and joining behavior
;;;    up to a component interface.  
;;;    For example:
;;;     1) An accumulator that has two entries, one for when there's
;;;    more stuff and a second for when the generator has run out
;;;     2) A generator that has two exits, one for when there's more stuff
;;;     and a second for when it's run out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Entry-point

(defclass entry-point (initial-object-mixin output-side-mixin core-task-mixin)
  ((join-name :initform nil :initarg :join-name :accessor join-name)
   (corresponding-join :Initform nil :initarg :corresponding-join :accessor corresponding-join))
  )

(defmethod add-entry-point ((parent has-children-mixin) name join-name &optional port-names-and-type-constraints)
  (let* ((entry-point (make-instance 'entry-point :name name :superior parent :task-type 'entry-point :join-name join-name)))
    (loop for entry in port-names-and-type-constraints
	do (destructuring-bind (port-name type-constraint &optional (conditional t)) entry
	     (when conditional
	       (let ((port (add-port 'output port-name entry-point)))
		 (add-port-type-description port type-constraint)))))
    (tell `[join-name-of ,entry-point ,join-name])
    (tell `[component-of ,parent ,entry-point])))

(defclass exit-point (final-object-mixin input-side-mixin core-task-mixin)
  ((branch-name :initform nil :initarg :branch-name :accessor branch-name)
   (corresponding-branch :initform nil :accessor corresponding-branch))
  )

(defmethod add-exit-point ((parent has-children-mixin) name branch-name &optional port-names-and-type-constraints)
  (let* ((exit-point (make-instance 'exit-point :name name :superior parent :task-type 'exit-point :branch-name branch-name)))
    (loop for entry in port-names-and-type-constraints
	do (destructuring-bind (port-name type-constraint &optional (conditional t)) entry
	     (when conditional
	       (let  ((port (add-port 'input port-name exit-point)))
		  (add-port-type-description port type-constraint)))))
    (tell `[branch-name-of ,exit-point ,branch-name])
    (tell `[component-of ,parent ,exit-point])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Path ends.  These are like exit-points except they don't match-up with anything
;;; in the superior abstract-task.
;;; These exist to allow consumers of a temporal sequence (or some such) to
;;; indicate that this is end of the control-flow path, typically underneath
;;; some enumerator.
;;; This is necessary in the case where this path has no further data-flow
;;; to disambiguate between a case where the computation continues via control flow (without data)
;;; and the case where this is really the end of the process of consuming members of the temporal sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass path-end (input-side-mixin core-task-mixin)
  ()
  )

(defmethod add-path-end ((parent has-children-mixin) name &optional port-names-and-type-constraints)
  (let* ((path-end (make-instance 'path-end :name name :superior parent :task-type 'path-end)))
    (loop for entry in port-names-and-type-constraints
	  do (destructuring-bind (port-name type-constraint &optional (conditional t)) entry
	       (when conditional
		 (let  ((port (add-port 'input port-name path-end)))
		   (add-port-type-description port type-constraint)))))
    (tell `[component-of ,parent ,path-end])
    path-end
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building task networks
;;; building the io port frame of the task - create task
;;; building the bodies of composite tasks - build-task-body

(defgeneric create-task (role-name task-type parent &rest properties))

(defmethod create-task :around (role-name task-type parent &rest properties)
  ;; this is a hack to let you pass in extra properties with the task type
  ;; this is mainly of use in defcliche where you'd like to say, for example,
  ;; that the accumulator is a duplicate-detector that extracts a key for the test
  (cond
   ((listp task-type)
    (apply #'create-task role-name (first task-type) parent (append (rest task-type) properties)))
   (t (with-atomic-action 
	  (let* ((new-task (apply #'call-next-method role-name task-type parent properties))
		 (actual-properties (properties new-task))
		 (dont-expand (getf actual-properties :dont-expand)))
	    ;; (controlled-tell `[component-of ,parent ,new-task] :justification :premise)
	    (when dont-expand (setf (dont-expand new-task) dont-expand))
	    (tell `[component-of ,parent ,new-task] :justification :premise)
	    (with-keywords-removed (new-properties actual-properties '(:dont-expand))
	      (loop for (key value) on new-properties by #'cddr
		  do ;; (controlled-tell `[property-value-of ,new-task ,key ,value])
		    ;; make sure that the properties are in the natsoft (not keyword) package
		    (tell `[property-value-of ,new-task ,(intern (string key) 'natsoft) ,value])))
	    new-task)))))

(defmethod create-task (role-name task-type parent &rest properties)
  (let ((new-task (make-instance 'basic-task
		   :name role-name
		   :task-type task-type
		   :superior parent
		   :properties properties)))
    new-task))

(defgeneric build-task-interface (task-type task))

(defmethod build-task-interface ((task-type t) (task t)) (values))

(defmethod build-task-interface ((task-type t) (task has-joins-mixin))
  (let* ((plist (properties task))
	 (branch (when plist (getf plist :branch))))
    (when branch
      (setf (corresponding-branch task) branch))))

(defgeneric body-instantiated? (task))

(defmethod body-instantiated? ((task task-interface-mixin)) t)

(defgeneric build-task-body (task-type task &key &allow-other-keys))

(defmethod build-task-body (task-type (the-task task-interface-mixin) &rest properties &key &allow-other-keys)
  (declare (ignore task-type))
  nil)

(defgeneric rebuild-task-for-new-type (task-type task &rest properties))

(defmethod rebuild-task-for-new-type ((new-task-type symbol) (task core-task-mixin) &rest properties)
  (untell `[type-of ,task ,(task-type task)])
  (setf (task-type task) new-task-type)
  (build-task-interface new-task-type task)
  (apply #'build-task-body new-task-type task properties)
  ;; (controlled-tell `[type-of ,task ,new-task-type] :justification :premise)  
  (tell `[type-of ,task ,new-task-type] :justification :premise)
  )




;;;;  The system description language 
;;;    The usage for the moment is going to be very simple
;;;    Just primitive tasks
;;;    The new extension is that ports will have typed data
(defparameter *task-type-hash-table* (make-hash-table))

;;;(defun type-branches (type-name)
;;;  (let ((answer nil))
;;;    (ask `[has-branch ,type-name ?branch-name]
;;;	 #'(lambda (just)
;;;	     (declare (ignore just))
;;;	     (push ?branch-name answer)))
;;;    answer))
;;;
;;;(defun type-joins (type-name)
;;;  (let ((answer nil))
;;;    (ask `[has-join ,type-name ?join-name]
;;;	 #'(lambda (just)
;;;	     (declare (ignore just))
;;;	     (push ?join-name answer)))
;;;    answer))

;;; Note: Still need to deal with joins in the same way as branches
;;; Currently this is using assertions to figure if this has branches/joins 
;;; but we should just specify it 

(defmacro defdata-type (name &rest stuff &key super-types parameters definition 
					      parts
					      other-assertions declarations
					      equivalences union 
					      bindings)
  (declare (ignore parts parameters definition other-assertions declarations equivalences union bindings))
  ;;(pushnew 'data-structure super-types)
  (remf stuff :super-types)
  (apply #'defstuff-internal name
       :data-structure t
       :super-types super-types
       stuff))

(defmacro deftask (type-name 
		   &rest stuff
		   &key implementation interface super-types primitive parameters other-assertions declarations bindings dont-expand)
  (declare (ignore implementation interface primitive parameters other-assertions declarations bindings dont-expand))
  (pushnew 'procedure super-types)
  (remf stuff :super-types)
  (apply #'defstuff-internal type-name
	 :procedure t
	 :super-types super-types
	 stuff))

(defmacro deftoplevel (type-name)
  (defstuff-internal type-name))    

(defun process-deftask-bindings (pairs)
  pairs)

(defun get-inherited-parameters (super-types)
  (let ((answer nil))
    (loop for type in super-types
	do (ask* `[has-property ,type ?property]
		 (pushnew ?property answer)))
    answer))

(defparameter *type-parameter-table* (make-hash-table))

(defun get-parameters-in-order (type)
  (gethash type *type-parameter-table* )
  )

(defun is-enumerator-task (super-types)
  (labels ((do-one (type)
	     (when (eql type 'basic-enumerator)
	       (return-from is-enumerator-task t))
	     (ask* `[subtype ,type ?parent]
		   (do-one ?parent))))
    (loop for type in super-types do (do-one type)))
  nil)

(defun is-inherited-branching-task (super-types)
  (labels ((do-one (type)
	     (ask* `[has-branch ,type ?]
		   (return-from is-inherited-branching-task t))
	     (ask* `[subtype ,type ?parent]
		   (do-one ?parent))))
    (loop for type in super-types do (do-one type)))
  nil)

(defun is-inherited-joining-task (super-types)
  (labels ((do-one (type)
	     (ask* `[has-join ,type ?]
		   (return-from is-inherited-joining-task t))
	     (ask* `[subtype ,type ?parent]
		   (do-one ?parent))))
    (loop for type in super-types do (do-one type)))
  nil)

(defgeneric property-defaulter (type properties &rest other-names))

(defun defstuff-internal (type-name 
			  &key procedure implementation interface primitive dont-expand ;only for procedure types
			       other-assertions declarations parameters super-types ;for anything
			       data-structure definition ;only for data-structures
			       equivalences union ;only for data-structures
			       parts	;only for data-structures
			       bindings
			       )
  (declare (ignore data-structure))
  ;; so that nothing is its own super-type
  (when (member type-name super-types)
    (setq super-types (remove type-name super-types)))
  (destructuring-bind (&key components dataflows control-flows) implementation
    (let* ((inputs (rest (assoc :inputs interface)))
	   (outputs (rest (assoc :outputs interface)))
	   (branches (rest (assoc :branches interface)))
	   (joins (rest (assoc :joins interface)))
	   (task-type (cond (components 'composite-task)
			    ((or joins (is-inherited-joining-task super-types)) (if primitive 'primitive-joining-task 'joining-task))
			    ((and primitive (is-enumerator-task (cons type-name super-types)))
			     'primitive-enumerator)
			    ;; Note: this means a non-primitive enumerator is a branching-task (as it should be)
			    ((or branches (is-inherited-branching-task super-types)) 
			     (if primitive 'primitive-branching-task 'branching-task))
			    (t 'task-interface)))
	   (extra-creation-args (when (and primitive (null joins) (null branches)) `(:primitive? t)))
	   (input-related-tells nil)
	   (input-related-code nil)
	   (output-related-tells nil)
	   (output-related-code nil)
	   (processed-branches nil)
	   (branch-related-tells nil)
	   (join-related-tells nil)
	   (processed-joins nil)
	   (expanded-dataflows nil)
	   (task-names (loop for (role-name) in components collect role-name))
	   (initial-tasks (copy-seq task-names))
	   (property-defaults nil)
	   (inherited-parameters (get-inherited-parameters super-types))
	   (local-parameters (loop for parameter in parameters
				  if (listp parameter) collect (first parameter)
				 else collect parameter))
	   (all-parameters (union local-parameters inherited-parameters))
	   (bindings-names (mapcar #'car bindings))
	   (parameters-plus-bindings (union bindings-names all-parameters))
	   (parameter-plist (loop for parameter in parameters
				for name = (if (symbolp parameter) parameter (first parameter))
				for default-value = (when (listp parameter) (second parameter))
				for keyword = (intern (string name) :keyword)
				when default-value 
				do (when (and (symbolp default-value) (not (member default-value parameters-plus-bindings)))
				     (setq default-value `',default-value))
				   (push `(when (eql :no-value (getf properties ,keyword :no-value))
					    (setq properties (nconc (list ,keyword ,default-value) properties)))
					 property-defaults)
				collect keyword collect name))
	   )
      (loop for (input-port output-port) in dataflows
	  for (input-name input-task) = input-port
	  for (output-name output-task-description) = output-port
	  for output-is-join-description = (consp output-task-description)
	  for output-task = (cond ((null output-task-description) nil)
				  (output-is-join-description (second output-task-description))
				  (t output-task-description))
	  for idirection = (cond ((and input-task output-task) 'output)
				 (input-task 'output)
				 (t 'input))
	  for odirection = (cond ((and input-task output-task) 'input)
				 (output-task 'input)
				 (t 'output))
	  for itask = (or input-task 'the-task)
	  for otask = (cond ((null output-task) 'the-task)
			    (output-is-join-description `(join-named ',(first output-task-description) ,output-task))
			    (t output-task))
	  when (and input-task output-task)
	  do (setq initial-tasks (delete output-task initial-tasks))
	  do (push (list idirection input-name itask
			 odirection output-name otask)
		   expanded-dataflows))
      (loop for (nil otask) in control-flows
	  do (setq initial-tasks (delete otask initial-tasks)))
      ;; do everything related to the interface: inputs, outputs, branches, joins
      ;; Inputs will not include the inputs to joins
      (loop for input in inputs
	  do (multiple-value-bind (code tells) (process-port-description type-name 'input 'the-task input parameters-plus-bindings)
	       (push code input-related-code)
	       (setq input-related-tells (append tells input-related-tells))))
      ;; outputs will not include the outputs of branches
      (loop for output in outputs
	  do (multiple-value-bind (code tells) (process-port-description type-name 'output 'the-task output parameters-plus-bindings)
	       (push code output-related-code)
	       (setq output-related-tells (append tells output-related-tells))))
      ;; branches will take care of their outputs
      (loop for description in branches 
	  do (multiple-value-bind (branch-code my-branch-related-tells)  (apply #'process-branch-description 
										:parameters parameters-plus-bindings :task-type type-name description)
	       (setq branch-related-tells (append my-branch-related-tells branch-related-tells))
	       (push branch-code processed-branches)))
	;;; joins will take care of their inputs
      (loop for description in joins
	  do (multiple-value-bind (join-code my-join-related-tells) (apply #'process-join-description :parameters parameters-plus-bindings
									   :task-type type-name description)
	       (setq join-related-tells (append my-join-related-tells join-related-tells))
	       (push join-code processed-joins)))
      
      ;; And finally the code
      `(eval-when (:load-toplevel :execute :compile-toplevel)
	 (setf (gethash ',type-name *type-parameter-table*) '(,@local-parameters))
	 (tell [is-type ,type-name])
	 ,@(loop for (part-name part-data-type) in parts
	       collect `(tell [has-part ,type-name ,part-name ,part-data-type]))
	 ,@(when union (loop for union-member in union collect `(tell [union-member ,union-member ,type-name])))
	 ,@(when equivalences (loop for (from to) in equivalences by #'cdr collect `(tell [viewpoint-equivalence ,from ,to])))
	 ,@(when definition `((tell [definition ,type-name ,definition])))
	 ,@(when other-assertions (loop for assert in other-assertions collect `(tell ,assert)))
	 ,@(loop for super in super-types collect `(tell [subtype ,type-name ,super]))
	 ,@(loop for parameter in (set-difference local-parameters inherited-parameters) collect `(tell [has-property ,type-name ,parameter]))
	 ,@input-related-tells
	 ,@output-related-tells
	 ,@(reverse branch-related-tells)
	 ,@(reverse join-related-tells)
	 ,@(when procedure
	     `(,@(unless (eql type-name 'computational-stuff)
		   `((setf (gethash ',type-name *task-type-hash-table*) t)))
		 (defmethod create-task (role-name (task-type (eql ',type-name)) parent &rest properties &key ,@all-parameters &allow-other-keys)
		   (declare (ignorable ,@all-parameters))
		   (let* ,(process-deftask-bindings bindings)
		     (declare (ignorable ,@bindings-names))
		     (setq properties (property-defaulter ',type-name properties ,@parameters-plus-bindings))
		     ,@(when super-types
			 (loop for super-type in (remove 'procedure (remove 'computational-stuff super-types))
			     for his-parameters = (get-parameters-in-order super-type)
			     collect `(setq properties (property-defaulter ',super-type properties ,@his-parameters))))
		     ,@(when dont-expand `((setq properties (nconc (list :dont-expand t) properties))))
		   (let ((the-task (make-instance ',task-type
				     :task-type ',type-name
				     :name role-name
				     :superior parent
				     :properties properties
				     ,@extra-creation-args
				     )))
		     (build-task-interface task-type the-task))))
		 (defmethod property-defaulter ((task-type (eql ',type-name)) properties &rest bindings)
		   (destructuring-bind (,@parameters-plus-bindings) bindings
		     (declare (ignorable ,@parameters-plus-bindings))
		     ,@(nreverse property-defaults)
		     properties))
		 (defmethod build-task-interface ((task-type (eql ',type-name)) the-task)
		   ;; :add-type-descriptions? t below means that if the parent has already provided a port
		   ;; just add typing information
		   ;; Inheritance of interface description from parent types
		   ,@(when super-types
		       (loop for super-type in (remove 'procedure (remove 'computational-stuff super-types))
			   collect `(build-task-interface ',super-type the-task)))
		   (destructuring-bind (&key ,@all-parameters &allow-other-keys) (properties the-task)
		   ,@declarations
		   (declare (ignorable ,@all-parameters))
		   (let* ,(process-deftask-bindings bindings)
		     (declare (ignorable ,@bindings-names))
		     ;; Any parameter that's rebound needs to be added to the property list
		     ,@(loop for parameter in local-parameters
			 when (member parameter bindings-names)
			      collect `(setf (getf (properties the-task) ',(intern (symbol-name parameter) 'keyword)) ,parameter))
		   
		   ,@(nreverse input-related-code)
		   ,@(nreverse output-related-code)
		   ,@(nreverse processed-joins)
		   ,@(nreverse processed-branches)
		   the-task)))
		 ,@(when (eql task-type 'composite-task)
		     `((defmethod build-task-body ((task-type (eql ',type-name)) (the-task ,task-type) &key ,@all-parameters &allow-other-keys)
			 ,@ (when super-types
			      (loop for super-type in super-types
				  collect `(build-task-body ',super-type the-task ,@parameter-plist)))
			 (let ,(loop for (role-name type-name) in components
				   collect `(,role-name (create-task ',role-name ',type-name the-task)))
			   ,@(loop for (input output) in control-flows
				 when (listp input)
				 do (setq input `(list ',(first input) ,(second input)))
				 when (listp output)
				 do (setq output `(list ',(first output) ,(second output)))
				 collect `(make-control-flow ,input ,output))
			   ,@(loop for task in initial-tasks
				 collect `(add-initial-task the-task ,task))
			   ,@(loop for (idirection input-name input-task odirection output-name output-task) 
				 in expanded-dataflows
				 collect `(make-dataflow ',idirection ',input-name ,input-task
							 ',odirection ',output-name ,output-task)))
			 (with-slots (body-instantiated) the-task
			   (setf body-instantiated  t))
			 the-task))))
	     )))))

(defun process-port-description (task-type direction parent description parameters &optional (predicate 'has-port) branch-name)
  (flet ((do-the-real-work (description &optional condition)
	   (let* ((real-name (if (symbolp description) description (first description)))
		  (type-constraint (unless (symbolp description) (second description)))
		  (fixed-type-constraint (cond ((listp type-constraint)
						(cons 'list (loop for thing in type-constraint collect (if (member thing parameters) thing `',thing))))
					       ((and (symbolp type-constraint) (not (member type-constraint parameters)))
						`',type-constraint)
					       )))
	     (values 
	      (if type-constraint
		  `(let ((new-port (add-port ',direction ',real-name ,parent)))
		     ,@(when type-constraint
			 `((tell `[port-type-constraint ,new-port ,(simplest-version ,(or fixed-type-constraint type-constraint))]))))
		`(add-port ',direction ',real-name ,parent))
	      (list*
	       `(tell [,predicate ,task-type ,(or branch-name direction) ,real-name ,condition])
	       (when type-constraint
		 `((tell `[has-data-type ,',task-type ,',direction ,',real-name ,',type-constraint])))))
	     )))
    (cond
     ((and (not (symbolp description)) (eql (first description) 'when))
      (Let ((condition (second description))
	    (real-description (third description)))
	   (multiple-value-bind (constraint tells) (do-the-real-work real-description condition)
	     (values `(when ,condition ,constraint)
		     tells))))
     (t (do-the-real-work description)))
    ))



(defun simplest-version (type-constraint)
  (or (is-definition-of type-constraint)
      type-constraint))

(defun compute-evaluation-version (form variables)
  (labels ((do-one-level (form)
	     (cond
	      ((symbolp form) (if (member form variables) form `',form))
	      (t `(list ,@(loop for thing in form collect (do-one-level thing)))))))
    (do-one-level form)))

(defun process-branch-description (&key task-type name condition outputs parameters)
  (let ((branch-type 'branch)
	(branch-related-tells nil)
	(port-code nil))
    (push `(tell [has-branch ,task-type ,name]) branch-related-tells)
    (loop for output in outputs 
	do (multiple-value-bind (code tells) (process-port-description task-type 'output 'the-branch output parameters 'has-branch-port name)
	     (push code port-code)
	     (setq branch-related-tells (append tells branch-related-tells))
	     ))  
    (setq condition (compute-evaluation-version condition parameters))
    (when condition
      (push `(tell [has-branch-condition ,task-type ,name ,condition]) branch-related-tells))
    (values
     `(let ((the-branch (make-instance ',branch-type
			  :task-type 'branch
			  :superior the-task
			  :name ,(if (member name parameters) name `',name)
			  :condition ,condition)))
       ,@(nreverse port-code)
       the-branch)
    branch-related-tells)))

(defun process-join-description (&key task-type name inputs parameters)
  (let ((join-type 'join)
	(join-related-tells nil)
	(port-code nil))
    (push `(tell [has-join ,task-type ,name]) join-related-tells)
    (loop for input in inputs
	do (multiple-value-bind (code tells) (process-port-description task-type 'input 'the-join input parameters 'has-join-port)
	     (push code port-code)
	     (setq join-related-tells (append tells join-related-tells))
	     ))
    (values
     `(let ((the-join (make-instance ',join-type
			:superior the-task
			:task-type 'join
			:name ,(if (member name parameters) name `',name)
			)))
	,@(nreverse port-code)
	the-join)
     join-related-tells)))

(defmacro defviewpoint (name &key operator super-types source destination equivalences terms)
  `(eval-when (:load-toplevel :execute)
     (tell [is-viewpoint ,name ,operator])
     ,@(when operator `((tell [is-type ,operator])      
			(tell [abstract-operator ,operator])))
     ,@(when (and operator super-types) (loop for super-type in super-types collect `(tell [subtype ,operator ,super-type])))
     (tell [can-be-viewed-as ,source ,destination ,name])
     ,@(loop for (from to) in equivalences
	   collect `(tell [viewpoint-equivalence ,from ,to]))
     ,@(loop for (viewpoint-name action subordinate-verb how) in terms
	   collect `(tell [terms-for-viewpoint ,viewpoint-name ,action ,subordinate-verb ,how]))))



(defmethod reparent-port ((direction (eql 'output)) (old-parent output-side-mixin) (new-parent output-side-mixin) (the-port port))
  (setf (outputs old-parent) (remove the-port (outputs old-parent)))
  (decf (n-outputs old-parent))
  (untell `[port-of ,old-parent ,the-port])
  (push the-port (outputs new-parent))
  (incf (n-outputs new-parent))
  (tell `[port-of ,new-parent ,the-port])
  (setf (task the-port) new-parent))

(defmethod reparent-port ((direction (eql 'input)) (old-parent input-side-mixin) (new-parent input-side-mixin) (the-port port))
  (setf (inputs old-parent) (remove the-port (inputs old-parent)))
  (decf (n-inputs old-parent))
  (untell `[port-of ,old-parent ,the-port])
  (push the-port (inputs new-parent))
  (incf (n-inputs new-parent))
  (tell `[port-of ,new-parent ,the-port])
  (setf (task the-port) new-parent))

#|

;;;; simple example
(test :a (:d 1 :e 2 :f 3))

(deftask bar
  :inputs (i)
  :outputs (o))

(deftask baz 
  :inputs ((i1 integer) (i2 (array pixel)))
  :outputs (o))

(deftask bam
  :inputs (i1 i2)
  :branches ((:name less :condition (< i1 i2))
             (:name equal :condition (= i1 i2))
	     (:name more :condition (> i1 i2))))

(deftask ban
  :outputs (o)
  :joins ((:name join1 :inputs (i))
          (:name join2 :inputs (i))))

(deftask foo
  :inputs (i1 i2)
  :outputs (o)
  :components ((a bar)
               (b bar)
	       (c bam)
               (d baz)
	       (e baz)
               (f ban))
  :control-flows (((branch1 c) d)
		  ((branch2 c) e))
  :dataflows (((i1) (i a))
              ((i2) (i b))
              ((o a) (i1 c))
              ((o b) (i2 c))
	      ((o a) (i1 d))
	      ((o b) (i2 d))
	      ((o a) (i1 e))
              ((o b) (i2 e))
              ((o d) (i (join1 f)))
	      ((o e) (i (join2 f)))
              ((o f) (o))))

(deftask foo1
  :inputs (i1 i2)
  :outputs (o)
  :components ((a bar)
               (b bar)
	       (c bam)
               (d baz)
	       (e baz)
               (f ban))
  :control-flows (((branch1 c) d)
		  ((branch2 c) e)
                  (a b))
  :dataflows (((i1) (i a))
              ((i2) (i b))
              ((o a) (i1 c))
              ((o b) (i2 c))
	      ((o a) (i1 d))
	      ((o b) (i2 d))
	      ((o a) (i1 e))
              ((o b) (i2 e))
              ((o d) (i (join1 f)))
	      ((o e) (i (join2 f)))
              ((o f) (o))))


(defun test1 ()
  (let ((task (create-task 'f 'foo nil)))
    (new-value 1 (port-named 'input 'i1 task))
    (new-value 2 (port-named 'input 'i2 task))
    (execute task t t)
    (clear task)
    (new-value 4 (port-named 'input 'i1 task))
    (new-value 4 (port-named 'input 'i2 task))
    (execute task nil t)
    task))
    

(defun x ()
  (loop for x across y using  z do (foo))
  )


|#