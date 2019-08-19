;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Synthesizing implementations
;;;  Finding appropriate refinements
;;;  Choosing between them etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The idea is that this proposes a refinement
;;; and the refinement is a procedure
;;; The procedure will return three values:
;;;   Applicable: T or NIl.  If nil the other values are irrelevant
;;;   Subgoals: Further things to achieve if approach is to succeed
;;;   A design in which some (or all) of the work has been achieved

(define-predicate plan-for (type thing arguments partial-plan choices-so-far my-choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate possible-implementation (plan task plan-name choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate selected-implementation (plan task choices) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate choice-behind-plan (plan choice) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; Says that in mapping to the implementation, we applied a viewpoint to the named port.
;;; Which means that in propagating values down during symbolic execution, we should add the viewpoint in 
(define-predicate viewpoint-applied (parent port-name viewpoint-name action object type-constraint) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;; for example for generate row-view this would be [terms-for-viewpint generate row-view traverse row_wise]
(define-predicate terms-for-viewpoint (viewpoint-name action subordinate-verb how) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; This tells you how to create an object of the object type
;;; Note: For right now, I'm ignoring arguments that conditon
;;; allocation (e.g. what size vector you want)
;; (define-predicate allocation-code (object-type code language) (ltms:ltms-predicate-model))
(define-predicate reason-for (implementation parent choice reason) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;;
;;; Contact points for cliche's that are intended to merge into another design
;;; The specific example is path accumulation being merged into tree traversal
;;; Here a port description is a keyword list (:component :branch :join :direction :name) where branch and join
;;; are both optional and mutually exclusive
(define-predicate contact-point (cliche-name Port-description) (ltms:ltms-predicate-model))
(define-predicate initializer-for-merge (cliche-name initializer-name) (ltms:ltms-predicate-model))
      
;;; Use-existing can be any of the following
;;; Nil: Start from the top level task and refine everything
;;; :keep-all: This thing has already been synthesized reuse everything
;;; :keep-top-level: We're really using a design (not a task) so keep that and synthesize everything
;;;                  below that level
(defun build-recursive-implementation (top-level-procedure design-editor &optional use-existing)
  (let ((dip (design-in-progress design-editor)))
    (unwind-protect
	(let ((plan-names nil))
	  (labels ((do-next-level (procedure &optional keep)
		     ;; clean out any stale information
		     (when (and (typep procedure 'task-interface-mixin)
				(not (primitive? procedure))
				(not (is-primitive-for-simulation procedure)))
		       (cond
			(keep (let ((implementation (selected-implementation procedure)))
				;; If we're in :keep-all mode then keep passing it down
				;; otherwise, we've done the first-level so not pass down
				;; nil meaning don't keep any more
				(loop for child in (children implementation) 
				    do (do-next-level child (if (eql use-existing :keep-all) keep nil)))))
			((dont-expand procedure))
			(t
			 ;; find any pre-existing implementations of this task and get rid of them
			 ;; is this actually necessary?
			 ;; (ask* `[possible-implementation ?plan ,procedure ? ?] (remove-design clim:*application-frame* ?plan))
			 ;; here keep is already nil (see first clause)
			 ;; so just pass down nil
			 (find-implementations-for procedure
						   #'(lambda (plan choices)
						       (destructuring-bind (plan-name . args) plan
							 (multiple-value-bind (implementation plan-name)
							     (implement-plan design-editor procedure plan-name (copy-object-if-necessary args) choices)
							   (push (list implementation plan-name choices) plan-names)
							   (loop for child in (children implementation)
							       do (do-next-level child nil)))))))))))
	    (do-next-level top-level-procedure use-existing))
	  plan-names)
    (switch-focus design-editor dip))))


(defparameter *trace-refinement* nil)
(defun find-implementations-for (procedure continuation)
  (let* ((task-type (task-type procedure))
	 (args (canonical-arguments-for task-type procedure))
	 (there-is-a-plan nil))
    (when *trace-refinement* (format *error-output* "~%Looking for ~a ~a ~a" task-type procedure args))
    (ask `[plan-for ,task-type ,procedure ,args ?plan nil ?choices]
	 #'(lambda (just)
	     (declare (ignore just))
	     (when *trace-refinement* (format *error-output* "~%Found ~a" ?plan))
	     (setq there-is-a-plan t)
	     (funcall continuation ?plan ?choices)))
    (when (null there-is-a-plan)
      (format *error-output* "~%no plan for ~a" procedure))
    ))

;;; ToDo: Generalize from (stream <x>) to more general 
;;; form including (sequence <x>)
(defun canonical-form-for (type-expression)
  (cond 
   ((or (numberp type-expression) (symbolp type-expression))
    (or (definition-for type-expression) type-expression))
   ((stringp type-expression) type-expression)
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
	 ;; for-design-editor = nil means that we're not doing this as part of a design editor buffer maninputing command
	 ;; but instead as part of synthesis.  Therefore, it shouldn't put in any assertions about possible implementations etc
	 ;; since we'll do that herel
	 (new-design (get-new-design unique-plan-name design-editor :class 'implementation :abstract-task parent :for-design-editor nil)))
    (apply plan-name new-design args)
    (tell `[possible-implementation ,new-design ,parent ,plan-name ,choices])
    (pushnew new-design (implementations parent))
    (loop for choice in choices
	do (tell `[choice-behind-plan ,new-design ,choice]))
    (values new-design plan-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Knowledge about refinements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-a-viewpoint-operator? (viewpoint-name)
  (ask* `[is-viewpoint ,viewpoint-name ?]
	(return-from is-a-viewpoint-operator? t))
  nil)

(defun is-viewpoint? (type)
  (cond ((atom type) nil)
	(t (is-a-viewpoint-operator? (first type)))))

(defun is-an-abstract-operator? (name)
  (ask* `[abstract-operator ,name]
	(return-from is-an-abstract-operator? t))
  nil)

(defun is-abstract-statement? (form)
  (cond
   ((atom form) nil)
   (t
    (is-an-abstract-operator? (car form)))))
			    
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
		   (ask `[possible-implementation ,selected-implementation ,sub-design ?reason ?choices]
			#'(lambda (just)
			    (declare (ignore just))
			    (setq choices ?choices)))
		   (setf (selected-implementation sub-design) selected-implementation)
		   (tell `[selected-implementation ,selected-implementation ,sub-design ,choices]))
	       (loop for child in (children selected-implementation)
		   do (do-next-level child))))))
    (do-next-level design)))
	   
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


(defmacro defreduction (reduction-name (type arguments) &key the-instance reduce-to prerequisites previous-choices 
							     new-choices actions subplans the-plan bindings)
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
	      ,@(process-bindings bindings)
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

;;; This is dead code?
(defun port-names (type direction)
  (let ((names nil))
    (ask `[has-port ,type ,direction ?name ?governing-parameter]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push (list ?name ?governing-parameter) names)))
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

  ;; Changed this to allow binding a bunch of things at once
  ;; if the first thing is a list then bind those variables to NIL
  ;; and collect a set of multiple-value-setq forms to be added after the
  ;; let.  
  ;; If the first thing is just a logic variable then bind to NIL
  ;; and add a setq form to be added after the let
  ;; Returns 2 values, the bindings and the multiple-value-setq/setq forms
  (defun process-bindings (binding-set)
    (let ((let-forms nil)
	  (mvs-forms nil))
      (loop for (name form) in binding-set do 
	     (cond
	      ((logic-variable-maker-p name)
	       (push (list (devariablize name 'normal)) let-forms)
	       ;; we have to do this so that the assignment order is correct
	       (push `(setq ,(devariablize name 'normal) ,(devariablize form 'raw))
		     mvs-forms))
	      ((listp name)
	       (loop for var in name
		   do 
		      (push (list (devariablize var 'normal)) let-forms))
	       (push `(multiple-value-setq
			  ,(loop for var in name collect (devariablize var 'normal))
			,(devariablize form 'raw))
		     mvs-forms))
	      (t (break "huh"))))
      (values (nreverse let-forms) (nreverse mvs-forms)))
    )

  (defun process-initial-input-description (form parent-name)
    (destructuring-bind (&key name type ((:port port-name)) ((:branch branch-name))) form
      (when (null port-name) (setq port-name name))
      `(add-initial-input ,parent-name ,(devariablize name 'form) ,(devariablize port-name 'form) 
			  ,(devariablize type 'normal) ,(devariablize branch-name))))

  (defun process-final-output-description (form parent-name)
    (destructuring-bind (&key name type ((:port port-name)) ((:branch branch-name))) form
      (when (null port-name) (setq port-name name))
      `(add-final-output ,parent-name ,(devariablize name) ,(devariablize port-name) 
			 ,(devariablize type 'normal) ,(devariablize branch-name))))
  
  (defun process-entry-point-description (form parent-name)
    (destructuring-bind (&key name ports) form
      (let ((ports (loop for entry in ports
		       if (member (first entry) '(if when))
		       collect (destructuring-bind (&key name type) (third entry)
				 (list name type (second entry)))
		       else collect (destructuring-bind (&key name type) entry 
				      (list name type)))))
	`(add-entry-point ,parent-name ,(devariablize name) ,(devariablize name) ,(devariablize ports 'normal)))))

  (defun process-exit-point-description (form parent-name)
    (destructuring-bind (&key name ports) form
      (let ((ports (loop for entry in ports
		       if (member (first entry) '(if when))
		       collect (destructuring-bind (&key name type) (third entry)
				 (list name type (second entry)))				   
		       else collect (destructuring-bind (&key name type) entry
				      (list name type)) )))
	`(add-exit-point ,parent-name  ,(devariablize name) ,(devariablize name) ,(devariablize ports 'normal)))))
  
  (defun process-path-end-description (form parent-name)
    (destructuring-bind (&key name ports) form
      (let ((ports (loop for entry in ports
		       if (member (first entry) '(if when))
		       collect (destructuring-bind (&key name type) (third entry)
				 (list name type (second entry)))				   
		       else collect (destructuring-bind (&key name type) entry
				      (list name type)) )))
	`(add-path-end ,parent-name  ,(devariablize name) ,(devariablize ports 'normal)))))


  (defun process-state-element-description (form parent-name)
    (destructuring-bind (&key direction name port-name port-type locality ((:state state-name))) form
      (unless state-name (setq state-name name))
      (ecase direction
	(source `(add-state-source ,parent-name ,(devariablize name) ,(devariablize port-name) 
				   ,(devariablize port-type 'normal) ,(devariablize state-name) 
				   ',locality))
	(sink `(add-state-sink ,parent-name ,(devariablize name) ,(devariablize port-name) 
			       ,(devariablize port-type 'normal) ,(devariablize state-name))))))

  (defun process-constant-description (form parent-name)
    (destructuring-bind (&key name type value local) form
      `(add-constant ,parent-name ,(devariablize name) ,(devariablize type) ,(devariablize value) ,(devariablize local))))

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
				     `(join-named ,(devariablize destination-join-name) 
						  (child-named ,parent-name ,(devariablize destination-component-name)))))
	    (source-component (if (null source-branch-name)
				  `(child-named ,parent-name ,(devariablize source-component-name))
				`(branch-named ,(devariablize source-branch-name) 
					       (child-named ,parent-name ,(devariablize source-component-name))))))
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
       (t (setq destination-component-name `(join-named ,(devariablize destination-branch-name) 
							(child-named ,parent-name ,(devariablize destination-component-name)))
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
        (string token)
        (list (ecase mode
                (raw (loop for thing in token collect (devariablize thing mode)))
                (top-level (loop for thing in token collect (devariablize thing 'normal)))
                (normal `(list ,@(loop for thing in token collect (devariablize thing 'normal))))
                (form (cons (devariablize (first token) 'raw)
                            (loop for thing in (rest token) collect (devariablize thing 'normal))))
                ))
        (keyword token)
        (symbol (if (eql mode 'raw) token `',token)))))

  (defun appropriate-part (form)
    (if (member (first form) '(when repeat)) (third form) form))

  ;; Right now this handles conditionals and numerical iteration
  ;; could probably add something like (over x list)
  (defun handle-annotation (form body)
    (cond
     ((eq (first form) 'when)
      `(when ,(devariablize (second form) 'raw)
         ,body))
     ((eq (first form) 'repeat) 
      (destructuring-bind (variable number) (second form)
        `(loop for ,(devariablize variable 'raw) below ,number do ,body)))
     (t body)))

  (defmacro do-wrapper (form)
    (destructuring-bind (op var last) form
      `(handle-annotation ,var (,op (appropriate-part ,var) ,last))))

  (defmacro defcliche (plan-name args &key components dataflows control-flows 
					   initial-inputs final-outputs path-ends
					   entry-points exit-points state-elements
					   contact-points initializer
					   bindings
					   declarations
					   constants
					   )
    (let ((parent-name (gensym)))
      (multiple-value-bind (let-forms mvs-forms) (process-bindings bindings)
	`(eval-when (:load-toplevel :execute :compile-toplevel)
	   (defun ,plan-name (,parent-name ,@(mapcar #'devariablize args))
	     (declare (ignorable ,@(mapcar #'devariablize args)))
	     (declare ,@declarations)
	     (let* ,let-forms
	       ,@mvs-forms
	       ,@(loop for input in initial-inputs collect 
		       (do-wrapper (process-initial-input-description input parent-name)))
	       ,@(loop for output in final-outputs collect
		       (do-wrapper (process-final-output-description output parent-name)))
	       ,@ (loop for end in path-ends collect
			(do-wrapper (process-path-end-description end parent-name)))
	       ,@(loop for entry-point in entry-points collect
		       (do-wrapper (process-entry-point-description entry-point parent-name)))
	       ,@(loop for exit-point in exit-points collect
		       (do-wrapper (process-exit-point-description exit-point parent-name)))
	       ,@ (loop for constant in constants collect
			(do-wrapper (process-constant-description constant parent-name)))
	       ,@(loop for state-element in state-elements collect
		       (do-wrapper (process-state-element-description state-element parent-name)))
	       ,@(loop for component in components collect
		       (do-wrapper (process-component-description component parent-name)))
	       ,@(loop for dataflow in dataflows collect
		       (do-wrapper (process-dataflow-description dataflow parent-name)))
	       ,@(loop for cflow in control-flows collect 
		       (do-wrapper (process-control-flow-description cflow parent-name)))
	       ))	 
	   ,@(loop for contact-point in contact-points collect (process-contact-point-description plan-name contact-point))
	   (tell `[initializer-for-merge ,',plan-name ,',initializer])
	   )
	)))
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