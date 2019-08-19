;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Most of the predicate definitions are in the file predicate-definitions
;;;  here are the rules that act on them
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Is of type is the transitive closure of sub-type
;;; this gets hairy with the interactions with 
;;; definitions, parameters and viewpoints
;;; It might be better to do this as an ask-data method
;;; since the rules are doing a bunch of lispy stuff

(defrule is-of-type-identity (:backward :importance 150)
  :then [is-of-type ?type ?type]
  :if t
  )

(defrule nil-isnt-subtype (:backward :importance 100)
  :then [is-of-type nil ?foo]
  :if (throw 'bad-type-query nil)
  )

(defrule is-of-type-direct-sub (:backward :importance 1)
  :then [is-of-type ?sub-type ?super-type]
  :if [subtype ?sub-type ?super-type])

;;; Keep this from going on a hunt when you don't know what the
;;; subtype is.
;;; When the subtype is known, move one step to the intermediate
(defrule is-of-type-intermediate-sub (:backward :importance 1)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?sub-type))
	   [subtype ?sub-type ?intermediate]
	   ;; data-structure is a sub-type of itself apparently
	   (not (eql ?sub-type ?intermediate))
	   [is-of-type ?intermediate ?super-type]]
  )

;;; This is the dual, only move one step from
;;; super-type
(defrule is-of-type-intermediate-sub-2 (:backward :importance 1)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   [subtype ?intermediate ?super-type]
	   ;; data-structure is a sub-type of itself apparently
	   (not (eql ?intermediate ?super-type))
	   [is-of-type ?sub-type ?intermediate]]
  )

;;; Make sure that the query was in fact a list
;;; This would only be true if super-type is without qualifiers
;;; this version works down from the supertype
(defrule is-of-type-strip-subtype (:backward :importance 1)
  :then [is-of-type ?qualified-sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   (not (listp ?super-type))
	   (and (not (unbound-logic-variable-p ?qualified-sub-type))
		(listp ?qualified-sub-type))
	   (unify ?sub-type (first ?qualified-sub-type))
	   [is-of-type ?sub-type ?super-type]]
  )

;;; This works up from the sub-type
(defrule is-of-type-strip-subtype-2 (:backward :importance 1)
  :then [is-of-type ?qualified-sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?qualified-sub-type))
	   (listp ?qualified-sub-type)
	   (not (unbound-logic-variable-p ?super-type))
	   (not (listp ?super-type))
	   (unify ?sub-type (first ?qualified-sub-type))
	   [is-of-type ?sub-type ?super-type]
	   ]
  )

(defrule is-of-type-definition (:backward :importance 1)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?sub-type))
	   [definition ?sub-type ?definition]
	   [is-of-type ?definition ?super-type]])

(defrule is-of-type-super-definition (:backward :importance 1)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   [definition ?super-type ?definition]
	   [is-of-type ?sub-type ?definition]])

(defrule is-of-type-union (:backward :importance 1)
  :then [is-of-type ?putative-subtype ?supertype]
  :if [union-member ?putative-subtype ?supertype]
  )

(defun union-members (union-type)
  (let ((answers nil))
    (ask* `[union-member ?sub-type ,union-type]
	  (push ?sub-type answers))
  answers))

;;; Fix these to always work from a grounded position
;;; This works from the super-type
;;; This answers the foo viewpoint of what can be regarded as a ?super-type
(defrule apply-viewpoint-equivalence-for-subtype (:backward :importance 1)
  :then [is-of-type (?viewpoint ?base-type) ?supertype]
  :if [and (not (unbound-logic-variable-p ?supertype))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   [is-of-type ?intermediate-type ?supertype]])

(defrule or-type (:backward)
  :then [is-of-type ?thing-1 (or ?type-1 ?type-2)]
  :if [or [is-of-type ?thing-1 ?type-1][is-of-type ?thing-1 ?type-2]]
)

;;; same with the base type qualified
(defrule apply-viewpoint-equivalence-for-subtype-unpack (:backward :importance 1)
  :then [is-of-type (?viewpoint (?base-type . ?qualifiers)) ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   [is-of-type (?intermediate-type . ?qualifiers) ?super-type]])

;;; This is answers what can the foo viewpoint of ?base-type be regarded as a sub-type of
(defrule apply-viewpoint-equivalence-for-subtype-2 (:backward :importance 1)
  :then [is-of-type (?viewpoint ?base-type) ?supertype]
  :if [and (not (unbound-logic-variable-p ?base-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   (not (unbound-logic-variable-p ?intermediate-type))
	   [is-of-type ?intermediate-type ?supertype]])

(defrule apply-viewpoint-equivalence-for-subtype-2-unpack (:backward :importance 1)
  :then [is-of-type (?viewpoint (?base-type . ?qualifiers)) ?supertype]
  :if [and (not (unbound-logic-variable-p ?base-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   (not (unbound-logic-variable-p ?intermediate-type))
	   [is-of-type (?intermediate-type . ?qualifiers) ?supertype]])



;;; Probably want to make sure that 
;;; that both things are actually lists
(defrule compound-is-of-type (:backward :importance 1)
  :then [is-of-type (?base-1 . ?qualifiers-1) (?base-2 . ?qualifiers-2)]
  :if [and [is-of-type ?base-1 ?base-2]
	   [is-of-type ?qualifiers-1 ?qualifiers-2]])

;;; this just chases the inheritance paths
;;; anything connected to this class specifically
;;; will be retrieved by the ASK when it fetches predications
(defrule ports-of-type-in-direction (:backward :importance 1)
  :then [has-port ?procedure ?direction ?port-name ?governing-parameter]
  :if [and [subtype ?procedure ?super-type]
	   [has-port ?direction ?super-type ?port-name ?governing-parameter]])

(defrule get-port-and-type-constraints-inheritance (:backward :importance 1)
  :then [has-data-type ?procedure-type ?direction ?name ?type-constraint]
  :if [and [subtype ?procedure-type ?super-type]
	   [has-data-type ?super-type ?direction ?name ?type-constraint]
	   ])

(defun get-port-type-constraints (type)
  (let ((entries nil))
    (ask* `[has-data-type ,type ?direction ?name ?type-constraint]
	  (let ((entry (find ?name entries :key #'first)))
	    (unless entry
	      (setq entry (list ?name ?direction nil))
	      (push entry entries))
	    (pushnew ?type-constraint (third entry))))
    (loop for (name direction type-constraints) in entries
	if (eql direction 'input)
	collect (list name type-constraints) into inputs
	else collect (list name type-constraints) into outputs
	finally (return (values inputs outputs)))))

(defrule assert-type (:forward)
  :if [component-of ?parent ?child]
  :then `[type-of ?child ,(task-type ?child)]
  )

(defrule qualified-type-inheritance (:forward)
  :if [and [type-of ?thing (?base-type ?element-type)]
	   (not (eql ?base-type 'stream))
	   ]
  :then [type-of ?thing ?base-type]
  )

(defrule forward-type-inheritance (:forward)
  :if [and [type-of ?child ?procedure-type]
	   [subtype ?procedure-type ?super-type]]
  :then [type-of ?child ?super-type])

(defrule property-inheritance (:backward)
  :then [has-property ?type ?property]
  :if [and [subtype ?type ?parent-type]
	   [has-property ?parent-type ?property]])

(defrule property-inheritance-through-definition (:backward)
  :then [has-property ?type ?property]
  :if [and [or [definition ?type ?supertype]
	       [definition ?type (?supertype ?element-type)]]
	   [has-property ?supertype ?property]
	   ])

(defun properties-of-type (type-name)
  (let ((properties nil))
    (ask* `[has-property ,type-name ?property]
	  (pushnew ?property properties))
    properties))


(define-predicate port-name-and-direction (port direction name)
  (tell-error-model false-query-error-model default-ask-model)
  )

(define-predicate-method (ask port-name-and-direction) (truth-value continuation do-backward-rules do-queries)
  (declare (ignore truth-value do-backward-rules do-queries))
  (with-statement-destructured (port direction name) self
    (when (unbound-logic-variable-p port)
      (error
	'ji:model-cant-handle-query
	:query self
	:model ' port-name-and-direction))
    (with-unification
	(when (and (unify direction (direction port))
		   (unify name (name port)))
	  (funcall continuation nil)))))

;;; you could write an ask method for this

(define-predicate-method (expand-forward-rule-trigger port-name-and-direction) (support-variable-name truth-value context bound-variables)
  (declare (ignore context bound-variables)) 
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  (with-predication-maker-destructured (port direction name) self
    `(:procedure
      (and (unify ,direction (direction ,port))
	   (unify ,name (name ,port)))
      ,support-variable-name
      ,(list name direction))))
  
;;; Do we actually need two different assertion types
;;; one for dependent data type constraints
;;; and one for absolute?

(define-predicate parameter-pattern-value (instance type pattern instantiated-pattern)
  (tell-error-model false-query-error-model default-ask-model)
  )

(define-predicate-method (fetch parameter-pattern-value) (continuation)
  (declare (ignore continuation))
  (values))

(define-predicate-method (expand-forward-rule-trigger parameter-pattern-value) (support-variable-name truth-value context bound-variables)
  (declare (ignore context bound-variables)) 
  (unless (eql truth-value +true+)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  (with-predication-maker-destructured (instance type pattern instantiated-pattern) self
    `(:procedure
      (multiple-value-bind (answer support) (instantiate-parameter-pattern ,pattern ,instance ,type)
	(when answer
	  (unify answer ,instantiated-pattern)
	  (succeed support)))
      ,support-variable-name
      ,(list instantiated-pattern))))

(defun property-value-of (object property-name)
  (ask `[property-value-of ,object ,property-name ?property-value]
       #'(lambda (just)
	   (return-from property-value-of
	     (values ?property-value
		     (ask-database-predication just))))))

(defun port-property-value-of (port property-name)
  (ask `[port-property-value-of ,port ,property-name ?property-value]
       #'(lambda (just)
	   (return-from port-property-value-of
	     (values ?property-value
		     (ask-database-predication just))))))

(defun instantiate-parameter-pattern (pattern instance type)
  (let ((predications nil))
    (labels ((do-it (sub-pattern)
	       (if (atom sub-pattern)
		   (get-instance-property-value sub-pattern)
		 (loop for term in sub-pattern
		     collect (do-it term))))
	     (get-instance-property-value (property-name)
	       ;; if the type doesn't have this symbol as a property
	       ;; then just return it, otherwise look up it's value
	       (ask `[has-property ,type ,property-name]
		    #'(lambda (just)
			(push (ask-database-predication just) predications)
			(ask `[property-value-of ,instance ,property-name ?property-value]
			     #'(lambda (just)
				 (push (ask-database-predication just) predications)
				 (return-from get-instance-property-value ?property-value)))
			(return-from get-instance-property-value nil)))
	       property-name))
      (let ((answer (do-it pattern)))
	(values answer predications)))))

;;;(defrule assert-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [port-of ?child ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?port-direction ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint])


;;;(defrule assert-branch-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [branch-of ?child ?branch]
;;;	   (unify ?branch-name (name ?branch))
;;;	   [port-of ?branch ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?branch-name ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint]
;;;  )


;;;(defrule assert-join-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [join-of ?child ?join]
;;;	   (unify ?join-name (name ?join))
;;;	   [port-of ?join ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?join-name ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint]
;;;  )


(defrule create-typical-value (:forward)
  :if [and [port-of ?x ?port]
	   (not (is-known? [typical-value-of ?port ?existing-value]))
	   ]
  :then `[typical-value-of ?port ,(intern (gensym "VALUE"))])

;;; for example: the typical disk drive has max write rate of x
(defrule assume-typical-property-value (:forward)
  :if [and [type-of ?thing ?type]
	   [typical-property-value-of ?type ?property ?value]
	   [has-property ?type ?property]
	   ]
  :then (tell `[property-value-of ?thing ?property ?value]
	      :justification `(:assumption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reload a file of facts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restore-core-facts (&key (clean? nil) (pathnames (list "natsoft:code;core-facts.lisp" "natsoft:code;reductions-and-cliches.lisp")))
  (when clean? 
    (clear t nil)
    (clrhash *task-type-hash-table*))
  (ji:with-joshua-readtable 
      (let ((*package* (find-package :natsoft)))
	(cond
	 ((listp pathnames)
	  (loop for pathname in pathnames
	      do (load pathname)))
	 ((pathnamep pathnames) 
	  (load pathnames)))	
	)))

(defun is-a-type? (type-name)
  (let ((found-it nil))
    (ask* `[is-type ,type-name]
	  (setq found-it t))
    found-it))

(defun is-of-type? (putative-sub super)
  (catch 'bad-type-query
    (block found-it
      (ask* `[is-of-type ,putative-sub ,super]
	    (return-from found-it t))
      nil)))

(defrule check-dataflow-port-compatibilty (:forward)
  :if [dataflow-from ?flow ?source-port ?destination-port]
  :then (check-port-compatability ?flow ?source-port ?destination-port))


(defun check-port-compatability (flow source-port destination-port)
  (declare (optimize (debug 3) (speed 1)))
  (let ((source-type (port-type-constraint source-port))
	(destination-type (port-type-constraint destination-port)))
    (unless (is-of-type? source-type destination-type)
      (complain-about-port-incompatibility flow source-port destination-port
					   source-type destination-type))))

(define-predicate is-problematic (port-or-dataflow) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate incompatible-port-types (source-type source-port destination-type destination-port) 
  (no-variables-in-data-mixin ltms:ltms-predicate-model)
  )

(defrule incompatibe-types-means-problematic-dataflow (:forward)
  :if [and [dataflow-from ?flow ?source-port ?destination-port]
	   [incompatible-port-types ?source-type ?source-port ?destination-type ?destination-port]
	   ]
  :then [is-problematic ?flow]
  )

(define-predicate-method (act-on-truth-value-change is-problematic) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (with-statement-destructured (thing) self
    (let ((current-truth-value (predication-truth-value self)))
      (cond
       ((and (eql old-truth-value +unknown+)
	     (eql current-truth-value +true+))
	(setf (is-problematic? thing) t))
       ((eql current-truth-value +unknown+)
	(setf (is-problematic? thing) nil))))))

(defrule problematic-flow-means-problematic-ports (:forward)
  :if [and [is-problematic ?flow]
	   (typep ?flow 'dataflow)
	   ]
  :then `[and [is-problematic ,(input ?flow)]
	      [is-problematic ,(output ?flow)]]
  )





(defun insert-transducer-for-problematic-thing ()
  (ask* [and [is-problematic ?flow]
	     [dataflow-from ?flow ?source-port ?destination-port]
	     [incompatible-port-types ?source-type ?source-port ?destination-type ?destination-port]
	     ]
	(insert-transducer ?flow ?source-port ?destination-port ?source-type ?destination-type)))


(defun port-type-constraints (port)
  (let ((answers nil))
    (ask* `[port-type-constraint ,port ?type]
	  (push ?type answers))
    answers))

(defun find-most-specific-types-of (object)
  (let ((types nil))
    (ask* `[type-of ,object ?type]
	  (unless (find ?type types :test #'(lambda (a b) (is-of-type? b a)))
	    (push ?type types))
	  (loop for thing in types
	      when (and (not (eql ?type thing)) (is-of-type? ?type thing))
	      do (setq types (delete thing types))))
    types))

;;;  Note this is dead code
;;;; (defun get-port-properties-of-object (object direction port-name)
;;;;   (let ((properties nil))
;;;;     (ask `[and [type-of ,object ?type]
;;;; 	       [has-port-property ?type ,direction ,port-name ?property-name]]
;;;; 	 #'(lambda (just)
;;;; 	     (declare (ignore just))
;;;; 	     (pushnew ?property-name properties)))
;;;;     properties))

(defun is-known? (predication)
  (ask* predication
	(return-from is-known? t))
  nil)


(defun state-a-speed-estimate (thing quantity units &key (verb "estimate") (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (I (intern-constant generator '|I|))
	 (estimate (intern-verb generator verb
			       :tense '|past|
			       ))
	 (speed (intern-entity generator '|speed|
					 :definite? t
					 :modifier thing
					 ))
	 (quantity (intern-constant generator quantity))
	 (is-a (intern-verb generator "is-a" :tense '|inf|))
	 (interned-units (intern-constant generator units))
	 (speed-is-clause (intern-and-add-texp generator (list speed is-a interned-units)))
	 )
    (create-main-clause generator I estimate speed-is-clause)
    (intern-and-add-texp generator (list speed-is-clause (intern-new-instance generator '|per|) (intern-new-instance generator '|second|)))
    (intern-and-add-texp generator (list interned-units (intern-new-instance generator '|has_quantity|) quantity))
    (when vocalize?
      (vocalize-generator generator :vocalize? vocalize?))
    generator
    ))

(defun build-a-comparison (thing1 thing2 comparative &key (connective '|than|) (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (first-guy (intern-entity generator thing1))
	 (second-guy (intern-entity generator thing2))
	 (verb (intern-verb generator "is"))
	 (adjective (intern-constant generator comparative))
	 (main-clause (create-main-clause generator first-guy verb adjective 
					  :is-imperative? nil
					  :is-question? nil :subject-is-wh? nil))
	 (has-property (intern-new-instance generator '|has_property|))
	 (than (intern-new-instance generator connective)))
    (intern-and-add-texp generator (list first-guy has-property adjective))
    (intern-and-add-texp generator (list main-clause than second-guy))
    (when vocalize?
      (vocalize-generator generator :vocalize? vocalize?))
    generator))

;;; Once you know the typical value of a port and the port type constraint
;;; apply that constraint to the typical value
;;; Fix: type-of should apply only to procedural things
;;; but here this is applying it to a data element.  Should be is-of-type?
(defrule type-typical-value (:forward)
  :if [and [typical-value-of ?port ?value]
	   [port-type-constraint ?port ?constraint]
	   ]
  :then [type-of ?value ?constraint])

;;; Create a typical element of every container
(defrule create-typical-element-of (:forward)
  :if [and [type-of ?value container]
	   (not (is-known? [typical-element-of ?value ?anything]))
	   ]
  :then `[typical-element-of ?value ,(intern (gensym "VALUE"))]
  )

;;; Create a typical element of every stream
(defrule create-stream-typical-element (:forward)
  :if [and [type-of ?value (stream ?subtype)]
	   (not (is-known? [typical-element-of ?value ?anything]))
	   ]
  :then `[typical-element-of ?value ,(intern (gensym "VALUE"))]
  )

;;; type the typical element of any stream or container
(defrule type-typical-element (:forward)
  :if [and [typical-element-of ?container ?value]
	   [type-of ?container (?container-type ?element-type)]]
  :then [type-of ?value ?element-type]
  )

;;; Type the typical value of a port that contains streams
(defrule type-typical-element-of (:forward)
  :if [and [typical-value-of ?port ?value]
	   [typical-element-of ?value ?element]
	   [type-of ?value (stream ?type)]]
  :then [type-of ?element ?type]
  )

(defrule compute-data-rate (:forward)
  :if [and [port-type-constraint ?port (stream ?stuff)]
	   [typical-value-of ?port ?port-stream]
	   [typical-element-of ?port-stream ?port-element]
	   [property-value-of ?port-stream stream-rate ?stream-rate]
	   ;; This is clearly wrong, just a hack for now
	   ;; really need to have typical object in ports
	   ;; and ask about that
	   [property-value-of ?port-element size-in-bytes ?byte-size]
	   ]
  :then `[port-property-value-of ?port data-rate ,(* ?stream-rate ?byte-size)]
  )

(defrule propagate-definition-type (:forward)
  :if [and [type-of ?thing ?type]
	   [definition ?type ?definition]]
  :then [type-of ?thing ?definition]
  )

(defrule compute-size-in-bytes-array (:forward)
  :if [and [type-of ?array array]
	   [property-value-of ?array x-dimension ?x]
	   [property-value-of ?array y-dimension ?y]
	   [typical-element-of ?array ?element]
	   [property-value-of ?element bytes-per-element ?n-bytes]
	   ]
    :then `[property-value-of ?array size-in-bytes ,(* ?x ?y ?n-bytes)]
    )



(defun is-definition-for (type)
  (ask* `[definition ,type ?other-type]
	(return-from is-definition-for ?other-type))
  nil)

(defun is-definition-of (type)
  (ask* `[definition ?other-type ,type]
	   (return-from is-definition-of ?other-type))
  nil)

(defun typical-value-of (thing)
  (ask* `[typical-value-of ,thing ?value]
	(return-from typical-value-of ?value))
  nil)

(defun is-typical-value-of (thing)
  (ask* `[typical-value-of ?value ,thing]
	(return-from is-typical-value-of ?value))
  nil)

(defun typical-element-of (thing)
  (ask* `[typical-element-of ,thing ?value]
	(return-from typical-element-of ?value))
  nil)


(defun is-typical-element-of (thing)
  (ask* `[typical-element-of ?value ,thing]
	(return-from is-typical-element-of ?value))
  nil)

(defun definition-of (complex-name)
  (ask* `[definition ?defined-name ,complex-name]
	(return-from definition-of ?defined-name))
  nil)

(defun ask-to-fill-in-information (&optional (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (you (intern-constant generator '|you|))
	 (verb (intern-verb generator "fill_in"))
	 (information (intern-entity generator '|information| :definite? '|this|))
	 (main (create-main-clause generator you verb information
				   :subject-is-wh? nil
				   :is-question? nil
				   :is-imperative? t
				   )))
    (intern-and-add-texp generator
			 (list main (intern-constant generator '|is_imperative|) (intern-constant generator '|Yes|)))
    (vocalize-generator generator :vocalize? vocalize?)))


;;; The issue with the above is that each method makes its own assertions, so if you run subordinate you make assertions
;;; on every iteration of the accepting-values loop, which isn't what you want to do.  Currently fixed this by doing two
;;; accepting-values.  Better is that if you're the top level you need to bind an alist (or hashtable) that the values
;;; are kept in and only the top level does the assertions at the end.

;;; (explain-purpose '|determine| '|speed| '|disk_drive| t) -> "I am trying to determine the speed of the disk drive"

;;; Note 100MB/sec is a reasonable number for disk speed these days.
;;; 1280 x 800 is the WXGA standard
;;; 15 frames per second?
;;; Gives 61,440,000 Byte/sec
									      
(defun find-rate-reducer (&optional (top-level-type 'transducer) (property 'data-rate))
  (let ((answers nil))
    (ask* `[has-postcondition ?component-type
			      [less-than (,property (output (?output-port ?component-type))) (,property (input (?input-port ?component-type)))]
			      ]
	  (when (is-of-type? ?component-type top-level-type)
	    (push (list ?component-type ?input-port ?output-port)
		  answers)))
    answers))

(defun make-a-suggestion (verb object property-of-object)
  (let* ((generator (make-instance 'question-generator))
	 (suggest (intern-verb generator "suggest"))
	 (me (intern-constant generator "I"))
	 (you (intern-constant generator "you"))
	 (verb (intern-verb generator verb))
	 (interned-object (intern-entity generator object :definite? t))
	 (interned-property (intern-entity generator property-of-object :definite? t))
	 (sub (intern-and-add-texp generator (list you verb interned-property)))
	 (has-modal (intern-constant generator "has_modal"))
	 (will (intern-constant generator "will"))
	 (has-tense (intern-constant generator "has_tense"))
	 (past (intern-constant generator "past"))
	 (main (create-main-clause generator me suggest sub))
	 )
    (create-possesive generator interned-property interned-object)
    (intern-and-add-texp generator (list main has-tense past))	 
    (intern-and-add-texp generator (list main has-modal will))
    generator
  ))


(defgeneric matches-signature (object1 object2)
  (:method-combination and)
  )

;;; This will apply to all objects, since everything is core-task-mixin
;;; Things only match if they are the same type of thing.
(defmethod matches-signature and ((object1 core-task-mixin) (object2 core-task-mixin))
  (eql (common-lisp:type-of object1) (common-lisp:type-of object2)))
  
(defmethod matches-signature and ((object1 branching-task) (object2 branching-task))
  (let ((branches1 (branches object1))
	(branches2 (branches object2)))
    (and (= (length branches1) (length branches2))
	 (loop for branch1 in branches1
	     for name1 = (name branch1)
	     for branch2 = (branch-named name1 object2)
	     always (and branch2
			 (matches-signature branch1 branch2))
		    )))  
  )

(defmethod matches-signature and ((object1 joining-task) (object2 joining-task))
  (let ((joins1 (joins object1))
	(joins2 (joins object2)))
    (and (= (length joins1) (length joins2))
	 (loop for join1 in joins1
	     for name1 = (name join1)
	     for join2 = (join-named name1 object2) 
	     always (and join2
			 (matches-signature join1 join2))
		    )))
  )

(defmethod matches-signature and ((object1 input-side-mixin) (object2 input-side-mixin))
  (let ((inputs1 (inputs object1))
	(inputs2 (inputs object2)))
    (and (= (length inputs1) (length inputs2))
	 (loop for input1 in inputs1
	     for name1 = (name input1)
	     for type-constraint-1 = (port-type-constraint input1)
	     for input2 = (port-named 'input name1 object2)
	     always (and input2
			 (equal (port-type-constraint input2) type-constraint-1)))))
  )

(defmethod matches-signature and ((object1 output-side-mixin) (object2 output-side-mixin))
  (let ((outputs1 (outputs object1))
	(outputs2 (outputs object2)))
    (and (= (length outputs1) (length outputs2))
	 (loop for output1 in outputs1
	     for name1 = (name output1)
	     for type-constraint-1 = (port-type-constraint output1)
	     for output2 = (port-named 'output name1 object2)
	     always (and output2
			 (equal (port-type-constraint output2) type-constraint-1)))))
  )

(defun find-matching-component (target-component top-level)
  (map-over-selected-implementation top-level
				    #'(lambda (sub-task history implementation)
					(declare (ignore history implementation))
					(when (matches-signature sub-task target-component)
					  (return-from find-matching-component sub-task))))
  nil
  )

(defun find-matching-compressor (top-level)
  (let* ((new-object (create-task 'foo 'image-stream-compressor nil '(output-type (sequence byte))))
	 (the-match (loop for component in (children top-level)
			for target = (find-matching-component new-object component)
			when target return target)))
    the-match
    ))

(defun build-can-be-done-using-statement (agent verb object &optional object-modifier agent-definite)
  (let* ((generator (make-instance 'question-generator))
	 (somebody (intern-constant generator "somebody"))
	 (interned-agent (intern-entity generator agent :definite? agent-definite))
	 (verb (intern-verb generator verb :voice '|passive| :modality '|can|))
	 (interned-object (intern-entity generator object :definite? nil))
	 (main (create-main-clause generator somebody verb interned-object)))
    (when object-modifier (create-modification generator interned-object object-modifier))
    (create-using-clause generator main interned-agent)
    generator))

(defun build-try-sentence (verb object &optional object-modifier)
  (let* ((generator (make-instance 'question-generator))
	 (me (intern-constant generator "I"))
	 (interned-verb (intern-verb generator verb :tense '|to|))
	 (interned-object (intern-entity generator object :definite? nil))
	 (try (intern-verb generator "try" :modality '|will|))
	 (clause (intern-and-add-texp generator (list me interned-verb interned-object))))
    (create-main-clause generator me try clause)    
    (when object-modifier (create-modification generator interned-object object-modifier))
    generator))
