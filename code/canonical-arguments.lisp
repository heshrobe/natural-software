;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)


;;; Rationale:
;;; The reason that this passes on both the original expression of the input and output types
;;; not just the canonical ones:
;;; The original ones might refer to something that needs to be viewed as something else (e.g. an array as a vector of rows)
;;; and somewhere in the process we want to record that we reduced from one to the other.
;;;
;;; As a consequence Reductions (i.e. the planning operators) take both canonical and as expressed arguments
;;; but the definitions of a plan-structure need not.

(defun get-one-property (object property-name)
  (ask* `[property-value-of ,object ,property-name ?answer]
	(return-from get-one-property (values (canonical-form-for ?answer) ?answer))))

(defmethod canonical-arguments-for ((type (eql 'transducer)) the-transducer)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-transducer 'input-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-transducer 'output-type)
      (List canonical-input-type canonical-output-type input-type output-type
	    (name (first (inputs the-transducer)))
	    (name (first (outputs the-transducer)))
	    ))))

(defmethod canonical-arguments-for ((type (eql 'branching-transducer)) the-transducer)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-transducer 'input-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-transducer 'output-type)
      (List canonical-input-type canonical-output-type input-type output-type
	    (name (first (inputs the-transducer)))
	    (name (first (outputs (branch-named 'more the-transducer))))
	    ))))

;;; because of final branches and initial joins these need to be a bit different
(defmethod canonical-arguments-for ((type (eql 'generator)) the-generator)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-generator 'input-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-generator 'output-type)
      (List canonical-input-type 
	    canonical-output-type
	    input-type			;before canonicalization
	    output-type			;ditto
	    (name (first (inputs the-generator))) ;input-name
	    (name (first (outputs (branch-named 'more the-generator)))) ;output-name
	    ))))

(defmethod canonical-arguments-for ((type (eql 'collector)) the-collector)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-collector 'input-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-collector 'output-type)
      (List canonical-input-type canonical-output-type input-type output-type
	    (name (first (inputs (join-named 'more the-collector))))
	    (name (second (inputs (join-named 'more the-collector))))
	    ))))

(defmethod canonical-arguments-for ((type (eql 'detect-duplicates)) the-detector)
  (let (canonical-input-type canonical-output-type canonical-key-extractor-type)
    (multiple-value-setq (canonical-output-type) (get-one-property the-detector 'output-type))
    (multiple-value-setq (canonical-key-extractor-type) (get-one-property the-detector 'key-extractor-type))
    (multiple-value-setq (canonical-input-type) (get-one-property the-detector 'input-type))
    (list canonical-input-type canonical-output-type canonical-key-extractor-type)))
    
(defmethod canonical-arguments-for ((type (eql 'tree-traverse)) the-traverser)
  (let (canonical-set-type canonical-recursive-call-name canonical-key-type)
    (multiple-value-setq (canonical-set-type) (get-one-property the-traverser 'set-type))
    (multiple-value-setq (canonical-recursive-call-name) (get-one-property the-traverser 'recursive-call-name))
    ;; (multiple-value-setq (canonical-element-type) (get-one-property 'element-type))
    (multiple-value-setq (canonical-key-type) (get-one-property the-traverser 'key-type))
    (list canonical-set-type canonical-recursive-call-name canonical-key-type)))

;;; Both of these might be capable of simplification
;;; This one doesn't have an argument for the key-type since it doesn't generate keys
;;; There are separate reductions for each with the second guy expecting a key-type argument
(defmethod canonical-arguments-for ((type (eql 'enumerator)) the-enumerator)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-enumerator 'set-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-enumerator 'element-type)
      (multiple-value-bind (canonical-key-type key-type) (get-one-property the-enumerator 'key-type)
	(multiple-value-bind (canonical-for-value for-value) (get-one-property the-enumerator 'for-value)
	  (declare (ignore key-type for-value))
	  (List canonical-input-type 
		canonical-output-type
		input-type		;before canonicalization
		output-type		;ditto
		(name (first (inputs the-enumerator))) ;input-name
		(name (first (outputs (branch-named 'more the-enumerator)))) ;output-name
		canonical-key-type
		canonical-for-value
		(get-one-property the-enumerator 'finally-option)
		))))))

(defmethod canonical-arguments-for ((type (eql 'enumerator-with-keys)) the-enumerator)
  (multiple-value-bind (canonical-input-type input-type) (get-one-property the-enumerator 'set-type)
    (multiple-value-bind (canonical-output-type output-type) (get-one-property the-enumerator 'element-type)
      (multiple-value-bind (canonical-key-type key-type) (get-one-property the-enumerator 'key-type)
	(declare (ignore key-type))
	(List canonical-input-type 
	      canonical-output-type
	      input-type		;before canonicalization
	      output-type		;ditto
	      (name (first (inputs the-enumerator))) ;input-name
	      (name (first (outputs (branch-named 'more the-enumerator)))) ;output-name
	      canonical-key-type
	      )))))

;;; Now a hack for implementing extract, this will be a cheat for a while wired to the needs of the json tree example.

(defmethod canonical-arguments-for ((type (eql 'extractor)) the-enumerator)
  (multiple-value-bind (input-type original-input-type) (get-one-property the-enumerator 'input-type)
    (declare (ignore original-input-type))
    (multiple-value-bind (output-type original-output-type) (get-one-property the-enumerator 'output-type)
      (declare (ignore original-output-type))
      (list input-type output-type)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerate-filter-accumulate and Enumerate-filter-affect-each
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod canonical-arguments-for ((type (eql 'enumerate-filter-accumulate)) the-e-f-a)
  (loop for property-name in '(input-type enumerator-type filter-type accumulator-type output-type)
      collect (get-one-property the-e-f-a property-name)))

(defmethod canonical-arguments-for ((type (eql 'numerical-accumulator)) the-accumulator)
  (loop for property-name in '(numeric-type op initial-value)
      collect (get-one-property the-accumulator property-name)))


(defmethod canonical-arguments-for ((type (eql 'enumerate-filter-affect-each)) the-e-f-a)
  (loop for property-name in '(input-type enumerator-type filter-type affector-type)
      collect (get-one-property the-e-f-a property-name)))

(defmethod canonical-arguments-for ((type (eql 'updater) ) the-normalizer)
  (loop for property-name in '(set-type element-type op-type)
      collect (get-one-property the-normalizer property-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is for the jdd demo
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod canonical-arguments-for ((type (eql 'jdd)) the-dup-detector)
  (declare (ignore the-dup-detector))
  nil)

(defmethod canonical-arguments-for ((type (eql 'sorted-list-test)) the-thing)
  (declare (ignore the-thing))
  nil)


(defmethod canonical-arguments-for ((type (eql 'array-acc)) the-accumulator)
  (let (input-type output-type)
    (multiple-value-setq (input-type) (get-one-property the-accumulator 'input-type))
    (multiple-value-setq (output-type) (get-one-property the-accumulator 'output-type))
    (list input-type output-type)
    ))

(defmethod canonical-arguments-for ((type (eql 'set-accumulator)) the-accumulator)
  (let (input-type output-type op initial-value)
    (multiple-value-setq (input-type) (get-one-property the-accumulator 'input-type))
    (multiple-value-setq (output-type) (get-one-property the-accumulator 'output-type))
    (multiple-value-setq (op) (get-one-property the-accumulator 'op))
    (multiple-value-setq (initial-value) (get-one-property the-accumulator 'initial-value))
    (list input-type output-type op initial-value)
    ))