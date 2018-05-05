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
	(declare (ignore key-type))
	(List canonical-input-type 
	      canonical-output-type
	      input-type		;before canonicalization
	      output-type		;ditto
	      (name (first (inputs the-enumerator))) ;input-name
	      (name (first (outputs (branch-named 'more the-enumerator)))) ;output-name
	      canonical-key-type
	      )))))

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

(defmethod canonical-arguments-for ((type (eql 'enumerate-filter-accumulate)) the-e-f-a)
  (let (input-type output-type enumerator-type filter-type accumulator-type)
      (multiple-value-setq (input-type) (get-one-property the-e-f-a 'input-type))
      (multiple-value-setq (output-type) (get-one-property the-e-f-a 'output-type))
      (multiple-value-setq (enumerator-type) (get-one-property the-e-f-a 'enumerator-type))
      (multiple-value-setq (filter-type) (get-one-property the-e-f-a 'filter-type))
      (multiple-value-setq (accumulator-type) (get-one-property the-e-f-a 'accumulator-type))
      (list input-type enumerator-type filter-type accumulator-type output-type)
      ))

;;; This is the demo

(defmethod canonical-arguments-for ((type (eql 'jdd)) the-dup-detector)
  (declare (ignore the-dup-detector))
  nil)

(defmethod canonical-arguments-for ((type (eql 'sorted-list-test)) the-thing)
  (declare (ignore the-thing))
  nil)

(defmethod canonical-arguments-for ((type (eql 'numerical-accumulator)) the-accumulator)
  (loop for property-name in '(numeric-type op initial-value)
      collect (get-one-property the-accumulator property-name)))

(defmethod canonical-arguments-for ((type (eql 'array-acc)) the-accumulator)
  (let (input-type output-type)
    (multiple-value-setq (input-type) (get-one-property the-accumulator 'input-type))
    (multiple-value-setq (output-type) (get-one-property the-accumulator 'output-type))
    (list input-type output-type)
    ))