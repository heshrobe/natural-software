;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

(deftoplevel computational-stuff)

;; this is a non-informative type constraint
;; since everything is sub type of data structure
;; but that's what clues the apprentice
;; into asking about it
;; [is-type data-structure]
;; [subtype data-structure computational-stuff]

(defdata-type data-structure
    :super-types (computational-stuff)
    )

(defdata-type signal
    :super-types (data-structure)
    )

;;; Things that are non-decomposable things like numbers
(defdata-type primitives
    :super-types (data-structure)
    )

(defdata-type symbol
    :super-types (primitives)
    )

(defdata-type string
    :super-types (primitives)
    )

(defdata-type boolean
    :super-types (primitives)
    :allocation-code ((:lisp () 'nil))
    ;; :other-assertions ([allocation-code list nil :lisp])
    )

;;; Things that are things like arrays, sets, sequences that contain other things

(defdata-type container
    :super-types (data-structure)
    :parameters (element-type)
    )

(defdata-type set
    :super-types (container)
    :parameters (element-type)
    :definition (non-duplicated element-type)
    )

(defdata-type tuple
    :super-types (container)
    :parameters (type-sequence)
    )

(defdata-type sequence
    :super-types (container)
    :parameters (element-type)
    )

(defdata-type spatial-sequence
    :super-types (sequence)
    :parameters (element-type)
    )

(defdata-type temporal-sequence
    :super-types (sequence)
    :parameters (element-type)
    )

(defdata-type vector
    :super-types (spatial-sequence)
    :parameters (dimension)
    )

(defdata-type list
    :super-types (spatial-sequence)
    :parameters (element-type)
    :equivalences (((setq ?x (adjoin ?y ?x)) (pushnew ?y ?x))
		      ((setq ?x (cons ?y ?x)) (push ?y ?x)))
    :allocation-code ((:lisp () '(list)))
    ;; :other-assertions ([allocation-code list (list) :lisp])
    )

(defdata-type stream
    :super-types (temporal-sequence)
    :allocation-code ((:lisp () '(make-empty-queue)))
    ;; :other-assertions ([allocation-code stream (make-empty-queue) :lisp])
    )

(defdata-type array
    :super-types (container)
    :parameters (x-dimension y-dimension element-type)
    )

(defdata-type mapping
    :super-types (container)
    :parameters (from-type to-type)
    :definition (set (pair from-type to-type))
    )

(defdata-type bit
    :super-types (primitives)
    )

(defdata-type number
    :super-types (primitives)
    :parameters (size)
    )

(defdata-type integer
    :super-types (number)
    )

(defdata-type float
    :super-types (number)
    )

(defdata-type byte
    :super-types (integer)
    )

(defdata-type pixel
    :super-types (integer)
    )

(defdata-type bit-vector
    :definition (vector bit)
    )

(defdata-type image
    :definition (array pixel 2)
    )

(defdata-type disk-buffer
    :parameters (size)
    :definition (vector byte)
    :allocation-code ((:lisp (size) `(make-array ,size :fill-pointer 0)))
    ;; :other-assertions ([allocation-code disk-buffer (make-array *disk-buffer-size* :fill-pointer 0) :lisp])
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trees and the like
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A non-terminal-tree-node is a sequence 
;;;   whose elements are tree-nodes
;;;   A tree-node is either a non-terminal-tree-node or a terminal-tree-node

(defdata-type pair
    :parameters (first-type second-type)
    :parts ((left first-type)
	    (right second-type))
    :equivalences (((left ?x) (first ?x))
		   ((right ?x) (rest ?x)))
    )


(defdata-type tree-node
    :super-types (data-structure)
    :union (tree-non-terminal-node tree-terminal-node)
    )

(defdata-type tree-non-terminal-node
    :super-types (set)
    )

(defdata-type tree-terminal-node
    :super-types (data-structure)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Labeled Trees
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdata-type labeled-tree-non-terminal-node
    :super-types (tree-non-terminal-node)
    :parameters (from-type to-type)
    :definition (mapping from-type to-type)
    )


;;; A vector of JSON-tree-nodes is a JSON-non-terminal-node
;;; A list of pairs of symbols and JSON-tree-nodes is a JSON-non-terminal-node
;;; A symbol is a JSON-terminal-node.

(defdata-type JSON-node
    :super-types (tree-node)
    :union (json-non-terminal-node json-terminal-node)
    )

;;; Note: Need to make the type system aware of Unions
(defdata-type json-key
    :super-types (data-structure)
    :union (integer symbol)
    )

(defdata-type json-path
    :definition (list json-key)
    :allocation-code ((:lisp () '(list 'top)))
    ;; :other-assertions ([allocation-code json-path (list 'top) :lisp])
    )

(defdata-type JSON-non-terminal-node
    :union (JSON-alist-node JSON-vector-node)
    :super-types (JSON-node labeled-tree-non-terminal-node)
    )

(defdata-type json-pair 
    :definition (pair symbol json-node))

(defdata-type JSON-alist-node
    :definition (list jsonp-air)
    )

(defdata-type JSON-vector-node
    :definition (vector JSON-node)
    )

(defdata-type JSON-terminal-node
    :union (number symbol string)
    :super-types (JSON-node tree-terminal-node)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; some useful abbreviations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdata-type byte-sequence
    :definition (sequence byte))

(defdata-type pixel-sequence
    :definition (sequence pixel))

(defdata-type pixel-sequence
    :definition (sequence pixel))

(defdata-type byte-stream
    :definition (stream byte))

(defdata-type pixel-stream
    :definition (stream pixel))

(defdata-type byte-vector
    :definition (vector byte))

(defdata-type pixel-vector
    :definition (vector pixel))

(defdata-type byte-array
    :definition (array byte))

;; Frame rate isn't really a data-type (it's a number I guess)
(defdata-type frame-rate
    :definition (stream-rate (stream image)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftask procedure
    :super-types (computational-stuff)
    )

;;; IO-Device is equivalent to peripheral
;;; IO-Device is either an input-device, output-device or birectional

(deftask io-device
    :super-types (procedure)
    :interface ((:inputs (control signal)))
    )

(deftask input-device
    :parameters (data-rate)
    :interface ((:outputs (data data-structure)))
    :super-types (io-device)
    )


(deftask output-device
    :parameters (data-rate)
    :interface ((:inputs (data data-structure)))
    :super-types (io-device)
    )

(deftask bidirectional-device
    :super-types (input-device output-device))

(deftask disk-drive
    :super-types (bidirectional-device)
    :parameters (max-write-speed average-write-speed min-write-speed)
    :interface ((:inputs (data (stream disk-buffer)))
		(:outputs (data (stream disk-buffer))))
    :other-assertions ([typical-property-value-of disk-drive max-write-speed 194]
		       [typical-property-value-of disk-drive average-write-speed 155]
		       [typical-property-value-of disk-drive min-write-speed 102]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transducers
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask transducer
    :parameters (input-type output-type)
    :super-types (procedure)
    :interface ((:Inputs (input-queue input-type))
		(:outputs (output-queue output-type)))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transducers that have branches for more and empty
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask branching-transducer
    :parameters (input-type output-type)
    :super-types (procedure) 
    :interface ((:inputs (input-queue input-type))
		(:branches (:name more :condition (not (empty raw-data)) :outputs ((new-data output-type)))
			   (:name empty :condition (empty raw-data))))
    )
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generators -- Starts with a container and emits a sequence of its elements
;;;  has two branches: one for there being more stuff and one for empty
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask generator
    :parameters (input-type output-type)
    :super-types (procedure)
    :interface ((:inputs (container input-type))
		(:branches (:name more :condition (not (empty container)) :outputs ((new-data output-type)))
			   (:name empty :condition (empty input))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compressors
;;;    A compressor is a type of transducer that transcodes from something
;;;    whose data-rate is higher to something whose data-rate is lower
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask compressor
    :parameters (input-type output-type)
    :super-types (transducer) 
    :interface ((:inputs (raw-data input-type))
		(:branches (:name more :condition (not (empty raw-data)) :outputs ((new-data output-type)))
			   (:name empty :condition (empty raw-data)))
		)
    :other-assertions ([has-postcondition compressor
					  [less-than (data-rate (output (new-data image-stream-compressor)))
						     (data-rate (input (raw-data image-stream-compressor)))]])
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image-stream-compressor
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Fix: the assertions don't get made by inheritance
;;;
;;; deftask actually does inheritance so we don't need to say more.
(deftask image-stream-compressor
    :parameters (input-type output-type)
    :super-types (compressor)
    :other-assertions ([has-postcondition image-stream-compressor 
					  [less-than (data-rate (output (new-data image-stream-compressor))) 
						     (data-rate (input (raw-data image-stream-compressor)))]] )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Collectors -- Starts with a sequence of elements and produces a sequence of 
;;;                             containers holding those elements
;;;     All collectors have at least two inputs cases:
;;;         1) Data available
;;;         2) Data exhausted
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fix:Accumuland here is really a stream of buffers?
(deftask collector
    :super-types (procedure)
    :parameters (input-type output-type input-container-type)
    :interface ((:outputs (chunks output-type))
		(:joins (:name more :inputs ((new-data (input-container-type input-type))
					     (queue output-type)))
			(:name empty :inputs ((queue output-type)))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Take and Put stream operators
;;; Take gobbles an element from an input stream and passes it through
;;; Put takes an element and pushes into its output stream
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftask Take
    :super-types (procedure) 
    :primitive t
    :parameters (input-container-type element-type)
    :interface ((:inputs (sequence-data (input-container-type element-type)))
		(:outputs (data element-type))))


(deftask Put
    :primitive t
    :super-types (procedure) 
    :parameters (element-type output-container-type)
    :interface ((:inputs (data element-type)
			 (sequence (output-container-type element-type)))
	        (:outputs (sequence-data (output-container-type element-type)))
		)
    )

;;; this is essentially return-from in an enumerator dominated block
(deftask truncate
    :primitive t
    :super-types (procedure)
    :parameters (element-type (return-value? nil))
    :interface ((:inputs (data element-type))
		(:outputs (data element-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerators -- Emits a sequence of elements
;;;  Since it's a primitive enumerator it has two branches:
;;;    one for more stuff
;;;    and one for when it's done.
;;;  Can be called with one of these combinations:
;;;    1) Set-type: a list of (collection-type element-type) and nothing else
;;;    2) Collection-type and element-type in which case the set-type is (collection-type element-type)
;;;    3) Set-type and element-type in which case these two are used as is.
;;;    4) Element-type is always raw and then wrapped inside a temporal-sequence as the type of the output-port
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdata-type enumerator-key-types
    :union (primitives tuple)
    )

(defgeneric index-type (thing-type))
(defmethod index-type ((thing cons)) (compound-index-type (first thing) (rest thing)))

(defgeneric compound-index-type (keyword args)) 

(defmethod compound-index-type ((thing (eql 'vector)) args) 
  (declare (ignore args))
  'integer)

(defmethod compound-index-type ((thing (eql 'array)) args) 
  (destructuring-bind (element-type number-of-dimensions) args
    (declare (ignore element-type))
    (loop repeat number-of-dimensions collect 'integer)))

(deftask basic-enumerator
    :primitive t
    :super-types (procedure)
    :parameters (set-type element-type collection-type (for-value t) output-type (Finally-option :for-effect) index-type)
    :bindings ((set-type (cond ((and set-type element-type) set-type)
			       ((and collection-type element-type) (list collection-type element-type))
			       (t set-type)))
	       (element-type (or element-type (second set-type)))
	       (index-type (index-type set-type))
	       (output-type (or output-type (if for-value element-type `(place ,set-type ,index-type))))
	       )
    :interface ((:inputs (the-set set-type))
		(:branches (:name more :condition (not (empty the-set)) :outputs ((the-elements (temporal-sequence output-type))))
			   (:name empty :condition (empty the-set)))))

;;; Can the key-type be eliminated through use of places
;;; or was that for the json demo?
(deftask enumerator
    :parameters (key-type)
    :super-types (basic-enumerator)
    :interface ((:branches (:name more :condition (not (empty the-set)) 
			    :outputs ((the-elements (temporal-sequence output-type))
				      (when key-type (the-keys (temporal-sequence key-type)))
				      (when key-type (the-set (temporal-sequence set-type)))
				      ))))
    )

;; an enumerator that generates both keys and values
(deftask enumerator-with-keys
    :super-types (basic-enumerator)
    :parameters ((key-type element-type)) ;; apparently key type is always expicitly provided, no need to default
    :interface ((:inputs (the-set set-type))
		(:branches (:name more :condition (not (empty the-set)) :outputs ((the-elements (temporal-sequence element-type)) 
										  (the-keys (temporal-sequence key-type))))))
    )

(deftask tree-traverse
    :primitive nil
    :super-types (basic-enumerator)
    :parameters ((recursive-call-name tt))
    )

(deftask terminal?
    :parameters (node-type terminal-type non-terminal-type)
    :primitive t
    :interface ((:inputs (the-node node-type))
		(:branches (:name non-terminal :condition (non-terminal-type the-node) :outputs ((the-node non-terminal-type)))
			   (:name terminal :condition (terminal-type the-node) :outputs ((the-node terminal-type)))))
    )

;;; these need to get fixed to match the enumerator interface
;;; Index enumerator should take as "the-set" a range from lower-bound to upper-bound
;;; This requires the construction of a "range-object" and the code generator for that
;;; should do nothing.

(defdata-type range
    :super-types ((set integer))
    :parameters ((element-type integer))
    )

;;; The propagate code for this must build an integer range object
;;; that will flow to the index-enumerator
(deftask range-constructor
    :primitive t
    :interface ((:inputs (lower-bound integer) (upper-bound integer))
		(:outputs (the-range (range integer)))
		))
    
;;; Fix?: Maybe range is compound?
(defmethod index-type ((thing (eql 'range))) 'integer)
(deftask index-enumerator
    :primitive t
    :parameters ((set-type range) (element-type integer) (for-value t) )
    :super-types (basic-enumerator)
    :interface ((:branches (:name more :condition (inbounds index) :outputs ((indices (temporal-sequence integer))))
			   (:name empty :condition (not (inbounds index)))))
    )

(deftask list-enumerator
    :primitive t
    :super-types (basic-enumerator)
    :parameters ((collection-type list))
    :interface ((:branches (:name more :condition (not (null the-list)) :outputs ((list-elements (temporal-sequence element-type))))
			   (:name empty :condition (null the-list)))))

;;; This mentions "the-list" should be vector and condition
;;; isn't null!!!!
(deftask vector-enumerator
    :primitive t
    :super-types (basic-enumerator)
    :parameters (element-type)
    :interface ((:inputs (the-vector (vector element-type)))
		(:branches (:name more :condition (not (null the-list)) :outputs ((the-elements (temporal-sequence element-type))
										  (the-indices (temporal-sequence integers))))
			   (:name empty :condition (null the-list)))))

;;; An enumerator for the "places in a data structure" (typically a vector, array or list)
;;; Ultimately, a lot of the other things can be built with this and a follow up operation
;;; like a Fetch or an Assign
;;; Also note that this isn't a primitive, it's composed from other things like index-enumerators
;;; This takes in a set and generates a complete and non-redundant set of "places" within the set
;;; Parameters: The set type (e.g. (vector number))

(defmethod offset-type-for-set-type ((set-type (eql 'vector)) &optional subtype) 
  (declare (ignore subtype))
  'integer)

(defmethod offset-type-for-set-type ((set-type (eql 'array)) &optional subtype)
  (rest subtype))

(defmethod offset-type-for-set-type ((set-type (eql 'list)) &optional subtype)
  (declare (ignore subtype))
  'list)


(deftask place-enumerator
    :primitive nil
    :super-types (basic-enumerator)
    :parameters (set-type)
    :bindings ((set-pure-type (If (symbolp set-type) set-type (first set-type) ))
	       (set-sub-type (unless (symbolp set-type) (rest set-type)))
	       (place-type `(place ,set-type ,(offset-type-for-set-type set-pure-type set-sub-type))))
    :interface ((:inputs (the-set set-type))
		(:branches (:name more :condition (not (empty the-set)) :outputs ((the-places (temporal-sequence place-type))))
			   (:name empty :condition (empty the-list)))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extractors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftask extractor
    :primitive nil
    :parameters (input-type output-type)
    :interface ((:inputs (the-container input-type))
		(:outputs (the-part output-type))))

(deftask identity
    :super-types (extractor)
    :parameters ((output-type input-type))
    :Primitive t
    )

(deftask left
    :super-types (extractor)
    :bindings ((real-input-type (or (definition-for input-type) input-type)))
    :parameters ((input-type  real-input-type)
		 (output-type (when real-input-type (second real-input-type)))
		 )
    :Primitive t)

(deftask right
    :super-types (extractor)
    :bindings ((real-input-type (or (definition-for input-type) input-type)))
    :parameters ((input-type real-input-type)
		 (output-type (when real-input-type (third real-input-type))))
    :Primitive t)

(deftask first
    :super-types (extractor)
    :bindings ((real-input-type (or (definition-for input-type) input-type)))
    :parameters ((input-type  real-input-type)
		 (output-type (when real-input-type (second real-input-type)))
		 )
    :Primitive t)

(deftask rest
    :super-types (extractor)
    :bindings ((real-input-type (or (definition-for input-type) input-type)))
    :parameters ((input-type real-input-type)
		 (output-type input-type))
    :Primitive t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector accessors and updaters
;;;  This operates not on stream but actual data
;;;  and therefore when used in a streaming context must be
;;;  bounded but Takes and Puts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask vector-accessor
    :primitive t
    :super-types (procedure)
    :parameters ((element-type integer) (container-type vector) vector-type)
    :bindings ((real-vector-type (or vector-type `(,container-type ,element-type))))
    :interface ((:inputs (vector real-vector-type)
			 (index integer))
		(:outputs (the-element element-type))))


(deftask vector-push
    :primitive t
    :super-types (procedure)
    :parameters (element-type vector-type)
    :interface ((:inputs (new-data element-type)
			 (vector (vector element-type)))
		(:outputs (updated-vector vector-type))))

(deftask vector-updater
    :primitive t
    :super-types (procedure)
    :parameters (element-type)
    :interface ((:inputs (vector (vector element-type))
			 (index integer)
			 (new-data element-type))
	        (:outputs (updated-vector (vector element-type)))))

(deftask vector-length
    :primitive t
    :super-types (procedure)
    :parameters (vector-type)
    :interface ((:inputs (vector vector-type))
		(:outputs (length integer))))

(deftask vector-full-test
    :primitive t
    :parameters (vector-type)
    :interface ((:inputs (the-vector vector-type))
		(:branches (:name full :condition (vector-full the-vector))
			   (:name room :condition (not (vector-full the-vector))))))

(deftask vector-empty-test
    :primitive t
    :parameters (vector-type)
    :interface ((:inputs (the-vector vector-type))
		(:branches (:name empty :condition (vector-empty the-vector))
			   (:name has-stuff :condition (not (vector-empty the-vector))))))

(deftask allocate
    :primitive t
    :parameters (object-type inputs)
    :interface ((:outputs (new-object object-type))))

(deftask simple-join
    :primitive t
    :parameters (object-type first-join-name second-join-name)
    :interface ((:joins (:name first-join-name :inputs ((the-object object-type)))
                        (:name second-join-name :inputs ((the-object object-type))))
                (:outputs (the-object object-type))))

(deftask simple-join-1
    :primitive t
    :parameters (object-type first-join-name second-join-name)
    :interface ((:joins (:name first-join-name :inputs ((the-object object-type)))
			(:name second-join-name :inputs ((the-object object-type))))
		(:outputs (the-object object-type))))


(deftask simple-join-2
    :primitive t
    :parameters (first-object-type second-object-type first-join-name second-join-name)
    :interface ((:joins (:name first-join-name :inputs ((first-object first-object-type) (second-object second-object-type)))
			(:name second-join-name :inputs ((first-object first-object-type) (second-object second-object-type))))
		(:outputs (first-object first-object-type) (second-object second-object-type))))

(deftask control-join
    :primitive t
    :parameters (first-join-name second-join-name)
    :interface ((:joins (:name first-join-name)
			(:name second-join-name))))

(deftask member-test
    :primitive t
    :parameters (set-type element-type)
    :interface ((:inputs (the-set (set-type element-type))
			 (the-element element-type))
		(:branches (:name yes :condition (member the-element the-set))
			   (:name no :condition t)))
    )

(deftask zero-test
    :primitive t
    :parameters ()
    :interface ((:inputs (index integer))
		(:branches (:name yes :condition (equal zero index))
			   (:name no :condition t)))
    )

(deftask equal-test
    :primitive t
    :parameters ((value-1-data-type symbol) (value-2-data-type image))
    :interface ((:inputs (value-1 value-1-data-type) (value-2 value-2-data-type))
		(:branches (:name yes :condition (equal value-1 value-2))
			   (:name no :condition :otherwise)))
    )

(deftask less-than-test
    :primitive t
    :parameters ((numeric-type integer))
    :interface ((:inputs (number-1 numeric-type) (number-2 numeric-type))
		(:branches (:name yes :condition (less-than number-1 number-2))
			   (:name no :condition :otherwise)))
    )

(deftask sorted-test
    :parameters ()
    :interface ((:inputs (the-vector (vector integer)))
		(:outputs (value boolean))))


(deftask add-to-set
    :declarations ((declare (ignore unique?)))
    :primitive t
    :parameters (set-type element-type unique?)
    :interface ((:inputs (the-set (set-type element-type))
			 (the-element element-type))
		(:outputs (the-set (set-type element-type))))
    )

(deftask tuple-constructor
    :primitive t
    :parameters (input-1-type input-2-type output-type)
    :bindings ((real-output-type (or output-type `(tuple ,input-1-type ,input-2-type))))
    :interface ((:inputs (input-1 (temporal-sequence input-1-type)) (input-2 (temporal-sequence input-2-type)))
		(:outputs (the-output (temporal-sequence real-output-type))))
    )

(deftask detect-duplicates
    :primitive nil
    :super-types (set-accumulator)
    )

(deftask filter
    :primitive t
    :parameters (input-type output-type condition)
    :interface ((:inputs (test-data input-type))
		(:branches (:name match :condition condition :outputs ((matching-data output-type)))
			   (:name no-match :condition (not condition)))))

(deftask type-split
    :super-types (filter)
    :primitive t
    :parameters ((non-matching-output-type input-type))
    :bindings ((actual-condition 
		(cond 
		 ((and condition (symbolp condition))
		  (list condition 'test-data))
		 ((not (null condition)) condition)
		 (t `(typep test-data ',output-type))))
	       (actual-output-type (or output-type (third actual-condition))))				 
    :interface ((:branches (:name match :condition actual-condition :outputs ((matching-data actual-output-type)))
			   (:name no-match :condition (not actual-condition) :outputs ((non-matching-data non-matching-output-type)))
			   ))
    )

;;; Fix: these need to be made to match the interface of filter
(deftask empty-test
    :primitive t
    :parameters (element-type)
    :interface ((:inputs (the-list (list element-type)))
		(:branches (:name not-empty :condition (not (null the-list)))
			   (:name empty :condition (null the-list)))))

(deftask queue-empty-test
    :primitive t
    :parameters ()
    :interface ((:inputs (the-queue stream))
		(:branches (:name not-empty :condition (not (empty the-queue)))
			   (:name empty :condition (empty the-queue)))))


(deftask enumerate-filter-accumulate
    :super-types (procedure)
    :parameters (input-type output-type enumerator-type filter-type accumulator-type)
    :primitive nil
    :interface ((:inputs (the-set input-type))
		(:outputs (the-accumuland output-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Do to each type things
;;;
;;; These are for when you enumerate a sequence and then affect each element
;;; with the result of a computation
;;;
;;; The enumerator is assumed to be producing a temporal sequence of places in some set
;;; 
;;; Parameters:
;;;  1) The set-type being enumerated (or alternatively a collection-type and element-type)
;;;  2) The type of the operation to be applied to each element
;;; 
;;; The operation itself receives these inputs:
;;;  1) A temporal sequence of places
;;;  2) An argument to the updating function which also gets a place from the temporal sequence
;;; 
;;; This has a 2 join input format similar to anything that is driver by an emuerator
;;;
;;; It has no outputs; this is for effect only
;;;
;;; At the moment this other input is a single value, but maybe that could be generalized
;;; somehow to a pseudo-tuple of values?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is always run for effect and has no output

(deftask updater
    :primitive nil
    :parameters (set-type element-type collection-type op-type)
    :bindings ((set-type (cond ((and set-type element-type) set-type)
			       ((and collection-type element-type) (list collection-type element-type))
			       (t set-type)))
	       (element-type (or element-type (second set-type)))
	       (place-type `(place ,set-type ,(index-type set-type))))
    :interface ((:joins (:name more :inputs ((the-places (temporal-sequence place-type)) 
					     (updater-arg element-type)))
			(:name empty)))
    )


;;; This is supposed to be the equivalent of enumerate accumulate except that it's for affecting each element
(deftask enumerate-filter-affect-each
    ;; Fix: This is open to question
    :bindings ((affector-arg-type (second input-type)))
    :parameters (input-type enumerator-type filter-type affector-type)
    :primitive nil
    :interface ((:inputs (the-set input-type)
			 (updater-arg affector-arg-type))
		(:outputs (the-affected-thing input-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftask print
    :primitive t
    :parameters (input-type)
    :interface ((:inputs (the-data input-type))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Viewpoints
;;; An Array can be viewed as a vector of columns
;;;  The construction is (column-major-viewpoint array) to which element-of can be applied to get a column-vector
;;;  The element type of the column-vector is the same as the element-type of the array
;;;  The type of the column-vector is (vector element-type) where element-type is the sub-type of the array
;;;  The operation element-of on the column-vector is defined as (element-of (column array j) i) = (element-of array i j)
;;;  These follow from the viewpoint construction given that these constraints apply to vectors
;;;  Constraints: The index j is between 0 and the x-dim of the array
;;;               The index i is between 0 and the y-dim of the array
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defviewpoint column-view
    :operator column
    :super-types (vector)
    :source (array ?element-type 2)
    :destination (vector (column ?element-type))
    :equivalences (
		   ((element-type (column-view (array ?element-type 2)))  (column ?element-type))
		   ((length (column-view ?array)) (array-dimension ?array 1))
		   ((element (column-view ?array)?index) (column ?array ?index))
		   ((length (element (column-view ?array) ?index)) (array-dimension ?array 2))
		   ((length (column ?array ?index)) (array-dimension ?array 2))
		   ((element (element (column-view ?array) ?i) ?j) (aref ?array ?i ?j))
		   ((assign (element (element (column-view ?array) ?i) ?j) ?k)
		    (setf (aref ?array ?i ?j) ?k))
		   ((element (column ?array ?i) ?j) (aref ?array ?i ?j))
		   ((assign (element (column ?array ?i) ?j) ?k)
		    (setf (aref ?array ?i ?j) ?k))
		   )
    :terms ((column-view generate traverse column_first))
    )

(defmethod compound-index-type ((thing (eql 'column)) args) (declare (ignore args)) 'integer)
(defmethod compound-index-type ((thing (eql 'column-view)) args) (declare (ignore args)) 'integer)
(defmethod compound-index-type ((thing (eql 'row-view)) args) (declare (ignore args)) 'integer)
(defmethod compound-index-type ((thing (eql 'row)) args) (declare (ignore args)) 'integer)

(defviewpoint row-view
    :operator row
    :super-types (vector)
    :source (array ?element-type 2)
    :destination (vector (row ?element-type))
    :equivalences (
		   ((element-type (row-view (array ?element-type 2)))  (row ?element-type))
		   ((length (row-view ?array)) (array-dimension ?array 1))
		   ((element (row-view ?array) ?index) (row ?array ?index))
		   ((length (element (row-view ?array) ?index)) (array-dimension ?array 2))
		   ((length (row ?array ?index)) (array-dimension ?array 2))
		   ((element (element (row-view ?array) ?i) ?j) (aref ?array ?i ?j))
		   ((assign (element (element (row-view ?array) ?i) ?j) ?k)
		    (setf (aref ?array ?i ?j) ?k))
		   ((element (row ?array ?i) ?j) (aref ?array ?i ?j))
		   ((assign (element (row ?array ?i) ?j) ?k)
		    (setf (aref ?array ?i ?j) ?k))
		   )
    :terms ((row-view generate traverse row_first))
    )




(defviewpoint low-byte-first-view
    :source pixel
    :destination (vector byte)
    :equivalences (
		   ((element-type (low-byte-first-view ?pixel)) byte)
		   ((element (low-byte-first-view ?pixel) ?index) (ldb (byte 8 (* 8 ?index)) ?pixel))
		   ((length (low-byte-first-view ?pixel)) 4)
		   ((element-type (low-byte-first-view ?pixel)) byte)
		   )
    :terms ((low-byte-first-view generate traverse low_byte_first))
    )

(defviewpoint high-byte-first-view
    :source pixel
    :destination (vector byte)
    :equivalences (
		   ((element-type (high-byte-first-view ?pixel)) byte)
		   ((element (high-byte-first-view ?pixel) ?index) (ldb (byte 8 (* 8 ?index)) ?pixel))
		   ((length (high-byte-first-view ?pixel)) 4)
		   ((element-type (high-byte-first-view ?pixel)) byte)
		   )
    :terms ((high-byte-first-view generate traverse high_byte_first))
    )

(deftask jdd 
    :primitive nil
    :interface ((:inputs (the-node json-node))
		(:outputs)))

(deftask sorted-list-test
    :parameters ()
    :interface ((:inputs (the-list (list integer)))
		(:outputs (the-value boolean))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numerical Operations
;;;   output port names
;;;   default initial-values for accumulation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask add
    :parameters (numeric-type)
    :Primitive t
    :interface ((:inputs (i-1 numeric-type) (i-2 numeric-type))
		(:outputs (sum numeric-type))))

(defmethod output-port-for-numeric-op ((op (eql 'add))) 'sum)
(defmethod default-initial-value-for-acc ((op (eql 'add))) 0)

(deftask subtract
    :parameters (numeric-type)
    :Primitive t
    :interface ((:inputs (i-1 numeric-type) (i-2 numeric-type))
		(:outputs (difference numeric-type))))

(defmethod output-port-for-numeric-op ((op (eql 'subtract))) 'difference)

(deftask multiply
    :parameters (numeric-type)
    :Primitive t
    :interface ((:inputs (i-1 numeric-type) (i-2 numeric-type))
		(:outputs (product numeric-type))))

(defmethod output-port-for-numeric-op ((op (eql 'multiply))) 'product)
(defmethod default-initial-value-for-acc ((op (eql 'multiply))) 1)

(deftask divide
    :parameters (numeric-type)
    :Primitive t
    :interface ((:inputs (i-1 numeric-type) (i-2 number))
		(:outputs (quotient numeric-type))))

(defmethod output-port-for-numeric-op ((op (eql 'divide))) 'quotient)

(deftask count
    :parameters () 
    :Primitive t 
    :interface ((:inputs (i-1 data-structure) (i-2 integer))
		(:outputs (count integer))))

(defmethod output-port-for-numeric-op ((op (eql 'count))) 'count)
(defmethod default-initial-value-for-acc ((op (eql 'count))) 0)

;;; probably should add min and max


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Places
;;;
;;; A "place' is an abstraction of the notion of a "reference" or a "pointer"
;;; Often it's useful to think of something generating a sequence of places
;;; rather than a sequence of values. 
;;; The values can then be extracted from the place
;;; Also the places can be assigned to, modifying the contents.
;;; A typical example is an array-slot or a position in a list
;;; 
;;; There are two abstract operations on places: Fetch and Assign
;;; for arrays, Fetch would refine to aref (in lisp)
;;;             Assign would refine to (assignf (aref ... ) ...)
;;;             in other languages these would be left and right-side usages
;;;
;;; Places are parameterized by the data type of the values held in the place
;;; and by the data-type of the containing object
;;; So unlike C, there are no raw pointers, I guess
;;; Also this second parameter would be used to guide refinement to a concrete
;;; operation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdata-type place
    :super-types (data-structure)
    :parameters (containing-object-type offset-data-type)
    :bindings ((element-data-type (second containing-object-type)))
    :parts ((container containing-object-type)
	    (offset offset-data-type))
    )

(deftask fetch
    :primitive t
    :parameters (containing-object-type index-type)
    :bindings ((element-data-type (second containing-object-type)))
    :interface ((:Inputs (the-place (place containing-object-type index-type)))
		(:Outputs (the-value element-data-type)))
    )

;;; Fix: the generation stage for this needs work if it gets an abstract
;;; object like (column x 1)
(deftask fetch-object
    :primitive t
    :parameters (containing-object-type index-type)
    :bindings ((element-data-type (first containing-object-type)))
    :interface ((:Inputs (the-place (place containing-object-type index-type)))
		(:Outputs (the-object containing-object-type)))
    )


(deftask fetch-index
    :primitive t
    :parameters (containing-object-type index-type)
    :bindings ((element-data-type (second containing-object-type)))
    :interface ((:Inputs (the-place (place containing-object-type index-type)))
		(:Outputs (the-index index-type)))
    )

(deftask assign
    :primitive t
    :parameters (containing-object-type index-type)
    :bindings ((element-data-type (second containing-object-type)))
    :interface ((:inputs (the-place (place containing-object-type index-type))
			 (new-value element-data-type))
		(:outputs)))


(deftask place-constructor
    :primitive t
    :parameters (set-type element-type collection-type (for-value t) output-type index-type)
    :bindings ((set-type (cond ((and set-type element-type) set-type)
			       ((and collection-type element-type) (list collection-type element-type))
			       (t set-type)))
	       (element-type (or element-type (second set-type)))
	       (index-type (index-type set-type))
	       (output-type (if for-value element-type `(place ,set-type ,index-type)))
	       )
    :interface ((:inputs (container set-type)
			 (offset index-type))
		(:outputs (the-place (place set-type index-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Accumulators
;;;
;;; there are variations on this theme.  For example
;;; does it take a manifested set (i.e. a data structure) as input
;;; or a temporal sequence
;;; Does it do extraction of the elements of the set, etc.
;;; Given this I just have two at the moment:
;;; 1) a simple numerical one that takes a temporal sequence of numbers (should extend with extractor)
;;; and reduces the number with some numerical op and initial value
;;; 2) A set accumulator that takes a data structure does extraction and produces a new set
;;; to do: make a basic accumulator and then sub-types that are set-accumulator and numerical-accumulator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask numerical-accumulator
    :super-types (procedure)
    :primitive nil
    :parameters (numeric-type op initial-value)
    :bindings ((real-input-type (if (eql op 'count) 'data-structure numeric-type))
	       (real-output-type (if (eql op 'count) 'integer numeric-type))
	       (initial-value (or initial-value (default-initial-value-for-acc op))))
    :interface ((:joins (:name more :inputs ((the-sequence (temporal-sequence real-input-type))))
			(:name empty))
		(:outputs (accumuland real-output-type))))

;;; This guy takes a manifested set (i.e. not a temporal sequence)
;;; and accumulates an output value
(deftask set-accumulator
    :super-types (procedure)
    :primitive nil
    :parameters (input-type output-type op initial-value)
    :bindings (;; (real-input-type (or (definition-for input-type) input-type))
	       ;; (real-output-type (or (definition-for output-type) output-type))
	       (initial-value (or initial-value (default-initial-value-for-acc op))))
    :interface ((:inputs (the-sequence input-type))
		(:outputs (accumuland output-type))))

(deftask array-acc
    :super-types ()
    :parameters (input-type output-type)
    :interface ((:inputs (the-set input-type))
		(:outputs (the-accumuland output-type)))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A task that normalizes each element of a set
;;;
;;; Fix: There are things above that purport to be this but they aren't
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

