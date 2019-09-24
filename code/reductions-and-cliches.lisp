;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The actual planning and cliche knowledge
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; input-type is (container-type input-element-type)
;;; There is an overall pattern that we should follow:
;;;  GENERATORS take objects as inputs, and produce sequences as outputs
;;;   So if two generators occur in a row, they have to be separated by a TAKE
;;;  ACCUMULATORS take sequences as inputs and produce streams as outputs

;;; this says find a plan for the viewpoint
;;; This uses the full capability of the defreduction macro, calling a subplan and accumulating choices made
;;; The original input and output types are what the query had before it was canonicalized (definitions replacing terms as in image -> (array pixel))

(defreduction view-as-sequence-generator (generator (?input-type 
						     (sequence ?output-type)
						     ?original-input-type 
						     ?original-output-type 
						     ?input-name 
						     ?output-name))
  :the-instance ?the-generator
  :the-plan ?the-plan
  :prerequisites ((not (is-of-type? ?input-type 'temporal-sequence))
		  (is-of-type? ?output-type 'primitives)
		  [can-be-viewed-as ?input-type ?view-type ?viewpoint]
		  (is-of-type? ?view-type 'sequence))
  :subplans ((subplan :type generator
		      :instance ?the-generator
		      :parameters ((?viewpoint ?input-type)
				   (temporal-sequence ?output-type)
				   ?original-input-type ?original-output-type 
				   ?input-name ?output-name)
		      :previous-choices ((viewpoint ?viewpoint) . ?choices-so-far)
		      :new-choices ?choices
		      :sub-plan ?the-sub-plan
		      ))
  :reduce-to ?the-sub-plan
  :previous-choices ?choices-so-far
  :new-choices ((viewwpoint ? viewpoint) . ?choices)
  :actions ((tell [viewpoint-applied ?the-generator ?input-name ?viewpoint generate ?original-output-type ?original-input-type]))
  )

;;; This is a version that deals with enumerators.  I don't think there's any reason to have both of these but I need to go through 
;;; and get rid of all versions of the old generator
;;; The canonical arguments are
;;; 1) The canonicalized input-type 
;;; 2) The canonicalized output-type
;;; 3) The input type as provided before canonicalization
;;; 4) The output type as provided before canonicalization
;;; 5) The input's name
;;; 6) The output's name
;;; 7) The canonicalized key type of the enumerator

(defreduction view-as-sequence (enumerator (?input-type 
					    ?output-type
					    ?original-input-type 
					    ?original-output-type 
					    ?input-name 
					    ?output-name
					    ?key-type
					    ?for-value
					    ?for-effect))
  :the-instance ?the-enumerator
  :the-plan ?the-plan
  :prerequisites ((not (is-of-type? ?input-type 'temporal-sequence))
		  ;;why does the output have to be a primitive?
		  ;;You could certainly have an array of non-primitive data-structuers
		  ;; and want to enumeratea a stream of them.
		  ;;(is-of-type? ?output-type 'primitives)
		  [can-be-viewed-as ?input-type ?view-type ?viewpoint]
		  (is-of-type? ?view-type 'sequence)
		  )
  :subplans ((subplan :type enumerator
		      :instance ?the-enumerator
		      :parameters ((?viewpoint ?input-type)
				   ?output-type
				   ?original-input-type ?original-output-type 
				   ?input-name ?output-name ?key-type ?for-value ?for-effect)
		      :previous-choices ((viewpoint ?viewpoint) . ?choices-so-far)
		      :new-choices ?choices
		      :sub-plan ?the-sub-plan
		      ))
  :reduce-to ?the-sub-plan
  :previous-choices ?choices-so-far
  :new-choices ?choices
  :actions ((tell [viewpoint-applied ?the-enumerator ?input-name ?viewpoint generate ?original-output-type ?original-input-type]))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here Orginal-<input/output>-type is what is expressed in the design before cononicalization
;;; while <input/output>-container-type is what it might be canonicalized to
;;; for example (sequence image) vs. (sequence (array pixel))
;;; where array is the container-type and pixel is the element-type
;;; This is taking 2 jumps at once.
;;; First is generate accumulate: e.g. generator of stream of element-type plus accumlate element-type
;;; Second is implementation of the generator

;;; NOTE: Idea is to split this into two steps
;;;  First is to a transducer and an collector
;;;  Second is To reduce that transducr to a take and generate.
;;;  Caveat: make sure that we don't come back here for the 2nd refinement.
(defreduction reduce-to-element (transducer ((stream ?input-type)
					     (stream ?output-type)
					     ?original-input-type ?original-output-type
					     ?input-name ?output-name))
  :reduce-to (generate-and-collect  ?original-input-type ?input-name
				    ?original-output-type ?output-name
				    ?input-type ?output-type
				    )
  ;; :bindings ((?input-container-type (first ?input-type)))
  :prerequisites ((is-of-type? (first ?input-type) 'container)
		  (is-of-type? (first ?output-type) 'container)
		  ;; we'll get viewpoints other ways
		  (not (is-a-viewpoint-operator? (first ?input-type))))
  )


(defreduction take-and-generate (branching-transducer ((stream ?input-type)
						       (temporal-sequence ?output-element-type)
						       ?original-input-type ?original-output-type
						       ?input-name ?output-name))
  :reduce-to (take-and-enumerate-elements 'stream ?original-input-type ?input-type ?input-name
					 ?output-element-type ?output-name
					 )
  :prerequisites (;; we'll get viewpoints other ways
		  ;; (not (is-viewpoint? (list ?input-container-type ?input-element-type)))
		  )
  )
 
;;; Notice that this takes a vector
;;; and generates its elements
;;; replaced generator with enumerator
;;; The output-type parameter is the element-type not (temporal-sequence element-type)
(defreduction vector-to-elements (enumerator (?input-type ?output-type
							  ?original-input-type ?original-output-type 
							  ?input-name ?output-name
							  ?key-type
							  ?for-value 
							  ?for-effect))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name ?key-type ?for-value ?for-effect)
  ;; I'm doing this so that I can use subtypes of vectors (e.g. columns)
  ;; rather than pattern matching on the :then pattern for (vector ?output-type)
  ;; which would only work if it was explictly a vector
  ;; This makes viewing an array as a vector of vectors work!
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?input-type element-type ?sub-type]
		  (is-of-type? ?sub-type ?output-type)
		  )
  )

;;; This one is used if the the element-type of the input set isn't the output-type
;;; So it enumerates the element types and then tries to use another enumerator to get 
;;; from that type to the output-type
(defreduction vector-to-elements-2 (enumerator (?input-type ?output-type
							    ?original-input-type ?original-output-type
							    ?input-name ?output-name
							    ?key-type
							    ?for-value 
							    ?for-effect))
  :reduce-to (double-enumerate ?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type ?for-value ?for-effect)
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?input-type element-type ?intermediate-type]
		  [not [unify ?intermediate-type ?output-type]]
		  )
  )

;;; This is for enumerators that generate keys as well as values
;;; Nuked generator
(defreduction vector-to-elements-3 (enumerator-with-keys (?input-type ?output-type ?original-input-type ?original-output-type 
								      ?input-name ?output-name ?key-type ?for-value ?for-effect))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name 'the-keys ?for-value ?for-effect)
  :the-instance ?the-enumerator
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?the-enumerator element-type ?sub-type]
		  (is-of-type? ?sub-type ?output-type)
		  )
  )

(defreduction implement-collector (collector (?input-type (stream ?output-type)
							      ?original-input-type 
							      (stream ?original-output-type)
							      ?input-name ?output-name))
  :reduce-to (buffer-and-ship ?input-type ?original-output-type ?input-name ?output-name )
  :the-instance ?the-collector
  :prerequisites ((is-of-type? ?output-type 'vector)
		  [property-value-of ?output-type element-type ?element-type]
		  (is-of-type? ?input-type ?element-type)
		  )
  )

;;; Original-input-type: type of the initial input
;;; Original-output-type: external type of the output
;;; Output-container-type: internal container-type of the output
;;; Output-element-type: internal element-type of the output
;;; E.g. (stream image), foo, (stream disk-buffer), vector, byte
(defcliche generate-and-collect (?original-input-type ?input-name ?original-output-type ?output-name  
						      ?input-type ?output-type)
  :bindings ((?output-container-type (first ?output-type))
	     (?output-element-type (second ?output-type)))
  :initial-inputs (;; this is the container of input stuff
		   (:name ?input-name :type ?original-input-type)
		   ;; this is the output queue
		   (:name ?output-name :type (stream (?output-container-type ?output-element-type)))
		   )
  :final-outputs ((:name chunks :port ?output-name :type ?original-output-type))
  :components ((:name tr :type branching-transducer :input-type ?original-input-type :output-type (temporal-sequence ?output-element-type))
	       (:name acc-1 :type collector :input-type ?output-element-type :input-container-type temporal-sequence :output-type ?original-output-type))
  :dataflows (((:component ?input-name :port ?input-name ) (:component tr :port input-queue))
	      ((:component tr :branch more :port new-data) (:component acc-1 :branch more :port new-data))
	      ((:component ?output-name :port ?output-name) (:component acc-1 :branch more :port queue))
	      ((:component ?output-name :port ?output-name) (:component acc-1 :branch empty :port queue))
	      ((:component acc-1 :port chunks) (:component chunks :port ?output-name))
	      )
  :control-flows (((:component tr :branch empty) (:component acc-1 :branch empty)))
  )

;;; Sequence-type: The input-container-type of the take
;;; Original-input-type: Full type description of the input
;;; Input-container-type: Container-type of the elements of the input to the take
;;; Input-element-type:   Element-type   of the elements of input to the take
;;; Output-element-type: Type of whatever comes out
;;; E.g. stream, (stream image), array, pixel, foo pixel, bar
;;; original-input-type is subtype of (sequence-type (input-container-type input-element-type))
(defcliche take-and-enumerate-elements (?sequence-type ?original-input-type
					 ?input-type ?input-name
					 ?output-element-type ?output-name
					 )
  ;; :bindings ((?input-container-type (first ?input-type)) (?input-element-type (second ?input-type)))
  :initial-inputs ((:name ?input-name :type ?original-input-type))
  :components ((:name t-1 :type take :element-type ?input-type :input-container-type ?sequence-type)
	       (:name sequence-done-test :type queue-empty-test)
	       (:name gen :type enumerator :set-type ?input-type 
		      :output-type ?output-element-type))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-element-type))))
		(:name empty))
  :dataflows (((:component ?input-name :port ?input-name) (:component sequence-done-test :port the-queue))
	      ((:component ?input-name :port ?input-name) (:component t-1 :port sequence-data))			  
	      ((:component t-1 :port data) (:component gen :port the-set))
	      ((:component gen :branch more :port the-elements) (:component more :port ?output-name)))
  :control-flows (((:component sequence-done-test :branch not-empty) (:component t-1))
		  ((:component sequence-done-test :branch empty) (:component empty))
		  )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerating the values in an array/vector
;;; Enumerating the "places" in a array/vector
;;;
;;; With for-value = t generates the elements of a vector
;;; With for-value = nil
;;; This geneates the places in a array or vector
;;; rather than the values
;;; The places can be fetched from or assigned to
;;;
;;; This gives a nice compositional account of composing an index geneator
;;  with a place maker to build a place generator
;;; and composing that with a place fetch to make a values enumerator
;;; This is now done by the enumerate-vector-elements cliche with the for-value argument = nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; With ?for-value = t generates the elements of the vector
;;; With ?for-value = nil generates the places of the vector
(defcliche enumerate-vector-elements (?input-type ?output-type ?input-name ?output-name ?key-type ?for-value ?finally)
  :bindings ((?place-type `(place ,?input-type integer))
	     (?real-output-type (if ?for-value ?output-type ?place-type)))
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name s-1 :port-name old-stream :port-type (temporal-sequence ?real-output-type) 
                               :state old-stream :locality :ignore)
                   )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?real-output-type))
                                    ))
                (:name empty)
		)
  :constants ((:name lower :type integer :value 0))
  :components ((:name initial-s1 :type allocate :object-type (temporal-sequence ?real-output-type))
               (:name length :type vector-length :vector-type ?input-type)
               (:name make-range :type range-constructor)
               (:name enum :type index-enumerator :finally-option ?finally)
               (:name t-1 :type Take :element-type integer :input-container-type temporal-sequence)
	       (when (not ?for-value) (:name place-maker :type place-constructor :set-type ?input-type :index-type integer))
               (when ?for-value (:name va :type vector-accessor :vector-type ?input-type :element-type ?real-output-type))
               (:name p-1 :type put :element-type ?real-output-type :output-container-type temporal-sequence)
               )
  :dataflows (((:component lower :port lower)  (:component make-range :port lower-bound))
              ((:component LENGTH :port LENGTH) (:component make-range :port UPPER-BOUND))
              ((:component ?input-name :port ?input-name) (:component length :port vector))
              ((:component make-range :port the-range) (:component ENUM :port the-set))
              ((:component enum :branch more :port indices) (:component t-1 :port sequence-data))
              (when ?for-value ((:component ?input-name :port ?input-name) (:component va :port vector)))
              (when ?for-value ((:component t-1 :port data) (:component va :port index)))
	      (when ?for-value ((:component va :port the-element) (:component p-1 :port data)))
	      (when (not ?for-value) ((:component ?input-name :port ?input-name) (:component place-maker :port container)))
	      (when (not ?for-value) ((:component t-1 :port data) (:component place-maker :port offset)))
              (when (not ?for-value) ((:component place-maker :port the-place) (:component p-1 :port data)))
              ((:component initial-s1 :port new-object) (:component s-1 :port initial-old-stream))
              ((:component s-1 :port old-stream) (:component p-1 :port sequence))
              ((:component p-1 :port sequence-data) (:component more :port ?output-name))
              )
  :control-flows (((:component enum :branch empty) (:component empty)))
  )

(defcliche double-enumerate (?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type ?for-value ?for-effect)
  :bindings ((?real-output-type (if ?for-value output-type `(place ,?intermediate-type ,(index-type ?intermediate-type)))))
  :initial-inputs ((:name ?input-name :type ?input-type))
  :components ((:name enum-1 :type enumerator :set-type ?input-type :element-type ?intermediate-type :for-value t :finally-option ?for-effect)
               (:name t-1 :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
               (:name enum-2 :type enumerator :set-type ?intermediate-type :element-type ?output-type :key-type ?key-type :for-value ?for-value :finally-option ?for-effect)
	       )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?real-output-type))
				    (when ?key-type (:name the-keys :type (temporal-sequence ?key-type)))
				    (when ?key-type (:name the-set :type (temporal-sequence ?intermediate-type)))))
                (:name empty)
		)
  :dataflows (((:component ?input-name :port ?input-name) (:component enum-1 :port the-set))
              ((:component enum-1 :branch more :port the-elements) (:component t-1 :port sequence-data))
              ((:component t-1 :port data) (:component enum-2 :port the-set))
              ((:component enum-2 :branch more :port the-elements) (:component more :port ?output-name))
	      (when ?key-type ((:component enum-2 :branch more :port the-keys) (:component more :port the-keys)))
	      (when ?key-type ((:component enum-2 :branch more :port the-set) (:component more :port the-set)))
	      )
  :control-flows (((:component enum-1 :branch empty) (:component empty)))
  )

;;; Input-type: The element-type of the stream of stuff coming in
;;; Output-type: The container-type into which the input elements are accumulated
;;; E.g. Byte, Disk-buffer, foo, bar
(defcliche buffer-and-ship (?input-type ?output-type ?input-name ?output-name)
  ;; First the initialization of the local state
  :state-elements ((:direction source :name s-1 :port-name buffer :port-type ?output-type :state buffer)
		   (:direction sink :name s-2 :port-name buffer :port-type ?output-type :state buffer))
  ;; Next we have two ways to enter this routine:
  ;; 1) Normal path with a data input and a queue: More
  ;; 2) When we've run out of inputs and a queue:  Empty
  ;; Here's the normal pathway
  :entry-points ((:name more :ports ((:name ?input-name :type (temporal-sequence ?input-type)) (:name ?output-name :type (stream ?output-type))))
		 (:name empty :ports ((:name ?output-name :type (stream ?output-type)))))
  :components (;; The stuff left pathway
	       (:name initial-buffer :type allocate :object-type ?output-type :size *disk-buffer-size*)
	       (:name t-1 :type take :element-type ?input-type :input-container-type temporal-sequence)
	       (:name vpush :type vector-push  :vector-type ?output-type :element-type ?input-type)
	       (:name full? :type vector-full-test :vector-type (vector ?input-type))
	       (:name p-1 :type put :element-type ?output-type :output-container-type stream)
	       (:name new-buffer :type allocate :object-type ?output-type :local t :size *disk-buffer-size*)
	       ;; Now for the empty pathway
	       (:name empty? :type vector-empty-test :vector-type (vector ?input-type))
	       (:name p-2 :type put :element-type ?output-type :output-container-type stream)
	       (:name sj :type simple-join :object-type (stream disk-buffer) :first-join-name normal :second-join-name empty))
  :final-outputs ((:name ?output-name :port chunks :type (stream ?output-type)))
  :dataflows (((:component initial-buffer :port new-object) (:component s-1 :port initial-buffer))
	      ((:component more :port ?input-name) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component vpush :port new-data))
	      ((:component s-1 :port buffer) (:component vpush :port vector))
	      ((:component vpush :port updated-vector) (:component full? :port the-vector))
	      ((:component new-buffer :port new-object) (:component s-2 :port buffer))
	      ((:component vpush :port updated-vector) (:component p-1 :port data))
	      ((:component more :port ?output-name) (:component p-1 :port sequence))
	      ((:component s-1 :port buffer) (:component empty? :port the-vector))
	      ((:component s-1 :port buffer) (:component p-2 :port data))
	      ((:component empty :port ?output-name) (:component p-2 :port sequence))
	      ((:component p-1 :port sequence-data) (:component sj :branch normal :port the-object))
	      ((:component p-2 :port sequence-data) (:component sj :branch empty :port the-object))
	      ((:component sj :port the-object) (:component ?output-name :port chunks))
	      )
  :control-flows (((:component full? :branch full) (:component p-1))
		  ((:component p-1) (:component new-buffer))
		  ((:component empty) (:component empty?))
		  ((:component empty? :branch has-stuff) (:component p-2))
		  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defreduction dd-as-list (detect-duplicates (?input-type ?output-type ?key-extractor-type))
  :reduce-to (detect-duplicates ?input-type ?output-type 'list ?key-extractor-type)
  :the-instance ?the-detector
  :prerequisites ()
  )

(defcliche detect-duplicates (?input-type ?output-type ?seen-set-type ?key-extractor-type)
  :bindings ((?real-input-type (or (definition-for ?input-type) ?input-type))
	     (?element-type (second ?real-input-type))
	     (?real-output-type (or (definition-for ?output-type) ?output-type))
	     (?output-set-type (first ?real-output-type))
	     (?key-type (second ?real-output-type)))
  :initial-inputs (
                   (:name THE-SEQUENCE :type ?input-type)
                   )
  :final-outputs (
                  (:name ACCUMULAND :type (?output-set-type ?key-type))
                  )
  :state-elements (
                   (:name EXISTING-DUPS :direction source :port-name EXISTING-DUPS :port-type (?output-set-type ?key-TYPE) :state dups 
			  :locality :local)
                   (:name EXISTING-SEEN :direction source :port-name EXISTING-SEEN :port-type (?seen-set-type ?key-TYPE) :state seen 
			  :locality :local)
		   (:name UPDATED-DUPS :direction sink :port-name UPDATED-DUPS :port-type (?output-set-type ?key-TYPE) :state dups)
		   (:name UPDATED-SEEN :direction sink :port-name UPDATED-SEEN :port-type (?seen-set-type ?key-TYPE) :state seen)
                   )
  :components ((:name enum-elements :type list-enumerator :element-type ?element-type) 
	       (:name TAKE-ONE :type TAKE :INPUT-CONTAINER-TYPE temporal-sequence :ELEMENT-TYPE ?element-type)
	       (:name key-extractor :type ?key-extractor-type :input-type ?element-type)
	       (:name initial-seen :type allocate :object-type (?seen-set-type ?key-type))
	       (:name initial-dups :type allocate :object-type (?output-set-type ?key-type))
               (:name ADD-TO-SEEN :type ADD-TO-SET :SET-TYPE ?seen-set-type :ELEMENT-TYPE ?key-TYPE :unique? nil)
               (:name SEEN? :type MEMBER-TEST :SET-TYPE ?seen-set-type :ELEMENT-TYPE ?key-TYPE)
               (:name ADD-TO-DUPS :type ADD-TO-SET :SET-TYPE ?output-set-type :ELEMENT-TYPE ?key-TYPE :unique? t)
               )
  :dataflows (
	      ((:component initial-seen :port new-object) (:component existing-seen :port initial-existing-seen))
	      ((:component initial-dups :port new-object) (:component existing-dups :port initial-existing-dups))
              ((:component EXISTING-DUPS :port EXISTING-DUPS) (:component ADD-TO-DUPS :port THE-SET))
	      ((:component TAKE-ONE :port data) (:component key-extractor :port the-container))
	      ((:component key-extractor :port the-part) (:component ADD-TO-DUPS :port THE-ELEMENT))
              ((:component EXISTING-SEEN :port EXISTING-SEEN) (:component SEEN? :port THE-SET))
              ((:component EXISTING-SEEN :port EXISTING-SEEN) (:component ADD-TO-SEEN :port THE-SET))
	      ((:component key-extractor :port the-part) (:component ADD-TO-SEEN :port THE-ELEMENT))
	      ((:component ADD-TO-DUPS :port THE-SET) (:component updated-DUPS :port updated-dups))
	      ((:component existing-DUPS :port existing-dups) (:component ACCUMULAND :port ACCUMULAND))
	      ((:component ADD-TO-SEEN :port THE-SET) (:component updated-SEEN :port updated-seen))
              ((:component key-extractor :port the-part) (:component SEEN? :port THE-ELEMENT))
              ((:component THE-SEQUENCE :port THE-SEQUENCE) (:component enum-elements :port the-set))
	      ((:component enum-elements :port list-elements :branch more) (:component take-one :port sequence-data))
              )
  :control-flows (((:component enum-elements :branch empty) (:component accumuland))
                  ((:component SEEN? :branch NO) (:component ADD-TO-SEEN))
                  ((:component SEEN? :branch YES) (:component ADD-TO-DUPS))
                  )
  )

(defreduction tree-traverse-reduce (tree-traverse (?node-type ?recursive-call-name ?key-type))
  :reduce-to (tree-traverse ?node-type (non-terminal-of-tree-type ?node-type) (terminal-of-tree-type ?node-type) ?recursive-call-name ?key-type)
  :the-instance ?the-tree
)

;;; This should maybe take parameters for the input and output names
(defcliche tree-traverse (?Node-type ?non-terminal-node-type ?terminal-node-type ?recursive-call-name ?key-type)
  :initial-inputs ((:name the-set :type ?node-type))
  :state-elements (
		   (:direction source :name node-sequence :port-name node-sequence :port-type (temporal-sequence ?node-type) 
			       :state the-sequence
			       :locality :ignore)
		   )
  :exit-points ((:name more :ports ((:name the-elements :type (temporal-sequence ?node-type))))
		(:Name empty))
  ;; :final-outputs ((:name the-sequence :type (temporal-sequence ?node-type)))
  :components ((:name new-temporal-sequence :type allocate :object-type (temporal-sequence ?node-type))
	       (:name add-to-sequence :type put :element-type ?node-type :output-container-type temporal-sequence)
               (:name non-terminal? :type type-split :input-type ?node-type :output-type ?non-terminal-node-type :non-matching-output-type ?terminal-node-type)
               (:name enumerate-nodes :type enumerator-with-keys :set-type ?non-terminal-node-type :element-type ?node-type :key-type ?key-type)
               (:name get-node :type take :input-container-type temporal-sequence :element-type ?node-type)
               (:name ?recursive-call-name :type tree-traverse :set-type ?node-type :element-type ?node-type :key-type ?key-type
		      :dont-expand t :recursive-call-name ?recursive-call-name)
               ;; (:name join :type simple-join :object-type (temporal-sequence ?node-type) :first-join-name non-primitive :second-join-name terminal)
	       ;; (:name exit-join :type control-JOIN :FIRST-JOIN-NAME enumerate-nodes :second-join-name traverse)
               )
  :dataflows (((:component new-temporal-sequence :port new-object) (:component node-sequence :port initial-node-sequence))
	      ((:component node-sequence :port node-sequence) (:component add-to-sequence :port sequence))
              ((:component the-set :port the-set) (:component add-to-sequence :port data))
              ((:component the-set :port the-set) (:component non-terminal? :port test-data))
              ((:component non-terminal? :port matching-data :branch match) (:component enumerate-nodes :port the-set))
              ((:component enumerate-nodes :port the-elements :branch more) (:component get-node :port sequence-data))
              ((:component get-node :port data) (:component ?recursive-call-name :port the-set))
              ;; ((:component traverse :port the-elements :branch more) (:component join :port the-object :branch non-primitive))
	      ((:component add-to-sequence :port sequence-data) (:component more :port the-elements))
              ;; ((:component add-to-sequence :port sequence-data) (:component join :port the-object :branch terminal))
              ;; ((:component join :port the-object) (:component more :port the-elements)
	      )              
  :control-flows (
		  ;; ((:component enumerate-nodes :branch empty) (:component exit-join :branch enumerate-nodes))
		  ;; this is to make acting on nodes happen before recursion
		  ;; but it wouldn't work in the new scheme and it shouldn't have worked
		  ;; anyhow.
		  ;;  ((:component add-to-sequence) (:component non-terminal?))
		  ;; ((:component ?recursive-call-name :branch empty) (:component exit-join :branch traverse))
		  ((:component enumerate-nodes :branch empty) (:component empty))
		  ;; ((:component exit-join) (:component empty)))
		  )
  )

(defreduction enumerate-subtypes (enumerator-with-keys (?input-node-type ?output-node-type ?original-input-type ?original-output-type ?input-name ?output-name 
							       ?key-type))
  :reduce-to (split-enumerate ?input-node-type ?first-sub-type ?second-sub-type ?output-node-type ?input-name ?output-name ?key-type)
  :the-instance ?the-tree
  :prerequisites ((let ((sub-types (union (union-members ?input-node-type) (sub-types ?input-node-type))))
		   (and (= (length sub-types) 2)
			(unify (first sub-types) ?first-sub-type)
			(unify (second sub-types) ?second-sub-type))))
		  )

(defcliche split-enumerate (?node-type ?first-sub-type ?second-sub-type ?output-node-type ?input-name ?output-name ?key-type)
  :initial-inputs ((:name ?input-name :type ?node-type))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-node-type))
				    (:name the-keys :type (temporal-sequence ?key-type))))
		(:name empty))
  ;; :final-outputs ((:name ?output-name :type (TEMPORAL-SEQUENCE ?output-node-type)))
  :components ((:name TYPE-SPLIT :type TYPE-SPLIT :input-TYPE ?node-type :output-TYPE ?first-sub-type :non-matching-output-type ?second-sub-type)
               (:name ENUM-1 :type ENUMERATOR-with-keys :SET-TYPE ?first-sub-type :ELEMENT-TYPE ?output-node-type :key-type ?key-type)
               (:name ENUM-2 :type ENUMERATOR-with-keys :SET-TYPE ?second-sub-type :ELEMENT-TYPE ?output-node-type :key-type ?key-type)
               (:name SJ :type SIMPLE-JOIN-2 :FIRST-JOIN-NAME ?first-sub-type :second-join-name ?second-sub-type 
		      :first-object-type (TEMPORAL-SEQUENCE ?output-node-type) :second-object-type (temporal-sequence ?key-type)
		      :corresponding-branch type-split)
	       (:name exit-join :type control-JOIN :FIRST-JOIN-NAME ?first-sub-type :second-join-name ?second-sub-type
		      :corresponding-branch type-split)
               )
  :control-flows (((:component enum-1 :branch empty) (:component exit-join :branch ?first-sub-type))
		  ((:component enum-2 :branch empty) (:component exit-join :branch ?second-sub-type))
		  ((:component exit-join) (:component empty)))
  :dataflows (
              ((:component ?input-name :port ?input-name) (:component TYPE-SPLIT :port test-data))
	      ((:component TYPE-SPLIT :port matching-data :branch match) (:component ENUM-1 :port THE-SET))
              ((:component TYPE-SPLIT :port non-matching-data :branch no-match) (:component ENUM-2 :port THE-SET))
              ((:component ENUM-1 :port THE-ELEMENTS :branch MORE) (:component SJ :port first-OBJECT :branch ?first-sub-type))
	      ((:component ENUM-1 :port the-keys :branch more) (:component sj :port second-object :branch ?first-sub-type))
              ((:component ENUM-2 :port THE-ELEMENTS :branch MORE) (:component SJ :port first-OBJECT :branch ?second-sub-type))
	      ((:component ENUM-2 :port the-keys :branch more) (:component sj :port second-object :branch ?second-sub-type))
              ((:component SJ :port first-OBJECT) (:component more :port ?output-name))
	      ((:component SJ :port second-OBJECT) (:component more :port the-keys))))

(defreduction list-of-struct-to-stuff (enumerator-with-keys (?input-type ?output-type
							       ?original-input-type ?original-output-type
							       ?input-name ?output-name
							       ?key-type))
  :reduce-to (enumerate-elements-then-extract ?input-type ?output-type ?intermediate-type ?input-name ?output-name ?local-key-type)
  :the-instance ?the-enumerator
  :prerequisites ((is-of-type? ?input-type 'list)
		  (unify (second ?input-type) ?intermediate-type)
		  [property-value-of ?the-enumerator element-type ?output-type]
		  [property-value-of ?the-enumerator key-type ?local-key-type]
		  [not [unify ?intermediate-type ?output-type]]
		  )
  )

(defcliche enumerate-elements-then-extract (?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type)
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name node-sequence :port-name node-sequence :port-type (temporal-sequence ?output-type) 
			       :state the-sequence :locality :ignore)
		   (:direction source :name key-sequence :port-name key-sequence :port-type (temporal-sequence ?key-type) 
			       :state the-key-sequence :locality :ignore))
  :components ((:name initial-node-sequence :type allocate :object-type (temporal-sequence ?output-type))
	       (:name initial-key-sequence :type allocate :object-type (temporal-sequence ?key-type))
	       (:name gen-1 :type list-enumerator :element-type ?intermediate-type)
	       (:name t-1 :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
	       (:name extract :type extractor :input-type ?intermediate-type :output-type ?output-type)
	       (:name key-extract :type extractor :input-type ?intermediate-type :output-type ?key-type)
	       (:name add-to-sequence :type put :element-type ?output-type :output-container-type temporal-sequence)
	       (:name add-to-key-sequence :type put :element-type ?key-type :output-container-type temporal-sequence)
	       )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))
				    (:name the-keys :type (temporal-sequence ?key-type))))
		(:name empty))
  :dataflows (((:component initial-node-sequence :port new-object) (:component node-sequence :port initial-node-sequence))
	      ((:component initial-key-sequence :port new-object) (:component key-sequence :port initial-key-sequence))
	      ((:component ?input-name :port ?input-name) (:component gen-1 :port the-set))
	      ((:component gen-1 :branch more :port list-elements) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component extract :port the-container))
	      ((:component t-1 :port data) (:component key-extract :port the-container))
	      ((:component extract :port the-part) (:component add-to-sequence :port data))
	      ((:component key-extract :port the-part) (:component add-to-key-sequence :port data))
	      ((:component node-sequence :Port node-sequence) (:component add-to-sequence :port sequence))
	      ((:component key-sequence :port key-sequence) (:component add-to-key-sequence :port sequence))
	      ((:component add-to-sequence :port sequence-data) (:component more :port ?output-name))
	      ((:component add-to-key-sequence :port sequence-data) (:component more :port the-keys)))
  :control-flows (((:component gen-1 :branch empty) (:component empty)))
  )

(defreduction extractor-reducer-left (extractor (?input-type ?output-type))
  :reduce-to (left-extractor ?input-type ?output-type)
  :the-instance ?the-extractor
  :prerequisites ((is-of-type? ?input-type 'json-pair)
		  (is-of-type? ?output-type 'json-key))
  )

(defcliche left-extractor (?input-type ?output-type)
  :initial-inputs ((:name the-container :type ?input-type))
  :final-outputs ((:name the-part :type ?output-type))
  :components ((:name left :type left :input-type ?input-type :output-type ?output-type))
  :dataflows (((:component the-container :port the-container) (:component left :port the-container))
	      ((:component left :port the-part) (:component the-part :port the-part))))

(defreduction extractor-reducer-right (extractor (?input-type ?output-type))
  :reduce-to (right-extractor ?input-type ?output-type)
  :the-instance ?the-extractor
  :prerequisites ((is-of-type? ?input-type 'json-pair)
		  (is-of-type? ?output-type 'json-node))
  )

(defcliche right-extractor (?input-type ?output-type)
  :initial-inputs ((:name the-container :type ?input-type))
  :final-outputs ((:name the-part :type ?output-type))
  :components ((:name right :type right :input-type ?input-type :output-type ?output-type))
  :dataflows (((:component the-container :port the-container) (:component right :port the-container))
	      ((:component right :port the-part) (:component the-part :port the-part))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerate (filter) Accumulate
;;;
;;;
;;; To make this a practical cliche' we need to standardize the interface to the pieces
;;; For the enumerator: We assume it follows an emuerator protocol
;;; For the filter: Similarly we assume that it follows a "filter protocol"
;;;  ideally the filter could be optional (maybe specified as identity)
;;; For the accumulator: Follows an accumulator protocol
;;; The passed in types are actually lists of the type together with a parameter list
;;; The relevant information is then extracted from these in the bindings.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If there's no filter reduce to enumerate-accumulate
(defreduction efa-reduce-to-enum-accum (enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type))
  :prerequisites ((null ?filter-type))
  :reduce-to (enumerate-accumulate ?input-type ?enumerator-type ?accumulator-type ?output-type))

(defun strip-out-properties (plist properties)
  (loop for (property value) on (rest plist) by #'cddr
      unless (member property properties)
      collect property 
      and collect value))

;;; need functions to produce the values
(defun process-enum-values (input-type enumerator-type output-type &optional (for-value t) (finally :for-effect))
  (let* ((enum-is-listp (listp enumerator-type))
	 (old-enum-properties (when enum-is-listp 
				(strip-out-properties enumerator-type '(:set-type :for-value :finally-options))))
	 (enum-set-type (or (and enum-is-listp (getf (rest enumerator-type) :set-type))  input-type))
	 (enum-element-type (or (and enum-is-listp (getf (rest enumerator-type) :element-type)) 
				(and (listp enum-set-type) (second enum-set-type))))
	 (enum-name (if enum-is-listp (first enumerator-type) enumerator-type))
	 (full-enum-type (list* enum-name :set-type enum-set-type :for-value for-value :finally-option finally old-enum-properties))
	 (enum-output-type (or enum-element-type output-type)))
    (values full-enum-type enum-output-type enum-element-type)))

;;; Fix: Shouldn't numeric type be element-type to be more general?
(defun process-accum-values (accumulator-type input-type)
  (let* ((accum-is-listp (listp accumulator-type))
	 (old-accum-properties (when accum-is-listp (strip-out-properties accumulator-type '(:numeric-type))))
	 (real-accum-name (if accum-is-listp (first accumulator-type) accumulator-type))
	 (enum-numeric-type (or (and accum-is-listp (getf (rest accumulator-type) :numeric-type)) 
				(and (listp input-type) (second input-type))
				input-type))
	 (real-accum-type (list* real-accum-name :numeric-type enum-numeric-type old-accum-properties)))
    (values real-accum-type )))

;;; The simple case of an enumerator composed with an accumulator
(defcliche enumerate-accumulate (?input-type ?enumerator-type ?accumulator-type ?output-type)
  :bindings (((?full-enum-type ?real-output-type) (process-enum-values ?input-type ?enumerator-type ?output-type t :for-effect))
	     (?real-accum-type (process-accum-values ?accumulator-type ?input-type))
	     )
  :initial-inputs ((:name the-set :type ?input-type))
  :final-outputs ((:name the-accumuland :type ?real-output-type))
  :components ((:name enumerator :type ?full-enum-type)
	       (:name accumulate :type ?real-accum-type :dont-expand nil)
	       )
  :control-flows (((:component enumerator :branch empty) (:component accumulate :branch empty)))
  :dataflows (
	      ((:component the-set :port the-set) (:component enumerator :port the-set))
	      ((:component enumerator :branch more :port the-elements) (:component accumulate :port the-sequence :branch more))
	      ((:component accumulate :port accumuland) (:component the-accumuland :port the-accumuland)))
  )

;;; Reduction and Cliche for when there is a filter between the enumerator and accumulator
(defreduction efa-reduce (enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type))
  :prerequisites ((not (null ?filter-type)))
  :reduce-to (enumerate-filter-accumulate ?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type))

;;; Fix: The branch doesn't propagate a value to its output.  But the interface to
;;; type-split says that the split produces a value on each branch.
;;; I think that's not actually the right model, branches just split control
;;; but things in the JDD world assume it does and build data flows.

(defun process-filter-values (filter-type enum-output-type)
  (let* ((filter-is-listp (listp filter-type))
	 (old-filter-properties (when filter-is-listp (strip-out-properties filter-type '(:input-type))))
	 (filter-input-type (or (and filter-is-listp (getf (rest filter-type) :input-type)) enum-output-type))
	 (filter-name (if filter-is-listp (first filter-type) filter-type))
	 (real-filter-type (list* filter-name :input-type filter-input-type old-filter-properties)))
    real-filter-type))	     

(defcliche enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type)
  :bindings (((?full-enum-type ?enum-output-type) (process-enum-values ?input-type ?enumerator-type ?output-type t :for-effect))
	     (?real-filter-type (process-filter-values ?filter-type ?enum-output-type))
	     (?real-output-type (or ?output-type ?enum-output-type))
	     (?real-accum-type (process-accum-values ?accumulator-type ?input-type)))
  :initial-inputs ((:name THE-SET :type ?input-type))
  :final-outputs ((:name THE-accumuland :type ?real-output-type))
  :components (
               (:name ALLOC :type ALLOCATE :OBJECT-TYPE (TEMPORAL-SEQUENCE ?enum-output-type))
               (:name ENUM :type ?full-enum-type)
               (:name DO-ONE :type TAKE :INPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE ?enum-output-type)
               (:name FILTER :type ?real-filter-type)
	       (:name RESTREAM :type PUT :OUTPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE ?enum-output-type)
               (:name ACCUM :type ?real-accum-type)
               )
  :dataflows (
	      ((:component THE-SET :port THE-SET) (:component ENUM :port THE-SET))
              ((:component ENUM :port THE-ELEMENTS :branch MORE) (:component DO-ONE :port SEQUENCE-DATA))
              ((:component ALLOC :port NEW-OBJECT) (:component RESTREAM :port SEQUENCE))
              ((:component DO-ONE :port DATA) (:component FILTER :port TEST-DATA))
              ((:component DO-ONE :port DATA) (:component RESTREAM :port DATA))
              ((:component RESTREAM :port SEQUENCE-DATA) (:component ACCUM :port THE-SEQUENCE ))
              ((:component ACCUM :port ACCUMULAND) (:component the-accumuland :port THE-accumuland))
              )
  :control-flows (
                  ;;((:component ENUM :branch EMPTY) (:component ACCUM :branch EMPTY))
                  ((:component FILTER :branch MATCH) (:component RESTREAM))
                  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerate and Affect Each
;;;  Takes a set of elements and a second input
;;;  Enumerates the elements and then
;;;  does some operation to each element
;;;  operates by effect
;;; but returns the input set as an output
;;;
;;; Could have a filter option like enumerate filter accumulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defreduction efae-reduce-to-enum-affect-each (enumerate-filter-affect-each (?input-type ?enumerator-type ?filter-type ?affector-type))
  :prerequisites ((null ?filter-type))
  :reduce-to (enumerate-affect-each ?input-type ?enumerator-type ?affector-type))

(defun process-affector-values (affector-type input-type)
  (let* ((affector-is-listp (listp affector-type))
	 (old-affector-properties (when affector-is-listp (strip-out-properties affector-type '(:element-type))))
	 (real-affector-name (if affector-is-listp (first affector-type) affector-type))
	 (real-affector-type (list* real-affector-name :set-type input-type  old-affector-properties)))
    (values real-affector-type)))

(defcliche enumerate-affect-each (?input-type ?enumerator-type ?affector-type)
  :bindings (((?full-enum-type ?real-output-type ?element-type) 
	      (process-enum-values ?input-type ?enumerator-type ?input-type nil :for-effect))
	     (?real-affector-type (process-affector-values ?affector-type ?input-type))
	     )
  :path-ends ((:name done-more))
  :initial-inputs ((:name the-set :type ?input-type)
		   (:name updater-arg :type ?element-type)
		   )
  :final-outputs ((:name the-affected-thing :type ?real-output-type))
  :components ((:name enumerator :type ?full-enum-type)
	       (:name updater :type ?real-affector-type :dont-expand nil)
	       )
  :control-flows (((:component enumerator :branch empty) (:component updater :branch empty))
		  ((:component enumerator :branch empty) (:component the-affected-thing))
		  ((:component updater) (:component done-more)))
  :dataflows (
	      ((:component the-set :port the-set) (:component enumerator :port the-set))
	      ((:component the-set :port the-set) (:component the-affected-thing :port the-affected-thing))
	      ((:component enumerator :branch more :port the-elements) (:component updater :port the-places :branch more))
	      ((:component updater-arg :port updater-arg) (:component updater :port  updater-arg :branch more))
	      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Jason Detect Duplicates Example
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fix: Make this work again

(defreduction jdd-reduce (jdd ())
  :reduce-to (jdd-plan))
					    
(defcliche jdd-plan ()
  :initial-inputs ((:name the-tree :type json-tree))
  :final-outputs ()
  :components ((:name generate-dups 
		      :type enumerate-filter-accumulate
		      :input-type json-tree :output-type (list symbol)
		      :enumerator-type (tree-traverse :set-type json-tree :element-type json-node :key-type json-key)
		      :filter-type (type-split :input-type json-node :output-type json-alist-node)
		      :accumulator-type (detect-duplicates :input-type json-alist-node :output-type (list symbol) :key-extractor-type left))
	       (:name null-test :type empty-test :element-type symbol)
	       (:name print-dups :type print :input-type (list symbol)))
  :dataflows (((:component the-tree :port the-tree) (:component generate-dups :port the-set))
	      ((:component generate-dups :port the-accumuland) (:component null-test :port the-list))
	      ((:component generate-dups :port the-accumuland) (:component print-dups :port the-data)))
  :control-flows (((:component null-test :branch not-empty) (:component print-dups))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulating the elements of an array into a set
;;;  as opposed to numerical reduction
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defreduction set-accum-reduce (set-accumulator (?input-type ?output-type ?op ?initial-value))
  :reduce-to (set-acc-plan ?input-type ?output-type ?op ?initial-value))
					    
(defcliche set-acc-plan (?input-type ?output-type ?op ?initial-value)
  :bindings ((?numeric-type (second ?input-type)))
  :initial-inputs ((:name the-set :type ?input-type))
  :final-outputs ((:name the-accumuland :type ?output-type))
  :components ((:name generate-it
		      :type enumerate-filter-accumulate
		      :input-type ?input-type :output-type ?output-type
	      :enumerator-type (enumerator :set-type ?input-type)
		      :filter-type identity
		      :accumulator-type (numeric-accumulator :op ?op :initial-value ?initial-value :numeric-type ?numeric-type)))
  :dataflows (((:component the-set :port the-set) (:component generate-it :port the-set))
	      ((:component generate-it :port the-accumuland) (:component the-accumuland :port the-accumuland))))

;;; fix this has a state element that isn't initialized
(defcliche path-accumulator (?element-type ?path-type)
  :initial-inputs ((:name the-path :type ?path-type))
  :state-elements ((:name path-sequence :direction source :port-name path-sequence :port-type (temporal-sequence ?path-type)))
  :final-outputs ((:name the-paths :type (temporal-sequence ?path-type)))
  :components ((:name get-key :type take :element-type ?element-type :input-container-type temporal-sequence)
               (:name add-to-path :type add-to-set :set-type list :element-type ?element-type :unique? nil)
	       (:name add-to-path-sequence :type put :element-type ?path-type :output-container-type temporal-sequence)
	       )
  :dataflows (((:component the-path :port the-path) (:component add-to-path :port the-set))
	      ((:component the-path :port the-path) (:component add-to-path-sequence :port data))
              ((:component get-key :port data) (:component add-to-path :port the-element))
	      ((:component path-sequence :port path-sequence) (:component add-to-path-sequence :port sequence))
              ((:component add-to-path-sequence :port sequence-data) (:component the-paths :port the-paths)))
  :contact-points ((:component get-key :direction input :name sequence-data)
		   (:component add-to-path :direction output :name the-set))
  :initializer path-accumulator-initializer
  )

(defcliche path-accumulator-initializer (?init-component ?path-key-type ?path-type)
  :state-elements ((:name initial-path :direction source :port-name initial-path :port-type ?path-type))
  :components ((:name allocate :type allocate :object-type ?path-type)
	       (:name take-path :type take :input-container-type temporal-sequence :element-type ?path-type))
  :dataflows (((:component allocate :port new-object) (:component initial-path :port initial-initial-path))
	      ((:component initial-path :port initial-path) (:component ?init-component :port the-path))
	      ((:component ?init-component :branch more :port the-paths) (:component take-path :port sequence-data))
	      ))

(defmethod composition-args ((thing (eql 'path-accumulator)) partner)
  (let* ((properties (properties partner))
	 (key-type (getf properties :key-type))
	 (path-description  `(list ,key-type))
	 (path (or (simplest-version path-description) path-description)))
    (list key-type path)))

(defun instantiate-cliche (cliche-name &rest cliche-arguments)
  (let* ((new-design (make-instance 'composite-task :name cliche-name))
	 (function (symbol-function cliche-name))
	 (formal-args (rest (excl:arglist function))))
    (loop for formal-arg in formal-args
	for real-arg in cliche-arguments
	do (setf (getf (properties new-design) (intern (string formal-arg) :keyword)) real-arg))			
    (apply function new-design cliche-arguments)
    new-design))

(defreduction sorted-list-test-reduce (sorted-list-test ())
  :reduce-to (for-all-cliche))

(defcliche sorted-list-test ()
  :initial-inputs ((:name the-list :type (list integer)))
  :final-outputs ((:name the-value :type boolean))
  :constants ((:name true :type boolean :value t)
              (:name false :type boolean :value nil))
  :state-elements (
                   (:name last :direction source :port-name last :port-type integer :state last)
                   (:name return-value :direction source :port-name value :port-type boolean :state value)
                   (:name next-last :direction sink :port-name last :port-type integer :state last)
                   (:name value :direction sink :port-name value :port-type boolean :state value)
                   )
  :components (
               (:name enum :type list-enumerator :element-type integer)
               (:name take :type take :input-container-type temporal-sequence :element-type integer)
               (:name tr :type truncate :element-type (temporal-sequence integer))
               (:name test :type less-than-test)
               (:name first :type first :output-type integer :input-type (list integer))
               (:name rest :type rest :output-type (list integer) :input-type (list integer))
               )
  :dataflows (
              ((:component enum :port list-elements :branch more) (:component take :port sequence-data))
              ((:component enum :port list-elements :branch more) (:component tr :port data))
              ((:component take :port data) (:component next-last :port last))
              ((:component take :port data) (:component test :port number-2))
              ((:component first :port the-part) (:component last :port initial-last))
              ((:component rest :port the-part) (:component enum :port the-set))
              ((:component last :port last) (:component test :port number-1))
              ((:component return-value :port value) (:component the-value :port the-value))
              ((:component true :port true) (:component value :port value))
              ((:component false :port false) (:component return-value :port initial-value))
              ((:component the-list :port the-list) (:component first :port the-container))
              ((:component the-list :port the-list) (:component rest :port the-container))
              )
  :control-flows (
                  ((:component enum :branch empty) (:component value))
                  ((:component test :branch no) (:component tr))
		  ((:component test :branch yes) (:component next-last))
                  )
  )

(defcliche sorted-vector-test (?container-type ?element-type)
  :initial-inputs (
                   (:name the-vector :type (?container-type ?element-type))
                   )
  :final-outputs (
                  (:name the-value :type boolean)
                  )
  :constants (
              (:name zero :type integer :value 0)
              (:name one :type integer :value 1)
              (:name false :type boolean :value false)
              (:name true :type boolean :value true)
              )
  :state-elements (
                   (:name return-value :direction source :port-name value :port-type boolean :state value)
                   (:name value :direction sink :port-name value :port-type boolean :state value)
                   (:name last :direction source :port-name last :port-type ?element-type :state last)
                   (:name next-last :direction sink :port-name last :port-type ?element-type :state last)
                   )
  :components (
	       (:name make-range :type range-constructor)
               (:name va :type vector-accessor :element-type ?element-type :container-type ?container-type)
               (:name init-va :type vector-accessor :element-type ?element-type :container-type ?container-type)
               (:name enum-indices :type index-enumerator)
               (:name length :type vector-length :vector-type (?container-type ?element-type))
               (:name test :type less-than-test :numeric-type ?element-type)
               (:name tr :type truncate :element-type (temporal-sequence integer))
               (:name take :type take :input-container-type temporal-sequence :element-type integer)
               )
  :dataflows (
	      ((:component enum-indices :branch more :port indices) (:component tr :port data))
              ((:component zero :port zero) (:component init-va :port index))
              ((:component one :port one)  (:component make-range :port lower-bound))
	      ((:component length :port length) (:component make-range :port upper-bound))
	      ((:component make-range :port the-range) (:component enum-indices :port the-set))
              ((:component va :port the-element) (:component test :port number-2))
              ((:component va :port the-element) (:component next-last :port last))
              ((:component init-va :port the-element) (:component last :port initial-last))
              ((:component enum-indices :port indices :branch more) (:component take :port sequence-data))
              ((:component the-vector :port the-vector) (:component length :port vector))
              ((:component the-vector :port the-vector) (:component init-va :port vector))
              ((:component the-vector :port the-vector) (:component va :port vector))
              ((:component take :port data) (:component va :port index))
              ((:component return-value :port value) (:component the-value :port the-value))
              ((:component last :port last) (:component test :port number-1))
              ((:component false :port false) (:component return-value :port initial-value))
              ((:component true :port true) (:component value :port value))
              )
  :control-flows (
                  ((:component enum-indices :branch empty) (:component value))
                  ((:component test :branch no) (:component tr))
		  ((:component test :branch yes) (:component next-last))
                  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Numerical Accumulation (also know as reduction) 
;;; e.g. summing up the elements of a (temporal) sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defreduction accumulator-to-numerically-accumulate (numerical-accumulator (?numeric-type ?op ?initial-value))
  :reduce-to (numerically-accumulate ?numeric-type ?op ?initial-value))

(defcliche numerically-accumulate (?numeric-type ?op-type ?initial-value)
  :bindings ((?real-input-type (if (eql ?op-type 'count) 'data-structure ?numeric-type))
	     (?real-numeric-type (if (eql ?op-type 'count) 'integer ?numeric-type))
	     (?op-output-port (output-port-for-numeric-op ?op-type))
	     (?accumuland-name (intern (string-upcase (format nil "The-~A" ?op-output-port))))
	     (?initial-accumuland-name (intern (string-upcase (format nil "Initial-The-~A" ?op-output-port))))
	     (?updated-accumuland-name (intern (string-upcase (format nil "Updated-The-~A" ?op-output-port))))
	     )
  :entry-points ((:name more :ports ((:name the-sequence :type (temporal-sequence ?real-input-type))))
		 (:name empty))
  ;; :initial-inputs ((:name the-sequence :type (temporal-sequence ?real-input-type)))
  :path-ends ((:name stop :ports ((:name input :type ?real-numeric-type))))
  :final-outputs ((:name accumuland :type ?real-numeric-type))
  :constants ((:name initial-value :type ?real-numeric-type :value ?initial-value))
  :state-elements ((:name current-accumuland :direction source :port-name ?accumuland-name :port-type ?real-numeric-type :state ?accumuland-name)
                   (:name updated-accumuland :direction sink :port-name ?accumuland-name :port-type ?real-numeric-type :state ?accumuland-name))
  :components ((:name ?op-type :type ?op-type :numeric-type ?real-numeric-type)
               (:name t-1 :type take :element-type ?real-input-type :input-container-type temporal-sequence))
  :dataflows (
              ((:component initial-value :port initial-value) (:component current-accumuland :port ?initial-accumuland-name))
              ((:component current-accumuland :port ?accumuland-name) (:component ?op-type :port i-2))
              ((:component more :port the-sequence) (:component t-1 :port sequence-data))
              ((:component t-1 :port data) (:component ?op-type :port i-1))
              ((:component ?op-type :port ?op-output-port) (:component updated-accumuland :port ?accumuland-name))
	      ((:component updated-accumuland :port ?updated-accumuland-name) (:component stop :port input))
	      ;; the-sum isn't always going to be the right name
              ((:component current-accumuland :port ?accumuland-name) (:component accumuland :port accumuland))
              )
  :control-flows (
		  ((:component empty) (:component accumuland))
		  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update Each Element
;;;              Affect each element of a temporal sequence of places
;;;              By applying an operation to each element and sticking
;;;              the result back
;;;              Normalizing the elements of a set is an example
;;;
;;; Parameters are the set-type, numeric-type, and the type of the operation
;;;   that's applied to each element
;;;
;;; This has a two join interface similar to accumulators (more and empty)
;;; The inputs are to the more join are
;;;  1) The temporal sequence of places
;;;  2) The other argument to the operation
;;;     (at the moment this is limited to a binary operation, but could be generalized
;;;      through some magic or other)
;;;
;;;  This is done for effect, so there is no final output
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defreduction reduce-to-update-each (updater (?set-type ?numeric-type ?numeric-op-type))
  :reduce-to (update-each-element ?set-type ?numeric-type ?numeric-op-type)
  )

(defcliche update-each-element (?set-type ?numeric-type ?numerical-op-type)
  :bindings ((?index-type (index-type ?set-type))
	     (?place-type `(place ,?set-type ,?index-type))
	     (?temporal-sequence-type `(temporal-sequence ,?place-type))
	     (?op-output-port (output-port-for-numeric-op ?numerical-op-type)))
  :entry-points ((:name empty)
                 (:name more :ports ((:name updater-arg :type ?numeric-type) 
				     (:name the-places :type ?temporal-sequence-type))))
  :final-outputs ()
  :path-ends ((:name done-more) (:name done-empty))
  :components (
               (:name get-one :type take :input-container-type temporal-sequence :element-type ?place-type)
               (:name get-value :type fetch :containing-object-type ?set-type :index-type ?index-type)
               (:name normalize :type ?numerical-op-type :numeric-type ?numeric-type)
               (:name update :type assign :containing-object-type ?set-type :index-type ?index-type)
               )
  :dataflows (((:component more :port updater-arg) (:component normalize :port i-2))
              ((:component more :port the-places) (:component get-one :port sequence-data))
              ((:component get-one :port data) (:component get-value :port the-place))
              ((:component get-one :port data) (:component update :port the-place))
              ((:component get-value :port the-value) (:component normalize :port i-1))
              ((:component normalize :port ?op-output-port) (:component update :port new-value))
              )
  :control-flows (((:component update) (:component done-more))
		  ((:component empty) (:component done-empty)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Easy to use things that set up test cases
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  
;;; This is operate on each element
(defcliche test ()
  :initial-inputs (
                   (:name NORMALIZER :type number)
                   (:name THE-SET :type (array number 2))
                   )
  :components (
               (:name NORMALIZE :type NORMALIZER :set-TYPE (array number 2))
               (:name ENUM :type ENUMERATOR :ELEMENT-TYPE NUMBER :SET-TYPE (array NUMBER 2) :INPUT-NAME THE-SET :OUTPUT-NAME THE-PLACES
		      :for-value nil)
               )
  :dataflows (
              ((:component ENUM :port THE-elements :branch MORE) (:component NORMALIZE :port THE-PLACES))
              ((:component NORMALIZER :port NORMALIZER) (:component NORMALIZE :port NORMALIZER))
              ((:component THE-SET :port THE-SET) (:component ENUM :port THE-SET))
              )
  )

;;; This is a generate accumulate
(defcliche test-2 ()
  :initial-inputs (
                   (:name THE-ARRAY :type (ARRAY INTEGER 2))
                   )
  :final-outputs (
                  (:name THE-SUM :type INTEGER)
                  )
  :components (
               (:name ACC :type NUMERICAL-ACCUMULATOR :INITIAL-VALUE 0 :OP ADD :NUMERIC-TYPE INTEGER)
               (:name ENUM :type ENUMERATOR :OUTPUT-TYPE INTEGER :ELEMENT-TYPE INTEGER :SET-TYPE (ARRAY INTEGER 2) :FOR-VALUE T)
               )
  :dataflows (
              ((:component ACC :port ACCUMULAND) (:component THE-SUM :port THE-SUM))
              ((:component ENUM :port THE-ELEMENTS :branch MORE) (:component ACC :port THE-SEQUENCE :branch MORE))
              ((:component THE-ARRAY :port THE-ARRAY) (:component ENUM :port THE-SET))
              )
  :control-flows (
                  ((:component ENUM :branch EMPTY) (:component ACC :branch EMPTY))
                  )
  )

(defun test-ea ()
  (enumerate-accumulate (dip)
			'(array integer 2)
			'enumerator
			'(numerical-accumulator :op add)
			'integer))

(defun test-ea-component ()
  (add-component *design-editor* 'test-ea 'enumerate-filter-accumulate
		 '((input-type (array integer 2))
		   (enumerator-type enumerator)
		   (accumulator-type (numerical-accumulator :op add))
		   (output-type integer)
		   )))

(defun test-efa ()
  (enumerate-filter-accumulate (dip)
			       '(array integer 2)
			       'enumerator
			       `(type-split :condition plusp)
			       '(numerical-accumulator :op add)
			       'integer))

(defun test-efa-component ()
  (add-component *design-editor* 'test-ea 'enumerate-filter-accumulate
		 '((input-type (array integer 2))
		   (enumerator-type enumerator)
		   (filter-type (type-split :condition plusp))
		   (accumulator-type (numerical-accumulator :op add))
		   (output-type integer)
		   )))

(defun test-enumerate-affect-each ()
  (enumerate-affect-each (dip)
	       '(array integer 2)
	       'enumerator
	       '(updater :op-type divide)))

;;; To do
(defun test-enumerate-affect-each-component ()
  (add-component *design-editor* 'test-eae 'enumerate-filter-affect-each
		 '((input-type (array integer 2))
		   (enumerator-type enumerator)
		   (filter-type nil)
		   (affector-type (updater :op-type divide))		   
		   ))
  )

(defcliche test-normalize-array ()
  :initial-inputs ((:name the-set :type (array integer 2)))
  :final-outputs ((:name the-affected-set :type (array integer 2)))
  :components ((:name test-ea :type enumerate-filter-accumulate 
		      :input-type (array integer 2) 
		      :output-type integer
		      :enumerator-type enumerator 
		      :accumulator-type (numerical-accumulator :op add))
               (:name test-eae :type enumerate-filter-affect-each :input-type (array integer 2) 
		      :enumerator-type enumerator :filter-type nil :affector-type (updater :op-type divide)))
  :dataflows (
              ((:component the-set :port the-set) (:component test-ea :port the-set))
              ((:component the-set :port the-set) (:component test-eae :port the-set))
              ((:component test-ea :port the-accumuland) (:component test-eae :port updater-arg))
              ((:component test-eae :port the-affected-thing) (:component the-affected-set :port the-affected-set))
              )
  )




;;; To Do: Create a clich for enumerate-affect-each similar to enumerate-accumulate
;;; get rid of all the confused things in here to create this
;;; look for anything that has an argument called "inices" it's probably a bogus 
;;; attempt to deal with the lack of "places".
;;; Fix up all the stuff associated with the JDD example which enumerates trees
;;; not vectors.
;;; Test whether the previous thing works if the input type is a vector vs an array (it should)

