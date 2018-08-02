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
						     (temporal-sequence ?output-type)
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
					    ?key-type))
  :the-instance ?the-enumerator
  :the-plan ?the-plan
  :prerequisites ((not (is-of-type? ?input-type 'temporal-sequence))
		  (is-of-type? ?output-type 'primitives)
		  [can-be-viewed-as ?input-type ?view-type ?viewpoint]
		  (is-of-type? ?view-type 'sequence)
		  )
  :subplans ((subplan :type enumerator
		      :instance ?the-enumerator
		      :parameters ((?viewpoint ?input-type)
				   ?output-type
				   ?original-input-type ?original-output-type 
				   ?input-name ?output-name ?key-type)
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
(defreduction reduce-to-element (transducer ((stream (?input-container-type ?input-element-type))
					     (stream (?output-container-type ?output-element-type))
					     ?original-input-type ?original-output-type
					     ?input-name ?output-name))
  :reduce-to (generate-and-collect  ?original-input-type ?input-name
				       ?original-output-type ?output-container-type ?output-element-type ?output-name
				       )
  :prerequisites ((is-of-type? ?input-container-type 'container)
		  (is-of-type? ?output-container-type 'container)
		  ;; we'll get viewpoints other ways
		  (not (is-a-viewpoint-operator? ?input-container-type)))
  )


(defreduction take-and-generate (branching-transducer ((stream (?input-container-type ?input-element-type))
						       (temporal-sequence ?output-element-type)
						       ?original-input-type ?original-output-type
						       ?input-name ?output-name))
  :reduce-to (take-and-generate-elements 'stream ?original-input-type ?input-container-type ?input-element-type ?input-name
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
							  ?key-type))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name ?key-type)
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
							    ?key-type))
  :reduce-to (double-enumerate ?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type)
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?input-type element-type ?intermediate-type]
		  [not [unify ?intermediate-type ?output-type]]
		  )
  )

;;; This is for enumerators that generate keys as well as values
;;; Nuked generator
(defreduction vector-to-elements-3 (enumerator-with-keys (?input-type ?output-type ?original-input-type ?original-output-type ?input-name ?output-name ?key-type))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name 'the-keys)
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
(defcliche generate-and-collect (?original-input-type ?input-name
				     ?original-output-type ?output-container-type ?output-element-type ?output-name)
  :initial-inputs (;; this is the container of input stuff
		   (:name ?input-name :type ?original-input-type)
		   ;; this is the output queue
		   (:name ?output-name :type (stream (?output-container-type ?output-element-type)))
		   )
  :final-outputs ((:name chunks :port ?output-name :type ?original-output-type))
  :components ((:name tr :type branching-transducer :input-type ?original-input-type :output-type (temporal-sequence ?output-element-type))
	       (:name acc-1 :type collector :input-type ?output-element-type :input-container-type temporal-sequence :output-type ?original-output-type))
  :dataflows (((:component ?input-name :port ?input-name ) (:component tr :port raw-data))
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
(defcliche take-and-generate-elements (?sequence-type ?original-input-type
					 ?input-container-type ?input-element-type ?input-name
					 ?output-element-type ?output-name
					 )
  :initial-inputs ((:name ?input-name :type ?original-input-type))
  :components ((:name t-1 :type take :element-type (?input-container-type ?input-element-type) :input-container-type ?sequence-type)
	       (:name gen :type generator :input-type (?input-container-type ?input-element-type) :output-type (temporal-sequence ?output-element-type)))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-element-type))))
		(:name empty))
  :dataflows (((:component ?input-name :port ?input-name) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component gen :port container))
	      ((:component gen :branch more :port new-data) (:component more :port ?output-name)))
  :control-flows (((:component gen :branch empty) (:component empty)))
  )

;;; Input-type: (vector of something)
;;; output-type: the something and conditionally the indices
;;; E.g. (vector integer), number, foo,  bar
;;; need an allocate and data flows for each
;;; If the indices-name argument is nil then all the structure for building a temporal sequence  of indices
;;; is omitted
(defcliche enumerate-vector-elements (?input-type ?output-type ?input-name ?output-name ?indices-name)
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name s-1 :port-name old-stream :port-type (temporal-sequence ?output-type) 
                               :state old-stream :locality :ignore)
		   (when ?indices-name
		     (:direction source :name s-2 :port-name old-indices :port-type (temporal-sequence integer) :state old-indices-stream
				 :locality :ignore))
		   (when ?indices-name
		     (:direction source :name s-3 :port-name old-vector :port-type (temporal-sequence ?input-type) :state old-vector-stream
				 :locality :ignore))
                   )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))
                                    (when ?indices-name (:name ?indices-name :type (temporal-sequence integer)))
				    (when ?indices-name (:name the-set :type (temporal-sequence ?input-type)))
                                    ))
                (:name empty)
		)
  :constants ((:name lower :type integer :value 0))
  :components ((:name initial-s1 :type allocate :object-type (temporal-sequence ?output-type))
               (when ?indices-name (:name initial-s2 :type allocate :object-type (temporal-sequence integer)))
	       (when ?indices-name (:name initial-s3 :type allocate :object-type (temporal-sequence ?input-type)))
               (:name length :type vector-length :vector-type ?input-type)
               (:name make-range :type range-constructor)
               (:name enum :type index-enumerator)
               (:name t-1 :type Take :element-type integer :input-container-type temporal-sequence)
               (:name va :type vector-accessor :vector-type ?input-type :element-type ?output-type)
               (:name p-1 :type put :element-type ?output-type :output-container-type temporal-sequence)
               (when ?indices-name (:name p-2 :type put :element-type integer :output-container-type temporal-sequence))
	       (when ?indices-name (:name p-3 :type put :element-type ?input-type :output-container-type temporal-sequence))
               )
  :dataflows (((:component lower :port lower)  (:component make-range :port lower-bound))
              ((:component LENGTH :port LENGTH) (:component make-range :port UPPER-BOUND))
              ((:component ?input-name :port ?input-name) (:component length :port vector))
              ((:component make-range :port the-range) (:component ENUM :port the-set))
              ((:component enum :branch more :port indices) (:component t-1 :port sequence-data))
              ((:component ?input-name :port ?input-name) (:component va :port vector))
              ((:component t-1 :port data) (:component va :port index))
              (when ?indices-name ((:component t-1 :port data) (:component p-2 :port data)))
	      (when ?indices-name ((:component ?input-name :port ?input-name) (:component p-3 :port data)))
              ((:component va :port the-element) (:component p-1 :port data))
              ((:component initial-s1 :port new-object) (:component s-1 :port initial-old-stream))
              (when ?indices-name ((:component initial-s2 :port new-object) (:component s-2 :port initial-old-indices)))
	      (when ?indices-name ((:component initial-s3 :port new-object) (:component s-3 :port initial-old-vector)))
              ((:component s-1 :port old-stream) (:component p-1 :port sequence))
              (when ?indices-name ((:component s-2 :port old-indices) (:component p-2 :port sequence)))
	      (when ?indices-name ((:component s-3 :port old-vector) (:component p-3 :port sequence)))
              ((:component p-1 :port sequence-data) (:component more :port ?output-name))
              (when ?indices-name ((:component p-2 :port sequence-data) (:component more :port ?indices-name)))
	      (when ?indices-name ((:component p-3 :port sequence-data) (:component more :port the-set)))
              )
  :control-flows (((:component enum :branch empty) (:component empty)))
  )

(defcliche double-enumerate (?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type)
  :bindings ()
  :initial-inputs ((:name ?input-name :type ?input-type))
  :components ((:name enum-1 :type enumerator :set-type ?input-type :element-type ?intermediate-type)
               (:name t-1 :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
               (:name enum-2 :type enumerator :set-type ?intermediate-type :element-type ?output-type :key-type ?key-type)
	       )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))
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
	       (:name initial-buffer :type allocate :object-type ?output-type)
	       (:name t-1 :type take :element-type ?input-type :input-container-type temporal-sequence)
	       (:name vpush :type vector-push  :vector-type ?output-type :element-type ?input-type)
	       (:name full? :type vector-full-test :vector-type (vector ?input-type))
	       (:name p-1 :type put :element-type ?output-type :output-container-type stream)
	       (:name new-buffer :type allocate :object-type ?output-type)
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

;;; To make this a practical cliche' we need to standardize enough about each of the pieces so that
;;; For the enumerator: We assume it follows an emuerator protocol
;;; For the filter: Similarly we assume that it follows a "filter protocol"
;;;  ideally the filter could be optional (maybe specified as identity)
;;; For the accumulator: Follows an accumulator protocol
;;; The passed in types are actually lists of the type together with a parameter list
;;; The relevant information is then extracted from these in the bindings.

(defreduction efa-reduce (enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type))
  :reduce-to (enumerate-filter-accumulate ?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type)
					  )

(defcliche enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type)
  :bindings ((?intermediate-type (getf (rest ?enumerator-type) :element-type)))
  :initial-inputs ((:name the-set :type ?input-type))
  :final-outputs ((:name the-accumuland :type ?output-type))
  :components ((:name enumerator :type ?enumerator-type :recursive-call-name do-it :introduce-labels do-it)
	       (:name do-one :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
	       (:name filter :type ?filter-type)
	       (:name accumulate :type ?accumulator-type :dont-expand nil)
	       )
  :dataflows (
	      ((:component the-set :port the-set) (:component enumerator :port the-set))
	      ((:component enumerator :branch more :port the-elements) (:component do-one :port sequence-data))
	      ((:component do-one :port data) (:component filter :port test-data))
	      ((:component filter :branch match :port matching-data) (:component accumulate :port the-sequence))
	      ((:component accumulate :port accumuland) (:component the-accumuland :port the-accumuland)))
  )
  
(defreduction jdd-reduce (jdd ())
  :reduce-to (jdd-plan))
					    
(defcliche jdd-plan ()
  :initial-inputs ((:name the-node :type json-node))
  :final-outputs ()
  :components ((:name generate-dups 
		      :type enumerate-filter-accumulate
		      :input-type json-node :output-type (list symbol)
		      :enumerator-type (tree-traverse :set-type json-node :element-type json-node :key-type json-key)
		      :filter-type (type-split :input-type json-node :output-type json-alist-node)
		      :accumulator-type (detect-duplicates :input-type json-alist-node :output-type (list symbol) :key-extractor-type left))
	       (:name null-test :type empty-test :element-type symbol)
	       (:name print-dups :type print :input-type (list symbol)))
  :dataflows (((:component the-node :port the-node) (:component generate-dups :port the-set))
	      ((:component generate-dups :port the-accumuland) (:component null-test :port the-list))
	      ((:component generate-dups :port the-accumuland) (:component print-dups :port the-data)))
  :control-flows (((:component null-test :branch not-empty) (:component print-dups))))

(defreduction array-accum-reduce (array-acc (?input-type ?output-type))
  :reduce-to (array-acc-plan ?input-type ?output-type))
					    
(defcliche array-acc-plan (?input-type ?output-type)
  :initial-inputs ((:name the-set :type (array ?input-type)))
  :final-outputs ((:name the-accumuland :type ?output-type))
  :components ((:name generate-it
		      :type enumerate-filter-accumulate
		      :input-type (array ?input-type) :output-type ?output-type
		      :enumerator-type (enumerator :set-type (array ?input-type))
		      :filter-type identity
		      :accumulator-type (numeric-accumulator :op add :initial-value 0 :numeric-type ?input-type)))
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
  :initial-inputs (
                   (:name the-list :type (list integer))
                   )
  :final-outputs (
                  (:name the-value :type boolean)
                  )
  :constants (
              (:name true :type boolean :value t)
              (:name false :type boolean :value nil)
              )
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


(defcliche numerically-update (?numeric-type ?op-type ?set-type ?index-type)
  :entry-points (
                 (:name empty)
                 (:name more :ports ((:name normalizer :type ?numeric-type) 
				     (:name the-sets :type (temporal-sequence (?set-type ?numeric-type))) 
				     (:name the-indices :type (temporal-sequence ?index-type))
				     (:name the-values :type (temporal-sequence ?numeric-type)))))
  :path-ends (
              (:name done-empty)
              (:name done :ports ((:name updated-vector :type (?set-type ?numeric-type))))
              )
  :components (
               (:name take-set :type take :element-type (?set-type ?numeric-type) :input-container-type temporal-sequence)
               (:name take-index :type take :element-type ?index-type :input-container-type temporal-sequence)
               (:name take-value :type take :element-type ?numeric-type :input-container-type temporal-sequence)
               (:name normalize :type ?op-type :numeric-type ?numeric-type)
               (:name updater :type vector-updater :element-type ?numeric-type)
               )
  :dataflows (
              ((:component more :port the-indices) (:component take-index :port sequence-data))
              ((:component more :port the-values) (:component take-value :port sequence-data))
              ((:component more :port the-sets) (:component take-set :port sequence-data))
              ((:component take-value :port data) (:component normalize :port i-1))
              ((:component more :port normalizer) (:component normalize :port i-2))
	      ((:component take-set :port data) (:component updater :port vector))
              ((:component take-index :port data) (:component updater :port index))
              ((:component normalize :port quotient) (:component updater :port new-data))
              ((:component updater :port updated-vector) (:component done :port updated-vector))
              )
  :control-flows (
                  ((:component empty) (:component done-empty))
                  )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerating the "places" in a array/vector
;;;
;;; This geneates the places in a array or vector
;;; rather than the valuesn
;;; The places can be fetched from or assigned to so this is the most general cliche
;;;
;;; This gives a nice compositional account of composing an index geneator
;;  with a place maker to build a place generator
;;; and composing that with a place fetch to make a values enumerator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defreduction reduce-to-enumerate-vector-places (place-enumerator (?set-type ?input-name ?output-name))
  :reduce-to (enumerate-vector-places ?set-type ?input-name ?output-name)
  )

(defcliche enumerate-vector-places (?input-type ?input-name ?output-name)
  :bindings ((?place-type `(place ,?input-type integer)))
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name s-1 :port-name old-stream :port-type (temporal-sequence ?place-type) 
                               :state old-stream :locality :ignore))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?place-type))
                                    ))
                (:name empty)
		)
  :constants ((:name lower :type integer :value 0))
  :components ((:name initial-s1 :type allocate :object-type (temporal-sequence ?place-type))
               (:name length :type vector-length :vector-type ?input-type)
               (:name make-range :type range-constructor)
               (:name enum :type index-enumerator)
               (:name t-1 :type Take :element-type integer :input-container-type temporal-sequence)
               (:name place-maker :type place-constructor :containing-object-type ?input-type :offset-data-type integer)
               (:name p-1 :type put :element-type ?place-type :output-container-type temporal-sequence)
               )
  :dataflows (((:component lower :port lower)  (:component make-range :port lower-bound))
              ((:component LENGTH :port LENGTH) (:component make-range :port UPPER-BOUND))
              ((:component ?input-name :port ?input-name) (:component length :port vector))
              ((:component make-range :port the-range) (:component ENUM :port the-set))
              ((:component enum :branch more :port indices) (:component t-1 :port sequence-data))
              ((:component ?input-name :port ?input-name) (:component place-maker :port container))
              ((:component t-1 :port data) (:component place-maker :port offset))
              ((:component place-maker :port the-place) (:component p-1 :port data))
              ((:component initial-s1 :port new-object) (:component s-1 :port initial-old-stream))
              ((:component s-1 :port old-stream) (:component p-1 :port sequence))
              ((:component p-1 :port sequence-data) (:component more :port ?output-name))
              )
  :control-flows (((:component enum :branch empty) (:component empty)))
  )

(defreduction reduce-to-normalize (normalizer (?numeric-type))
  :reduce-to (normalize ?numeric-type)
  )

;;; To Do: Add the step to update the place and the data flows
;;; Get rid of the built-in assumption that this is an array or at least change it to vector
(defcliche normalize (?numeric-type)
  :initial-inputs (
                   (:name NORMALIZER :type ?NUMERIC-TYPE)
                   (:name THE-PLACES :type (TEMPORAL-SEQUENCE (PLACE (VECTOR ?NUMERIC-TYPE) INTEGER)))
                   )
  :components (
               (:name UPDATE :type ASSIGN :CONTAINING-OBJECT-TYPE (VECTOR ?NUMERIC-TYPE) :OFFSET-DATA-TYPE INTEGER)
               (:name GET-ONE :type TAKE :INPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE (PLACE (VECTOR ?NUMERIC-TYPE) INTEGER))
               (:name GET-VALUE :type FETCH :CONTAINING-OBJECT-TYPE (VECTOR ?NUMERIC-TYPE) :OFFSET-DATA-TYPE INTEGER)
               (:name NORMALIZE :type DIVIDE :NUMERIC-TYPE ?NUMERIC-TYPE)
               )
  :dataflows (
              ((:component GET-ONE :port DATA) (:component GET-VALUE :port THE-PLACE))
              ((:component GET-ONE :port DATA) (:component UPDATE :port THE-PLACE))
              ((:component GET-VALUE :port THE-VALUE) (:component NORMALIZE :port I-1))
              ((:component NORMALIZE :port QUOTIENT) (:component UPDATE :port NEW-VALUE))
              ((:component NORMALIZER :port NORMALIZER) (:component NORMALIZE :port I-2))
              ((:component THE-PLACES :port THE-PLACES) (:component GET-ONE :port SEQUENCE-DATA))
              )
  )



#|

(defcliche values-enumerator ()
  :initial-inputs (
                   (:name THE-SET :type (VECTOR NUMBER))
                   )
  :final-outputs (
                  (:name SUM :type NUMBER)
                  )
  :state-elements (
                   (:name SEQUENCE :direction source :port-name SEQUENCE :port-type (TEMPORAL-SEQUENCE NUMBER) :state SEQUENCE :locality :ignore)
                   )
  :components (
               (:name NEW-SEQUENCE :type ALLOCATE :OBJECT-TYPE (TEMPORAL-SEQUENCE NUMBER))
               (:name PUT :type PUT :OUTPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE NUMBER)
               (:name FETCH :type FETCH :CONTAINING-OBJECT-TYPE (VECTOR NUMBER) :OFFSET-DATA-TYPE INTEGER)
               (:name ACC :type NUMERICAL-ACCUMULATOR :NUMERIC-TYPE NUMBER :OP ADD :INITIAL-VALUE 0)
               (:name T-1 :type TAKE :INPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE (PLACE (VECTOR NUMBER) INTEGER))
               (:name P-E-2 :type PLACE-ENUMERATOR :ELEMENT-TYPE NUMBER :SET-TYPE (VECTOR NUMBER))
               )
  :dataflows (
              ((:component NEW-SEQUENCE :port NEW-OBJECT) (:component SEQUENCE :port INITIAL-SEQUENCE))
              ((:component SEQUENCE :port SEQUENCE) (:component PUT :port SEQUENCE))
              ((:component THE-SET :port THE-SET) (:component P-E-2 :port THE-SET))
              ((:component PUT :port SEQUENCE-DATA) (:component ACC :port THE-SEQUENCE :branch MORE))
              ((:component FETCH :port THE-VALUE) (:component PUT :port DATA))
              ((:component ACC :port ACCUMULAND) (:component SUM :port SUM))
              ((:component T-1 :port DATA) (:component FETCH :port THE-PLACE))
              ((:component P-E-2 :port THE-PLACES :branch MORE) (:component T-1 :port SEQUENCE-DATA))
              )
  :control-flows (
                  ((:component P-E-2 :branch EMPTY) (:component ACC :branch EMPTY))
                  )
  )

|#

(defcliche test ()
  :initial-inputs (
                   (:name NORMALIZER :type number)
                   (:name THE-SET :type (vector number))
                   )
  :components (
               (:name NORMALIZE :type NORMALIZER :NUMERIC-TYPE NUMBER)
               (:name ENUM :type PLACE-ENUMERATOR :ELEMENT-TYPE NUMBER :SET-TYPE (VECTOR NUMBER) :INPUT-NAME THE-SET :OUTPUT-NAME THE-PLACES)
               )
  :dataflows (
              ((:component ENUM :port THE-PLACES :branch MORE) (:component NORMALIZE :port THE-PLACES))
              ((:component NORMALIZER :port NORMALIZER) (:component NORMALIZE :port NORMALIZER))
              ((:component THE-SET :port THE-SET) (:component ENUM :port THE-SET))
              )
  )