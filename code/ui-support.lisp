;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLIM support for refinement.lisp and task-descriptions.lisp
;;; So that we can build a core natsoft without CLIM
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; From refinement.lisp

(defparameter *vocalize-implementation-choice* t)

(defparameter *complain-out-loud* t)

(defmethod guide-synthesis (parse main (pattern (eql 'generator-accumulator)))
  (declare (ignore parse main))
  (vocalize-text "What is the type of the input?" :vocalize? t :wait? t)
  (let* ((input-type (get-response))
	 (parse (parse-text input-type))
	 (aggregate nil)
	 (type nil))
    (ask* `[parse-type-answer ,(get-main parse) ?agg ?type]
	  (setq aggregate (convert-start-string-to-lisp-atom ?agg)
		type (find-singular-of-noun (convert-start-string-to-lisp-atom ?type))))
    (let* ((name (intern (string-upcase (concatenate 'string "the-" (string aggregate)))))
	   (initial-input (add-initial-input (dip) name name (list aggregate type)))
	   (enumerator (add-component clim:*application-frame* 'num 'enumerator `((:set-type (,aggregate ,type))))))
      (add-dataflow (port-named 'output name initial-input)
		    (Port-named 'input 'the-set enumerator))
      (clim:redisplay-frame-pane clim:*application-frame* (clim:get-frame-pane clim:*application-frame* 'display) :force-p t)
      (vocalize-text "What is the numeric operation" :vocalize? t :wait? t)
      (let* ((op (intern (string-upcase (string-trim '(#\space) (get-response)))))
	     (accumulator (add-component clim:*application-frame* 'acc 'numerical-accumulator
					 `((:op ,op) (:initial-value 0) (:numeric-type ,type))))
	     (final-output (add-final-output (dip) name name type)))
	(add-dataflow (port-named 'output 'accumuland accumulator)
		      (port-named 'input name final-output))
	(add-dataflow (port-named 'output 'the-elements (branch-named 'more enumerator))
		      (Port-named 'input 'the-sequence (join-named 'more accumulator)))
	(make-control-flow (branch-named 'empty enumerator) (join-named 'empty accumulator))
      ))))
			 

(defrule parse-type-answer (:backward)
  :then [parse-type-answer ?main ?aggregate ?type]
  :if [and (typep ?main 'instance)
	   [as-subject ?main ?clause]
	   [relation-of ?clause ? |related-to|]
	   [object-of ?clause ? ?type]
	   [subject-of ?clause ? ?aggregate]]
  )

(defun choose-selected-implementation (parent-design name possible-implementations)
  (declare (ignore name))
  (let ((learned-stuff-pane (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
    (switch-focus clim:*application-frame* (superior parent-design))
    (select-object parent-design clim:*application-frame*)
    (clim:redisplay-frame-pane clim:*application-frame* (clim:get-frame-pane clim:*application-frame* 'display) :force-p t)
    (center-display-pane-on-task clim:*application-frame* parent-design)
    (clim:with-text-style (learned-stuff-pane *learned-facts-style*)
      (let ((map (loop for (implementation name) in possible-implementations
		     for choice = (find-choice implementation)
		     collect (list* implementation name choice))))
	(loop for (nil nil nil action object type-constraint verb verb-modifier) in map
	    for first = t then nil
	    for owner = (find-best-description-of-constraint type-constraint)
	    when first
	    do (let ((generator (explain-purpose action object owner)))
		 (multiple-value-bind (generator text) (vocalize-generator generator :vocalize? *vocalize-implementation-choice* :wait? t)
		   (declare (ignore generator))
		   (format learned-stuff-pane "~%~a" text)))
	    do (let ((generator (describe-an-option action object verb verb-modifier (not first))))
		 (multiple-value-bind (generator text) (vocalize-generator generator :vocalize? *vocalize-implementation-choice* :wait? t)
		   (declare (ignore generator))
		   (format learned-stuff-pane "~%~10t~a" text))))
	(let ((alist (Loop for (implementation nil nil action object nil verb verb-modifier) in map 
			   collect (list verb-modifier :value (list implementation action object verb verb-modifier)))))
	  (vocalize-text "Which do you prefer?" :vocalize? *vocalize-implementation-choice* :wait? t)
	  (let ((answer (clim:accept-from-string `(clim:member-alist ,alist) (get-response))))
	    (destructuring-bind (implementation action object verb verb-modifier) answer
	      (format learned-stuff-pane "~%~5t~a"
		      (generate-text (describe-selected-option action object verb verb-modifier)))
	      (vocalize-text "What was the reason for your choice?" :vocalize? *vocalize-implementation-choice* :wait? t)
	      (let ((answer (get-response)))
		(assert-text answer)
		(format learned-stuff-pane "~%~5tRationale: ~a" answer)
		(with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
				 :if-does-not-exist :create
				 :if-exists :append)
		  (print `[reason-for ,(full-symbolic-description implementation) 
				      ,(full-symbolic-description parent-design) ,verb-modifier ,answer] stream))
		(tell `[reason-for ,implementation ,parent-design ,verb-modifier ,answer]))
	      implementation)))))))

(defun ask-which-join-to-use (interface name 
					  &optional (stream (clim:get-frame-pane clim:*application-frame* 'interactor)))
  (format stream "~%You are adding an output named ~a but the task has multiple entry points~%" name)
  (clim:accept `(member ,@(loop for join in (joins interface) collect (name join)))
	       :prompt "Which entry point do you want to use?"
	       :stream stream))

(defun ask-which-branch-to-use (interface name 
				&optional (stream (clim:get-frame-pane clim:*application-frame* 'interactor)))
  (format stream "~%You are adding an output named ~a but the task has multiple exit points~%" name)
  (clim:accept `(member ,@(loop for branch in (branches interface) collect (name branch)))
	       :prompt "Which exit point do you want to use?"
	       :stream stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From task descriptions.lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complain-about-port-incompatibility (flow source-port destination-port source-type destination-type)
  (let* ((generator (make-instance 'question-generator))
	 (output (intern-entity generator "output" :definite? t :singular? t :modifier (string (name source-port))))
	 (component-1 (intern-entity generator "component" :definite? t :singular? t :modifier (string (name (task source-port)))))
	 (input (intern-entity generator "input" :definite? t :singular? t :modifier (string (name destination-port))))
	 (component-2 (intern-entity generator "component" :definite? t :singular? t :modifier (string (name (task destination-port)))))
	 (verb (intern-new-instance generator "is"))
	 (with (intern-new-instance generator "with"))
	 (compatible (intern-constant generator "compatible")))
    (create-main-clause generator output verb compatible :is-question? nil :is-imperative? nil :subject-is-wh? t)
    (make-verb-negative generator verb)
    (create-possesive generator output component-1 :is-pp? nil)
    (create-possesive generator input component-2 :is-pp? nil)
    (add-relative-clause generator verb with input)
    (let ((first (intern-entity generator "first" :definite? t :singular? t))
	  (produce (intern-new-instance generator "produce"))
	  (source-type-entity (intern-entity generator (string (if (listp source-type) (first source-type) source-type)) :definite? nil :singular? t)))
      (create-main-clause generator first produce source-type-entity :is-question? nil)
      (when (listp source-type)
	(recursively-qualify generator source-type-entity (rest source-type)))
      )
    (let ((second (intern-entity generator "second" :definite? t :singular? t))
	  (consumes (intern-new-instance generator "consume"))
	  (destination-type-entity (intern-entity generator (string (if (listp destination-type) (first destination-type) destination-type)) 
						  :definite? nil :singular? t)))
      (create-main-clause generator second consumes destination-type-entity :is-question? nil)
      (when (listp destination-type)
	(recursively-qualify generator destination-type-entity (rest destination-type))))
    (setf (current-parse generator) (nreverse (current-parse generator)))
    (setf (is-problematic? source-port) t
	  (is-problematic? destination-port) t)
    (vocalize-generator generator :vocalize? *complain-out-loud* :wait? t)
    (let ((source-type-constraint (tell `[port-type-constraint ,source-port ,source-type] :justification :none))
	  (destination-type-constraint (tell `[port-type-constraint ,destination-port ,destination-type] :justification :none))
	  (flow-assertion (tell `[dataflow-from ,flow ,source-port ,destination-port] :justification :none)))
      (tell `[incompatible-port-types ,source-type ,source-port ,destination-type ,destination-port]
	    :justification `(incompatible-types (,source-type-constraint ,destination-type-constraint ,flow-assertion))))
    (let ((stream (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
      (clim:with-text-style (stream *learned-facts-style*)
	(terpri stream)
	(princ (substitute #\newline #\; (generate-text generator)) stream)))
    ))

(defparameter *created-task-counter* 0)

(defmethod insert-transducer ((flow dataflow) (source-port port) (destination-port port) source-type destination-type)
  (remove-dataflow flow)
  (let ((transducer (create-task (intern (make-symbol (format nil "TRANS-~d" (incf *created-task-counter*)))) 
				 'transducer 
				 (superior (task source-port))
				 :input-type source-type
				 :output-type destination-type)
				 ))
    (tell `[property-value-of ,transducer input-type ,source-type] :justification :premise)
    (tell `[property-value-of ,transducer output-type ,destination-type] :justification :premise)
    (let ((raw-data-port (port-named 'input 'raw-data transducer))
	  (reformatted-data-port (port-named 'output 'new-data transducer)))
      (add-dataflow source-port raw-data-port)
      (add-dataflow reformatted-data-port destination-port))
    (select-object transducer clim:*application-frame*)
    transducer
    ))

(defun handle-new-procedure-type-description (new-type super-type)
  (multiple-value-bind (inputs outputs) (get-port-type-constraints super-type)
    (flet ((check-for-informative-type-constraint (direction name type-constraints)
	     (unless (find 'data-structure type-constraints :test-not 'eql)
	       (query-about-io-type (string new-type) (string name) :direction direction :vocalize? t)
	       (parse-text (get-response))
	       )))
      (let ((learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (cond
	   ((and (is-a-type? new-type)
		 (is-of-type? new-type super-type))
	    (let ((string 
		   (format nil "~%I already knew that ~a is a subtype of ~a"
			   new-type super-type)))
	      (write-string string learned-facts-pane)
	      (vocalize-text string :vocalize? t :wait? t)))
	   (t (format learned-facts-pane "~%~a is a subtype of ~a" new-type super-type)
	    (tell `[is-type ,new-type])
	    (tell `[subtype ,new-type ,super-type])
	    (with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
			     :if-does-not-exist :create
			     :if-exists :append)
	      ;; (print `[is-type ,new-type] stream)
	      ;; (print `[subtype ,new-type ,super-type] stream)
	    (let ((new-inputs-with-constraints nil)
		  (new-outputs-with-constraints nil))
	      (loop for (name type-constraint) in inputs
		  for response = (check-for-informative-type-constraint 'input name type-constraint)
		  when response 
		  do (push (make-type-assertion-about-io-typing response 'input name new-type) new-inputs-with-constraints))
	      (loop for (name type-constraint) in outputs
		  for response = (check-for-informative-type-constraint 'output name type-constraint)
		  when response 
		  do (push (make-type-assertion-about-io-typing response 'output name new-type) new-outputs-with-constraints))
	      (let ((new-task-description `(deftask ,new-type
					       :super-types (,super-type)
					       ;; Deftask no longer annotates the io names
					       ;; we leave that for Joshua assertions
					       :interface ((:inputs ,@new-inputs-with-constraints)
							   (:outputs ,@new-outputs-with-constraints))
					       )))
		(with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
				 :if-does-not-exist :create
				 :if-exists :append)
		  (print new-task-description stream))
		;; we have to do this since deftask is a macro
		(eval new-task-description)))))))))))

(defmethod make-type-assertion-about-io-typing ((parse core-parser-mixin) direction sub-atom task-type)
  (declare (ignore direction))
  (let* ((main (get-main parse))
	 (subject (subject main))
	 (subject-name (name subject))
	 (object (object main))
	 (object-name (name object))
	 (relation (relation main))
	 (relation-name (name relation))
	 (learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
    (flet ((canonicalize-type (type)
	     (if (eql (find-singular-of-noun (convert-start-string-to-lisp-atom (name type))) 'string)
		 (let ((real-type (find-related-to type)))
		   (values real-type (name real-type)))
	       (values type (name type))))
	   (add-assertion (type)
	     (let ((type (find-singular-of-noun (convert-start-string-to-lisp-atom type)))
		   (task-is-io (is-of-type? task-type 'io-device)))
	       (when task-is-io
		 (setq type (list 'stream type)))
	       (clim:with-text-style (learned-facts-pane *learned-facts-style*)
		 (format learned-facts-pane
			 "~%The ~a port of ~a devices contains objects of type ~a"
			 sub-atom task-type type))
;;;	       (with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
;;;				:if-does-not-exist :create
;;;				:if-exists :append)
;;;		 (print `[has-data-type ,task-type ,direction ,sub-atom ,type] stream))
;;;	       (tell `[has-data-type ,task-type ,direction ,sub-atom ,type])
	       (list sub-atom type))))
      (cond
       ((or (eql relation-name '|produce|) (eql relation-name '|consume|))
	(multiple-value-bind (real-object real-object-name) (canonicalize-type object)
	  (declare (ignore real-object))
	  (add-assertion real-object-name)))
       ;; If you respond with just a noun for the answer
       ;; START turns that into a question of the form "can you tell me about a <noun>"
       ((and (eql relation-name '|tell|)
	     (eql subject-name '|you|)
	     (eql object-name 'I))
	(let ((the-type (find-related-to main :connective '|about|)))
	  (multiple-value-bind (real-type real-type-name) (canonicalize-type the-type)
	    (declare (ignore real-type))
	    (add-assertion real-type-name))))
       ((eql relation-name '|is-a|)
	(multiple-value-bind (real-object real-object-name) (canonicalize-type object)
	  (declare (ignore real-object))
	  (add-assertion real-object-name)))
       ))))


;;; This is what you do when somebody gives you a description 
;;; of a new task type

;;; Notice that this buffer is different than the one used
;;; by the top-level.  A poor man's input context

;;; Notice that we open and close the learned-facts file 3 times here.
;;; This could be optimized by holding it open longer, but I doubt it matters much.

(defun handle-new-task-type-description (new-type super-type)
  ;; for example "an imager is an input-device"
  (unless (is-a-type? super-type)
    (error "There is no type named ~a" super-type))
  (cond
   ((is-of-type? super-type 'procedure)
    (handle-new-procedure-type-description new-type super-type))
   ((is-of-type? super-type 'data-structure)
    (handle-new-data-type-description new-type super-type)
    )))

(defun handle-new-data-type-description (new-type super-type)
  ;; if this looks like a java dotted name that got changed by START
  ;; in that case defer to the special method for that
  (let* ((string (string new-type))
	 (position (position #\- string :test #'char-equal))
	 (first-component (subseq string 0 position)))
    (unless (string-equal first-component "JAVA")
      (let* ((learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (format learned-facts-pane "~%~a is a kind of ~a" new-type super-type)
	  (unless (probe-file "natsoft:code;learned-stuff.lisp")
	    (with-open-file (f "natsoft:code;learned-stuff.lisp" :direction :output :if-does-not-exist :create)
	      (format f ";;; -*- Mode: Common-lisp; Package: Natsoft; Readtable: Joshua -*~%")))
	  (with-open-file (f "natsoft:code;learned-stuff.lisp"
			   :direction :output 
			   :if-exists :append
			   :if-does-not-exist :create)
	    (let ((form `(defdata-type ,new-type
			     :super-types (,super-type)
			     )))
	      (print form f)))
	  )))))

;;; for handling is-a type statements
;;; There are 2 cases: <X> is a <Y> 
;;;                    <X> is a <kind-of|type-of> <Y>
;;; in the second case you need to find the related-to texp to find the type

;;; There are two types of utterances that might come through here
;;; The first describes an element of the design, e.g. "this component is an imager"
;;; The second describes a type sub-type relationship, e.g. "imagers are input-devices"
;;; To distinguish, check for singular, definite and subject is a member of {component, task, guy, thing}

(defmethod handle-is-a ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (declare (special *design-editor*))
  (let* ((subject (subject texp))
	 (object (object texp))
	 (subject-determiner (find-related-to subject :connective '|has_det|))
	 (subject-number (find-related-to subject :connective '|has_number|))
	 (object-determiner (find-related-to object :connective '|has_det|))
	 (object-number (find-related-to object :connective '|has_number|)))
    (declare (optimize debug))
    (cond
     ((and 
       ;; i.e this that the
       (not (eql (name subject-determiner) '|indefinite|))
       (eql (name subject-number) '|singular|)
       ;; i.e. a an
       (eql (name object-determiner) '|indefinite|)
       (eql (name object-number) '|singular|)
       (member (name subject) '(|component| |task| |object| |guy|)))
      (handle-is-a-about-specific-object (first (selected-objects *design-editor*)) (name object)))
     (t (handle-type-sub-type-assertion subject object))
    )))

(defmethod handle-is-a-about-specific-object ((object core-task-mixin) (type symbol))
  (declare (special *design-editor*))
  (let ((type-atom (find-singular-of-noun (convert-start-string-to-lisp-atom type))))
    (unless (is-a-type? type-atom)
      (confess-ignorance-about-a-type type)
      (query-about-unknown-type type)
      (let ((response (get-response)))
	;; This will take you back to the same place as if he had just said
	;; something like "imagers are input devices"
	(handle-is-a (parse-text response))))
    ;; so if what he said now makes this a type, then apply it to the selected-object
    (when (is-a-type? type-atom)
      (let ((properties (properties object)))
	(apply #'rebuild-task-for-new-type type-atom (first (selected-objects *design-editor*)) properties)))))

(defmethod handle-type-sub-type-assertion ((sub-type-parse-object occurs-in-mixin) (super-type-parser-object occurs-in-mixin))
  (let ((sub-type-atom (find-singular-of-noun (convert-start-string-to-lisp-string (name sub-type-parse-object))))
	(super-type-atom (find-singular-of-noun (convert-start-string-to-lisp-string (name super-type-parser-object)))))
    (cond
     ((member super-type-atom '(type kind))
      ;; we're in case 2 
      (let ((super-type-object (find-related-to super-type-parser-object)))
	(handle-new-task-type-description sub-type-atom (convert-start-string-to-lisp-atom (name super-type-object )))))
     (t (handle-new-task-type-description sub-type-atom super-type-atom)))))

;;; the user said "replace the flow with a transducer"
(defmethod handle-replace-request ((parse core-parser-mixin) (main texp))
  (let ((new-thing (find-related-to main :connective "with")))
    (when (eql (name new-thing) '|transducer|)
      (insert-transducer-for-problematic-thing))))

;;; The user said "insert a transducer"
;;; for the moment we're begging the issue about context and just
;;; assuming he means the current flow
(defmethod handle-insert-request ((parse core-parser-mixin) (main texp))
  (let ((new-thing (object main)))
    (when (eql (name new-thing) '|transducer|)
      (insert-transducer-for-problematic-thing ))))


;;; for handling statements like the A port of Module B is connected to the C port of Module D
(defmethod handle-connect ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (let* ((source (object texp))
	 (source-qualifier (find-related-to source))
	 (agent (subject texp)))
    (let* ((destination (loop for possible-to-texp in (as-subject texp)
			   when (eql (name (relation possible-to-texp)) '|to|)
			    return (object possible-to-texp)))
	   (destination-qualifier (when destination (find-related-to destination))))
      (values source source-qualifier destination destination-qualifier agent))))

(defmethod handle-make-request ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (declare (special *design-editor*))
  (let* ((thing-to-make (object texp))
	 (type-name (name thing-to-make))
	 (type (convert-start-string-to-lisp-atom type-name))
	 (name (intern (string-upcase (get-first-initials (string type-name))))))
    (add-component *design-editor* 
		name
		(intern type)
		nil)
    ))

(defun get-first-initials (string)
  (loop for pos = 0 then (1+ next-pos)
      for next-pos = (or (position #\- string :start pos :test #'char-equal)
			 (position #\_ string :start pos :test #'char-equal))
      collect (subseq string pos (1+ pos)) into chars
      until (null next-pos)
      finally (return (apply #'concatenate 'string chars))))

(defmethod handle-implement-request ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (declare (ignore texp))
  (loop for task in (selected-objects *design-editor*)
      do (produce-implementations task nil)))

(defmethod handle-generate-request ((parser core-parser-mixin) texp language)
  (declare (ignore texp))
  (generate-the-code-for-a-buffer (design-in-progress *design-editor*) language))


(defun generate-the-code-for-a-buffer (design language)
  (when (or (not (typep design 'implementation))
            (null (abstract-task design)))
    (create-abstract-task-for-diagram design))
  (let ((task-interface (abstract-task design)))
    (produce-implementations task-interface :keep-top-level language))
  (switch-focus *design-editor* design)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Speed computations
;;;
;;;   Every port has a typical value
;;;   Every container has a typical element
;;;   Every stream has a typical element
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-keep-up-request ((parser core-parser-mixin) main original-thing1 original-thing2)
  (declare (ignore main))
  ;; first get into the format used in the system
  (let* ((thing1 (convert-start-string-to-lisp-atom original-thing1))
	 (thing2 (convert-start-string-to-lisp-atom original-thing2))
	 (dip (design-in-progress *design-editor*))
	 (components (children dip))
	 (first-guy  (find thing1 components :key #'task-type))
	 (second-guy (find thing2 components :key #'task-type))
	 (first-guys-port nil)
	 (first-guys-port-type-constraint nil)
	 ;; (second-guys-port-type-constraint nil)
	 (second-guys-port nil))
    (when (and first-guy second-guy)
      (labels ((trace-it (next-guy his-successor initial-port)
		 (loop for first-port in (outputs next-guy)
		     when (null initial-port)
		     do (setq initial-port first-port)
		     do (loop for his-flow in (outgoing-flows first-port)
			    for second-port = (output his-flow)
			    for next-task = (task second-port) 
			    if (eql next-task his-successor)
			    do (setq first-guys-port initial-port second-guys-port second-port)
			       (return (values))
			    else do (trace-it next-task his-successor initial-port)))))
	(trace-it first-guy second-guy nil)))
    (when (and first-guys-port second-guys-port)
      (setq first-guys-port-type-constraint (port-type-constraint first-guys-port)
	    ;; second-guys-port-type-constraint (port-type-constraint second-guys-port)
	    )
      (vocalize-generator (explain-purpose "calculate" "speed" original-thing1 t) :vocalize? t :wait? t)
      (find-size-in-bytes-from-user first-guys-port (if (symbolp first-guys-port-type-constraint)
							first-guys-port-type-constraint
						      (first first-guys-port-type-constraint))
				    (if (symbolp first-guys-port-type-constraint)
					nil
				      (rest first-guys-port-type-constraint))
				    *standard-input*
				    t)
      (let* ((thing1-rate (round (port-property-value-of first-guys-port 'data-rate) 1e6))
	     (thing2-rate (property-value-of second-guy 'max-write-speed))
	     (learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (speak-and-print learned-facts-pane (state-a-speed-estimate original-thing1 thing1-rate '|Mega-bytes| :verb "calculate" :vocalize? nil))
	  (speak-and-print learned-facts-pane (state-a-speed-estimate original-thing2 thing2-rate '|Mega-bytes| :vocalize? nil))
	  (speak-and-print learned-facts-pane (build-a-comparison thing1 thing2 (if (> thing1-rate thing2-rate) '|faster| '|slower|) :vocalize? nil))
	  (when (> thing1-rate thing2-rate)
	    (speak-and-print learned-facts-pane (make-a-suggestion "compress" (string original-thing1) "output"))
	    (let ((victim (find-matching-compressor dip)))
	      (when victim
		(rebuild-task-for-new-type 'image-stream-compressor victim)
		(select-object victim *design-editor*)
		(switch-focus *design-editor* (superior victim))))))
	))))

(defparameter *image-formats* '(vga xvga hd720 hd1080))

(defparameter *image-format-alist*
    '((XVGA 1024 768 1 20)
      (HD720 1280 720 4 30)
      (HD1080 1920 1080 4 30)
      ))

(defmethod find-size-in-bytes-from-user (x (type (eql 'image)) qualifiers &optional (stream *standard-input*) (top-level? nil) (ht (make-hash-table :test #'equal)))
  (flet ((do-the-work (stream)
	   (let* ((format (gethash 'format ht))
		  (x-dim (gethash (list x 'x-dimension) ht))
		  (y-dim (gethash (list x 'y-dimension) ht))
		  (typical-element (typical-element-of x))
		  (the-parent-stream (is-typical-element-of x))
		  (size-in-bytes (gethash (list typical-element 'bytes-per-element) ht)))
	     (when format
	       (unless (equal (assoc format *image-format-alist*) (list format x-dim y-dim size-in-bytes))
		 (setq format nil)))
	     (terpri stream)
	     (setq format (clim:accept `(clim:member-sequence ,*image-formats*) :stream stream :default format))
	     (setf (gethash 'format ht) format)
	     (when format
	       (destructuring-bind (width height size-in-bytes frame-rate) (rest (assoc format *image-format-alist*))
		 (setf (gethash (list x 'x-dimension) ht) width
		       (gethash (list x 'y-dimension) ht) height
		       (gethash (list typical-element 'bytes-per-element) ht) size-in-bytes
		       (gethash (list the-parent-stream 'stream-rate) ht) frame-rate
		       ))))
	   (find-size-in-bytes-from-user x 'array (cons 'pixel qualifiers) stream nil ht)))
    (if (not top-level?)
	(do-the-work stream)
      (clim:accepting-values (stream :own-window t)
	(clim:with-text-face (stream :bold)
	  (write-string "Please fill in this information" stream))
	(do-the-work stream)
	)) 
    (remhash 'format ht)
    (when top-level?
      (maphash #'(lambda (key value)
		   (destructuring-bind (object property) key
		       (tell `[property-value-of ,object ,property ,value])))
		ht))))

(defmethod find-size-in-bytes-from-user (x (type (eql 'array)) qualifiers 
					 &optional (stream *standard-input*) (top-level? nil) (ht (make-hash-table :test #'equal)))
  (let* ((typical-element (typical-element-of x))
	 (defined-as (definition-of `(array ,@qualifiers)))
	 )
    (labels ((get-value (object property default-value stream prompt)
	       (let ((entry (gethash (list object property) ht)))
		 (unless entry 
		   (setf (gethash (list object property) ht) default-value))
		 (terpri stream)
		 (setf (gethash (list object property) ht)
		   (clim:accept 'integer
				:view clim:+textual-view+
				:prompt prompt
				:default entry
				:stream stream))))
	     (do-the-questions (stream)
	       (clim:with-text-size (stream :large)
		 (get-value x 'x-dimension 0 stream (format nil "Width of the ~a" (or defined-as 'array)))
		 (get-value x 'y-dimension 0 stream (format nil "Height of the ~a" (or defined-as 'array)))
		 (get-value typical-element 'bytes-per-element 0 stream (format nil "Number of bytes per ~a" 
										(or (first qualifiers) "array element"))))))
      (if (not top-level?)
	  (do-the-questions stream)
	(clim:accepting-values (stream :own-window t)
	  (clim:with-text-face (stream :bold)
	    (write-string "Please fill in this information" stream))
	  (do-the-questions stream)
	  )))
    (when top-level?
      (maphash #'(lambda (key value)
		   (destructuring-bind (object property) key
		     (tell `[property-value-of ,object ,property ,value])))
		ht))))


;;; Notice this only holds if nothing else does, so if it's not a definition
;;; it should probably be an error
(defmethod find-size-in-bytes-from-user (x type qualifiers &optional (stream *standard-input*)
								     (top-level? nil)
								     (ht (make-hash-table :test #'equal)))
  (let ((defined-as (is-definition-for (if qualifiers (list* type qualifiers) type))))
    (cond
     ((null defined-as) (Error "Don't know how to find size in bytes of ~a of type (~a ~{~A~^ ~}" x type qualifiers))
     ((symbolp defined-as) (find-size-in-bytes-from-user x defined-as nil stream top-level? ht))
     (t (find-size-in-bytes-from-user x (first defined-as) (rest defined-as) stream top-level? ht)))))


;;; Here x is a port
(defmethod find-size-in-bytes-from-user (x (type (eql 'stream)) qualifiers 
					 &optional (stream *standard-input*) (top-level? t) (ht (make-hash-table :test #'equal)))
  
  ;;; idea is to set up the accepting-values and to ask for the stream rate 
  ;;; then call the appropriate routine for the qualifier
  (let* ((stream-of-what (first qualifiers))
	 (element-type (if (symbolp stream-of-what) stream-of-what (first stream-of-what)))
	 (element-qualifiers (if (symbolp stream-of-what) nil (rest stream-of-what)))
	 (task-type (task-type (task x)))
	 (the-stream (typical-value-of x))
	 (defined-name (definition-of `(stream-rate (stream ,@qualifiers)))))
    (flet ((body (stream)
	     (clim:with-text-size (stream :large)
	       (clim:with-text-face (stream :bold)
		 (let ((entry (gethash (list the-stream 'stream-rate) ht)))
		   (unless entry (setf (gethash (list the-stream 'stream-rate) ht) 0))
		   (setf (gethash (list the-stream 'stream-rate) ht)
		     (clim:accept 'integer 
				  :view clim:+textual-view+
				  :stream stream 
				  :default entry
				  :prompt (if defined-name
					      (format nil "~%What is the ~a of the ~a" defined-name task-type)
					    (format nil "~%At what rate does the ~a produce ~a" task-type stream-of-what))))
		   (find-size-in-bytes-from-user (typical-element-of the-stream) element-type element-qualifiers stream nil ht)
		   )))))
      (cond
       (top-level?
	(ask-to-fill-in-information t)
	(clim:accepting-values (stream :own-window t :resynchronize-every-pass t)
	  (body stream)))
       (t
	(body stream)))
      (when top-level?
	(maphash #'(lambda (key value)
		     (destructuring-bind (object property) key
		       (tell `[property-value-of ,object ,property ,value])))
		  ht)))))

(defmethod handle-locate-component-request ((parser core-parser-mixin) main verb object qualifier)
  (declare (ignore main))
  ;; Total wind up toy after here
  (when (and (eql verb '|compress|)
	     (eql object '|streams|))
    (when (eql qualifier '|video|)
      (let* ((learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (speak-and-print learned-facts-pane (build-can-be-done-using-statement "Java_Media_Framework" "build" "component" "video_compession" t))
	  (speak-and-print learned-facts-pane (build-try-sentence "find" "component" "video_compression"))
  )))))


