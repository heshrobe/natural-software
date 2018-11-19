;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(In-package :natsoft)

;;; This is stolen from clim-env:lisp-debugger-hook
;;; and gives us the CLIM debugger rather than the built-in
(defvar *design-editor* nil)

;;; for debugging ease

(defun so () (first (selected-objects *design-editor*)))
(defun dip () (design-in-progress *design-editor*))

(defun debugger-hook (condition hook)
  (declare (ignore hook))
  (let* ((*application-frame* *design-editor*)
	 (*error-output* (clim:frame-standard-output *application-frame*))
	 (stream (clim:get-frame-pane *application-frame* 'interactor)))
    (clim:stream-close-text-output-record stream)
    (clim-utils:letf-globally
        (((clim:stream-current-output-record stream) (clim:stream-output-history stream))
	 ((clim:stream-recording-p stream) t)
	 ((clim:stream-drawing-p stream) t)
	 ((clim-internals::stream-current-redisplay-record stream) nil))
      (setf (clim:command-menu-enabled 'clim-env:listener-restarts *application-frame*) t)
      (clim-env:enter-debugger condition stream :own-frame t ))))

(defun my-follow-path (path)
  (loop for this-guy = (design-in-progress *design-editor*) then next-guy
      for name in path
      for next-guy = (typecase this-guy
		       (has-children-mixin
			(child-named this-guy name))
		       (task-interface
			(or (port-named 'input name this-guy)
			    (port-named 'output name this-guy))))
      when (null next-guy)
      return nil
      finally (return next-guy)))

(defmacro >> (&rest path)
  `(my-follow-path '(,@ path)))

(clim:define-application-frame design-editor (has-display-tick-mixin clim:standard-application-frame)
  ((design-in-progress :initform (make-instance 'implementation :name 'initial-design) :accessor design-in-progress)
   (all-designs :initform nil :accessor all-designs)
   (speech-input-process :initform nil :accessor speech-input-process)
   (speech-enabled? :initform nil :accessor speech-enabled?)
   (speech-engine :initform 'siri :accessor speech-engine)
   (selected-object :initform nil :accessor selected-objects)
   )
  (:top-level (design-editor-top-level))
  (:menu-bar nil)
  (:panes
   (display :application
            :display-after-commands t
	    :incremental-redisplay nil
	    :display-function 'display-device-under-design
            :scroll-bars t)
   ;; to be used for showing things like class diagrams
   ;; rather than a design in progress
   (alt-display :application
		:display-after-commands t
		:incremental-redisplay nil
		:display-function 'the-hack
		:scroll-bars t)
   (learned-facts :application
		  :max-height '(7 :line)
		  :display-after-commands nil
		  :scroll-bars :vertical)
   (pointer-doc :pointer-documentation
		:scroll-bars nil
                :borders t
                :max-height '(2 :line)
                :height '(2 :line))
   (current-design :application
		   :display-after-commands t
		   :incremental-redisplay nil
		   :height '(2 :line)
		   :max-height '(2 :line)
		   :display-function 'display-current-design-name
		   :scroll-bars nil)
   (menu :command-menu 
	 :height :compute
	 :display-function '(clim:display-command-menu
			     :x-spacing 30
			     :row-wise t
			     :n-columns 7)
	 :max-height '(6 :line)
	 :height '(4 :line)
	 :scroll-bars t
	 :borders t)
   (interactor :interactor
               :borders t
               :scroll-bars :vertical
	       :height '(5 :line)
               :max-height '(10 :line)))
  (:layouts
   (normal (clim:vertically ()
             (.6 display)
	     (.1 learned-facts)
             menu
             pointer-doc
	     current-design
	     (:fill interactor)))
   (alt (clim:vertically ()
             (.6 alt-display)
	     (.1 learned-facts)
             menu
             pointer-doc
	     current-design
	     (:fill interactor)))))

;;; Beyond the norma stuff that a top-level does
;;; this kicks off a process that repeatedly reads input from 
;;; an editor buffer which is assume to be speech input, but in any
;;; event is supposed to be parsable text.
;;; Whenever the process gets input it forces into the design-editor
;;; a command to process the text, I guess this has the same behavior
;;; as if a mouse blip had come in rather than typed text.
;;; TO BE DETERMINED: If in processing such text I want to ask another question
;;; for speech input, can I do that (using a different buffer) or will the two get confused.

;;; Emacs will put in the asterisks for you 
;;; doing this as a common parameter so that I don't 
;;; accidentally type it differently in two places
;;; and get 2 buffers

(defparameter *input-buffer-name* "*get commands*")
(defparameter *response-buffer-name* "*get response*")
(defparameter *waiting-for-read-command* :waiting-for-read-command)

(defmethod design-editor-top-level ((frame design-editor) &REST OPTIONS)
  ;; Note that you need to arrange to kill this process after
  ;; the editor-dies.  That's in the frame-exit method below
  (let ((*package* (find-package (string-upcase "natsoft")))
	(*debugger-hook* #'debugger-hook))
    (push (design-in-progress frame) (all-designs frame))
    ;; this doesn't solve the problem
    (APPLY #'clim:default-frame-top-level
	   frame
	   :prompt ">"
	   OPTIONS)))

(defmethod clim:default-frame-top-level :around ((frame design-editor) &key)
  ;; (setup-speech-process frame)
  (call-next-method))

(defmethod clim:frame-exit :before ((frame design-editor) &key)
  (let ((related-process (speech-input-process frame)))
    (when related-process
      (mp:process-kill related-process))))


(lep:define-query kill-buffer (buffer-name) (lep:simple-query)
  (:replyp nil)
  ("kill-buffer" buffer-name))

(Lep:define-query switch-to-buffer (buffer-name) (lep:simple-query)
  (:replyp nil) 
  ("fi::switch-to-buffer" buffer-name))

(lep:define-query set-common-lisp-mode 
    ()
  (lep:simple-query)
  (:replyp nil)
  ("fi:common-lisp-mode")
  )
	
(define-design-editor-command (com-restore-core-facts :name t :menu t)
    ((clean? 'boolean :default nil)
     &key (pathname 'clim:pathname 
		    :display-default t
		    :default #p"natsoft:code;core-facts.lisp"))
  (if pathname
      (restore-core-facts :clean? clean? :pathnames pathname)
    (restore-core-facts :clean? clean?)
    ))

(defmethod clim:read-frame-command :before ((frame design-editor) &key)
  (when (speech-enabled? frame)
    (switch-to-buffer :buffer-name *input-buffer-name*)
    ;; turn on the microphone
    (case (speech-engine frame)
      (siri
       #+macosx
       (run-ascript "natsoft:code;double-command-click.scpt" t))
      (dragon 
       #+macosx
       (run-ascript "natsoft:code;dragonmicon.scpt" t)
       ))
    ;; now activate emacs
    #+macosx
    (run-ascript "natsoft:code;activate-emacs.scpt" t)))

(defmethod clim:read-frame-command ((frame design-editor)
				    &key (stream *standard-input*) ;--- FRAME-STANDARD-INPUT?
					 ;; should the rest of the *command-parser*
					 ;; etc. variables be passed as keywords or bound?
					 )
  (let ((result (clim:read-command-or-string (clim:frame-command-table frame) :stream stream)))
    ;; turn off the mic
    (case (speech-engine frame)
      (siri 
       ;; for siri the very act of hitting the newline caused Siri to stop listening
       )
      ;; for dragon we need to turn the mic off
      (dragon #+macosx (run-ascript "natsoft:code;dragonmicoff.scpt")))
    ;; now go back to CLIM
    #+macosx (run-ascript "natsoft:code;activate-x.scpt" t)
    (if (stringp result)
	`(com-handle-speech-input ,result)
      result)))

(define-design-editor-command (com-enable-speech :name t :menu t)
    ((yes-or-no 'clim:boolean)
     &key (speech-system '(clim:member dragon siri) :default 'siri))
  (if yes-or-no
      (enable-speech-process clim:*application-frame* :speech-system speech-system)
    (disable-speech-process clim:*application-frame*)))

(defmethod enable-speech-process ((frame design-editor) &key speech-system)
  (unless (speech-enabled? clim:*application-frame*)
    (setf (speech-enabled? clim:*application-frame*) t
	  (speech-engine clim:*application-frame*) speech-system)
    (when (eql speech-system 'dragon) #+macosx (run-ascript "natsoft:code;startdragon.scpt" t))
    (setf (speech-input-process frame) 
      (mp:process-run-function "speech-input-for-MUSE"
	#'(lambda (frame buffer-name)
	    ;; it seems to be critical that this is outside the loop
	    (lep:with-editor-listener-stream (stream :name buffer-name)
	      (loop 
		  do (format stream "~%Do you have something to tell me~%")
		     (let ((input (read-line stream))) 
		       #+macosx (run-ascript "natsoft:code;activate-x.scpt" t)
		       (clim:execute-command-in-frame frame `(com-handle-speech-input ,input)))
		     )))
	frame *input-buffer-name*))))

(defmethod disable-speech-process ((frame design-editor) &key)
  (when (speech-enabled? frame)
    (case (speech-engine frame)
      (dragon #+macosx (run-ascript "natsoft:code;dragonmicoff.scpt" t))
      (siri "nothing to do"))
    (let ((process (speech-input-process frame)))
      (when process
	(mp:process-kill process))
      (setf (speech-enabled? frame) nil
	    (speech-input-process frame) nil))))

Something that;;; This is used when you want to take the initiative and query the user directly
(defun get-response ()
  (switch-to-buffer :buffer-name *response-buffer-name*)
  (case (speech-engine clim:*application-frame*)
    (siri #+macosx (run-ascript "natsoft:code;double-command-click.scpt" t))
    (dragon 
     ;; turn the mic on
     #+macosx
     (run-ascript "natsoft:code;dragonmicon.scpt" t)
     ))
  ;; then switch to emacs
  #+macosx
  (run-ascript "natsoft:code;activate-emacs.scpt" t)
  (let ((response (lep:with-editor-listener-stream (stream :name *response-buffer-name*)
		    (read-line stream))))
    ;; Toggle listening mode
      (case (speech-engine clim:*application-frame*)
	(siri 
	 ;; To answer he would have had to hit Carriage return which
	 ;; would have turned off SIRI listening
	 ;; (run-ascript "natsoft:code;double-command-click.scpt" t)
	 )
	(dragon 
	 ;; turn mic off
	 #+macosx
	 (run-ascript "natsoft:code;dragonmicoff.scpt" t)
	 ))
      ;; now switch back to CLIM
      #+macosx
    (run-ascript "natsoft:code;activate-x.scpt" t)
    (sleep 1)
    response)
  )

(define-design-editor-command (com-handle-speech-input :name t)
    ((text 'string))
  (unless (= (length text) 0)
    ;; this is then supposed to parse and process
    (let* ((parse (parse-text text)))
      (if (null (current-parse parse))
	  (clim:Notify-user clim:*application-frame*
			    "I couldn't parse what you said"
			    :title "Unparseable"
			    :style :warning
			    :foreground clim:+white+
			    :background (clim:find-named-color "red" (clim:frame-palette clim:*application-frame*)))
	(loop for main in (get-mains parse)
	    for connective = (name (relation main))
	    for found-one = nil
	    do (ask `[is-appropriate-response ,connective ,main ?function ?args]
		    #'(lambda (just)
			(declare (ignore just))
			(apply ?function parse main (copy-object-if-necessary ?args))
			(setq found-one t)
			;; (return-from do-it)
			))
	    unless found-one
	    do (clim:Notify-user clim:*application-frame*
				 "I couldn't figure out how to respond to that"
				 :title "Don't know what to do"
				 :style :warning
				 :foreground clim:+white+
				 :background (clim:find-named-color "red" (clim:frame-palette clim:*application-frame*)))
	       )))))

#|
      (cond
       ((string-equal connective "is-a")
	(handle-is-a parse main))
       ((string-equal connective "replace")
	(handle-replace-request parse main))
       ((string-equal connective "insert")
	(handle-insert-request parse main))
       ((or (string-equal connective "implement")
	    (string-equal connective "synthesize"))
	(handle-implement-request parse main))
       ((or (string-equal connective "make")
	    (string-equal connective "add"))
	(handle-make-request parse main))
       ((string-equal connective "can")
	(handle-can-request parse main))
       (t
	(print (string-for parse :text) *standard-output*)))
	)))  |#

(defrule is-a-handler (:backward)
  :then [is-appropriate-response |is-a| ?main handle-is-a ()]
  :if t)

(defrule replace-handler (:backward)
  :then [is-appropriate-response |replace| ?main handle-replace-request ()]
  :if t)

(defrule insert-handler (:backward)
  :then [is-appropriate-response |insert| ?main handle-insert-request ()]
  :if t)

(defrule implement-handler (:backward)
  :then [is-appropriate-response |implement| ?main handle-implement-request ()]
  :if t)

(defrule synthesize-handler (:backward)
  :then [is-appropriate-response |synthesize| ?main handle-implement-request ()]
  :if t)

(defrule generate-handler (:backward)
  :then [is-appropriate-response |generate| ?main handle-generate-request (?language)]
  :if [and [subject-of ?main ? |you|]
           [object-of ?main ? |code|]
           [as-subject ?main ?texp]
           [object-of ?texp ? |design|]
           [relation-of ?texp ? |for|]
           (unify ?language (find-language ?main))
           ])

;; I'm doing this as a function rather than clauses in the rule above
;; so that I can easily default to :lisp if the user doesn't mention a language
(defun find-language (main)
  (let ((language :lisp))
    ;; For some reason that I used to understand
    ;; the way I did these predicates doesn't interact
    ;; with the AND predicate the same way that it does
    ;; with the rule compiler, meaning you really have to
    ;; do the code as this set of nested stuff.
    (ask* `[as-subject ,main ?in-clause]
	  (ask* [relation-of ?in-clause ? |in|]
		(ask* [object-of ?in-clause ? ?language]
		      (setq language (intern (string-upcase (string ?language)) :keyword)))))
    language))


(defrule make-handler (:backward)
  :then [is-appropriate-response |make| ?main handle-make-request ()]
  :if t)

(defrule locate-handler (:backward)
  :then [is-appropriate-response ?verb ?main handle-locate-component-request (?verb-name ?object-name ?qualifier-name)]
  :if [and [or (eql ?verb '|locate|) (eql ?verb '|find|)]
	   [object-of ?main ?object |component|]
	   [as-subject ?object ?clause]
	   [relation-of ?clause ?description-verb ?verb-name]
	   ;; meaning its not a lexicon texp
	   (not (typep ?description-verb 'constant))
	   [object-of ?clause ?description-object ?object-name]
	   [as-subject ?description-object ?qualifier-clause]
	   [relation-of ?qualifier-clause ?rel |has_property|]
	   [object-of ?qualifier-clause ? ?qualifier-name]
	   ])

;;; Can ?x keep up with ?y
(defrule handle-keep-up (:backward)
  :then [is-appropriate-response |keep_up| ?texp handle-keep-up-request (?source-name ?consumer-name)]
  :if [and [relation-of ?texp ?relation |keep_up|]
	   [subject-of ?texp ? ?consumer-name]
	   [as-subject ?texp ?putative-with-clause]
	   [relation-of ?putative-with-clause ? |with|]
	   [object-of ?putative-with-clause ? ?source-name]
	   [as-subject ?texp ?putative-can-clause]
	   [relation-of ?putative-can-clause ? |has_modal|]
	   [object-of ?putative-can-clause ? |can|]
	   ])


(defrule faster-than (:backward)
  :then [is-appropriate-response |is| ?texp handle-keep-up-request (?source-name ?consumer-name)]
  :if [and [subject-of ?texp ? ?source-name]
	   [object-of ?texp  ? |faster|]
	   [as-subject ?texp ?putative-than-clause]
	   [relation-of ?putative-than-clause ? |than|]
	   [object-of ?putative-than-clause ? ?consumer-name]
	   ])

(defrule slower-than (:backward)
  :then [is-appropriate-response |is| ?texp handle-keep-up-request (?source-name ?consumer-name)]
  :if [and [subject-of ?texp ? ?consumer-name]
	   [object-of ?texp  ? |slower|]
	   [as-subject ?texp ?putative-than-clause]
	   [relation-of ?putative-than-clause ? |than|]
	   [object-of ?putative-than-clause ? ?source-name]
	   ])

(defrule compose-utterance (:backward)
  :then [is-appropriate-response |compose| ?texp handle-composition-request (?what ?cliche-name)]
  :if [and [subject-of ?texp ? |you|]
	   [object-of ?texp ?component |component|]
	   [as-subject ?component ?putative-this-clause]
	   [relation-of ?putative-this-clause ? |has_det|]
	   [object-of ?putative-this-clause ? |this|]
	   (unify ?what (first (selected-objects clim:*application-frame*)))
	   [as-subject ?texp ?putative-with-clause]
	   [relation-of ?putative-with-clause ? |with|]
	   [object-of ?putative-with-clause ? ?cliche-name]
	   ])

(defrule compose-utterance-2 (:backward)
  :then [is-appropriate-response |compose| ?texp handle-composition-request (?what ?cliche-name)]
  :if [and [subject-of ?texp ? |you|]
	   [object-of ?texp ?design |design|]
	   [as-subject ?design ?putative-property-clause]
	   [relation-of ?putative-property-clause ? |has_property|]
	   [or [object-of ?putative-property-clause ? |current|]
	       [object-of ?putative-property-clause ? |active|]]
	   (unify ?what (abstract-task (design-in-progress clim:*application-frame*)))
	   [as-subject ?texp ?putative-with-clause]
	   [relation-of ?putative-with-clause ? |with|]
	   [object-of ?putative-with-clause ? ?cliche-name]
	   ])

(defrule connect-utterance (:backward)
  :then [is-appropriate-response |connect| ?texp handle-connect-request (?from-what ?to-what)]
  :if [and [subject-of ?texp ? |you|]
	   [object-of ?texp ? |component|]
	   [as-subject ?texp ?putative-with-clause]
	   [or [relation-of ?putative-with-clause ? |with|]
	       [relation-of ?putative-with-clause ? |to|]]
	   [object-of ?putative-with-clause ? ?start-version-of-to-what]
	   (unify ?from-what (first (selected-objects clim:*application-frame*)))
	   (unify ?to-what (convert-start-string-to-lisp-atom ?start-version-of-to-what))
	   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff to support PASCALI integration demo
;;; 
;;; An array is a sequence in Java
;;; You can test whether a sequence is sorted by ...
;;; Can you tell if "sorted" occurs frequently in the corpus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule java-class-is-of-type (:backward)
  :then [is-appropriate-response |is-a| ?main handle-java-subtype-statement (?java-class-name ?ontology-type-name)]
  :if `[and [object-of ?main ? ?start-version-of-ontology-type-name]
	    [unify ,(convert-start-string-to-lisp-atom ?start-version-of-ontology-type-name) ?ontology-type-name]
	    (is-a-type? ?ontology-type-name)
	    [subject-of ?main ? ?java-class-name]
	    [as-subject ?main ?putative-in-clause]
	    [relation-of ?putative-in-clause ? |in|]
	    [object-of ?putative-in-clause ? |Java|]
	    ])

(defun convert-start-string-to-java-name (name)
  (let ((name (string name)))
    (intern (substitute #\. #\_ name))))

(defparameter *learned-facts-style* (clim:make-text-style :fix :bold :large))

;;; For each java subtype statement just squirl it away in a file
(defun handle-java-subtype-statement (parse main java-class-name ontology-type-name)
  (declare (ignore main parse))
  (let* ((learned-facts-pane (clim:get-frame-pane clim:*application-frame* 'learned-facts))
	 (new-type (convert-start-string-to-java-name java-class-name)))
    (clim:with-text-style (learned-facts-pane *learned-facts-style*)
      (format learned-facts-pane "~%~a is a kind of ~a" new-type ontology-type-name)
      (unless (probe-file "natsoft:code;learned-stuff.lisp")
	    (with-open-file (f "natsoft:code;learned-stuff.lisp" :direction :output :if-does-not-exist :create)
	      (format f ";;; -*- Mode: Common-lisp; Package: Natsoft; Readtable: Joshua -*~%")))
      (with-open-file (f "natsoft:code;learned-stuff.lisp"
		       :direction :output 
		       :if-exists :append
		       :if-does-not-exist :create)
	(let ((form `(defdata-type ,new-type
			 :super-types (,ontology-type-name)
			 )))
	  (print form f)))
      )
    (with-open-file (F "natsoft:code;subtypes.text"
		     :direction :output 
		     :if-exists :append 
		     :if-does-not-exist :create)
      (format f "~%~a, ~a" ontology-type-name new-type))))

;; can you test whether
(define-predicate decompose-test (test arguments) (default-predicate-model))

(defrule how-to-test-utterance (:backward)
  :then [is-appropriate-response |whether| ?texp handle-test-definition 
				  (?test-object-name ?test-result-word 
				   ?relation-name ?quantifier-name ?property-name)
				  ]
  :if [and [or [subject-of ?texp ?test |test|]
	       [subject-of ?texp ?test |determine|]
	       [subject-of ?texp ?test |tell|]
	       [subject-of ?texp ?test |check|]]
	   ;; You test
	   [as-relation ?test ?putative-you-clause]
	   [subject-of ?putative-you-clause ? |you|]
	   ;; Can modality
	   [as-subject ?putative-you-clause ?putative-modal-clause]
	   [relation-of ?putative-modal-clause ? |has_modal|]
	   [object-of ?putative-modal-clause ? |can|]
	   ;; What can you determine
	   [or [object-of ?texp ?if |whether|] [object-of ?texp ?if |if|]]
	   [as-relation ?if ?if-clause]
	   [subject-of ?if-clause ?test-result-instance ?test-result-word]
	   ;; the test itself
	   ;; What object you need to perform the test on
	   [as-relation ?test-result-instance ?testing-clause]
	   [or [subject-of ?testing-clause ? |testing|]
	       [subject-of ?testing-clause ? |checking|]]
	   [object-of ?testing-clause ?test-object ?test-object-name]
	   ;; What test do you perform on that object
	   [object-of ?if-clause ?test-to-be-performed ?]
	   [decompose-test ?test-to-be-performed (?relation-name ?quantifier-name ?property-name)]
	   ])

;; you can test if
(defrule how-to-test-utterance-2 (:backward)
  :then [is-appropriate-response |if| ?texp handle-test-definition ?answer]
  :if [is-appropriate-response |whether| ?texp handle-test-definition ?answer]
  )



(defrule decompose-comparison (:backward)
  :then [decompose-test ?instance (?relation-name ?quantifier-name ?property-name)]
  :if [and 
	   [as-relation ?instance ?is-clause]
	   ;; what are we comparing
	   [or [subject-of ?is-clause ?subject |element|]
	       [subject-of ?is-clause ?subject |member|]]
	   ;; what the relationship is
	   [object-of ?is-clause ? ?relation-name]
	   ;; Who what are comparing to
	   [as-subject ?is-clause ?comparison-clause]
	   [relation-of ?comparison-clause ? |than|]
	   [object-of ?comparison-clause ?object ?]
	   ;; How is the subject quantified
	   [as-subject ?subject ?putative-quantifier-clause]
	   [relation-of ?putative-quantifier-clause ? |has_quantifier|]
	   [object-of ?putative-quantifier-clause ? ?quantifier-name]
	   ;; How is the object described
	   [as-subject ?object ?putative-property-clause]
	   [relation-of ?putative-property-clause ? |has_property|]
	   [object-of ?putative-property-clause ? ?property-name]])

(defrule look-for-frequent-corpus-2 (:backward)
  :then [is-appropriate-response |whether| ?texp handle-look-in-corpus-request (?property ?frequency)]
  :if [is-appropriate-response |if| ?texp handle-look-in-corpus-request (?property ?frequency)])

(defrule look-for-frequent-corpus (:backward)
  :then [is-appropriate-response |if| ?texp handle-look-in-corpus-request (?property ?frequency)]
  :if [and 
       ;; The verb is the subject in this case
       ;; Make sure it's a directive to investigate
       [or [subject-of ?texp ?subject |see|]
	   [subject-of ?texp ?subject |tell|]
	   [subject-of ?texp ?subject |determine|]]
       ;; Make sure it's either an imperative
       ;; Or a modal request like can you determine
       [as-relation ?subject ?see-clause]
       ;; the subject must be "You"
       [subject-of ?see-clause ? |you|]
       ;; And the "see" clause either has modality "can"
       ;; or it's an imperative
       [as-subject ?see-clause ?putative-modality-clause]
       [or [and [relation-of ?putative-modality-clause ? |has_modal|]
		[object-of ?putative-modality-clause ? |can|]]
	   [and [relation-of ?putative-modality-clause ? |is_imperative|]
		[object-of ?putative-modality-clause ? |Yes|]]]
       ;; Is it asking about occurences in the network
       [object-of ?texp ?object |occur|]
       ;; And what is it asking about
       [as-relation ?object ?putative-occurs-clause]
       [subject-of ?putative-occurs-clause ? ?property]
       ;; And is it ask about the corpus
       [and [as-subject ?putative-occurs-clause ?putative-corpus-clause]
	    [relation-of ?putative-corpus-clause ? |in|]
	    [object-of ?putative-corpus-clause ? |corpus|]]
       ;; And it asks about frequent 
       [and [as-subject ?putative-occurs-clause ?putative-frequently-clause]
	    [relation-of ?putative-frequently-clause ? |has_modifier|]
	    [object-of ?putative-frequently-clause ? ?frequency]]
       ]
  )

(defun handle-look-in-corpus-request (parse main concept frequency)
  (declare (ignorable main parse concept frequency))
  (let* ((subtypes-pathname #p"natsoft:code;subtypes.text")
	 (subtypes-truename (truename subtypes-pathname))
	 (subtypes-string (namestring subtypes-truename))
	 (command "python ~/integration-test/frontend.py")
	 (command (format nil "~a ~a" command subtypes-string))
	 (answer-pathname "/home/hes/integration-test/ordering_results/typicalset.txt"))
    (let* ((learned-facts-pane (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
      (clim:window-clear learned-facts-pane)
      (clim:with-text-style (learned-facts-pane *learned-facts-style*)
	(format learned-facts-pane "~%Looking in the corpus for instances of ~a" concept)
	(format learned-facts-pane "~%Get some coffee, this might take a while")
	(force-output learned-facts-pane)
	))
    (switch-to-buffer :buffer-name "*common-lisp*")
    (when (probe-file answer-pathname)
      (delete-file answer-pathname))
    (excl:run-shell-command command :wait t)
    (look-for-message-file answer-pathname)
    ))

(defrule look-for-count-in-corpus (:backward)
  :then [is-appropriate-response |find| ?main find-count-in-corpus (?property ?count)]
  :if [and 
       ;; you find examples
       [subject-of ?main ? |you|]
       [object-of ?main ?examples |examples|]
       ;; find in the corpus
       [as-subject ?main ?putative-in-clause]
       [relation-of ?putative-in-clause ? |in|]
       [object-of ?putative-in-clause ? |corpus|]
       ;; examples of what
       [as-subject ?examples ?putative-examples-clause]
       [relation-of ?putative-examples-clause ? |related-to|]
       [object-of ?putative-examples-clause ? ?property]
       ;; count = x
       [or [and
	    [as-subject ?main ?putative-count-clause]
	    [relation-of ?putative-count-clause ? |for|]
	    [object-of ?putative-count-clause ? ?count-symbol]]
	   [and
	   [as-subject ?examples ?putative-quantity-clause]
	   [relation-of ?putative-quantity-clause ? |has_quantity|]
	   [object-of ?putative-quantity-clause ? ?count-symbol]]]
       (unify ?count (read-from-string (string ?count-symbol)))
       ]
       )

(defun find-count-in-corpus (parse main concept count)
  (declare (ignorable main parse concept))
  (let* ((subtypes-pathname #p"natsoft:code;subtypes.text")
	 (subtypes-truename (truename subtypes-pathname))
	 (subtypes-string (namestring subtypes-truename))
	 (command "python ~/integration-test/frontend.py")
	 (command (format nil "~a ~a ~d" command subtypes-string count))
	 (answer-pathname "/home/hes/integration-test/ordering_results/typicalset.txt"))
    (let* ((learned-facts-pane (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
      (clim:window-clear learned-facts-pane)
      (clim:with-text-style (learned-facts-pane *learned-facts-style*)
	(format learned-facts-pane "~%Looking in the corpus for ~s instances of ~a" 
		count concept)
	(format learned-facts-pane "~%Get some coffee, this might take a while")
	(force-output learned-facts-pane)
	))
    (switch-to-buffer :buffer-name "*common-lisp*")
    (when (probe-file answer-pathname)
      (delete-file answer-pathname))
    (excl:run-shell-command command :wait t)
    (look-for-message-file answer-pathname)
    ))

(defun look-for-message-file (&optional (pathname "/home/hes/integration-test/ordering_results/typicalset.txt"))
  (loop for file-exists = (probe-file pathname)
      when file-exists
      do (ed pathname)
	 (delete-file pathname)
	 (return-from look-for-message-file)
      do (sleep 5)))

(defun process-message-file (&optional (pathname "natsoft:code;update-message.text"))
  (let (concept setters (*package* (find-package :natsoft)))
    (with-open-file (f pathname)
      (let ((form (read f)))
	(destructuring-bind (c frequency) form
	  (when (eql frequency 'frequent)
	    (setq concept c
		  setters (read f))))))
    (when concept
      (with-open-file (f "natsoft:code;learned-stuff.lisp"
		       :direction :output 
		       :if-exists :supersede
		       :if-does-not-exist :create)
	(let ((form `(deftype ,concept
			 :super-types (spatial-sequence)
			 )))
	  (print form f))
	(loop for setter in setters
	    for form = `(deftask ,setter
			    :primitive t
			    :parameters (sequence-type)
			    :bindings ((element-type (second sequence-type)))
			    :interface ((:inputs (the-sequence (spatial-sequence element-type)))
					(:outputs (the-sorted-sequence (sorted-sequence element-type))))
			    )
	    do (print form f)))
      (restore-core-facts :clean? nil :pathnames (list "natsoft:code;learned-stuff.lisp")))))


(defrule design-pattern (:backward)
  :then [is-appropriate-response |follow| ?main guide-synthesis (?pattern)]
  :if [and [subject-of ?main ? |design|]
	   [object-of ?main ? ?object]
	   (Unify ?pattern (check-and-extract-pattern ?object))
	   (not (null ?pattern))
	   ])

(defun check-and-extract-pattern (noun)
  (let* ((string (string noun))
	 (position (position #\_ string :test #'char-equal :from-end t))
	 (putative-pattern (subseq string (1+ position)))
	 (pattern-name (subseq string 0 position)))
    (when (string-equal putative-pattern "pattern")
      (convert-start-string-to-lisp-atom pattern-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *integration-listener* nil)

(defun start-integration-process ()
  (setq *integration-listener*
    (mp:process-run-function
	"Design-Editor Integration Listener"
      #'look-for-message-file
      )))
  
(define-design-editor-command (com-start-integration-listener :name t)
    ()
  (unless *integration-listener*
    (start-integration-process)))

(define-design-editor-command (com-stop-integration-listener :name t)
    ()
  (when *integration-listener* 
    (mp:process-kill *integration-listener*)
    (setq *integration-listener* nil)))

(defun run-ascript (script-pathname &optional (wait t))
  (let ((command (format nil "osascript ~a" (truename script-pathname))))
    ;; (format t "~%~a" command)
    (excl:run-shell-command command :wait wait)
    command))



(clim:define-presentation-type basic-task
    ()
  )

(clim:define-presentation-method clim:present (task (type basic-task) stream
						 (view clim:textual-view)
						 &key)
  (print-pathname task stream (design-in-progress clim:*application-frame*)))

(clim:define-presentation-method clim:accept ((type basic-task) stream (view clim:textual-view) &key)
  (let* ((top (design-in-progress clim:*application-frame*))
	 (children (children top)))
  (clim:completing-from-suggestions (stream :partial-completers '(#\.))
    (loop for m in children
	do (clim:suggest (with-output-to-string (string-stream) 
			   (print-pathname m string-stream top))
			 m)))))

(clim:define-presentation-type branch
    ()
  )

(clim:define-presentation-method clim:present (branch (type branch) stream
						 (view clim:textual-view)
						 &key)
  (print-pathname branch stream (design-in-progress clim:*application-frame*)))

(clim:define-presentation-method clim:accept ((type branch) stream (view clim:textual-view) &key)
  (let* ((top (design-in-progress clim:*application-frame*))
	 (children (children top)))
    (clim:completing-from-suggestions 
     (stream :partial-completers '(#\.))
     (loop for child in children
	 when (typep child 'has-branches-mixin)
	 do (loop for branch in (branches child)
		do (clim:suggest (with-output-to-string (string-stream) 
				   (print-pathname branch string-stream top))
				 branch))))))

(clim:define-presentation-type join
    ()
  )

(clim:define-presentation-method clim:present (join (type join) stream
						 (view clim:textual-view)
						 &key)
  (print-pathname join stream (design-in-progress clim:*application-frame*)))

(clim:define-presentation-method clim:accept ((type join) stream (view clim:textual-view) &key)
  (let* ((top (design-in-progress clim:*application-frame*))
	 (children (children top)))
    (clim:completing-from-suggestions 
     (stream :partial-completers '(#\.))
     (loop for child in children
	 when (typep child 'has-joins-mixin)
	 do (loop for join in (joins child)
		do (clim:suggest (with-output-to-string (string-stream) 
				   (print-pathname join string-stream top))
				 join))))))



(clim:define-presentation-type composite-task
			       ()
  )

(clim:define-presentation-type task-interface
			       ()
  )

(clim:define-presentation-type port (direction)
  )

(clim:define-presentation-method clim:presentation-typep (object (type port))
  (or (eq direction '*)
      (eql (direction object) direction)))

(clim:define-presentation-method clim:presentation-subtypep ((type1 port) type2)
  (let ((direction1 (clim:with-presentation-type-parameters (port type1) 
		      direction))
	(direction2 (clim:with-presentation-type-parameters (port type2)
		      direction)))
    (values (or (eql direction1 direction2)
		(eq direction2 '*))
	    t)))

(clim:define-presentation-method clim:present (port (type port) stream (view clim:textual-view) &key)
  (let* ((task (task port)))
    (typecase task
      (branch 
       (let* ((branch-name (name task))
	      (task (superior task))
	      (task-name (name task)))
	 (format stream "~a-~a.~a.~a"
		 task-name branch-name
		 (direction port)
		 (name port))))
      (join
       (let* ((join-name (name task))
	      (task (superior task))
	      (task-name (name task)))
	 (format stream "~a-~a.~a.~a"
		 task-name join-name
		 (direction port)
		 (name port))))
      (otherwise
       (let* ((task-name (name task)))
	 (format stream "~a.~a.~a"
		 task-name 
		 (direction port)
		 (name port))))
      )))
       
(clim:define-presentation-method clim:describe-presentation-type ((type port) stream plural-count)
  (let ((exactly-one (and (integerp plural-count) (= plural-count 1))))
    (if (eql direction '*)
	(if exactly-one
	    (format stream "a port")
	  (format stream "ports"))
      (if exactly-one
	  (format stream "an ~a port" direction)
	(format stream "~a ports" direction)))))

(clim:define-presentation-type-abbreviation input-port ()
  `(port input))

(clim:define-presentation-type-abbreviation output-port ()
  `(port output))

(defmethod ports-containers-of ((thing joining-task))
  (cons 
   (cons (name thing) thing)
   (loop for join in (joins thing) collect (cons (name join) join))))

(defmethod ports-containers-of ((thing has-branches-mixin))
  (cons 
   (cons (name thing) thing)
   (loop for branch in (branches thing) collect (cons (format nil "~a-~a" (name thing) (name branch)) branch))))

(defmethod ports-containers-of ((thing has-joins-mixin))
  (cons 
   (cons (name thing) thing)
   (loop for join in (joins thing) collect (cons (format nil "~a-~a" (name thing) (name join)) join))))

(defmethod ports-containers-of (thing)
  (list (cons (name thing) thing)))

;;; Good example of how to do multiple field ptypes
(clim:define-presentation-method clim:accept ((type port) stream (view clim:textual-view) &key)
  (clim:with-delimiter-gestures (#\.)
    (let (task direction port delimiter)
      (clim:with-accept-help ((:subhelp #'(lambda (stream action string)
					    (declare (ignore action string))
					    (write-string "Enter task name" stream))))
	(setq task (clim:accept `(clim:member-alist ,(loop for child in (children (design-in-progress clim:*application-frame*))
							 append (ports-containers-of child)))
				:view view
				:prompt nil
				:stream stream)))
      (when (eql task nil)
	(return-from clim:accept (list task nil)))
      ;; Read the delimiter -- it should be a period, but if it is not, signal a parse-error.
      (setq delimiter (clim:stream-peek-char stream))
      (cond 
       ((char-equal delimiter #\.)
	(clim:stream-read-char stream)
	(clim:with-accept-help ((:subhelp #'(lambda (stream action string)
					      (declare (ignore action string))
					      (write-string "Enter the port direction" stream))))
	  (setq direction (clim:accept '(clim:member input output) :stream stream :view view :prompt nil)))
	(setq delimiter (clim:stream-peek-char stream))
	(cond ((char-equal delimiter #\.)
	       (clim:stream-read-char stream)
	       (clim:with-accept-help ((:subhelp #'(lambda (stream action string)
						     (declare (ignore action string))
						     (write-string "Enter the port name" stream))))
		 (let ((ports (case direction (input (inputs task)) (output (outputs task)))))
		   (setq port (clim:accept `(clim:member-alist ,(loop for port in ports collect (cons (name port) port)))
					   :prompt nil
					   :stream stream
					   :view view)))))
	      (t (clim:simple-parse-error "Invalid delimiter: ~S" delimiter))))
       (t (clim:simple-parse-error "Invalid delimiter: ~S" delimiter)))
      port)))



(defmacro with-fresh-viewport ((stream) &body body)
  "Ensures that output from body starts on a fresh viewport on stream."
  `(let ((window (find-real-stream ,stream)))
     (window-fresh-viewport window)
     ,@body))

(defun find-real-stream (stream)
  "Recurses down syn-stream indirections to find real stream."
  (cond #+genera
        ((and (symbolp stream)
	      (search "SYN-STREAM" (symbol-name stream) :test #'char-equal :from-end t))
	 (find-real-stream (cdr (scl:locf (scl:symbol-function stream)))))
	(t stream)))

(defmethod window-fresh-viewport ((window clim:sheet))
  "Positions the viewport over fresh window space."
  (fresh-line window)				;must be on the right line
  (multiple-value-bind (x y)
      (clim:stream-cursor-position window)
    (declare (ignore x))
    (clim:window-set-viewport-position window 0 y)
    (clim:note-viewport-position-changed
      (window-stream-to-application-frame window) window 0 y)))

(defun window-stream-to-application-frame (window)
  "Returns the application frame controlling window."
  (declare (inline))
  (let ((toplevel (clim:window-top-level-window window)))
    (clim:pane-frame toplevel)))



;;; This is the display method for the "known options" pane of the
;;; frame.  

(defvar *all-devices* nil)

(defmethod display-devices ((frame design-editor) stream
			     &key max-width max-height &allow-other-keys)
  (clim:with-text-size (stream :small)
      (clim:formatting-item-list (stream :max-width (or max-width
							(clim:window-inside-size stream))
					 :max-height max-height
					 :row-wise t)
	(loop for item in *all-devices*
	      do (clim:formatting-cell (stream)
		   (clim:with-text-face (stream :bold)
		     (clim:with-text-size (stream :large)
		       (clim:present item 'composite-task :stream stream))))))))

(defgeneric task-component-children (task-component))
(defgeneric display-task-component (task-component stream))
(defgeneric task-network-arrow-drawer (collector from-object to-object x1 y1 x2 y2))

(defmethod is-initial-task ((task input-side-mixin))
  (let ((his-inputs (inputs task)))
    (and (null (is-target-of-branch task))
	 (or (null his-inputs)
	     (every #'(lambda (port) (null (incoming-flows port)))
		    his-inputs)))))

(defmethod is-initial-task ((task has-joins-mixin))
  (every #'(lambda (join) (is-initial-task join))
	 (joins task)))


(defmethod display-current-design-name ((frame design-editor) stream)
  (let* ((current-design (design-in-progress frame))
	 (design-name (when current-design (name current-design))))
    (if (and design-name (typep current-design 'implementation))
	(let ((path (implementation-path current-design)))
	  (loop for designs on path
	      for this-design = (first designs)
	      do (clim:present this-design 'design :stream stream)
		 (when (rest designs)
		   (format stream " <-- "))))
      (format stream "Not an implementation of anything"))))

(defmethod implementation-path ((start implementation))
  (cons start
	(loop for current-implementation = start then his-parent
	    for task = (abstract-task current-implementation)
	    for his-parent = (when task (superior task))
	    until (not (typep his-parent 'implementation))
	    collect his-parent)))


(defmethod display-device-under-design ((frame design-editor) stream)
  (display-task (design-in-progress frame) stream))

(defun print-task (task pathname)
  (with-open-file (file-stream pathname :direction :output :if-exists :supersede)
    (clim:with-output-to-postscript-stream (stream file-stream)
      (display-task task stream))))

(defmethod print-device-under-design ((frame design-editor) pathname)
  (print-task (design-in-progress frame) pathname))

(clim:define-gesture-name :delete :pointer-button (:middle :control))
(clim:define-gesture-name :alternate-delete :pointer-button (:middle :meta))
(clim:define-gesture-name :add-select :pointer-button (:left :super))
(clim:define-gesture-name :switch-to-design :pointer-button (:left :super :meta))
(clim:define-gesture-name :switch-to-selected-implementation :pointer-button (:middle :super :meta))
(clim:define-gesture-name :produce-implementations :pointer-button (:right :super :meta))
(clim:define-gesture-name :add-control-flow :pointer-button (:right :control))

(define-design-editor-command (com-start-new-design :name t :menu t)
    ((name 'symbol))
  (setf (design-in-progress clim:*application-frame*)
    (make-instance 'composite-task
      :name name))
  (incf (display-output-tick clim:*application-frame*)))

(clim:define-presentation-type-abbreviation task-type nil
  `((member ,@(loop for type being the hash-keys of *task-type-hash-table*
		  collect type))
    :partial-completers '(#\,)
    ))

(define-design-editor-command (com-rename-component :name t :menu t)
    ((task 'basic-task)
     (new-name 'symbol))
  (incf (display-output-tick clim:*application-frame*))
  (incf (display-output-tick task))
  (setf (name task) new-name)
  )

;;; ToDo: Make this type take an argument, the type,
;;; and then it alternates between member (gethash task-type *task-type-hash-table*), and clim:expression
(clim:define-presentation-type-abbreviation property-list nil
  '(clim:sequence (clim:sequence-enumerated symbol clim:expression)))

(clim:define-presentation-type-abbreviation type-parameter-list (type)
  `(clim:sequence (or (clim:sequence-enumerated (member ,@(properties-of-type type)) clim:expression)
		      (clim:member-alist (("None" . nil)))
		      )))

(define-design-editor-command (com-add-component :name t :menu t)
    ((name 'symbol)
     (type 'task-type)
     (properties `(type-parameter-list ,type)))
  (add-component clim:*application-frame* name type (remove 'nil properties))
  )

(defmethod add-component ((frame design-editor) (name symbol) type properties)
  (incf (display-output-tick frame))
  (let* ((design-in-progress (design-in-progress frame))
	 (new-task (apply #'create-task name type design-in-progress 
			   (loop for (key value) in properties collect (intern (string key) :keyword) collect value))))
    (select-object new-task frame)
    new-task
    ))

;;; Allows me to specify a number of inputs to the make-instance/new 
;;; which are sort of unique to the type we're allocating.
;;; Particularly in Java you can have different new methods with different signatures
(defmethod create-task :around ((name t) (type (eql 'allocate)) (parent t) &rest properties)
  (let* ((the-allocator (call-next-method))
         (inputs (getf properties :inputs)))
    (loop for (name type) on inputs by #'cddr
        for port =  (add-port 'input name the-allocator)
        do (tell `[port-type-constraint ,port ,type])) 
    the-allocator
    ))

(define-design-editor-command (com-remove-component :name t :menu t)
    ((component 'basic-task))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (incf (display-output-tick clim:*application-frame*))
    (unselect-object component clim:*application-frame*)
    (remove-child design-in-progress component)
    ))

(clim:define-presentation-to-command-translator kill-component
    (basic-task com-remove-component design-editor
		:gesture :delete)
  (object)
  (list object))

(define-design-editor-command (com-add-state-variable :name t)
    ((name 'symbol)
     (direction '(clim:member source sink))
     (type 'clim:expression)
     &key (port-name 'symbol)
     (state-name 'symbol))
  (incf (display-output-tick clim:*application-frame*)) 
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (case direction
      (source
       (add-state-source design-in-progress name (or port-name name) type state-name))
      (sink
       (add-state-sink design-in-progress name (or port-name name) type state-name)
       ))))

(define-design-editor-command (com-add-constant :name t :menu t)
    ((name 'symbol)
     (type 'clim:expression)
     (value 'clim:expression)
     &key (local? 'boolean :default nil))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-constant design-in-progress name type value local?)))

(define-design-editor-command (com-add-initial-input :name t :menu t)
    ((name 'symbol)
     (type 'clim:expression)
     &key (branch-name 'symbol))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-initial-input design-in-progress name name type branch-name)))

(define-design-editor-command (com-add-final-output :name t :menu t)
    ((name 'symbol)
     (type 'clim:expression)
     &key
     (branch-name 'symbol))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-final-output design-in-progress name name type branch-name)))

(define-design-editor-command (com-add-port :name t :menu t)
    ((component 'basic-task)
     (direction '(member input output))
     (name 'symbol)
     &key (type-description 'clim:expression))
  (incf (display-output-tick clim:*application-frame*))
  (let ((port (add-port direction name component)))
    (when type-description
      (add-port-type-description port type-description))))

(define-design-editor-command (com-add-entry-point :name t :menu t)
    ((name 'symbol)
     (entry-point-name 'symbol)
     &key (ports 'property-list))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-entry-point design-in-progress name entry-point-name ports)))

(define-design-editor-command (com-add-exit-point :name t :menu t)
    ((name 'symbol)
     (exit-point-name 'symbol)
     &key (ports 'property-list))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-exit-point design-in-progress name exit-point-name ports)))

(define-design-editor-command (com-add-path-end :name t :menu t)
    ((name 'symbol)
     &key (ports 'property-list))
  (incf (display-output-tick clim:*application-frame*))
  (let ((design-in-progress (design-in-progress clim:*application-frame*)))
    (add-path-end design-in-progress name ports)))


(defmethod select-object ((object core-task-mixin) (editor design-editor))
  (loop for object in (selected-objects editor)
      do (unselect-object object editor))
  (setf (selected? object) t)
  (push object (selected-objects editor)))

(defmethod add-select-object ((object core-task-mixin) (editor design-editor))
  (setf (selected? object) t) 
  (push object (selected-objects editor)))

(defmethod unselect-object ((object core-task-mixin) (editor design-editor))
  (setf (selected? object) nil)
  (setf (selected-objects editor)
	(remove object (selected-objects editor))))

(define-design-editor-command (com-select-object :name t :menu t)
    ((object 'basic-task))
  (select-object object clim:*application-frame*))

(clim:define-presentation-to-command-translator click-to-select
    (basic-task com-select-object design-editor
		:gesture :select)
  (object)
  (list object))

(define-design-editor-command (com-add-select-object :name t :menu t)
    ((object 'basic-task))
  (add-select-object object clim:*application-frame*))

(clim:define-presentation-to-command-translator click-to--addselect
    (basic-task com-add-select-object design-editor
		:gesture :add-select)
  (object)
  (list object))

(define-design-editor-command (com-unselect-object :name t :menu t)
    ((object 'basic-task))
  (unselect-object object clim:*application-frame*))

(define-design-editor-command (com-set-type-description :name t :menu t)
    ((p '(port *))
     (description 'clim:expression))
  (remove-port-type-descriptions p)
  (add-port-type-description p description))

(define-design-editor-command (com-remove-port :name t :menu t)
    ((port '(port *)))
  (incf (display-output-tick clim:*application-frame*))
  (let ((direction (direction port))
	(name (name port))
	(component (task port)))
    (remove-port direction name component)))

(clim:define-presentation-to-command-translator kill-port
    ((port *) com-remove-port design-editor
	      :gesture :delete)
  (object)
  (list object))

(defmethod line-dragging-drag-and-drop-feedback
    ((frame design-editor) presentation stream
     initial-x initial-y new-x new-y state)
  (declare (ignore state presentation))		;we'll just use XOR
  (clim:with-output-recording-options (stream :record nil)
    (clim:draw-line* stream new-x new-y initial-x initial-y
		     :ink clim:+flipping-ink+)))


(define-design-editor-command (com-add-dataflow :name t :menu t)
    ((from-port 'output-port)
     (to-port 'input-port))
  (make-dataflow 'output (name from-port) (task from-port)
		 'input (name to-port) (task to-port))
  (incf (display-output-tick clim:*application-frame*)))

;; It appears that using presentation-abbreviations here doesn't
;; work.
(clim:define-drag-and-drop-translator dragging-add-dataflow
    ((port output) clim:command (port input) design-editor
		   :feedback line-dragging-drag-and-drop-feedback
		   :documentation "add a dataflow")
  (object destination-object)
  `(com-add-dataflow ,object ,destination-object))

(define-design-editor-command (com-remove-dataflow :name t :menu t)
    ((from-port 'output-port)
     (to-port 'input-port))
  (incf (display-output-tick clim:*application-frame*))
  (let ((flow (dataflow-from from-port to-port)))
    (when flow 
      (remove-dataflow flow))))

;;; it appears that using the presenation-abbreviation here input-port
;;; doesn't work.
(clim:define-drag-and-drop-translator dragging-kill-dataflow
    (output-port clim:command input-port design-editor
		 :feedback line-dragging-drag-and-drop-feedback
		 :gesture :alternate-delete
		 :documentation "remove a dataflow")
  (object destination-object)
  ;; There really can only be 1 incoming flow to a port
  `(com-remove-dataflow ,object ,destination-object))

(define-design-editor-command (com-add-controlflow :name t :menu t)
    ((from '(or branch basic-task) :prompt "a task or a branch")
     (to-task '(or join basic-task) :prompt "a task or a join branch"))
  (incf (display-output-tick clim:*application-frame*))
  (make-control-flow from to-task))

(clim:define-drag-and-drop-translator dragging-add-controlflow
    ((or branch basic-task) clim:command basic-task design-editor
	    :feedback line-dragging-drag-and-drop-feedback
	    :documentation "add a controlflow" 
	    :gesture :add-control-flow
	    )
  (object destination-object)
  `(com-add-controlflow ,object ,destination-object))

(define-design-editor-command (com-remove-controlflow :name t)
    ((from-branch '(or branch basic-task))
     (to-task '(or join basic-task)))
  (incf (display-output-tick clim:*application-frame*))
  (let ((the-flow (control-flow-from from-branch to-task)))
    (remove-control-flow the-flow)
    ))

(clim:define-drag-and-drop-translator dragging-remove-controlflow
    (branch clim:command basic-task design-editor
	    :feedback line-dragging-drag-and-drop-feedback
	    :gesture :alternate-delete
	    :documentation "remove a controlflow"  
	    )
  (object destination-object)
  `(com-remove-controlflow ,object ,destination-object))

(define-design-editor-command (com-add-property :name t)
    ((task 'basic-task)
     (property 'symbol)
     (value 'clim:expression))
  (setf (getf (properties task) (intern (string property) :keyword))
    value))

(define-design-editor-command (com-refresh :name t)
                    ()
  (incf (display-output-tick clim:*application-frame*))
  ;; this is an enormous kludge to force the command-menu to get redisplayed
  ;; when cilm screws up
  (incf (slot-value (clim:find-command-table 'design-editor) 'clim-internals::menu-tick))
  (incf (display-output-tick clim:*application-frame*))
  (clim:window-clear (clim:get-frame-pane clim:*application-frame* 'display))
  (values))

(define-design-editor-command (com-change-task-type :name t :menu t)
    ((task 'basic-task)
     (new-type 'task-type))
  (rebuild-task-for-new-type new-type task)
  )

(define-design-editor-command (com-clear-design :name t :menu t)
    (&key (just-this-design 'clim:boolean :default t))
  (clim:window-clear (clim:get-frame-pane clim:*application-frame* 'interactor))
  (clim:window-clear (clim:get-frame-pane clim:*application-frame* 'learned-facts))
  (let ((dip (design-in-progress clim:*application-frame*)))
    (loop for component in (children dip)
	do (remove-child dip component)))
  (unless just-this-design
    (setf (all-designs clim:*application-frame*) nil)
    (setf (design-in-progress clim:*application-frame*) (make-instance 'implementation :name 'initial-design))
    (push (design-in-progress clim:*application-frame*) (all-designs clim:*application-frame*))
    (restore-core-facts :clean? t))
  )

(define-design-editor-command (com-clean-windows :name t)
    ()
  (clim:window-clear (clim:get-frame-pane clim:*application-frame* 'interactor)) 
  (clim:window-clear (clim:get-frame-pane clim:*application-frame* 'learned-facts))
  )

(define-design-editor-command (com-change-track-spacing :name t)
    ((spacing 'integer))
  (setq clim-internals::*track-spacing* spacing)
  (values))

(clim:define-presentation-type design
    ()
  )

(clim:define-presentation-method clim:present (design (type design) stream
						 (view clim:textual-view)
						 &key)
  (princ (name design) stream))

(clim:define-presentation-method clim:accept ((type design) stream (view clim:textual-view) &key)
  (clim:completing-from-suggestions (stream)
     (loop for design in (all-designs clim:*application-frame*)
	 do (clim:suggest (string (name design))
			  design))))

(define-design-editor-command (com-switch-to-design :name t)
    ((new-design 'design))
  (switch-focus clim:*application-frame* new-design)
  )

(clim:define-presentation-to-command-translator jump-to-design
    (design com-switch-to-design design-editor
	    :gesture :switch-to-design)
  (object)
  (list object))

(define-design-editor-command (com-switch-to-implementation :name t)
    ((new-design 'basic-task))
  (switch-to-selected-implementation new-design))

(defun switch-to-selected-implementation (new-design)
  (let ((selected-implementation (selected-implementation new-design)))
    (if selected-implementation
	(switch-focus clim:*application-frame* selected-implementation)
      (clim:beep))))

(clim:define-presentation-to-command-translator jump-to-selected-design
    (basic-task com-switch-to-design design-editor
		:documentation "Switch to implementation"
	    :tester ((object) (not (null (selected-implementation object))))
	    :gesture :switch-to-selected-implementation)
  (object)
  (list (selected-implementation object)))

(defmethod switch-focus ((frame design-editor) (new-design composite-task))
  (setf (design-in-progress frame) new-design))

(define-design-editor-command (com-new-design :name t)
    ((name 'symbol))
  (get-new-design name clim:*application-frame*
		  :for-design-editor t))

(define-design-editor-command (com-design-implementation :name t)
    ((abstract-task 'basic-task))
  (let ((implementation-name (intern (gensymbol (name abstract-task) "IMPLEMENTATION"))))
    (get-new-design implementation-name clim:*application-frame*
		    :class 'implementation
		    :abstract-task abstract-task
		    :for-design-editor t)
    ))

;;; For design-editor is T whenever this is called from a design-editor command (or speech interface)
;;; that is manipulating the buffers in the editor
;;; It's NIL when called from implement-design, because that will screw around with the 
;;; various assertion and slots that related designs to tasks
(defun get-new-design (name design-editor &key (class 'implementation) abstract-task (for-design-editor t))
  (let ((new-design (make-instance class :name name)))
    (when abstract-task
      (untell `[selected-implementation ? ,abstract-task ?])
      (push new-design (implementations abstract-task))
      (setf (abstract-task new-design) abstract-task)
      (when for-design-editor
	(tell `[selected-implementation ,new-design ,abstract-task nil])
	(tell `[possible-implementation ,new-design ,abstract-task user-specified nil])
	(setf (selected-implementation abstract-task) new-design)))
    (push new-design (all-designs design-editor))
    (switch-focus design-editor new-design)
    new-design))

(define-design-editor-command (com-kill-current-design :name t)
    ()
  (let ((current-design (design-in-progress clim:*application-frame*)))
    (remove-design clim:*application-frame* current-design)))

(define-design-editor-command (com-remove-design :name t)
    ((existing-design 'design))
  (remove-design clim:*application-frame* existing-design))

(define-design-editor-command (com-kill-all-designs :name t)
    (&key (except-for '(sequence design)))
  (loop for design in (all-designs clim:*application-frame*)
      unless (member design except-for)
      do (remove-design clim:*application-frame* design)))

(define-design-editor-command (com-kill-all-implementations :name t)
    ((existing-design 'design))
  (Kill-all-implementations existing-design))

(defun kill-all-implementations (design)
  (labels ((do-one (this-design)
	     (loop for component in (children this-design)
		 when (typep component 'task-interface-mixin)
		 do (loop for implementation in (implementations component)
			do (do-one implementation)
			   (remove-design clim:*application-frame* implementation))
		    )))
    (do-one design))
  (let ((current-abstract-task (abstract-task design)))
    (when current-abstract-task
      (remove-child (superior current-abstract-task) current-abstract-task)))
  (setf (abstract-task design) nil)
  )

(defmethod remove-design ((frame design-editor) design)
  (setf (all-designs frame) (remove design (all-designs frame)))
  (ask* `[possible-implementation ,design ?task ? ?]
	(untell `[possible-implementation ,design ? ? ?])
	(untell `[selected-implementation ,design ? ?])
	(setf (implementations ?task) nil
	      (selected-implementation ?task) nil))
  (when (eql design (design-in-progress frame))
    (if (null (all-designs frame))
	(get-new-design 'initial-design frame :for-design-editor t)
      (switch-focus frame (first (all-designs frame)))))
  (kill design)
  )

(define-design-editor-command (com-produce-implementations :name t)
    ((task 'basic-task)
     &key 
     (language '(clim:member-alist (("Lisp" . :lisp) ("Java" . :Java))) :default :lisp)
     )
  (produce-implementations task nil language))

(define-design-editor-command (com-implement-design :name t)
    ((language '(clim:member-alist (("Lisp" . :lisp) ("Java" . :Java))) :default :lisp))
  (let* ((design (design-in-progress clim:*application-frame*)))
    (when (or (not (typep design 'implementation))
              (null (abstract-task design)))
      (create-abstract-task-for-diagram design)
      )
    (let ((task-interface (abstract-task design)))
      (produce-implementations task-interface :keep-top-level language))
    (switch-focus clim:*application-frame* design)
    ))

;;; Use-existing can be any of the following
;;; Nil: Start from the top level task and refine everything
;;; :keep-all: This thing has already been synthesized reuse everything
;;; :keep-top-level: We're really using a design (not a task) so keep that and synthesize everything
;;;                  below that level
;;; Need to make this  consistent with the 3 posssible values above
(defun produce-implementations (task &optional (use-existing nil) (language :lisp))
  (let ((implementations (unless (eql use-existing :keep-all) 
			   ;; Note: this just avoids wasting time since build-recursive-implementation
			   ;; wouldn't do anything if use-existing = :keep-all
			   (build-recursive-implementation task clim:*application-frame* use-existing)))
        (stream (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
    (cond
     ((or implementations use-existing)
      (if use-existing (initialize-design task) (show-implementations stream task implementations))
      (lep:with-output-to-temp-buffer (stream "*code-buffer*")
	;; If any part of the hierarchy is rebuilt (which is true unless use-exiting = :keep-all)
	;; then we need to rebuild the selected-impementation map (the 2nd argument)
        (let ((code (generate-code task (not (eql use-existing :keep-all)) language )))
          (pretty-print-for-language code stream language)))
      (switch-to-buffer :buffer-name "*code-buffer*")
      #+macosx
      (run-ascript "natsoft:code;activate-emacs.scpt" nil))
     (t
      (format stream
              "~%No designs for ~a found" (name task))))))


(defmethod pretty-print-for-language (code stream (language (eql :lisp)))
  (pprint code stream))

(defun create-abstract-task-for-diagram (&optional (design (design-in-progress clim:*application-frame*)))
  (let* ((initial-inputs (find-initial-inputs design))
         (final-outputs (find-final-outputs design))
         (new-design (get-new-design (intern (string-upcase (concatenate 'string "abstract-task-" (string (name design)))))
                                     clim:*application-frame* 
				     :for-design-editor t))
         (component (create-task (name design) 'procedure new-design))
         (generation-controller (generation-controller design)))
    (setf (generation-controller new-design) generation-controller)
    ;; set up relationship between the new task and the diagram
    (unless (typep design 'implementation)
      (change-class design 'implementation))
    (setf (abstract-task design) component
	  (selected-implementation component) design)
    (pushnew design (implementations component))
    (tell `[selected-implementation ,design ,component ()])
    (tell `[possible-implementation ,design ,component 'user-specified nil])
    ;; now give the component input and output ports to match
    ;; the initial-inputs and final-outputs
    (loop for ii in initial-inputs
        for name = (name ii)
        for port = (first (outputs ii))
        for type-constraint = (port-type-constraint port)
        for new-port = (add-port 'input name component)
        do (add-port-type-description new-port type-constraint))
    (loop for fo in final-outputs
        for name = (name fo)
        for port = (first (inputs fo))
        for type-constraint = (port-type-constraint port)
        for new-port = (add-port 'output name component)
        do (add-port-type-description new-port type-constraint)))
  )


(clim:define-presentation-to-command-translator produce-implementations
    (basic-task com-produce-implementations design-editor
	    :gesture :produce-implementations)
  (object)
  (list object))

(defun show-implementations (stream task implementations)
  (cond 
   (implementations
    (format stream "~%Designs implementing ~a~%" (name task))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
	(clim:formatting-cell (stream) (write-string "Design" stream))
	(clim:formatting-cell (stream) (write-string "Method" stream))
	(clim:formatting-cell (stream) (write-string "Choices" stream)))
      (loop for (implementation method-name choices) in implementations
	  do (clim:formatting-row (stream)
	       (clim:formatting-cell (stream)
		 (clim:present implementation 'design :stream stream))
	       (clim:formatting-cell (stream)
		 (princ method-name stream))
	       (loop for choice in choices
		   do (clim:formatting-cell (stream)
			(princ choice stream)))
	       ))))
   (t (format stream "~%No designs implement ~a" (name task)))))

(define-design-editor-command (com-switch-to-parent-design :name t :menu t)
    ()
  (let ((current-design (design-in-progress clim:*application-frame*)))
    (block find-one
      (ask `[possible-implementation ,current-design ?parent ? ?]
	   #'(lambda (just)
	       (declare (ignore just))
	       (switch-focus clim:*application-frame* (superior ?parent))
	       (return-from find-one (values)))))))

(define-design-editor-command (com-show-implementations :name t)
    ((task 'basic-task))
  (let ((answers nil)
	(stream (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
    (ask `[possible-implementation ?implementation ,task ?method ?choices]
	 #'(lambda (just)
	     (declare (ignore just))
	     (push (list ?implementation ?method ?choices) answers)))
    (show-implementations stream task answers)))


(define-design-editor-command (com-print-drawing :name t)
    ((file 'clim:pathname))
  (print-device-under-design clim:*application-frame* file))


; (define-design-editor-command (add-device :menu t :name t)
;     ((task-type 'symbol)
;      (task-name 'symbol))
;   (let ((the-task (create-task task-name task-type nil) ))
;     (setq *all-devices* (append *all-devices* (list the-task)))
;     (values)))

; (define-design-editor-command (remove-device :menu t :name t)
;     ((task 'composite-task))
;   (setq *all-devices* (remove task *all-devices*))
;   (values))

; (clim:define-presentation-to-command-translator composite-task-to-remove-it
;    (composite-task remove-device design-editor
; 			     :echo nil
; 			     :gesture :shift-button
; 			     :documentation "Remove Device")
;     (object presentation)
;   (declare (ignore presentation))
;   (list object))

; (define-design-editor-command (clear-screen :menu t :name t)
;     ()
;   (clim:window-clear *standard-output*))





; (define-design-editor-command (graph-device :menu t)
;     ((task 'composite-task))
;   (unless (body-instantiated? task) (build-task-body (task-type task) task))
;   (format t "~3%Structure of Design ~a~3%" (name task))
;   (let ((*the-top-level-task* task)
;         (*port-map* (make-hash-table)))
;     (declare (special *the-top-level-task* *port-map*))
;     (clim:format-graph-from-roots
;      (inputs task)
;      #'display-task-component
;      #'task-component-children
;      :graph-type :cad
;      :merge-duplicates t
;      :maximize-generations nil
;      :arc-drawer #'ertask-network-arrow-drawer))
;   (values))

;;;(clim:define-presentation-to-command-translator composite-task-to-graph-it
;;;   (composite-task graph-device design-editor
;;;			     :echo nil
;;;			     :gesture :button
;;;			     :documentation "Graph a Device")
;;;    (object presentation)
;;;  (declare (ignore presentation))
;;;  (list object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Starting up
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun screen-size ()
  (let ((graft (clim:find-graft)))
    (clim:rectangle-size (clim:sheet-region graft))))

;; This doesn't work because the lisp listener is still doing its thing
;; you could probably use the design-editor's windows however
(defun run-design-editor ()
  (mp:process-run-function
	"Design-Editor"
    #'(lambda ()
	(multiple-value-bind (width height) (screen-size)
	  (declare (ignorable width height))
	  (setq *design-editor* (clim:make-application-frame 'design-editor
							     :pretty-name "Design Editor"
							     :width (* .8 width)
							     :height (* .9 height))))
	;; I moved this here from the run-frame-top-level code
	;; so that it doesn't rerun when you switch layouts
	(add-component *design-editor* 'i 'procedure nil)
	(restore-core-facts :clean? nil)
	(clim:run-frame-top-level *design-editor*))
    ))
    

(clim-env::define-lisp-listener-command (com-start-design-editor :name t)
    ()
  (run-design-editor))

(defmethod center-display-pane-on-task ((frame design-editor) (task core-task-mixin))
  (let* ((pane (clim:get-frame-pane frame 'display))
	 (region (clim:pane-viewport-region pane))
	 (task-output-record (output-record task))
	 (cad-graph-output-record (clim:output-record-parent task-output-record)))
    (multiple-value-bind (left top right bottom) (clim:bounding-rectangle* cad-graph-output-record)
      (let ((task-mid-x (+ left (/ (abs (- right left)) 2)))
	    (task-mid-y (+ top (/ (abs (- top bottom)) 2)))
	    (pane-half-width (/ (abs (- (clim:bounding-rectangle-right region) (clim:bounding-rectangle-left region))) 2))
	    (pane-half-height (/ (abs (- (clim:bounding-rectangle-top region) (clim:bounding-rectangle-bottom region))) 2)))
	(clim:window-set-viewport-position pane 
					   (- task-mid-x pane-half-width)
					   (- task-mid-y pane-half-height))))))
	
	 
    
(defun graph-type-hierarchy (&key (base 'computational-stuff) (pathname "rp:muse;type-hierarchy.ps"))
  (with-open-file (file pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (clim:with-output-to-postscript-stream (stream file)
      (clim:format-graph-from-roots 
	 (list base)
	 #'princ
	 #'subtypes
	 :stream stream
	 :merge-duplicates t)))
  (values))

(defun subtypes (type)
  (let ((answers nil))
    (ask `[subtype ?a ,type]
	 #'(lambda (just)
	     (declare (ignore just))
	     (pushnew ?a answers)))
    answers))
				

(defun read-and-assert (pathname)
  (with-open-file (f pathname :direction :input)
    (loop for line = (read-line f nil 'eof)
	until (eql line 'eof)
	do (assert-text line))))


(defrule define-type-information (:backward)
  :then [is-appropriate-response |create| ?texp handle-type-definitions ()]
  :if [and [subject-of ?texp ? |you|]
	   [or [object-of ?texp ? |type_definitions|]
	       [object-of ?type ? |type_information|]]
	   ])

(defmethod handle-type-definitions ((parser core-parser-mixin) main)
  (declare (ignore main))
  (vocalize-text "Please enter the type information here" :vocalize? t :wait? t)
  (switch-to-buffer :buffer-name "*type-information*")
  #+macosx
  (run-ascript "natsoft:code;activate-emacs.scpt" t)
  (sleep 2)
  #+macosx
  (run-ascript "natsoft:code;activate-x.scpt" t)
  (switch-to-buffer :buffer-name "generated-types.lisp")
  #+macosx
  (run-ascript "natsoft:code;activate-emacs.scpt" t)
  (sleep 4)
  )

(defmethod handle-test-definition ((parser core-parser-mixin) main test-object-name test-result-word relation-name quantifier-name property-name)
  (declare (ignore main test-object-name test-result-word relation-name quantifier-name property-name))
  ;; total canned answer
  (let* ((new-design (get-new-design 'sorting clim:*application-frame* :for-design-editor t))
	 (component (add-component clim:*application-frame* 'sorted-list-test 'sorted-list-test ())))
    (declare (ignorable new-design))
    (produce-implementations component nil)
    (switch-to-selected-implementation component)
    )
  )

(clim-env:define-lisp-listener-command (com-prepare-for-darpa-demo :name t)
    ()
  (when (probe-file "natsoft:code;subtypes.text")
    (delete-file "natsoft:code;subtypes.text"))
  (when (probe-file "natsoft:code;learned-stuff.lisp")
    (delete-file "natsoft:code;learned-stuff.lisp"))
  (switch-to-buffer :buffer-name "typicalset.txt")
  (kill-buffer :buffer-name "typicalset.txt")
  (switch-to-buffer :buffer-name "*common-lisp*")
  (com-start-design-editor)
  )
(define-design-editor-command (com-set-java-parameters :name t)
    ()
  (set-java-generation-parameters (clim:get-frame-pane clim:*application-frame* 'interactor)))