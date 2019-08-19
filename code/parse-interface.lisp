;;; -*- Mode: common-lisp; package: natsoft; readtable: joshua -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My understanding of the XML encoding used by the START server
;;; The basic elements are TEXP (ternary expression)
;;; Subject, Relation, and Object of TEXP
;;; For each of Subject and Object
;;;   It might contain:
;;;     A TEXP (so the whole thing is recursive)
;;;     An Instance (which has a name and an index)
;;;     A constant (just a string)
;;;   I think relations are always either constants or instances
;;;
;;;
;;; Instances should be interned (using 2 place equal hash-table)
;;; Constants should be interned in the current package
;;; TEXP's should be interned (using 3 place equal hash-table)
;;;
;;; The top level just acculumates a list of TEXP's using a front and back pointer
;;;
;;; Basically this is a pushdown automaton.
;;; Current-element holds the thing we're working on now (usually a list, see below)
;;; The Stack holds a list of elements whose processing has been temporarily suspended
;;;   Current-element is pushed on the stack when a new element is started
;;; Return-value holds the result of the last entry processing
;;; 
;;; When an element is completed it's interned and then inserted into the appropriate slot of the previous
;;; element if any.

;;; Handling each type of element:
;;; TEXP: 
;;;   On entry set current-element to a list (TEXP NIL NIL NIL)
;;;   On exit Intern the list in the hash-table
;;;   If the stack is empty add the interned TEXP to the top-level list of TEXP's
;;;                   not empty set Return-value to TEXP
;;;
;;; Instance: The index is part of the attrs of start-entry call but, the name
;;;  has to wait for a content call.  
;;; On entry: set current-element to a list (INDEX NIL NIL)
;;; On Exit: Intern the list to get an instance and set Return-value to the interned instance
;;;
;;; Constant: You have to wait for a content call to give you the string
;;; On Entry: Set current-element to a list (CONSTANT NIL)
;;; On Exit: Intern the string in the second position
;;; set return-value to the interned-string
;;;
;;; subject, object, relation
;;; These don't build new objects themselves, they always include one of: texp, constant, instance
;;; On entry: don't change current object, don't push anything
;;; On exit: Set the appropriate slot of the current-object to the value in Return-value
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :natsoft)

(defgeneric print-as (parse format &optional top-level? stream))

;;; Basic classes

(defclass occurs-in-mixin ()
  ((as-subject :initform nil :accessor as-subject)
   (as-object :initform nil :accessor as-object)
   (as-relation :initform nil :accessor as-relation))
  )

;;; Ternary Expressions
(defclass texp (occurs-in-mixin)
  ((subject :initform nil :accessor subject :initarg :subject)
   (relation :initform nil :accessor relation :initarg :relation)
   (object :initform nil :accessor object :initarg :object)))

(defmethod print-object ((texp texp) stream)
  (format stream "#<TEXP ~a ~a ~a>"
	  (subject texp)
	  (relation texp)
	  (object texp)))

(defmethod print-as ((texp texp) (format (eql :xml)) &optional top-level? (stream *standard-output*))
  (declare (ignore top-level?))
  (format stream "~%<texp>")
  (format stream "~%<subject>")
  (print-as (subject texp) :xml nil stream)
  (format stream "~%</subject>")
  (format stream "~%<relation>")
  (print-as (relation texp) :xml nil stream)
  (format stream "~%</relation>")
  (format stream "~%<object>")
  (print-as (object texp) :xml nil stream)
  (format stream "~%</object>")
  (format stream "~%</texp>")
  )

(defmethod print-as ((texp texp) (format (eql :text)) &optional top-level? (stream *standard-output*))
  (cond (top-level?
	 (format stream "~%[")
	 (print-as (subject texp) :text nil stream)
	 (format stream " ")
	 (print-as (relation texp) :text nil stream)
	 (format stream " ")
	 (print-as (object texp) :text nil stream)
	 (format stream "]")
	 )
	(t (print-as (relation texp) :text nil stream)
	   )))

(defmethod print-as ((texp texp) (format (eql :viz)) &optional top-level? (stream *standard-output*))
  (cond (top-level?
	 (format stream "~%[")
	 (print-as (subject texp) format nil stream)
	 (format stream " ")
	 (print-as (relation texp) format nil stream)
	 (format stream " ")
	 (print-as (object texp) format nil stream)
	 (format stream "]")
	 )
	(t (format stream "<")
	   (print-as (relation texp) format nil stream)
	   (format stream ">")
	   )))

;;; Instances

;;; Big Change: Name is now an interned symbol
(defclass instance (occurs-in-mixin)
  ((name :initform nil :accessor name :initarg :name)
   (index :initform nil :accessor index :initarg :index))
  )

(defmethod print-object ((instance instance) stream)
  (format stream "#<Instance ~a ~a>"
	  (name instance)
	  (index instance)))

(defmethod print-as ((instance instance) (format (eql :xml)) &optional top-level? stream)
  (declare (ignore top-level?))
  ;; apparently START doesn't put this on a new line
  (format stream "<instance index=\"~a\">~a</instance>" 
	  (index instance)
	  (name instance))
  )

(defmethod print-as ((instance instance) (format (eql :text)) &optional top-level? stream)
  (declare (ignore top-level?))
  (format stream "~a+~a" 
	  (name instance)
	  (index instance))
  )

(defmethod print-as ((instance instance) (format (eql :viz)) &optional top-level? stream)
  (declare (ignore top-level?))
  (format stream "~a+~a" 
	  (name instance)
	  (index instance))
  )

;;; Constants

;;; Big Change: The name is now an interned symbol

(defclass constant (occurs-in-mixin)
  ((name :initform nil :accessor name :initarg :name)))

(defmethod print-object ((constant constant) stream)
  (format stream "#<Constant ~a>"
	  (name constant)))

(defmethod print-as  ((constant constant) (format (eql :xml)) &optional top-level? stream)
  (declare (ignore top-level?))
  (format stream "<constant>")
  (format stream "~a" (name constant))
  (format stream "</constant>")
  )

(defmethod print-as  ((constant constant) (format (eql :text)) &optional top-level? stream)
  (declare (ignore top-level?))
  (format stream "~a" (name constant))
  )

(defmethod print-as  ((constant constant) (format (eql :viz)) &optional top-level? stream)
  (declare (ignore top-level?))
  (format stream "~a" (name constant))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexing the above objects within a parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass texp-constant-index-interner ()
  ((instance-hash-table :accessor instance-hash-table :initform (make-hash-table :test #'equal))
   (constant-hash-table :accessor constant-hash-table :initform (make-hash-table :test #'equal))
   (texp-hash-table :accessor texp-hash-table :initform (make-hash-table :test #'equal))
   ))

(defmethod intern-texp ((interner texp-constant-index-interner) (triple list))
  (let* ((ht (texp-hash-table interner))
	 (original-list triple)
	 (texp (gethash original-list ht)))
    (unless texp
      (setq texp (make-instance 'texp
		    :subject (pop triple)
		    :relation (pop triple)
		    :object (pop triple)))
      (setf (gethash original-list ht) texp))
    texp))

(defmethod unintern-texp ((interner texp-constant-index-interner) (texp texp))
  (let ((key (list (subject texp) (relation texp) (object texp))))
    (setf (gethash key (texp-hash-table interner)) nil))
  (values))

(defmethod index-texp ((interner texp-constant-index-interner) (texp texp))
  (let ((subject (subject texp))
	(object (object texp))
	(relation (relation texp)))
    (push texp (as-subject subject))
    (push texp (as-object object))
    (push texp (as-relation relation)))
  )

(defmethod unindex-texp ((interner texp-constant-index-interner) (texp texp))
  (let ((subject (subject texp))
	(object (object texp))
	(relation (relation texp)))
    (setf (as-subject subject) (delete texp (as-subject subject))
	  (as-object object) (delete texp (as-object object))
	  (as-relation relation) (delete texp (as-relation relation))))
  (values))

(defmethod intern-instance ((interner texp-constant-index-interner) (pair list))
  (let* ((ht (instance-hash-table interner))
	 (original-pair pair)
	 (name (let ((first (pop original-pair)))
		 (if (symbolp first) first (intern first))))
	 (index (pop original-pair))
	 (new-key (list name index))
	 (instance (gethash new-key ht)))
    (unless instance
      (setq instance (make-instance 'instance
		       ;; note the intern
		       :name name
		       :index index))
      (setf (gethash new-key ht) instance))
    instance))

(defmethod intern-constant ((interner texp-constant-index-interner) name)
  (when (stringp name) (setq name (intern name)))
  (let* ((ht (constant-hash-table interner))
	 (constant (gethash name ht)))
    (unless constant
      (setq constant (make-instance 'constant
		    :name name))
      (setf (gethash name ht) constant))
    constant))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers
;;; All parsers can take multiple parses (for multiple sentences) using
;;; a single interning structure
;;;
;;; Parsers differ on what kind on exchange format they use
;;;  At the moment this means XML or TEXT
;;;
;;; However the internal representation is common to all of them
;;; namely a set of texps
;;;
;;; They maintain a set of parses, queries, and reply qualities
;;; as well as the current version of each that is being worked on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass core-parser-mixin (texp-constant-index-interner)
  ((current-parse :initform nil :accessor current-parse)
   (current-reply-quality :initform nil :accessor current-reply-quality)
   (current-query :initform nil :accessor current-query)
   (all-parses :initform nil :accessor all-parses)
   (all-reply-qualities :initform nil :accessor all-reply-qualities)
   (all-queries :initform nil :accessor all-queries)
   ))

(defmethod remove-texp ((parser core-parser-mixin) (texp texp))
  (setf (current-parse parser) (delete texp (current-parse parser)))
  (unintern-texp parser texp)
  (unindex-texp parser texp))

(defmethod add-texp ((parser core-parser-mixin) (texp texp))
  (index-texp parser texp)
  (push texp (current-parse parser))
  texp
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating text
;;;  Make-a-generate-request asks START to convert the parsers texps into an English string
;;;   It does this by first creating a representation of the texps in an exchange format
;;;   And then passing this to START.  
;;;
;;;  Any parser can create a string from the common internal representation 
;;;  The type of string generated is dependent on the exchange format specified in the requeset
;;;  This string is a representation of the texps and is produced by the appropriate string-for method
;;;
;;;  The request to start also specifies the exchange format for the returned Engish string
;;;   this can be either TEXT or XML.  If XML, you need to parse the XML and extract the CDATA elements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-text (text &key format server))
(defgeneric process-parsed-form (input-string parser-or-format))

(defgeneric generate-text (parser &key format server output-format))
(defgeneric string-for (current-parser format))

(defmethod parse-text (text &key (format :text) (server "guest"))
  (let ((encoding (case format
		    (:xml "xml")
		    (:text "formatted-text"))))
    (multiple-value-bind (answer code) (make-a-parse-request text :encoding encoding :server server)
      (values (process-parsed-form answer format)
	      code)
      )))

(defmethod assert-text (text &key (format :text) (server "guest"))
  (let ((encoding (case format
		    (:xml "xml")
		    (:text "formatted-text"))))
    (multiple-value-bind (answer code) (make-an-assert-request text :encoding encoding :server server)
      (values (process-parsed-form answer format)
	      code)
      )))

(defmethod string-for ((parser core-parser-mixin)  (format (eql :viz)))
  (with-output-to-string (stream)
    (loop for this-parse in (all-parses parser)
	do (loop for texp in this-parse
	       do (print-as texp format t stream)))))

(defmethod string-for ((parser core-parser-mixin)  (format (eql :text)))
  (with-output-to-string (stream)
    (loop for this-parse in (all-parses parser)
	do (loop for texp in this-parse
	       do (print-as texp format t stream)))))

;;; Note:  START doesn't wants the XML for a generate request to mirror what it gives us
;;; on a parse request. It just wants <input> around all the texps

(defmethod string-for ((parser core-parser-mixin)  (format (eql :xml)))
  (with-output-to-string (stream)
    (format stream "<input>")
    ;; The revere below is because START gets it backwards
    ;; even though I'm sending the texps in the right order
    (loop for this-parse in (reverse (all-parses parser))
	do ;; (format stream "<interaction>")    
	   (loop for texp in this-parse 
	       do (print-as texp format t stream))
	   ;; (format stream "</interaction>")
	   )
    (format stream "</input>")
    ))

;;; To be handled later
;;;(defun assert-text (text &key (format :text) (server "guest"))
;;;  (let ((encoding (case format
;;;		    (:xml "xml")
;;;		    (:text "formatted-text"))))
;;;    (multiple-value-bind (answer code) (make-an-assert-request text :encoding encoding :server server)
;;;      (values (process-parsed-form answer (parser-for-format format))
;;;	      code)
;;;      ))
;;;  )

;;; To be handled later
;;;(defun query-text (text &key (format :text) (server "guest"))
;;;  (let ((encoding (case format
;;;		    (:xml "xml")
;;;		    (:text "formatted-text"))))
;;;    (multiple-value-bind (answer code) (make-a-query-request text :encoding encoding :server server)
;;;      (values (process-parsed-form answer (query-parser-for-format format))
;;;	      code)
;;;      )))

;;; This is now obsolete
;;;(defun parser-for-format (format)
;;;  (case format
;;;    (:xml (make-instance 'xml-ternary-expression-parser))
;;;    (:text (make-instance 'text-ternary-expression-parser))))

;;;(defun query-parser-for-format (format)
;;;  (case format
;;;    (:xml (make-instance 'xml-query-ternary-expression-parser))
;;;    (:text (make-instance 'text-query-ternary-expression-parser)))
;;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A parser for the text format produced by START
;;; This is a top level string with a [<subject> <relation> <object>] 
;;; on each line
;;; The subtleties:
;;;   A logically embedded texp is represented by the string of its relation
;;;    so you have to maintain a making of relation names to texps
;;;   Indexes are represented by strings of the form mumble+81
;;;    so you have to explode the string to look for + signs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-ternary-expression-parser (core-parser-mixin)
  ((relation-name-to-texp-hash-table :initform (make-hash-table :test #'eql) :accessor relation-name-to-texp-hash-table))
  )

(defun make-start-triple-readtable ()
  "Makes a readtable for parsing START's triples delimited by brackets"
  (let* ((new-readtable (copy-readtable nil)))
    (setf (readtable-case new-readtable) :preserve)
    (flet ((triple-reader (stream char)
	     (declare (ignore char))
	     (read-delimited-list #\] stream nil)))
      (set-macro-character #\[ #'triple-reader nil new-readtable)
      (set-syntax-from-char #\] #\) new-readtable)
      new-readtable)))

(defparameter *start-readtable* (make-start-triple-readtable))

(defun find-index-if-exists (string)
  (let ((index (position #\+ string :from-end t :test #'char-equal)))
    (if index
	(values (subseq string 0 index) (read-from-string string nil nil :start index))
      string)))

(defmethod intern-token ((parser text-ternary-expression-parser) (token symbol))
  (multiple-value-bind (name index) (find-index-if-exists  (string token))
    (if index (intern-instance parser (list name index)) (intern-constant parser name))))

(defmethod intern-token ((parser text-ternary-expression-parser) (token number))
  (intern-constant parser (format nil "~d" token)))

(defmethod intern-subject-or-object ((parser text-ternary-expression-parser) token)
  (let ((interned-thing (intern-token parser token)))
    (or (gethash interned-thing (relation-name-to-texp-hash-table parser)) interned-thing)))

(defmethod intern-relation ((parser text-ternary-expression-parser) (token symbol))
  (let ((interned-thing (intern-token parser token)))
    interned-thing
    ))

;;; just make the parser and then ask it to do the work
(defmethod process-parsed-form ((input string) (format (eql :text)))
  (process-parsed-form input (make-instance 'text-ternary-expression-parser)))

;;; Fix:
;;; At present this ignores the structuring of text exchange format into multiple sentences
;;; which are delimited by lines in front of each sentence:
;;; ===> <the sentence> before the texps
;;; and a blank line after the texps
(defmethod process-parsed-form ((input string) (parser text-ternary-expression-parser))
  (with-input-from-string (stream input)
    (let ((*readtable* *start-readtable*))
      (loop with header = "===>"
	  with header-length = (length header)
	  for next-line = (read-line stream nil 'eof)
	  for header-position = nil
	  until (eql next-line 'eof)
		;; A line beginning with ====> indicates a new sentence
	  when (setq header-position (search "===>" next-line :test #'char-equal))
	  do (setf (current-parse parser) nil
		   (current-query parser) (subseq next-line (+ header-position header-length 1))
		   (current-reply-quality parser) nil)
	  when (= (length next-line) 0)
	  do (push (nreverse (current-parse parser)) (all-parses parser))
	     (push (current-query parser) (all-queries parser))
	     (push (current-reply-quality parser) (all-reply-qualities parser))
	  when (and (> (length next-line) 0) (char-equal #\[ (aref next-line 0)))
	  do (let ((next-triple (read-from-string next-line)))
	       (destructuring-bind (subject relation object) next-triple
	       ;;handle the subject
	       (let ((interned-subject (intern-subject-or-object parser subject))
		     (interned-relation (intern-relation parser relation))
		     (interned-object (intern-subject-or-object parser object)))
		 (setf (first next-triple) interned-subject
		       (second next-triple) interned-relation
		       (third next-triple) interned-object)
		 (let ((new-texp (intern-texp parser next-triple)))
		   (add-texp parser new-texp)
		   (setf (gethash interned-relation (relation-name-to-texp-hash-table parser))
		     new-texp))))))))
  (setf (all-parses parser) (nreverse (all-parses parser))
	(all-queries parser) (nreverse (all-queries parser))
	(all-reply-qualities parser) (nreverse (all-reply-qualities parser)))
  parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A parser specialized for xml input format of a parse request
;;;  query requests would require slightly different handling
;;;  due to bindings and multiple answers
;;; This is based on the SAX parser provided with Allegro
;;; Which is a callback based system, one for each XML element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass xml-ternary-expression-parser (core-parser-mixin sax-parser)
  ((current-element :initform nil :accessor current-element)
   (current-string :initform nil :accessor current-string)
   (return-value :initform nil :accessor return-value)
   (stack :initform nil :accessor stack))
  )

(defmethod process-parsed-form ((input string) (format (eql :xml)))
  (process-parsed-form input (make-instance 'xml-ternary-expression-parser)))

(defmethod process-parsed-form ((input string) (parser xml-ternary-expression-parser))
  (multiple-value-bind (flag parse) (sax-parse-string input :class parser)
    (declare (ignore flag))
    parse))


;;; To be handled later
;;;(defclass xml-query-ternary-expression-parser (xml-ternary-expression-parser)
;;;  ()
;;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SAX call-back handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ignore this for the moment
(defmethod start-document ((parser xml-ternary-expression-parser))
  )

(defmethod end-document ((parser xml-ternary-expression-parser))
  (with-slots (net.xml.sax::char-table) parser
    (setq net.xml.sax::char-table nil))
  )

;;; The handlers for specific tags
(defgeneric my-start-element (parser interned-local-name attrs))
(defgeneric my-end-element (parser interned-local-name the-element))

(defparameter *start-tracing* nil)
(defmacro start-trace (stream control-string &rest args)
  `(when *start-tracing*
     (format ,stream ,control-string ,@args)))


;;; Tags I'm ignoring for now
(defparameter *ignorable-elements* 
    (list  
     "coreference"			; Could be valuable later on
     "anaphor"				;these are the two components of a coreference
     "antecedent-head"
     "query"				;what you asked
     "bindings"				;a ste of match bindings when doing match
     "binding"				;an individual binding
     "variable"				;of a binding
     "value"				;what it's bound to
     "reply-content"			;these show up in queries, but I think it's providing html
     "notification"			;when you screw up particularly in spelling
     "from"				;what you typed
     "to"				;suggested correction
     "alternatives"			;binds together suggestions
     "alternative"
     "original-input"			;the whole utterance that you typed
     "possible-antecedent-head"	;beats me
     ))

;;; This is the dispatcher to specific tag handlers
;;; One each for entry and exit
(defmethod start-element ((parser xml-ternary-expression-parser) iri localname qname attrs)
  (declare (ignore iri qname))
  (unless (member localname *ignorable-elements* :test #'string-equal)
    (let ((interned-local-name (intern (string-upcase localname))))
      (start-trace t "~%Starting element ~s" interned-local-name)
      (my-start-element parser interned-local-name attrs)))
  (values))

(defmethod end-element ((parser xml-ternary-expression-parser) iri localname qname)
  (declare (ignore iri qname))
  (unless (member localname *ignorable-elements* :test #'string-equal)
    (let ((interned-local-name (intern (string-upcase localname) )))
      (start-trace t "~%Ending element ~s" interned-local-name)
      (my-end-element parser interned-local-name (current-element parser))))
  (values))

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'exception)) attrs)
  (start-trace t "~%Starting exceptions ~{~a~^,~}" attrs)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'exception)) the-current-element)
  (start-trace t "~%Ending exceptions ~a" the-current-element)
  )

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'interactions)) attrs)
  (start-trace t "~%Starting interactions ~{~a~^,~}" attrs)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'interactions)) the-current-element)
  (start-trace t "~%Ending interactions ~a" the-current-element)
  (setf (all-parses parser) (nreverse (all-parses parser))
	(all-reply-qualities parser) (nreverse (all-reply-qualities parser)))
  )

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'interaction)) attrs)
  (start-trace t "~%Starting interaction ~{~a~^,~}" attrs)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'interaction)) the-current-element)
  (start-trace t "~%Ending interaction ~a" the-current-element)
  )

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'reply)) attrs)
  (setf (current-reply-quality parser) (cdr (assoc "quality" attrs :test #'string-equal)))
  (start-trace t "~%Starting reply")
  (unless (current-reply-quality parser)
    (setf (current-parse parser) nil))
  (setf (current-element parser) nil
	(return-value parser) nil)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'reply)) the-current-element)
  (declare (ignore the-current-element))
  (start-trace t "~%Ending reply")
  (when (current-reply-quality parser)
    (push (current-reply-quality parser) (all-reply-qualities parser))
    (setf (current-reply-quality parser) nil))
  (when (current-parse parser)
    (push (nreverse (current-parse parser)) (all-parses parser))
    (setf (current-parse parser) nil))
  )

;;;(defmethod my-end-element :after ((parser xml-query-ternary-expression-parser) (localname (eql 'reply)) the-current-element)
;;;  (declare (ignore the-current-element))
;;;  (start-trace t "~%Ending reply")
;;;  (when (current-reply-quality parser)
;;;    (push (current-reply-quality parser) (all-reply-qualities parser))
;;;    (setf (current-reply-quality parser) nil))
;;;  (when (current-parse parser)
;;;    (push (current-parse parser) (all-parses parser))
;;;    (setf (current-parse parser) nil))
;;;  )

;;; This doesn't interact with the pushdown machinery at all it just captures two attributes
(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'query)) attrs)
  (let* ((raw (cdr (assoc "raw" attrs :test #'string-equal)))
	 (processed (cdr (assoc "processed" attrs :test #'string-equal))))
    (setf (current-query parser) (list raw processed))
    )
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'query)) the-current-element)
  (declare (ignore the-current-element))
  (values))


;;; The following routines for TEXP, INSTANCE, CONSTANT all push the state on entry
;;; and pop it on exit
(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'texp))  attrs)
  (declare (ignore attrs))
  (start-trace t "~%Starting ternary expression")
  (let* ((new-texp (list 'texp nil nil nil)))
    (push (current-element parser) (stack parser))
    (setf (current-element parser) new-texp)
    (values)
    ))

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'texp)) the-expression)
  (start-trace t "~%Ending ternary expression ~a" the-expression)
  ;; Reset to the stack before I was called
  (setf (current-element parser) (pop (stack parser)))
  (let ((new-texp (intern-texp parser (rest the-expression))))
    ;; There are two cases:
    ;; Either the Stack is now empty and we are a top-level TEXP in which case 
    ;;  and we should link into the top level structure
    ;; Or the Stack isn't empty and we are just a return value
    (if (null (stack parser))
	(add-texp parser new-texp)
	(setf (return-value parser) new-texp)))
  (values))

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'instance)) attrs)
  (start-trace t "~%Starting instance")
  (let* ((new-instance (list 'instance nil (read-from-string (cdr (assoc "index" attrs :test #'string-equal))))))
    (push (current-element parser) (stack parser))
    (setf (current-element parser) new-instance
	  (current-string parser) nil)
    (setf (return-value parser) nil)
    (values)
    ))

;;; At this point RETURN-VALUE will hold the name of the thing
(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'instance)) the-instance)
  (setf (second the-instance) (current-string parser)
	(current-string parser) nil)
  (start-trace t "~%Ending instance ~a" the-instance)
  (setf (current-element parser) (pop (stack parser)))
  (let ((interned-instance (intern-instance parser (rest the-instance))))
    (setf (return-value parser) interned-instance))
  (values))

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'constant)) attrs)
  (declare (ignore attrs))
  (start-trace t "~%Starting constant")
  (let* ((new-constant (list 'constant nil)))
    (push (current-element parser) (stack parser))
    (setf (current-element parser) new-constant
	  (current-string parser) nil))
  (setf (return-value parser) nil)
  (values)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'constant)) the-constant)
  (setf (second the-constant) (current-string parser)
	(current-string parser) nil)
  (start-trace t "~%Ending constant ~a" the-constant)
  (setf (current-element parser) (pop (stack parser)))
  (let ((interned-constant (intern-constant parser (second the-constant))))
    (setf (return-value parser) interned-constant))
  (values))

;;; These guys SUBJECT, RELATION, OBJECT don't push the state
;;; They are always followed by one of the 3 things above (TEXP, INSTANCE, CONSTANT)
;;; They just update their parent TEXP with the return value from whatever gets called within them
(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'subject)) attrs)
  (declare (ignore attrs))
  ;; On entering I don't do anything
  ;; Following me one of Instance, Constant, or TEXP will occur
  ;; leaving the state the same as here, but with a return-value
  (start-trace t "~%Starting subject")
  (values)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'subject)) the-triple)
  (let ((the-subject (return-value parser))
	(the-triple (rest the-triple)))
    (start-trace t "~%Ending subject ~a" the-subject)
    ;; I should just stick the return value into the right slot of the-triple
    ;; and then do nothing else
    (setf (first the-triple) the-subject))
  (setf (return-value parser) nil)
  (values))

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'relation)) attrs)
  (declare (ignore attrs))
  (start-trace t "~%Starting relation")
  (values)
  )

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'relation)) the-triple)
  (let ((the-relation (return-value parser))
	(the-triple (rest the-triple)))
    (start-trace t "~%Ending relation ~a" the-relation)
    (setf (second the-triple) the-relation))
  (setf (return-value parser) nil)
  (values))

(defmethod my-start-element ((parser xml-ternary-expression-parser) (localname (eql 'object)) attrs)
  (declare (ignore attrs))
  (start-trace t "~%Starting object")
  (values))

(defmethod my-end-element ((parser xml-ternary-expression-parser) (localname (eql 'object)) the-triple)
  (let ((the-object (return-value parser))
	(the-triple (rest the-triple)))
    (start-trace t "~%Ending object ~a" the-object)
    (setf (third the-triple) the-object))
  (setf (return-value parser) nil)
  (values))


;;; CONTENT always occurs within an INSTANCE or CONSTANT for which it acculuates a string
;;; which is held in current-string
;;; It does not affect the stack state
(defmethod content ((parser xml-ternary-expression-parser) content start end ignorable)
  (declare (ignore ignorable))
  (unless (eql end (1+ start))
    (let ((substring (string-trim '(#\newline #\return #\space) (subseq content start end)))
	  (current-string (current-string parser)))
      (start-trace t "~%Getting content ~a" substring)
      ;; The documentation says that you may get repeated calls for fragments of the string
      ;; so this checks for that and concatenates them if so.
      ;; I haven't seen this actually happen.
      (setf (current-string parser) 
	(if (null current-string)
	    substring
	  (concatenate 'string current-string substring))
      )))
  (values))

;;; These are ignored
(defmethod start-prefix-mapping ((parser xml-ternary-expression-parser) prefix iri)
  (start-trace t "sax callback: start-prefix-mapping ~s -> ~s~%" prefix iri)
  nil
  )

(defmethod end-prefix-mapping ((parser xml-ternary-expression-parser) prefix)
  (start-trace t "sax callback: end-prefix-mapping ~s~%" prefix)
  )

(defmethod processing-instruction ((parser xml-ternary-expression-parser) target data)
  (start-trace t "sax callback: processing-instruction  target: ~s, data: ~s~%" target data)
  ;; 
  nil)

(defmethod content-character ((parser xml-ternary-expression-parser) character ignorable)
  (start-trace t "sax callback: ~:[~;ignorable~] content-char ~s~%" ignorable character)
  nil)

(defmethod compute-external-format ((parser xml-ternary-expression-parser) encoding ef)
  (let ((ans (call-next-method)))
     (start-trace t "sax callback: compute-external-format of ~s is ~s (current is ~s)~%" encoding ans ef)
    ans))

(defmethod comment ((parser xml-ternary-expression-parser) string)
  ;;
  ;; called when <!-- ..... --> is seen
  ;;
  (start-trace t "sax callback: comment: ~s~%" string)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XML PARSER for generate-requests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass xml-generate-parser (xml-ternary-expression-parser)
  ()
  )



;;; for query processing you have to handle structure for a set of bindings
; ; <bindings>
; ; <binding>
; ; <variable>
; ; </variable>
; ; <value>
; ; </value>
; ; </binding>
; ; ...
; ; /<bindings>

; ; where variable and value can contain instance or constant
					; ; and there can be multiple sets of bindings


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making semantic sense of the parse
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-mains ((parser core-parser-mixin))
  (let* ((main-connective (gethash '|is_main| (constant-hash-table parser)))
	 (texps (as-relation main-connective)))
    (loop for texp in texps
	for answer = (subject texp)
		     ;; Note that it must be Yes
	for yes = (gethash '|yes| (constant-hash-table parser))
	for boolean = (object texp)
	when (eql boolean yes)
	collect answer)))

(defmethod get-main ((parser core-parser-mixin))
  (let* ((main-connective (gethash '|is_main| (constant-hash-table parser)))
	 (texp (first (as-relation main-connective)))
	 (answer (subject texp))
	 ;; Note that it must be Yes
	 (yes (gethash '|yes| (constant-hash-table parser)))
	 (boolean (object texp)))
    (when (eql boolean yes)
      answer)))

(defmethod find-related-to ((thing occurs-in-mixin) &key (connective '|related-to|))
  (loop for texp in (as-subject thing)
      for relation = (relation texp)
      when (eql (name relation) connective)
      return (object texp)))

;;; Here the symbol is fully elaborated, we don't add in "has_".  Call with string if you want to do that.
(defmethod get-syntactic-property ((parser core-parser-mixin) (full-property symbol) &optional (texp (get-main parser)))
  (let ((descriptions-of (as-subject texp))
	(has-property (intern-constant parser full-property)))
    (multiple-value-bind (value texp) (loop for thing in descriptions-of
					    when (eql (relation thing) has-property)
					    return (values (object thing)
							   thing))
      (values value texp full-property))))

;;; A shorthand method.  Call with "tense" for example and this will change it to '|has_tense| and call the
;;; method above
(defmethod get-syntactic-property ((parser core-parser-mixin) (property string) &optional (texp (get-main parser)))
  (let* ((full-property (if (find #\_ property :test #'char-equal) property (concatenate 'string "has_" property))))
    (get-syntactic-property parser (intern full-property) texp)))

(defmethod get-tense ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (get-syntactic-property parser '|has_tense| texp))

(defmethod is-question? ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (get-syntactic-property parser '|is_question| texp)
  )

;;; It appears that nobody calls this
;;; The perferred interface uses interned symbols
(defmethod set-syntactic-property ((parser core-parser-mixin) (property symbol) intended-property-value &optional (texp (get-main parser)))
  (multiple-value-bind (current-property-value property-texp) (get-syntactic-property parser property texp)
    (let ((intended-property-value (intern-constant parser intended-property-value)))
      (unless (eql intended-property-value current-property-value)
	(when property-texp
	  (remove-texp parser property-texp))
	(let ((new-texp (intern-texp parser (list texp property intended-property-value))))
	  (add-texp parser new-texp))))))

;;; The string method
(defmethod set-syntactic-property ((parser core-parser-mixin) (property string) intended-property-value &optional (texp (get-main parser)))
  (let* ((full-property (if (find #\_ property :test #'char-equal) property (concatenate 'string "has_" property))))
    (set-syntactic-property parser (intern full-property) intended-property-value texp)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating Text
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun intern-and-add-texp (parser list)
  (let ((texp (intern-texp parser list)))
    ;; note that add-texp does index-texp
    (add-texp parser texp)
    texp))

(defclass question-generator (text-ternary-expression-parser)
  ((counter :initform 0 :accessor counter))
  )

(defmethod intern-new-instance ((generator question-generator) name)
  (intern-instance generator (list name (incf (counter generator)))))

(defun confess-ignorance-about-a-type (type-name &optional (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (the-type (intern-entity generator (string (find-plural-of-noun type-name)) :definite? 'null :singular? nil))
	 (iap (intern-entity generator "I" :singular? t :definite? 'null))
	 (definition (intern-entity generator "definition" :definite? t :singular? t))
	 (verb (intern-entity generator "know" :definite? 'none :singular? 'none)))
    (create-main-clause generator iap verb definition
			:is-question? nil
			:subject-is-wh? nil)
    (make-verb-negative generator verb)
    (create-possesive generator definition the-type)
    (vocalize-generator generator :vocalize? vocalize? :wait? t)))


;;; To generation something like "I don't know what an imager is.  Please tell me about imagers"
(defun query-about-unknown-type (unknown-type &key (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 ;; The type
	 (the-type (intern-entity generator (string (find-plural-of-noun unknown-type)) :definite? nil :singular? nil))
	 ;; The Apprentice
	 (iap (intern-entity generator "I" :singular? t :definite? 'null))
	 ;; the user
	 (you (intern-entity generator "you" :singular? t :definite? 'null))
	 ;; tell
	 (verb (intern-new-instance generator "tell"))
	 )
    (create-main-clause generator you verb iap
			:is-imperative? nil
			:subject-is-wh? t
			:is-question? t)
    (create-modality generator verb '|can|)
    (create-about-phrase generator verb the-type)
    (vocalize-generator generator :vocalize? vocalize? :wait? t)))


;; To generate something like: "What is the type of the <input|output> <signal-name> that the <thing> produces?"
(defun query-about-IO-type (thing IO-name &key thing-definite? thing-plural? IO-name-plural? (direction 'input) (vocalize? t))
  (when IO-name-plural? (setq IO-name (string (find-plural-of-noun IO-name))))
  (when thing-plural? (setq thing (string (find-plural-of-noun thing))))
  (LET* ((generator (make-instance 'question-generator))
	 ;; The Imager
	 (thing (intern-entity generator thing :definite? thing-definite?))
	 ;; The output data
	 (IO-name (intern-entity generator IO-name :definite? t :modifier (ecase direction (input "input") (output "output"))))
	 ;; produce
	 (produce-or-consume (intern-new-instance generator (ecase direction (input "consume") (output "produce"))))
	 (type (intern-new-instance generator "type"))
	 )
	;; What is the type
	(create-main-clause generator (intern-new-instance generator "What")
			    (intern-new-instance generator "is-a")
			    type
			    :subject-is-wh? t
			    :is-imperative? nil
			    :is-question? t)
	;; the possessive "of the data"
	(create-possesive generator type IO-name :is-pp? t)
	;; The relative clasue "produced by the Imager"
	(add-relative-clause generator thing produce-or-consume IO-name :connective "that")
	(setf (current-parse generator) (nreverse (current-parse generator)))
	(vocalize-generator generator :vocalize? vocalize? :wait? t)
	))

;;; To generate something like "what is the rate at which x produces y?"
(defun query-about-IO-rate (thing IO-name &key thing-definite? thing-plural? IO-name-plural? (vocalize? t))
  (when IO-name-plural? (setq IO-name (string (find-plural-of-noun IO-name))))
  (when thing-plural? (setq thing (string (find-plural-of-noun thing))))
  (let* ((generator (make-instance 'question-generator))
	 ;; The Imager
	 (thing (intern-entity generator thing :definite? thing-definite?))
	 ;; The output data
	 (IO-name (intern-entity generator IO-name :definite? 'null))
	 ;; produce
	 (produce (intern-new-instance generator "produce"))
	 (rate (intern-new-instance generator "rate"))
	 (main (create-main-clause generator thing produce io-name 
				   :is-imperative? nil
				   :is-question? t :subject-is-wh? nil))
	 (at (intern-new-instance generator "at"))
	 (at-clause (intern-and-add-texp generator (list main at rate))))
	;; camera produces images
	(intern-and-add-texp generator (list at-clause (intern-constant generator "has_position") (intern-constant generator "trailing")))
	(intern-and-add-texp generator (list rate (intern-constant generator "has_det") (intern-constant generator "what")))
	;; (setf (current-parse generator) (nreverse (current-parse generator)))
	(vocalize-generator generator :vocalize? vocalize? :wait? t)
	))

(defun vocalize-generator (generator &key vocalize? wait?)
  (push (current-parse generator) (all-parses generator))
  (let ((string (generate-text generator)))
    (vocalize-text string :vocalize? vocalize? :wait? wait?)
    (values generator string)))

;;; Note: This is unlikely to work as is in any environment
;;; other than MACOS.  It depends on there being a shell command
;;; called "SAY" and the version of run-shell-command may only 
;;; work in Unix like environments.

(defun vocalize-text (string &key vocalize? wait?)
  (let ((vector (make-array 3)))
    (setf (aref vector 0) "say"
	  (aref vector 1) "say"
	  (aref vector 2) string)
    (when vocalize? (excl:run-shell-command vector :wait wait?))
    string))

;;; basic actions:
;;;  interning an entity as either a definite or not
;;;  introducing a main clause 
;;;  introducing a subordinate clause with or without a connective specified

(defmethod intern-entity ((generator question-generator) entity-name &key (definite? t) (modifier nil) (singular? t))
  (let ((interned-entity (intern-new-instance generator entity-name)))
    (unless (eql singular? 'none)
      (intern-and-add-texp generator (list interned-entity 
					   (intern-constant generator "has_number")
					   (intern-constant generator (if singular? "singular" "plural")))))
    (unless (eql definite? 'none)
      (intern-and-add-texp generator (list interned-entity 
					   (intern-constant generator "has_det")
					   (cond
					    ((eql definite? 'null) (intern-constant generator "null"))
					    ((null definite?) (intern-constant generator "indefinite"))
					    ((eql definite? t) (intern-constant generator "definite"))
					    ((symbolp definite?) (intern-constant generator definite?))
					    ))))
    (when modifier (create-modification generator interned-entity modifier))
    interned-entity))

(defmethod create-modification ((generator question-generator) interned-entity modifier)
  (let ((interned-modifier (intern-constant generator modifier))
	(has-property (intern-constant generator "has_property")))
    (values interned-modifier (intern-and-add-texp generator (list interned-entity has-property interned-modifier)))
    ))

(defmethod create-possesive ((generator question-generator) (owner instance) (owned instance) &key (is-pp? t))
  (let* ((related-to-texp (intern-and-add-texp generator (list owner (intern-new-instance generator '|related-to|) owned))))
    (when is-pp?
      (intern-and-add-texp generator (list related-to-texp (intern-constant generator '|is_pp|)
					   (intern-constant generator '|yes|)))
    related-to-texp
    )))

(defmethod intern-verb ((generator question-generator) (verb-string string) &key negative tense progressive modality voice)
  (let ((the-verb (intern-new-instance generator verb-string)))
    (when negative (make-verb-negative generator the-verb))
    (when tense (add-verb-modifier generator the-verb '|has_tense| tense))
    (when progressive (add-verb-modifier generator the-verb '|is_progressive| '|yes|))
    (when modality (add-verb-modifier generator the-verb '|has_modal| modality))
    (when voice (add-verb-modifier generator the-verb '|has_voice| voice))
    the-verb))

(defmethod add-verb-modifier ((generator question-generator) (the-verb instance) (modification-type symbol) (modification-value symbol))
  (intern-and-add-texp generator (list the-verb 
				       (intern-constant generator modification-type)
				       (intern-constant generator modification-value))))

				       

(defmethod make-verb-negative ((generator question-generator) (verb instance))
  (intern-and-add-texp generator (list verb
				       (intern-constant generator '|is_negative|)
				       (intern-constant generator '|yes|))))

(defmethod add-relative-clause ((generator question-generator) (subject instance) (relation instance) (object instance) &key (connective nil))
  (let ((the-clause (intern-and-add-texp generator (list subject relation object))))
    (values
     the-clause
     (when connective
       (intern-and-add-texp generator (list the-clause 
					    (intern-constant generator '|has_clause_type|)
					    (intern-constant generator connective)))))))

(defmethod create-main-clause ((generator question-generator) subject relation object 
			       &key (is-question? nil) (subject-is-wh? nil) (is-imperative? nil) (is-passive? nil))
  (let ((main-clause (intern-and-add-texp generator (list subject relation object)))
	(yes-constant (intern-constant generator '|yes|)))
    (intern-and-add-texp generator (list main-clause
					 (intern-constant generator '|is_main|)
					 yes-constant))
    (when is-imperative?
      (intern-and-add-texp generator (list main-clause
					   (intern-constant generator '|is_imperative|)
					   yes-constant)))
    (when is-question?
      (intern-and-add-texp generator (list main-clause 
					   (intern-constant generator '|is_question|)
					   yes-constant)))
    (when is-passive?
      (intern-and-add-texp generator (list main-clause 
					   (intern-constant generator '|has_voice|)
					   (intern-constant generator '|passive|))))
    ;; identifying that "what" is a wh
    (when subject-is-wh?
      (intern-and-add-texp generator (list subject (intern-constant generator '|is_wh|) yes-constant)))
    main-clause))

(defmethod create-about-phrase ((generator question-generator) (verb instance) (object instance))
  (intern-and-add-texp generator (list verb (intern-new-instance generator '|about|) object)))

(defmethod create-modality ((generator question-generator) (verb instance) (modality symbol))
  (intern-and-add-texp generator (list verb (intern-constant generator '|has_modal|) (intern-constant generator modality))))

(defmethod create-using-clause ((generator question-generator) clause entity)
  (intern-and-add-texp generator (list clause (intern-new-instance generator "using") entity)))

(defparameter *ignorable-lexical-entry-elements* '()
  )

(defclass xml-lexical-entry-parser (sax-parser)
  ((current-string :initform nil :accessor current-string)
   (current-category-entry :initform nil :accessor current-category-entry)
   (current-property-entry :initform nil :accessor current-property-entry)
   (definition-entry :Initform nil :accessor definition-entry)
   (state :initform nil :accessor state)
   (stack :initform nil :accessor stack)
   ))

(defclass has-other-properties-mixin ()
  ((other-properties :initform nil :accessor other-properties)))

(defclass definition-entry (has-other-properties-mixin)
  ((word :initform nil :Initarg :word :accessor word)
   (categories :Initform nil :accessor categories)))

(defclass category-entry (has-other-properties-mixin)
  ((category :initform nil :initarg :category :accessor category)
   (inflection-set :Initform nil :accessor inflection-set)
   ))

(defmethod start-element ((parser xml-lexical-entry-parser) iri localname qname attrs)
  (declare (ignore iri qname))
  (unless (member localname *ignorable-lexical-entry-elements* :test #'string-equal)
    (let ((interned-local-name (intern (string-upcase localname))))
      (start-trace t "~%Starting element ~s ~{~a~^, ~}" interned-local-name attrs)
      (my-start-element parser interned-local-name attrs)
      ))
  (values))

(defmethod end-element ((parser xml-lexical-entry-parser) iri localname qname)
  (declare (ignore iri qname))
  (unless (member localname *ignorable-lexical-entry-elements* :test #'string-equal)
    (let ((interned-local-name (intern (string-upcase localname) )))
      (start-trace t "~%Ending element ~s" interned-local-name)
      (my-end-element parser interned-local-name (state parser))
      ))
  (values))

(defmethod my-start-element ((parser xml-lexical-entry-parser) (localname (eql 'definition)) attrs)
  (let ((word (cdr (assoc "word" attrs :test #'string-equal))))
    (when word
      (let ((word (intern word)))
	(let ((entry (make-instance 'definition-entry :word word)))
	  (setf (state parser) 'definition
		(definition-entry parser) entry)
	  )))))

(defmethod my-end-element ((parser xml-lexical-entry-parser) (localname (eql 'definition)) current-state)
  (unless (eql current-state 'definition)
    (error "Unmatched definition entries"))
  )

(defmethod my-start-element ((parser xml-lexical-entry-parser) (localname (eql 'category)) attrs)
  (let ((category (cdr (assoc "value" attrs :test #'string-equal))))
    (start-trace t "~%Category ~a ~{~a~^, ~}" category attrs)
    (when category 
      (let ((category (intern category)))
	(let ((current-entry (make-instance 'category-entry :category category)))
	  (push current-entry (categories (definition-entry parser)))
	  (push (state parser) (stack parser))
	  (setf (current-category-entry parser) current-entry
		(state parser) 'category)
	  )))))

(defmethod my-end-element ((parser xml-lexical-entry-parser) (localname (eql 'category)) current-state)
  (unless (eql current-state 'category)
    (error "Unmatched category entries"))
  (setf (current-category-entry parser) nil
	(state parser) (pop (stack parser)))
  )

(defmethod my-start-element ((parser xml-lexical-entry-parser) (localname (eql 'inflection_set)) attrs)
  (declare (ignore attrs))
  (push (state parser) (stack parser))
  (setf (state parser) 'inflection-set)
  )

(defmethod my-end-element ((parser xml-lexical-entry-parser) (localname (eql 'inflection_set)) current-state)
  (unless (eql current-state 'inflection-set)
    (error "Unmatched inflection sets"))
  (setf (state parser) (pop (stack parser)))
  )

(defmethod my-start-element ((parser xml-lexical-entry-parser) (localname (eql 'property)) attrs)
  (let ((old-state (state parser)))
    (push (state parser) (stack parser))
    (setf (state parser) 'property)
    (let ((property-name (cdr (assoc "name" attrs :test #'string-equal))))
      (when property-name
	(let ((entry (list (convert-start-string-to-lisp-atom property-name) nil)))
	  (setf (current-property-entry parser) entry)
	(cond
	 ((eql old-state 'inflection-set)
	  (push entry (inflection-set (current-category-entry parser))))
	 ((eql old-state 'category)
	  (push entry (other-properties (current-category-entry parser))))
	 ((eql old-state 'definition)
	  (push entry (other-properties (definition-entry parser))))
	 ))))))

(defmethod my-end-element ((parser xml-lexical-entry-parser) (localname (eql 'property)) current-state)
  (unless (eql current-state 'property)
    (error "Unmatched property entries"))
  (setf (state parser) (pop (stack parser))
	(current-property-entry parser) nil)
  )

(defmethod my-start-element ((parser xml-lexical-entry-parser) (localname (eql 'value)) attrs)
  (declare (ignore attrs))
  (setf (current-string parser) nil)
  )

(defmethod my-end-element ((parser xml-lexical-entry-parser) (localname (eql 'value)) current-state)
  (cond ((eql current-state 'property)
	 (setf (second (current-property-entry parser))
	   (convert-start-string-to-lisp-atom (current-string parser))))
	(t (error "Orphaned value entry"))))

;;; CONTENT always occurs within an INSTANCE or CONSTANT for which it acculuates a string
;;; which is held in current-string
;;; It does not affect the stack state
(defmethod content ((parser xml-lexical-entry-parser) content start end ignorable)
  (declare (ignore ignorable))
  (unless (eql end (1+ start))
    (let ((substring (string-trim '(#\newline #\return #\space) (subseq content start end)))
	  (current-string (current-string parser)))
      (start-trace t "~%Getting content ~a" substring)
      ;; The documentation says that you may get repeated calls for fragments of the string
      ;; so this checks for that and concatenates them if so.
      ;; I haven't seen this actually happen.
      (setf (current-string parser) 
	(if (null current-string)
	    substring
	  (concatenate 'string current-string substring))
      )))
  (values))

(defmethod my-start-element ((parser xml-lexical-entry-parser) localname attrs)
  (values localname attrs)
  )

(defmethod my-end-element ((parser xml-lexical-entry-parser) localname current-element)
  (values localname current-element)
  )

;;; note: anyplace that wants to find singular or plural form or any other lexicon lookup
;;; should first conver to a lisp-atom (convert-string-to-lisp-atom ..)
;;; and then do the lexicon lookup.  Or they can call parse-lexical-entry with a string
;;; which will do this for them.

(defparameter *lexicon* (make-hash-table))

(defmethod parse-lexical-entry ((word symbol) (format (eql :xml)) &optional (server "guest"))
  (or (gethash word *lexicon*)
      (let ((start-lexicon-format (convert-to-lexicon-format word)))
        (multiple-value-bind (input code) (get-lexical-entry start-lexicon-format :encoding "xml" :server server)
          (declare (ignore code))	  
          (multiple-value-bind (flag parse) (sax-parse-string input :class ' xml-lexical-entry-parser)
            (declare (ignore flag))
            (setf (gethash word *lexicon*) (definition-entry parse))
            (definition-entry parse)
            )))))

(defmethod parse-lexical-entry ((word string) (format (eql :xml)) &optional (server "guest"))
  (let ((word (convert-start-string-to-lisp-atom word)))
    (parse-lexical-entry word format server)))

(defun convert-to-lexicon-format (string-or-symbol)
  (let* ((string (string string-or-symbol))
         (length (length string))
         (new-string (make-string length)))
    (loop for i below length
        for old-char = (aref string i)
        for new-char = (if (member old-char '(#\- #\_) :test #'char-equal)
                           #\space
                         old-char)
        do (setf (aref new-string i) new-char))
    new-string))

(defun convert-start-string-to-lisp-atom (string)
  (intern (convert-start-string-to-lisp-string string)))

(defun convert-start-string-to-lisp-string (string-or-symbol)
  (let* ((string (string string-or-symbol))
	 (length (length string))
	 (new-string (make-string length)))
    (loop for i below length
	for old-char = (aref string i)
	for new-char = (if (member old-char '(#\space #\_))
			   #\-
			 (char-upcase old-char))
	do (setf (aref new-string i) new-char))
    new-string))

(defun find-plural-of-noun (noun)
  (let* ((definition-entry (parse-lexical-entry noun :xml)))
    (if definition-entry
	(let ((noun-entry (find 'noun (categories definition-entry) :key #'category)))
	  (second (assoc 'has-plural (inflection-set noun-entry))))
      (intern (string-upcase (concatenate 'string (string noun) "s")))
      )))

(defun find-singular-of-noun (noun)
  (let* ((definition-entry (parse-lexical-entry noun :xml)))
    (if definition-entry
	(let ((noun-entry (find 'noun (categories definition-entry) :key #'category)))
	  (second (assoc 'has-singular (inflection-set noun-entry))))
      noun)))

(defun get-inflection-of-adjective (adjective inflection)
  (let ((definition-entry (parse-lexical-entry adjective :xml)))
    (if definition-entry 
	(let ((keyword (case inflection (superlative 'has-superlative) (comparative 'has-comparative) (root 'adjective-root)))
	      (adjective-entry (find 'adjective (categories definition-entry) :key #'category)))
	  (second (assoc keyword (other-properties adjective-entry))))
      adjective)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Parsing xml output format of generate request
;;;
;;;   All we really want to do is get the cdata forms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass xml-generate-response-parser (sax-parser)
  ((current-string :initform nil :accessor current-string)
   (all-strings :initform nil :accessor all-strings)))

(defmethod start-document ((parser xml-generate-response-parser))
  )

(defmethod end-document ((parser xml-generate-response-parser))
  (with-slots (net.xml.sax::char-table) parser
    (setq net.xml.sax::char-table nil))
  (setf (all-strings parser) (nreverse (all-strings parser)))
  )

;;; This is the dispatcher to specific tag handlers
;;; One each for entry and exit
(defmethod content ((parser xml-generate-response-parser) content start end ignorable)
  (declare (ignore ignorable))
  (unless (eql end (1+ start))
    (let ((substring (string-trim '(#\newline #\return #\space) (subseq content start end)))
	  (current-string (current-string parser)))
      (start-trace t "~%Getting content ~a" substring)
      ;; The documentation says that you may get repeated calls for fragments of the string
      ;; so this checks for that and concatenates them if so.
      ;; I haven't seen this actually happen.
      (setf (current-string parser) 
	(if (null current-string)
	    substring
	  (concatenate 'string current-string substring))
      )))
  (values))

(defmethod start-element ((parser xml-generate-response-parser) iri localname qname attrs)
  (declare (ignore qname))
  (let ((interned-local-name (intern (string-upcase localname))))
    (start-trace t "~%Starting element ~a ~a ~{~a~^, ~}" iri interned-local-name attrs)
    (when (eql interned-local-name 'interaction)
      (setf (current-string parser) nil)))
  (values))

(defmethod end-element ((parser xml-generate-response-parser) iri localname qname)
  (declare (ignore iri qname))
  (let ((interned-local-name (intern (string-upcase localname))))
    (start-trace t "~%Ending element ~a" interned-local-name)
    (when (eql interned-local-name 'interaction)
      (push (current-string parser) (all-strings parser))
      ))
  (values))

(defmethod generate-text ((parser core-parser-mixin) &key (format :text) (server "guest") (output-format :text))
  (let ((response (make-a-generate-request (string-for parser format)
					   :server server
					   :output-format output-format
					   :format format)))
    (case output-format
      (:text
       (string-trim '(#\newline #\space) response))
      (:xml
       (multiple-value-bind (flag parser) (sax-parse-string response :class 'xml-generate-response-parser)
	 (declare (ignore flag))
	 (apply #'concatenate 'string (all-strings parser))))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for groveling over START triples
;;;
;;; This should allow us to write comprehensible rules
;;; that say what function should be called to process
;;; a top-level utterance
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicate is-appropriate-response (texp-relation texp function args) (default-predicate-model))
(define-predicate as-relation (interned-object texp) (default-predicate-model))
(define-predicate as-subject (interned-object texp) (default-predicate-model))
(define-predicate as-object (interned-object texp) (default-predicate-model))
(define-predicate relation-of (texp interned-object interned-object-name) (default-predicate-model))
(define-predicate subject-of (texp interned-object interned-object-name) (default-predicate-model))
(define-predicate object-of (texp interned-object interned-object-name) (default-predicate-model))
(define-predicate name (interned-object interned-object-name) (default-predicate-model))

(define-predicate parse-type-answer (texp aggregate type) (default-predicate-model))

(define-predicate-method (ask-data relation-of) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	   :query self
	   :model (common-lisp:type-of self)))
  (with-statement-destructured (texp interned-thing name) self
    (cond
     ((unbound-logic-variable-p texp)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t (let* ((relation (relation texp))
	       (rel-name (name relation)))
	  (with-unification
	      (unify  relation interned-thing)
	    (unify rel-name name)
	    (stack-let ((backward-support (list self +true+ '(ask-data relation-of))))
		       (funcall continuation backward-support))))))))

(define-predicate-method (ask-data subject-of) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	   :query self
	   :model (common-lisp:type-of self)))
  (with-statement-destructured (texp interned-thing name) self
    (cond
     ((unbound-logic-variable-p texp)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t (let* ((subject (subject texp))
	       (subject-name (when (not (typep subject 'texp)) (name subject))))
	  (with-unification
	      (unify subject interned-thing)
	    (unify subject-name name)
	    (stack-let ((backward-support (list self +true+ '(ask-data subject-of))))
		       (funcall continuation backward-support))))))))

(define-predicate-method (ask-data object-of) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	   :query self
	   :model (common-lisp:type-of self)))
  (with-statement-destructured (texp interned-thing name) self
    (cond
     ((unbound-logic-variable-p texp)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t (let* ((object (object texp))
	       (object-name (when (not (typep object 'texp)) (name object))))
	  (with-unification
	      (unify (object texp) interned-thing)
	    (unify object-name name)
	    (stack-let ((backward-support (list self +true+ '(ask-data object-of))))
		       (funcall continuation backward-support))))))))

(define-predicate-method (ask-data as-relation) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	    :query self
	    :model (common-lisp:type-of self)))
  (with-statement-destructured (interned-thing texp) self
    (cond
     ((unbound-logic-variable-p interned-thing)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t
      (loop for possible-answer in (as-relation interned-thing)
	  do (with-unification
		 (unify possible-answer texp)
	       (stack-let ((backward-support (list self +true+ '(ask-data as-relation))))
			  (funcall continuation backward-support))))))))

(define-predicate-method (ask-data as-subject) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	    :query self
	    :model (common-lisp:type-of self)))
  (with-statement-destructured (interned-thing texp) self
    (cond
     ((unbound-logic-variable-p interned-thing)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t
      (loop for possible-answer in (as-subject interned-thing)
	  do (with-unification
		 (unify possible-answer texp)
	       (stack-let ((backward-support (list self +true+ '(ask-data as-subject))))
			  (funcall continuation backward-support))))))))

(define-predicate-method (ask-data as-object) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	    :query self
	    :model (common-lisp:type-of self)))
  (with-statement-destructured (interned-thing texp) self
    (cond
     ((unbound-logic-variable-p interned-thing)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t
      (loop for possible-answer in (as-object interned-thing)
	  do (with-unification
		 (unify possible-answer texp)
	       (stack-let ((backward-support (list self +true+ '(ask-data as-object))))
			  (funcall continuation backward-support))))))))

(define-predicate-method (ask-data name) (truth-value continuation)
  (unless (eql truth-value +true+)
    (error 'ji:model-can-only-handle-positive-queries
	    :query self
	    :model (common-lisp:type-of self)))
  (with-statement-destructured (interned-thing name) self
    (cond
     ((unbound-logic-variable-p interned-thing)
      (error 'ji:model-cant-handle-query
	     :query self
	     :model (common-lisp:type-of self)))
     (t
      (with-unification
	  (unify name (name interned-thing))
	(stack-let ((backward-support (list self +true+ '(ask-data name))))
		   (funcall continuation backward-support)))))))

(defun stringify (thing &optional recursive)
  (typecase thing
    (symbol (string (if recursive (find-plural-of-noun thing) thing)))
    (list (if recursive 
	      (format nil "~{~a~^ of ~}" 
		      (loop for x in thing collect (stringify x t)))
	    (format nil "a ~a of ~{~a~^ of ~}"
		    (first thing) (loop for x in (rest thing) collect (stringify x t)))))))


(defun recursively-qualify (generator initial-entity list-of-stuff)
  (loop for this-entity = initial-entity then next-entity
      for thing in list-of-stuff
      for next-entity = (if (symbolp thing) 
			    (intern-entity generator (string (find-plural-of-noun thing)) :definite? nil :singular? nil)
			  (recursively-qualify generator this-entity thing))
      do (create-possesive generator this-entity next-entity)
      finally (return next-entity)
	      ))


(defmacro speak-and-print (stream form)
  `(let ((generator ,form)
	 (string nil))
     (multiple-value-setq (generator string) (vocalize-generator generator :vocalize? t :wait? t))
     (format ,stream "~%~a" string)))