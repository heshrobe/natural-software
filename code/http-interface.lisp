;;; -*- Mode: Common-lisp; package: natsoft -*-

(in-package :natsoft)
;;; This does a simple request to parse.
;;; It doesn't check the response code (yet)
;;; The return value is a string including triples delimited by brackets

(defparameter *start-server* "http://start.csail.mit.edu/api.php")
;;; old address is http://start.csail.mit.edu/ askstart.cgi
;;; (defparameter *start-server* "http://start.csail.mit.edu/askstart.cgi")

(defun make-a-parse-request (text &key (encoding "formatted-text") (server  "guest"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:query `(("server" . ,server)
		 ("action" . "parse")
		 ("uuid" . "iap-project")
		 ("te" . ,encoding)
		 ("kb" . "no")
		 ("query" . ,text)))
    (declare (ignore headers uri))
    (values response code)
    ))

(defun make-an-assert-request (text &key (encoding "formatted-text") (server "guest"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:query `(("server" . ,server)
		 ("action" . "parse")
		 ("uuid" . "iap-project")
		 ("te" . ,encoding)
		 ("kb" . "yes")
		 ("query" . ,text)))
    (declare (ignore headers uri))
    (values response code)
    ))

(defun make-a-query-request (text &key (encoding "formatted-text") (server "guest") (uuid "nerd-project"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:query `(("server" . ,server)
		 ("action" . "query")
		 ("uuid" . ,uuid)
		 ("te" . ,encoding)
		 ("kb" . "yes")
		 ("query" . ,text)))
    (declare (ignore headers uri))
    (values response code)
    ))

;;; could also use get-word-base and get-word-inflections as the actions
(defun get-lexical-entry (word &key (encoding "formatted-text") (server "guest") (external-sources "w"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
        :query `(("server" . ,server)
                 ("action" . "show-word")
                 ("uuid" . "iap-project")
                 ("te" . ,encoding)
                 ("kb" . "yes")
                 ("query" . ,word)
                 ("ed" , external-sources)))
    (declare (ignore headers uri))
    (values response code)
    ))

(defun get-word-base (word category &key (encoding "formatted-text") (server "guest"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:query `(("server" . ,server)
		 ("action" . "get-word-base")
		 ("uuid" . "iap-project")
		 ("te" . ,encoding)
		 ("kb" . "yes")
		 ("query" . ,(format nil "(~s ~a)" word category))))
    (declare (ignore headers uri))
    (values response code)
    ))

(defun get-word-inflections (word category &key (encoding "formatted-text") (server "guest"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:query `(("server" . ,server)
		 ("action" . "get-word-inflections")
		 ("uuid" . "iap-project")
		 ("te" . ,encoding)
		 ("kb" . "yes")
		 ("query" . ,(format nil "(~s ~a)" word category))))
    (declare (ignore headers uri))
    (values response code)
    ))

(defun add-lexical-entry (word &key (type 'noun)
				    use-sentence
				    (encoding "formatted-text")
				    (server "guest"))
  (let ((query (format nil "(~a ~s ~s)" (string type) (string word) use-sentence )))
    (do-http-request  *start-server*
      :query `(("server" . ,server)
	       ("action" . "create-word-def")
	       ("uuid" . "iap-project")
	       ("te" . ,encoding)
	       ("kb" . "yes")
	       ("query" . ,query)))))

;;;Syntax: (noun "singular" "plural" gender proper? mass?)
;;;Notes: You can supply nil if the plural is regular; the plural form will be derived according to standard English spelling rules.
;;;Examples:
;;;(noun "mibby" nil neuter nil t)
;;;Neuter mass noun, regular plural "mibbies" (although as a mass noun, the plural will rarely if ever be used).
;;;(noun "floofe" "flooves" masculine nil nil)
;;;Masculine noun, irregular plural "flooves".
;;;(noun "Frobozz" nil neuter t nil)
;;;Neuter proper noun, plural "Frobozzes" (although as a proper noun, the plural will rarely if ever be used).

;;;verb:
;;;Syntax: (verb "infinitive" "past" "present-participle" "past-participle" "present-3rd-singular" transitive connective)
;;;Notes: You can supply nil in place of regular forms other than the infinitive; they will be derived according to standard English spelling rules. The "connective" property refers to what kinds of sentential clauses a verb allows, e.g., "Mary thinks that John is happy", "Mary believes John to be happy"—if you need to specify this, talk to us.
;;;Examples:
;;;(verb "mibby" nil nil nil nil nil nil)
;;;Regular intransive verb: "Now I mibby, yesterday I mibbied, I like mibbying, I have often mibbied, Mary also mibbies".
;;;(verb "hear" "heard" "hearing" "heard" "hears" t nil)
;;;Irregular transitive verb.
;;;(verb "flooble" nil nil nil nil (t nil) nil)
;;;Regular verb that can be transitive or intransitive.


(defun add-noun-definition (word &key (plural nil)
				      (gender '|neuter|)
				      (proper? nil)
				      (mass? nil)
				      (server "guest"))
  (let ((definition (format nil "(noun ~s ~a ~a ~a ~a)" word plural gender proper? mass?)))
    (add-word-definition definition :server server)))

(defun add-verb-definition (word &key infinitive past present-participle past-participle present-3rd-singular transitive? connective
				      (server "guest"))
  (let ((definition (format nil "(verb ~s ~a ~a ~a ~a ~a ~a ~a)" 
			    word infinitive past present-participle past-participle present-3rd-singular transitive? connective)))
    (add-word-definition definition :server server)))

(defun add-adjective-definition (word &key (server "guest"))
  (let ((definition (format nil "(adjective ~s)" word)))
    (add-word-definition definition :server server)))

;;; NOTE: The definition must be a string that reads as a list
;;; See add-noun-definition above
(defun add-word-definition (definition &key (server "guest"))
    (multiple-value-bind (response code headers uri)
	(do-http-request *start-server*
	  :query `(("server" . ,server)
		   ("action" . "add-word")
		   ("uuid" . "iap-project")
		   ("te" . "formatted-text")
		   ("kb" . "yes")
		   ("query" . ,definition)))
      (declare (ignore headers uri))
      (values response code)
      ))

(defun add-word-definition-by-example (word category sentence
				       &key (server "guest"))
  (let ((definition (format nil "(~a ~s ~s)" category (string word) sentence)))
    (add-word-definition (create-word-definition definition :server server)
			 :server server
			 )))

(defun create-word-definition (definition &key (server "guest"))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
		       :query `(("server" . ,server)
				("action" . "create-word-def")
				("uuid" . "iap-project")
				("te" . "formatted-text")
				("kb" . "yes")
				("query" . ,definition)))
    (declare (ignore headers uri))
    (values response code)
    ))


(defun flush-knowledge-base (&optional (server "guest"))
  (do-http-request *start-server*
    :query `(("server" . ,server)
	     ("action" . "flush")
	     ("uuid" . "iap-project")
	     ("te" . "text")
	     ("kb" . "yes")
	     ("query" . "Forget everything"))))

(defun make-a-generate-request (triples &key (format :text) (server "guest") (output-format :text))
  (multiple-value-bind (response code headers uri)
      (do-http-request *start-server*
	:method :post
	:protocol :http/1.0
	:query `(("server" . ,server)
		 ("action" . "generate")
		 ("uuid" . "iap-project")
		 ("qe" . ,(case format (:text "formatted-text") (:xml "XML")))
		 ("te" . ,(case output-format (:text "formatted-text") (:xml "XML")))
		 ("kb" . "no")
		 ("query" . ,triples)))
    ;; (declare (ignore headers uri))
    (values response code headers uri)))

(defparameter *test-sentences*
    (list  "the computer has a sensor"
	   "a camera is a type of sensor"
	   "a camera is a kind of input device"
	   "all input devices have controls"
	   "the bandwidth of the sensor is 220 kilobytes per second"
	   "The output rate of the camera is 100 kilobytes per second"
	   "a byte has 8 bits"
	   "there are 8 bits to a byte"
	   "there are 1000 bytes in a kilobyte"
	   "a kilobyte has 1000 bytes"
	   "the imager's output is an image"
	   "an image is an array of 8 bit bytes"
	   "The conduit connects port A of module B to port C of module D"
	   "Port A of module B is connected to port C of module D"

	   ))

(defparameter gen-ex "[girl+27 kick+1 ball+28]
           [girl+27 has_number singular]
           [girl+27 has_det definite]
           [ball+28 has_number singular]
           [ball+28 has_det definite]
           [kick+1 is_main Yes]
           [kick+1 has_person 3]
           [kick+1 has_tense past]")