;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 3:
;;; Pretty Print for a Particular Language
;;;  for LISP this is trivial
;;;  for Java we walk the "AST" built by the generation stage and dispatch to specific formatters
;;;   for each form type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass java-print-controller ()
  ((class-name :initform 'main :accessor class-name :initarg :class-name)
   (super-class :initform nil :accessor super-class)
   (include-main? :initform nil :accessor include-main? :initarg :include-main?)
   (context :initform nil :accessor context)
   (protections :initform nil :accessor protections)
   (static? :initform nil :accessor static?)
   ))

(defun set-java-generation-parameters (&optional (stream *standard-output*))
  (let* ((dip (design-in-progress clim:*application-frame*))
         (controller (generation-controller dip)))
    (unless controller
      (setq controller (make-instance 'java-print-controller))
      (setf (generation-controller dip) controller))
    (clim:accepting-values (stream :own-window t)
      (setf (class-name controller) 
        (clim:accept 'symbol :stream stream :prompt "Class name" 
                     :default (class-name controller)))
      (terpri stream)
      (setf (super-class controller) 
        (clim:accept 'symbol :stream stream :prompt "Superior Class" 
                     :default (super-class controller)))
      (terpri stream)
      (setf (include-main? controller) 
        (clim:accept 'boolean :stream stream :prompt "Include Main?"  
                     :default (include-main? controller)))
      (terpri stream)
      (setf (context controller) 
        (clim:accept 'symbol :stream stream :prompt "Context"  
                     :default (context controller)))
      (terpri stream)
      (setf (protections controller) 
        (clim:accept '(clim:subset "private" "public" "protected" "final") :stream stream :prompt "Protections"  
                     :default (protections controller)))
      (terpri stream)
      (setf (static? controller) 
        (clim:accept 'clim:boolean :stream stream :prompt "Is Method Static?"  
                     :default (static? controller)))
      )))

(define-predicate context-include (context package-name))
(define-predicate context-package (context package-name))

(defun format-contextual-stuff (context stream)
  (when context
    (let ((imports nil)
          (package nil))
      (ask* `[context-include ,context ?include]
            (push ?include imports))
      (ask* `[context-package ,context ?package]
            (setq package ?package ))
      (when package
        (format stream "~%package ~:a;" package)
        (terpri stream))
      (when imports
        (loop for import in imports 
            do (format stream "~%import ~:a;" import))
        (terpri stream)))))

;;; (defparameter *foo* nil)

(defmethod pretty-print-for-language (code stream (language (eql :java)))
  ;;  (declare (special foobar))
  ;;  (setq foobar code)
  (let ((type (first code)))
    (jpp type (cdr code) stream 0))
  )

(defun javaize-identifier (identifier)
  (cond 
   ((not (symbolp identifier)) identifier)
   ((get identifier 'java-identifier))
   (t (let ((string (string identifier)))
        (let* ((fragments (loop 
                              for last-dash-pos = 0 then (1+ next-dash-pos)
                              for first = t then nil
                              for next-dash-pos = (position #\- string :test #'char-equal :start last-dash-pos)
                              for fragment = (subseq string last-dash-pos next-dash-pos)
                              collect (if first (string-downcase fragment) (string-capitalize fragment))
                              until (null next-dash-pos)
                                    ))
               (fixed-string (apply #'concatenate 'string fragments))
               (new-identifier (intern fixed-string)))
          (setf (get identifier 'java-identifier) new-identifier)
          new-identifier)))))

(defun javaize-class-name (identifier)
  (if (not (symbolp identifier)) identifier
    (or (get identifier 'java-class-name)
        (let ((string (string identifier)))
          (let* ((fragments (loop for last-dash-pos = 0 then (1+ next-dash-pos)
                                for next-dash-pos = (position #\- string :test #'char-equal :start last-dash-pos)
                                for fragment = (subseq string last-dash-pos next-dash-pos)
                                collect (string-capitalize fragment)
                                until (null next-dash-pos)))
                 (fixed-string (apply #'concatenate 'string fragments))
                 (new-identifier (intern fixed-string)))
            (setf (get identifier 'java-class-name) new-identifier)
            new-identifier)))))


; public class HelloWorld {

;     public static void main (String[] args) {
;         // Prints "Hello, World" to the terminal window.
;         System.out.println("Hello, World");
;     }

; }

(defun jpp-dispatch (form stream indent)
  (if (or (numberp form) (symbolp form))
      (format stream "~:a" form)
    (jpp (first form) (rest form) stream indent)))

(defgeneric jpp (type code stream indent))

(defmethod jpp ((type (eql :class-definition)) code stream indent)
  (destructuring-bind (&key visible? name super-class methods variables context) code
    ;; for now ignore class variables
    (declare (ignore variables))
    (format-contextual-stuff context stream)
    (format stream "~%~vt~:[private~;public~] class ~a~@[ extends ~:a~] {" 
	    indent visible? name super-class)
    (loop for method in methods do (jpp-dispatch method stream (+ indent 3)))
    (format stream "~%~vt}" indent)
    ))

;;; MAIN :VISIBLE? T :STATIC? T :RESULT |void| :ARGS (|String| [] |args|) :BODY ((|DoIt| NIL))
(defmethod jpp ((type (eql :method)) code stream indent)
  (destructuring-bind (&key name static? result args body state-variable-code outputs protections) code
    (format stream "~%~vt~{~:a ~^ ~}~:[~;static~] ~a ~a"
	    indent protections static? 
	    (java-version-of-type (first result))
	    name)
    (format stream " (")
    (loop for (arg . rest) on args
	do (jpp-dispatch arg stream 0)
	when rest do (format stream ", "))
    (format stream ")")
    (format stream " {")
    (jpp-format-block state-variable-code stream (+ indent 3))
    (jpp-format-block body stream (+ indent 3))
    (when outputs
      (format stream "~%~vtreturn " indent)
      (jpp-dispatch (first outputs) stream indent)
      (format stream ";~%~vt}" indent))))

(defun jpp-format-block (body stream indent &optional introduce-block)
  (when introduce-block 
    (format stream "~%~vt{" indent)
    (incf indent))
  (loop for stuff in body 
      do (format stream "~%~vt" indent)
	 (let ((needs-semi-colon (jpp-dispatch stuff stream indent)))
	   (when needs-semi-colon
	     (format stream ";")))
      finally (decf indent)
	      (when introduce-block (format stream "~%~vt}" indent))
	      ))

;;; Operation . arguments e.g. (:application System.out.println "Hello, World" "Mumble")
(defmethod jpp ((type (eql :application)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (function object &rest args) code
    ;; object is nil if making a call to a static method
    (when object 
      (jpp-dispatch object stream 0)
      (format stream "."))
    (format stream "~:a(" (javaize-identifier function))
    (loop for (token . rest) on args
	if (or (symbolp token) (numberp token))
	do (format stream "~:a" token)
	else do (jpp-dispatch token stream 0)
	when rest do (format stream ", "))
    (format stream ")"))
  (values t)
  )

(defmethod jpp ((type (eql :array-access)) code stream indent)
  (destructuring-bind (array . indices) code
    (jpp-dispatch array stream indent)
    (loop for index in  indices
	do
	  (format stream "[")
	  (jpp-dispatch index stream indent)
	  (format stream "]"))
    ))

;;; For 2nd dimension if you have the first dimension
;;; passed in then use it.
(defmethod jpp ((type (eql :array-length)) code stream indent)
  (destructuring-bind (array index &optional (other-index '(:constant 0))) code
    (jpp-dispatch array stream indent)
    (cond
     ((eql index 1) (format stream ".length"))
     (t (format stream "[")
	(jpp-dispatch other-index stream indent)
	(format stream "].length" other-index)))))

(defmethod jpp ((type (eql :break)) code stream indent)
  (declare (ignore indent))
  (format stream "break ~:a" (first code))
  (values t)
  )

;;; This is the template for loop with both normal and abnormal exits
; abnormal: {
;     for (i = 0; i < a.length; i++)
; 	{if (abnormal-exit-test)
; 		{
; 		    abnormal-exit-code;
; 		    break abnormal;
; 		}
; 	}
;     normal-exit-code;
; }
					; code-after-for-loop
;;; At the moment, I don't think I know whether there's an abnormal exit
;;; so I'll generate this template always until that gets fixed.



(defmethod jpp ((type (eql :for)) code stream indent)
  (destructuring-bind (loop-variable &key init refresh terminate body exit-code abnormal-exit?) code
    (when abnormal-exit?
      (format stream "abnormal-exit:{"))
    ;; iteration description
    (format stream "~%~vtfor (" indent)
    (jpp-dispatch loop-variable stream indent)
    (format stream " = ")
    (jpp-dispatch init stream 0)
    (format stream "; " stream)
    (jpp-dispatch terminate stream 0)
    (format stream "; " stream) 
    (jpp-dispatch refresh stream 0)
    (format stream ")")
    ;; Now the body
    (jpp-format-block Body stream (+ indent 3) t)
    ;; And the exit code if any
    (jpp-format-block exit-code stream indent)
    (when abnormal-exit?
      (format stream "~%~vt}" indent)))
  (values nil))

;;; this is something like a + b which doesn't start a new line
;;; usually inside an assignment or a for

(defmethod jpp ((type (eql :binary-op)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (operation operand-1 operand-2) code
    (jpp-dispatch operand-1 stream 0)
    (format stream " ~a " operation)
    (jpp-dispatch operand-2 stream 0))
  (values nil))

(defmethod jpp ((type (eql :binary-comparison)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (operation operand-1 operand-2) code
    (jpp-dispatch operand-1 stream 0)
    (format stream " ~a " operation)
    (jpp-dispatch operand-2 stream 0)
    )
  (values nil))

(defmethod jpp ((type (eql :increment)) code stream indent)
  (declare (ignore indent))
  (let* ((variable (first code)))
    (jpp-dispatch (javaize-identifier variable) stream 0)
    (format stream "++;")
    )
  (values nil))
  
  
(defmethod jpp ((type (eql :assignment)) code stream indent)
  (declare (ignore indent))
  (destructuring-bind (variable right-hand-side) code
    (jpp-dispatch variable stream 0)
    (format stream " = ")
    (jpp-dispatch right-hand-side stream 0))
  (values t))

(defmethod jpp ((type (eql :constant)) constant stream indent)
  (declare (ignore indent))
  (let ((the-value (first constant))
        (type (second constant)))
    (if (eql type 'string)
        (format stream "~s" (javaize-identifier the-value))
      (format stream "~:a" (javaize-identifier the-value))
    )))

(defmethod jpp ((type (eql :variable)) identifier stream indent)
  (declare (ignore indent))
  (let ((the-variable (javaize-identifier (first identifier))))
    (format stream "~:a" the-variable)
    ))

(defgeneric java-version-of-type (type))

;;; default method
(defmethod java-version-of-type ((type t)) (javaize-class-name type))

(defmethod java-version-of-type ((type (eql 'int))) '|int|)
(defmethod java-version-of-type ((type (eql 'integer))) '|Integer|)
(defmethod java-version-of-type ((type (eql 'boolean))) '|boolean|)
(defmethod java-version-of-type ((type (eql 'vector))) '|Vector|)
(defmethod java-version-of-type ((type (eql 'float))) '|float|)
(defmethod java-version-of-type ((type (eql 'double))) '|double|)

(defmethod java-version-of-type ((type list)) 
  ;; it's a parameterized type
  (intern
   (with-output-to-string (s)
     (format s "~:a<" (java-version-of-type (first type)))
     (loop for (type . rest) on (rest type)
	 do (format s "~:a" (java-version-of-type type))
	 when rest do (format s ", "))
     (format s ">"))))
    
;;; Fix?: May the array type should contain another field which is
;;; the number of dimensions of the array
;;; or maybe a list of the dimensions?
;;; For the moment a vector is 1-d and an array is 2-d
(defmethod jpp ((type (eql :top-level-arg)) binding-form stream indent)
  (declare (ignore indent))
  (destructuring-bind (variable type) binding-form
    (cond
     ((and (listp type) (member (first type) '(array vector)))
      ;; In java this follows a different pattern, namely
      ;; int a[] rather than Vector<Integer>
      ;; so first the element type
      (format stream "~:a " (java-version-of-type (second type)))
      ;; then the variable name
      (jpp-dispatch variable stream 0)
      ;; and then the brackets
      ;; 2 for arrays, 1 for vector
      (when (eql (first type) 'array)
	(format stream "[]"))
      (format stream "[]"))
     (t
      (format stream "~:a " (java-version-of-type type))
      (jpp-dispatch variable stream 0)))
    nil))

(defmethod jpp ((type (eql :bind)) binding-form stream indent)
  (destructuring-bind (variable type value &body body) binding-form
    (cond
     ((and (listp type) (member (first type) '(vector array)))
      ;; In java this follows a different pattern, namely
      ;; int a[] rather than Vector<Integer>
      ;; so first the element type
      (format stream "~:a " (java-version-of-type (second type)))
      ;; then the variable name
      (jpp-dispatch variable stream 0)
      ;; and then the brackets
      (when (eql (first type) 'array)
	(format stream "[]"))
      (format stream "[]"))
     (t
      (format stream "~:a " (java-version-of-type type))
      (jpp-dispatch variable stream 0)))
    (when value
      (format stream " = " )
      (jpp-dispatch value stream indent))
    (format stream ";")
    (jpp-format-block body stream indent nil))
  (values nil))

(defmethod jpp ((type (eql :allocate)) form stream indent)
  (declare (ignore indent))
  (destructuring-bind (variable object-type &rest parameters) form
    (let ((class-name (javaize-class-name object-type)))
      (format stream "~:a ~:a = new ~:a(~{~:a~^, ~});" 
              class-name (javaize-identifier variable) class-name
              (mapcar #'javaize-identifier parameters)
              ))
    )
  )

(defmethod jpp ((type (eql :conditional)) clauses stream indent)
  (loop for (condition . body) in clauses
      for first = t then nil
      for last-one = (eql (first condition) :if-terminator)
      unless first
      do (format stream "~%~vt" indent)
      do (format stream "~:a "
		 (cond (first '|if|)
		       (last-one '|else|)
		       (t '|elseif|)))
	 ;; print the condition
	 (unless last-one
	   (format stream "(")
	   (jpp-dispatch condition stream 0)
	   (format stream ")"))
	 ;; print the block under the condition
	 (jpp-format-block body stream (+ indent 3) t))
  (values nil))

(defparameter for-test '(:for (:variable |i|) :init (:constant 0) 
                         :refresh (:binary-op + (:variable |i|) (:constant 1))
                         :terminate (:binary-comparison < (:variable |i|) (:constant 10))
                         :body ((:application |System.out.println| (:variable |i|) (:constant 10)))
                         :exit-code ((:application |System.out.println| (:constant "Done")))
                         :abnormal-exit? t))



;;; Apparently dead code
;;;(defmethod initial-methods ((task implementation))
;;;  (loop for child-task in (children task)
;;;      when (and (loop for input in (inputs child-task)
;;;		    for his-token = (symbolic-token input)
;;;		    always (or (typep his-token 'initial-input-value-token)
;;;			       (typep his-token 'state-variable-value-token)
;;;			       ))
;;;		(null (incoming-control-flows child-task)))
;;;      collect child-task))

		


