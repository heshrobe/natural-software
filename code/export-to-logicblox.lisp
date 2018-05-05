;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exporting Type Definitions
;;;
;;; For each deftype (e.g. "foo") we need to:
;;; 1) Create the type-name and arity
;;;    +Type_name(type),  +Type_from_name["foo"] = tn, +Arity[tn] = <arity>.
;;; 2) Create the type-expression (only if arity isn't 0)
;;;    Foo(te) -> Type_expression(te).
;;;    Foo_from_args[arg1 arg2] = foo -> Type(arg1), Type(arg2), Foo(foo).  -- Don't need use te1 - te4
;;;    lang:constructor(`Foo_from_args). -- Don't need
;;; 3) Create the Type_name extractor rules
;;;    Type_name_of_type_expression[te] = tn <- Foo(te), Type_from_name["foo"] = tn.  -- Don't need same
;;; 4) Create the argument extractor rule
;;;    Arg_of_type_expression[type, 1] = value <- foo(type), Foo_from_args[value,_] = type. -- Don't need same
;;;    Arg_of_type_expression[type, 2] = value <- foo (type), Foo_from_args[_,value] = type.
;;; 5) Create For each direct super-type of foo  +Direct_subtype(foo, parent)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; It would probably be better to first output all of the step 1's
;;; Then all of the step 5's since step 1 declares all the types while 5
;;; establishes the subtying which doesn't work if the super type hasn't yet
;;; been defined.  This also groups all the exec stuff together and all the 
;;; rule like schema stuff after.

(defun export-type-to-logicblox-1 (type-name &optional (stream *standard-output*))
  (flet ((make-lb-string (lisp-symbol-or-string)
	   (let ((string (string lisp-symbol-or-string)))
	     (substitute #\_ #\- (string-capitalize string) :test #'char-equal))))	     
    (let ((parameters nil)
	  (super-types nil)
	  (type-name-string (make-lb-string type-name)))
      (ask* `[has-property ,type-name ?property] (pushnew (string-capitalize (string ?property)) parameters))
      (ask* `[subtype ,type-name ?super] (pushnew ?super super-types))
      (setq parameters (nreverse (mapcar #'make-lb-string parameters))
	    super-types (nreverse (mapcar #'make-lb-string super-types)))
      (let* ((arity (length parameters)))
	;; step 1
	;; The Arity of the type name can't be done by a + since Arity is inferred
	;; for each type expression
	(format stream "~%Type_name(~a), Type_from_name[~s] = ~a."
		type-name-string type-name-string type-name-string type-name-string)
	;; step 2
	(when (plusp arity)
	  (loop for parameter in parameters
	      for i from 1
	      do (format stream "~%Has_property(Type_from_name[~s], ~s)."
			 type-name-string parameter)))))))

(defun export-type-to-logicblox-2 (type-name &optional (stream *standard-output*))
  (flet ((make-lb-string (lisp-symbol-or-string)
	   (let ((string (string lisp-symbol-or-string)))
	     (substitute #\_ #\- (string-capitalize string) :test #'char-equal))))	     
    (let ((parameters nil)
	  (super-types nil)
	  (type-name-string (make-lb-string type-name)))
      (ask* `[has-property ,type-name ?property] (pushnew (string-capitalize (string ?property)) parameters))
      (ask* `[subtype ,type-name ?super] (pushnew ?super super-types))
      (setq parameters (nreverse (mapcar #'make-lb-string parameters))
	    super-types (nreverse (mapcar #'make-lb-string super-types)))
      (let* ((arity (length parameters))
	     (constructor-arity-string (format nil "Te_~darg" arity)))
	;; step 2
	(when (plusp arity)
	  ;; Arity[tn] = 2 <- Type_name(tn), Type_from_name["Json_Vector_Node"] = tn.
	  (format stream "~%Arity[tn] = ~d <- Type_name(tn), Type_from_name[~s] = tn."
		  arity type-name-string)
	  ;; step 4
	  (loop for parameter in parameters
	      for i from 1
	      ;; for initial-blanks = (loop for j from 1 below i collect "_")
	      ;; for final-blanks = (loop for j from i below arity collect "_")
	      do 
		 (format stream "~%Property_value_of[te, ~s] = ~a <- ~a(te), Type_name_of_type_expression[te] = tn, Type_from_name[~s] = tn, Arg_of_type_expression[te, ~d] = ~a."
			 parameter  parameter constructor-arity-string type-name-string i parameter)
		 ))))))

(defun export-type-to-logicblox-3 (type-name &optional (stream *standard-output*))
  (flet ((make-lb-string (lisp-symbol-or-string)
	   (let ((string (string lisp-symbol-or-string)))
	     (substitute #\_ #\- (string-capitalize string) :test #'char-equal))))	     
    (let ((super-types nil)
	  (type-name-string (make-lb-string type-name)))
      (ask* `[subtype ,type-name ?super] (pushnew ?super super-types))
      (setq super-types (nreverse (mapcar #'make-lb-string super-types)))
      (loop for super in super-types
	  do (format stream "~%Direct_subtype(Type_from_name[~s], Type_from_name[~s])." 
		     type-name-string (string-capitalize (string super)))))))


;;; Example of what Step4 should be:

;;; Property_value_of[Te_2arg, "Dimension"] = Dimension <-
;;;   Te_2arg(te),
;;;   Type_name_of_type_expression[te] = tn,
;;;   Type_from_name["Json_vector_node"] = tn,
;;;   Arg_of_type_expression[te, 2] = Dimension.


(defun export-all-types-to-schema-file (&optional (pathname #p"rp:muse;logicblox;full-type-schema.lb"))
  (with-open-file (f pathname :direction :output 
		   :if-exists :supersede 
		   :if-does-not-exist :create)
    (format f "~%open hes~%addblock <doc>")
    (export-all-types-to-logicblox f)
    (format f "~%</doc>~%close")
    ))

(defun export-all-types-to-logicblox (&optional (stream *standard-output*))
  (let ((all-types (types-in-order)))
    (loop for type in all-types
	do (terpri stream)
	   (export-type-to-logicblox-1 type stream)
	   (export-type-to-logicblox-2 type stream)
	   (export-type-to-logicblox-3 type stream))
    ))

(defun types-in-order ()
  (let ((queue nil)
	(result nil))
    (labels ((get-entry (type)
	       (let ((parents nil))
		 ;; the entry is the type and all its parents
		 (ask* `[subtype ,type ?parent] (push ?parent parents))
		 (ask* `[union-member ,type ?parent] (push ?parent parents))
		 (cons type parents)))
	     (enqueue (new-guy)
	       ;; (format t "~%Enqueueing ~a" new-guy)
	       (push new-guy queue))
	     (all-parents-in-result (new-guy)
	       (loop for parent in (rest new-guy) always (member parent result)))
	     (dequeue ()
	       (let ((new-guy (pop queue)))
		 ;; (format t "~% Dequeuing ~a" new-guy)
		 (cond
		  ;; If he's already collected there's 
		  ;; nothing to do.
		  ((member (first new-guy) result)
		   ;; (format t "~%Dequeuing ~a but already in Result" new-guy)
		   )
		  ((all-parents-in-result new-guy)
		   (collect-result (first new-guy)))
		  ;; He's not already collected and
		  ;; he has a parent not collected
		  ;; so throw him away and wait for that 
		  ;; parent to get collected and requeue him
		  (t
		   ))))
	     (collect-result (new-guy)
	       (when (member new-guy result) (error "Trying to recollect ~a" new-guy))
	       ;; (format t "~%Collecting ~a" new-guy)
	       (push new-guy result)
	       (ask* `[union-member ?sub ,new-guy] (enqueue (get-entry ?sub)))
	       (ask* `[subtype ?sub ,new-guy] (enqueue (get-entry ?sub)))))
      (collect-result 'data-structure)
      (loop doing (dequeue) until (null queue))
      (nreverse result))))
	     