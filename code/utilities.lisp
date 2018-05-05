;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

(defmacro with-keywords-removed ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (remove-keywords ,list ,keywords-to-remove)))
     ,@body))

(defun remove-keywords (list keywords)
  (macrolet ((remove-keywords-1 (name-var predicate-form)
	       `(let ((head nil)
		      (tail nil))
		  (do ()
		      ((null list))
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setq head (list ,name-var value)))
			(return))))
		  (do ()
		      ((null list) head)
		    (let ((,name-var (pop list))
			  (value (pop list)))
		      (unless ,predicate-form
			(setq tail (setf (cddr tail) (list ,name-var value)))))))))
    (cond ((null list) nil)
	  ((null keywords) list)
	  ;; Special case: use EQ instead of MEMBER when only one keyword is supplied.
	  ((null (cdr keywords))
	   (let ((keyword (car keywords)))
	     (remove-keywords-1 name (eq name keyword))))
	  (t
	   (remove-keywords-1 name (member name keywords))))))

(defvar *gensymbol* 0)
(defun gensymbol (&rest parts)
  (declare (dynamic-extent parts))
  (when (null parts) (setf parts '(gensymbol)))
  (make-symbol (lisp:format nil "~{~A-~}~D" parts (incf *gensymbol*))))