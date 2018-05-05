;;; -*- Mode:Comman-lisp; Package: cl-json -*-

(in-package :cl-json)

(defun dup-detector (thing)
  (labels ((do-one-level (thing path-so-far)
	     (typecase thing
	       (array (loop for sub-thing across thing 
			  for index from 0
			  do (do-one-level sub-thing (cons `(Vector element ,index) path-so-far))))
	       (list (let ((keys nil))
		       (loop for (key . value) in thing
			   for entry = (assoc key keys)
			   unless entry do (setq entry (cons key 0))
					   (push entry keys)
			   do (incf (cdr entry))
			   do (do-one-level value (cons key path-so-far)))
		       (loop for (key . count) in keys
			   when (> count 1)
			   do (format t "~%Duplicate key ~a at ~{~a~^ -> ~}" key (reverse path-so-far)))))
	       (otherwise (values)))))
    (do-one-level thing '(top-level))))
							  