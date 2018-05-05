;;; -*- Mode: Common-lisp; Package: Natsoft -*-

(in-package :natsoft)

(defun json-alist-node-p (x) (listp x))
(defun json-vector-node-p (x) (typep x '(simple-array t)))
(defun json-non-terminal-node-p (x) (or (json-alist-node-p x)  (json-vector-node-p x)))
(defun json-terminal-node-p (x) (or (numberp x) (symbolp x) (stringp x)))

(deftype json-alist-node () `(satisfies json-alist-node-p))
(deftype json-vector-node () `(satisfies json-vector-node-p))
(deftype json-non-terminal-node () `(satisfies json-non-terminal-node-p))
(deftype json-terminal-node () `(satisfies json-terminal-node-p))

(defun jd-tester (the-node)
  (let ((initial-path (list 'top)))
    (labels ((tt (the-node the-path)
               (when (typep the-node 'json-alist-node)
                 (let ((seen (list)) (dups (list)))
                   (loop for list-element-2 in the-node 
		       do (let ((symbol-1 (first list-element-2)))
			    (cond ((member symbol-1 seen) (pushnew symbol-1 dups))
				  (t (push symbol-1 seen)))) 
		       finally (when (not (null dups)) 
				 (format t "~%The key~p ~{~a~^, and ~} ~:[was~;were~] duplicated at node ~{~a~^.~}" 
					 (length dups) dups (cdr dups) (reverse the-path))))))
               (when (typep the-node 'json-non-terminal-node)
                 (cond ((typep the-node 'json-alist-node)
                        (loop for list-element-1 in the-node 
			    do (tt (rest list-element-1) (cons (first list-element-1) the-path))))
                       ((typep the-node 'json-vector-node)
                        (loop for index-1 below (length the-node) 
			    do (tt (aref the-node index-1) (cons index-1 the-path))))))))
      (tt the-node initial-path))))

(ql:quickload "cl-json")

(setq json:*json-array-type* 'vector)

(defun do-json-test (path)
  (jd-tester (read-json-file path)))

(defun read-json-file (pathname)
  (let ((json:*json-symbols-package* nil))
    (json:decode-json-from-source (pathname pathname))))

(clim-env:define-lisp-listener-command (com-json-test :name t)
    ((pathname 'clim:pathname :default "natsoft:code;*.json"))
  (clim-env::show-file pathname *standard-output* :verbose nil)
  (do-json-test pathname))
	
