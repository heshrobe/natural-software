
(defun jdd (the-node)
  (labels 
      ((do-it (the-set)
	 (when (typep the-set 'json-non-terminal-node)
	   (cond ((typep the-set 'json-alist-node)
		  (loop for list-element-1 in the-set 
		      do (do-it (rest list-element-1)))
		  (when (typep the-set 'json-alist-node)
		    (let ((seen nil) (dups nil))
		      (loop for list-element-2 in the-set 
			  do (let ((symbol-1 (first list-element-2)))
			       (cond ((member symbol-1 seen) 
				      (setq dups (adjoin dups symbol-1)))
				     (t (setq seen (cons seen symbol-1)))))
			  finally (when (not (null dups)) (print dups))))))
		 ((not (typep the-set 'json-alist-node)) 
		  (loop for index-1 from 0 below (length the-set) 
		      do (do-it (aref the-set index-1))))))))
    (do-it the-node)))