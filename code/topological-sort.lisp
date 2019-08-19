;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

(defparameter *debug-topological-sort* nil)

(defmacro ts-debug (control-string &rest args)
  `(when *debug-topological-sort*
     (format *error-output* ,control-string ,@args)))

(defun topological-sort (initial-elements predecessor-function successor-function)
  (declare (Dynamic-extent predecessor-function successor-function)
           (function predecessor-function successor-function)
           (ftype (function (t) list) predecessor-function successor-function)
           )
  (let ((queue nil)
        (result nil))
    (labels ((get-entry (new-guy)
               (cons new-guy (funcall predecessor-function new-guy) ))
             (enqueue (new-guy)
               (ts-debug "~%Enqueueing ~a" new-guy)
               (push new-guy queue))
             (all-predecessors-in-result (new-guy)
               (loop for predecessor in (rest new-guy) always (member predecessor result)))
             (dequeue ()
               (let ((new-guy (pop queue)))
                 (ts-debug "~% Dequeuing ~a" new-guy)
                 (cond
                  ;; If he's already collected there's 
                  ;; nothing to do.
                  ((member (first new-guy) result)
                   (ts-debug "~%Dequeuing ~a but already in Result" new-guy)
                   )
                  ((all-predecessors-in-result new-guy)
                   (collect-result (first new-guy)))
                  ;; He's not already collected and
                  ;; he has a predecessor not collected
                  ;; so throw him away and wait for that 
                  ;; predecessor to get collected and requeue him
                  (t
                   (ts-debug "~%Discarding ~a missing ~a" 
			     new-guy
			     (loop for predecessor in (rest new-guy)
				 unless (member predecessor result)
					collect predecessor))
                   ))))	     
             (collect-result (new-guy)
               (when (member new-guy result) (error "Trying to recollect ~a" new-guy))
               (ts-debug "~%Collecting ~a" new-guy)
               (push new-guy result)
               (loop for successor in (funcall successor-function new-guy)
                   do (enqueue (get-entry successor)))))
      (loop for initial-element in initial-elements do (collect-result initial-element))
      (loop doing (dequeue) until (null queue))
      (nreverse result))))
