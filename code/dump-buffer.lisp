;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dumping a buffer
;;;
;;;
;;; The Plan Language
;;;
;;;
;;; Language Definition:
;;;   Initial-input:  (:name <name> :type <type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Final-output:   (:name <name> :type <type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Entry-point:    (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   Exit-point:     (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   State-element   (:direction <source/sink> :name <name> :port-name <port-name> :port-type <port-type>)
;;;   Component:      (:name <name> :type <type> ,@[<propterty-keyword <property-value> ... ])
;;;   Dataflow:       ((:component <component-name> :port <port-name> (optional :branch <branch-name>)
;;;                    (:component <component-name> :port <port-name> (optional :branch <branch-name>)))
;;;   Control-flow:   ((:component <component-name> (optional :branch <branch-name>))
;;;                    (:component <component-name> (optional :branch <join-name>)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-design-editor-command (com-write-code-for-buffer :name t :menu t)
    (&key (only-selected-components 'clim:boolean)
          (name 'string :default (string (name (design-in-progress clim:*application-frame*)))))
  (let ((dip (design-in-progress clim:*application-frame*)))
    (switch-to-buffer :buffer-name "*code-buffer*")
    (set-common-lisp-mode)
    (lep:with-output-to-temp-buffer (stream "*code-buffer*")
      (dump-a-drawing-as-code dip :stream stream :selected-only (and only-selected-components (selected-objects *design-editor*)) :name name))))

;;; The dump-a-drawing-as-code code is in reducdions-and-cliches at the moment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Output to files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-drawings (task pathname)
  (let ((counter 0)
	(*graphing-text-style* (clim:make-text-style :fix :bold :very-small))
	(*show-output-type-constraints* nil)
	(*show-input-type-constraints* nil)
	(*graph-generation-separation* 0)
	(*control-port-length* 20))
    (map-over-selected-implementation 
     task
     #'(lambda (sub-task history implementation)
	 ;; (declare (ignore sub-task history))
	 (with-open-file (file (format nil "~a-~d.ps" pathname (incf counter))
			  :if-exists :supersede :if-does-not-exist :create :direction :output)
	   (clim:with-output-to-postscript-stream (stream file)
	     (display-task implementation stream)
	     (format stream "~%~a <-- ~{~a~^ <--~} ~%" sub-task history)))))))

(define-design-editor-command (com-print-design-hierarchy :name t)
    ((task 'basic-task)
     (file 'clim:pathname))
  (print-drawings task file))

(defun print-a-drawing (task pathname)
  (with-open-file (file pathname :if-exists :supersede :if-does-not-exist :create :direction :output)
    (clim:with-output-to-postscript-stream (stream file)
      (display-task task stream))))

