;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

;;; Like command-or-form but instead accepts either a command or an arbitrary string

(eval-when (:compile-toplevel :load-toplevel)
  (export (list 'COMMAND-OR-STRING 'READ-COMMAND-OR-STRING) :clim-internals)
  (import (list 'COMMAND-OR-STRING 'READ-COMMAND-OR-STRING) :clim)
  (export (list 'COMMAND-OR-STRING 'READ-COMMAND-OR-STRING) :clim))

(define-presentation-type command-or-string
                          (&key (command-table (frame-command-table *application-frame*)))
  :options ((auto-activate nil boolean)))

(define-presentation-method presentation-type-history ((type command-or-string))
  (presentation-type-history-for-frame type *application-frame*))

(define-presentation-method accept ((type command-or-string) stream (view textual-view)
                                    &rest args)
  (declare (dynamic-extent args))
  (let ((command-type `(command :command-table ,command-table))
        (start-position (and (input-editing-stream-p stream)
                             (stream-scan-pointer stream)))
	(string-type 'string)
        (replace-input-p nil))
    (multiple-value-bind (object type)
        (with-input-context (command-type) (command command-presentation-type nil options)
             (with-input-context (string-type) (string string-presentation-type nil options)
                  (let ((gesture (read-gesture :stream stream :peek-p t)))
                    (cond ((and (characterp gesture)
                                (find gesture *command-dispatchers* :test #'char-equal))
                           (read-gesture :stream stream)        ;get out the colon
                           (apply #'accept command-type
                                  :stream stream :prompt nil :view view
                                  :history type args))
                          (t (apply #'accept 'string
                                    :stream stream :prompt nil :view view
                                    :history type args))))
                (t (when (getf options :echo t)
                     (setq replace-input-p t))
                   (values string string-presentation-type)))
	     (t (when (getf options :echo t)
		  (setq replace-input-p t))
		(when (partial-command-p command)
		  ;; this is here for command-menu items that take
		  ;; more arguments.  The command name is already there
		  ;; but without the colon and the partial-command-processor
		  ;; throws out and never returns to the same form below that adds 
		  ;; the colon
		  (when replace-input-p
		    (unless (stream-rescanning-p stream)
		      (replace-input stream (string (first *command-dispatchers*))
				     :buffer-start start-position)
		      (incf start-position)))
		  (setq command (funcall *partial-command-parser*
					 command command-table stream start-position)))
              (when replace-input-p
                (unless (stream-rescanning-p stream)
                  (replace-input stream (string (first *command-dispatchers*))
                                 :buffer-start start-position)
                  (incf start-position)))
              (values command command-presentation-type)))
      (when replace-input-p
        (presentation-replace-input stream object type view
                                    :buffer-start start-position))
      (values object type))))

(define-presentation-method present (thing (type command-or-string) stream view &rest args)
  (declare (dynamic-extent args))
  (setq command-table (find-command-table command-table))
  (apply #'present thing (if (object-is-command-p thing command-table)
                             `(command :command-table ,command-table)
                             `string)
         :stream stream :view view args))

(define-presentation-method presentation-typep (object (type command-or-string))
  (or (object-is-command-p object command-table)
      ;; Everything that's not a command is a string
      t))

(define-presentation-method presentation-subtypep ((sub command-or-string) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))


(defun read-command-or-string (command-table
			       &key (stream *standard-input*)
				    (command-parser *command-parser*)
				    (command-unparser *command-unparser*)
				    (partial-command-parser *partial-command-parser*)
				    (use-keystrokes nil))
  (if use-keystrokes
      (with-command-table-keystrokes (keystrokes command-table)
        (read-command-using-keystrokes command-table keystrokes
                                       :stream stream
                                       :command-parser command-parser
                                       :command-unparser command-unparser
                                       :partial-command-parser partial-command-parser))
    (let ((*command-parser* command-parser)
	  (*command-unparser* command-unparser)
	  (*partial-command-parser* partial-command-parser))
      ;; spr16572: Due to the identity translator, it is possible to
      ;; click on anything that has been presented as a command, including
      ;; disabled commands, commands from other command tables, and lists
      ;; that aren't even funcallable.  This problem may be rooted in the 
      ;; design of the identity translator and all the cures I have tried
      ;; create much bigger problems.
      ;; JPM 5/27/98.
      (values (accept `(command-or-string :command-table ,command-table)
		      :stream stream :prompt nil)))))
