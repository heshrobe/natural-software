;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel)
  (export (list (intern "READ-COMMAND-OR-STRING" :clim)) :clim)
  )


;;; Like the original command-or-form but instead accepts either a command or some other
;;; presentation type that's specified.  Command-or-form is know about and handled specially
;;; by the command-processor, so this is a patch that extends Command-or-form rather than 
;;; introduce a new type.

;; We have to give command this extra argument too because
;; command-or-form pretends to be a super-type of command

(define-presentation-type command
    (&key (command-table (frame-command-table *application-frame*))
	  (other-ptype 'form)) ;;  <----------------
  )

(define-presentation-type command-or-form
    (&key (command-table (frame-command-table *application-frame*))
	  (other-ptype 'form))                                       ;;  <----------------
  :options ((auto-activate nil boolean)))

(define-presentation-method accept ((type command-or-form) stream (view textual-view)
                                    &rest args)
  (declare (dynamic-extent args))
  (let ((command-type `(command :command-table ,command-table))
        (form-type (if (eql other-ptype 'form)                    ;; <---------------------
		       `((form) :auto-activate ,auto-activate)
		       other-ptype))                              ;; <---------------------
        (start-position (and (input-editing-stream-p stream)
                             (stream-scan-pointer stream)))
        (replace-input-p nil))
    (multiple-value-bind (object type)
        (with-input-context (command-type) (command command-presentation-type nil options)
             (with-input-context (form-type) (form form-presentation-type nil options)
                  (let ((gesture (read-gesture :stream stream :peek-p t)))
                    (cond ((and (characterp gesture)
                                (find gesture *command-dispatchers* :test #'char-equal))
                           (read-gesture :stream stream)        ;get out the colon
                           (apply #'accept command-type
                                  :stream stream :prompt nil :view view
                                  :history type args))
                          (t (apply #'accept form-type
                                    :stream stream :prompt nil :view view
                                    :history type args))))
                (t (when (getf options :echo t)
                     (setq replace-input-p t))
                   (values form form-presentation-type)))
           (t (when (getf options :echo t)
                (setq replace-input-p t))
              (when (partial-command-p command)
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

(define-presentation-method present (thing (type command-or-form) stream view &rest args)
  (declare (dynamic-extent args))
  (setq command-table (find-command-table command-table))
  (apply #'present thing (if (object-is-command-p thing command-table)
                             `(command :command-table ,command-table)
                             other-ptype)             ;; <----------------------------------
         :stream stream :view view args))



;;; We have to include every use of with-presentation-type-parameters for command
;;; or command or form, because it's a macro that's expanded at compile time
;;; in terms of the current definition of the ptype.

(define-presentation-method presentation-typep (object (type command-or-form))
  (or (object-is-command-p object command-table)
      ;; Everything that's not a command is a form
      t))


(define-presentation-method presentation-subtypep ((sub command-or-form) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))


(define-presentation-method presentation-subtypep ((sub command) super)
  (let ((ct1 (find-command-table (with-presentation-type-parameters (command sub)
                                   command-table)))
        (ct2 (find-command-table (with-presentation-type-parameters (command super)
                                   command-table))))
    (command-table-presentation-subtypep ct1 ct2)))

(define-presentation-method map-over-presentation-type-supertypes ((type command) function)
  (map-over-presentation-type-supertypes-augmented type function
    ;; Include COMMAND-OR-FORM in the supertypes
    (with-presentation-type-parameters (command type)
      (with-stack-list (new-type 'command-or-form :command-table command-table)
        (funcall function 'command-or-form new-type)))))

(defun document-presentation-to-command-translator
       (translator presentation context-type frame event window x y stream)
  ;; If we're translating to a command, it's a pretty sure bet that
  ;; we can run the body.  The command name will surely provide enough
  ;; information, and it's faster than unparsing the whole command.
  ;; If the user wants more, he can do it himself.
  (catch 'no-translation
    (let ((command-name
            (or (presentation-translator-command-name translator)
                (command-name
                  (call-presentation-translator translator presentation context-type
                                                frame event window x y)))))
      (with-presentation-type-parameters (command context-type)
        (with-stack-list (type 'command-name ':command-table command-table)
          ;; Use a lower level function for speed
          (return-from document-presentation-to-command-translator
            (funcall-presentation-generic-function present
              command-name type stream +textual-view+))))))
  (format stream "Command translator ~S" (presentation-translator-name translator))
  (values))



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
      (clim:with-accept-help (((:top-level-help :override) "You are being asked to enter a command or a string")) 
	(values (accept `(command-or-form :command-table ,command-table :other-ptype string)
			:stream stream :prompt nil))))))
