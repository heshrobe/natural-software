;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dumping a drawing
;;;
;;;
;;; the plan language
;;;
;;;
;;; language definition:
;;;   initial-input:  (:name <name> :type <type> (optional :port <port-name> defaults to name) (optional :branch <branch-name>))
;;;   final-output:   (:name <name> :type <type> (optional :port <port-name> defaults to name) (optional :branch <branch-name>))
;;;   entry-point:    (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   exit-point:     (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   state-element   (:direction <source/sink> :name <name> :port-name <port-name> :port-type <port-type>)
;;;   component:      (:name <name> :type <type> ,@[<propterty-keyword <property-value> ... ])
;;;   dataflow:       ((:component <component-name> :port <port-name> (optional :branch <branch-name>)
;;;                    (:component <component-name> :port <port-name> (optional :branch <branch-name>)))
;;;   control-flow:   ((:component <component-name> (optional :branch <branch-name>))
;;;                    (:component <component-name> (optional :branch <join-name>)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    
(defun dump-a-drawing-as-code (design &key (stream *standard-output*) args-plist selected-only name)
  (let ((components nil)
	(initial-inputs nil)
	(final-outputs nil)
	(entry-points nil)
	(exit-points nil)
	(path-ends nil)
	(constants nil)
	(state-sources nil)
	(state-sinks nil)
	(dataflows nil)
	(controlflows nil)
	(args-reverse-alist (loop for (keyword value) on args-plist by #'cddr
				collect (cons value (intern (concatenate 'string "?" (string keyword)) 'natsoft)))))
    (flet ((value-to-arg (value)
	     (let ((arg (rest (assoc value args-reverse-alist :test #'equal))))
	       (or arg value))))
      ;; first output the header and the first line
      (format stream ";;; -*- mode: common-lisp; package: natsoft -*-")
      (format stream "~%;;; plan format code for ~a" name)
      (format stream "~3%(defcliche ~a (~{~a~^ ~})" name (mapcar #'cdr args-reverse-alist))
      ;; next collect and sort into bins all the components
      (loop for c in (children design)
	  unless (and selected-only (not (member c selected-only)))
	  do (typecase c
	       (initial-input (push c initial-inputs))
	       (final-output (push c final-outputs))
	       (entry-point (push c entry-points))
	       (exit-point (push c exit-points))
	       (constant-value (push c constants))
	       (state-source (push c state-sources))
	       (state-sink (push c state-sinks))
	       (path-end (push c path-ends))
	       (otherwise (push c components)))
	     (typecase c
	       (output-side-mixin
		(loop for port in (outputs c)
		    do (loop for dataflow in (outgoing-flows port)
			   do (pushnew dataflow dataflows))))
	       (has-branches-mixin
		(loop for branch in (branches c)
		    do (loop for port in (outputs branch)
			   do (loop for dataflow in (outgoing-flows port)
				  do (pushnew dataflow dataflows))))))
	     (typecase c
	       (output-side-mixin
		(loop for control-flow in (outgoing-control-flows c)
		    do (pushnew control-flow controlflows)))
	       (has-branches-mixin
		(loop for branch in (branches c)
		    do (loop for control-flow in (outgoing-control-flows branch)
			   do (pushnew control-flow controlflows)))))
	     )
      ;; output the initial inputs
      (when initial-inputs 
	(let* ((prefix ":initial-inputs")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for initial-input in initial-inputs do (format-initial-input stream initial-input prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; output th final outputs
      (when final-outputs
	(let* ((prefix ":final-outputs")
	       (prefix-indent (+ 4 (length prefix))))
	  (when final-outputs 
	    (format stream "~%~2t~a (" prefix)
	    (loop for final-output in final-outputs do (format-final-output stream final-output prefix-indent #'value-to-arg))
	    (format stream "~%~vt)" prefix-indent))))
      ;; output the entry points
      (when entry-points
	(let* ((prefix ":entry-points")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for entry-point in entry-points do (format-entry-point stream entry-point prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; exit points
      (when exit-points 
	(let* ((prefix ":exit-points")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for exit-point in exit-points do (format-exit-point stream exit-point prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      (when path-ends
	(let* ((prefix ":path-ends")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for path-end in path-ends do (format-path-end stream path-end prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; constants
      (when constants 
	(let* ((prefix ":constants")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for constant in constants do (format-constant stream constant prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; state elements
      (when (or state-sources state-sinks)
	(let* ((prefix ":state-elements")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for state-source in state-sources do (format-state-source stream state-source prefix-indent #'value-to-arg))
	  (loop for state-sink in state-sinks do (format-state-sink stream state-sink prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; vanilla components
      (when components
	(let* ((prefix ":components")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for component in components do (format-component stream component prefix-indent #'value-to-arg))
	  (format stream "~%~vt)" prefix-indent)))
      ;; dataflows
      (when dataflows
	(let* ((prefix ":dataflows")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for dataflow in dataflows do (format-dataflow stream dataflow prefix-indent))
	  (format stream "~%~vt)" prefix-indent)))
      ;; control flows
      (when controlflows
	(let* ((prefix ":control-flows")
	       (prefix-indent (+ 4 (length prefix))))
	  (format stream "~%~2t~a (" prefix)
	  (loop for controlflow in controlflows do (format-controlflow stream controlflow prefix-indent))
	  (format stream "~%~vt)" prefix-indent)))
      (format stream "~%~2t)~%")
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; in all the stuff below
;;; anything that could have been a parameter is inverted back
;;; by calling value-to-arg on it
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-initial-input (stream initial-input indent value-to-arg)
  (let ((name (name initial-input))
	(port-name (port-name initial-input))
	(type (port-type-constraint (first (outputs initial-input))))
	(branch (branch-name initial-input))
	)
    (when (eql port-name name) (setq port-name nil))
    (format stream "~%~vt(:name ~a :type ~a~@[ :port ~a~]~@[ :branch ~a~])"
	    indent 
	    (funcall value-to-arg name)
	    (funcall value-to-arg type)
	    (funcall value-to-arg port-name)
	    branch)))

(defun format-final-output (stream final-output indent value-to-arg)
  (let ((name (name final-output))
	(port-name (port-name final-output))
	(type (port-type-constraint (first (inputs final-output))))
	(branch (branch-name final-output))
	)
    (when (eql port-name name) (setq port-name nil))
    (format stream "~%~vt(:name ~a :type ~a~@[ :port ~a~]~@[ :branch ~a~])"
	    indent 
	    (funcall value-to-arg name)
	    (funcall value-to-arg type)
	    (funcall value-to-arg port-name)
	    branch)))

(defun format-entry-point (stream entry-point indent value-to-arg)
  (let ((name (name entry-point))
	(ports (outputs entry-point)))
    (format stream "~%~vt(:name ~a" indent name)
    (when ports
      (loop for port in ports
	  for name = (name port)
	  for type-constraint = (port-type-constraint port)
	  for first = t then nil
	  if first 
	  do (format stream " :ports ((:name ~a :type ~a)" 
		     (funcall value-to-arg name)
		     (funcall value-to-arg type-constraint)
		     )
	  else do (format stream " (:name ~a :type ~a)" 
			  (funcall value-to-arg name)
			  (funcall value-to-arg type-constraint)
			  )
	  finally (format stream ")")))
    (format stream ")")))

(defun format-exit-point (stream exit-point indent value-to-arg)
  (let ((name (name exit-point))
	(ports (inputs exit-point)))
    (format stream "~%~vt(:name ~a" indent name)
    (when ports
      (loop for port in ports
	  for name = (name port)
	  for type-constraint = (port-type-constraint port)
	  for first = t then nil
	  if first 
	  do (format stream " :ports ((:name ~a :type ~a)" 
		     (funcall value-to-arg name)
		     (funcall value-to-arg type-constraint)
		     )
	  else do (format stream " (:name ~a :type ~a)" 
			  (funcall value-to-arg name)
			  (funcall value-to-arg type-constraint)
			  )
	  finally (format stream ")")))
    (format stream ")")))


(defun format-path-end (stream path-end indent value-to-arg)
  (let ((name (name path-end))
	(ports (inputs path-end)))
    (format stream "~%~vt(:name ~a" indent name)
    (when ports
      (loop for port in ports
	    for name = (name port)
	    for type-constraint = (port-type-constraint port)
	    for first = t then nil
	    if first 
	    do (format stream " :ports ((:name ~a :type ~a)" 
		       (funcall value-to-arg name)
		       (funcall value-to-arg type-constraint)
		       )
	    else do (format stream " (:name ~a :type ~a)" 
			    (funcall value-to-arg name)
			    (funcall value-to-arg type-constraint)
			    )
	  finally (format stream ")")))
    (format stream ")")
    ))

(defun format-constant (stream constant indent value-to-arg)
  (let ((name (name constant))
	(type (type constant))
	(value (value constant)))
    (format stream "~%~vt(:name ~a :type ~a :value ~a)"
	    indent
	    (funcall value-to-arg name)
	    (funcall value-to-arg type)
	    (funcall value-to-arg value))))

(defun format-state-source (stream state-source indent value-to-arg)
  (let* ((name (name state-source))
	 (port (first (outputs state-source)))
	 (port-name (name port))
	 (state-name (state-name state-source))
	 (type (port-type-constraint port)))
    (format stream "~%~vt(:name ~a :direction source :port-name ~a :port-type ~a :state ~a)"
	    indent 
	    (funcall value-to-arg name)
	    (funcall value-to-arg port-name)
	    (funcall value-to-arg type)
	    (or state-name name)
	    )
    ))

(defun format-state-sink (stream state-sink indent value-to-arg)
  (let* ((name (name state-sink))
	 (port (first (inputs state-sink)))
	 (port-name (name port))
	 (state-name (state-name state-sink))
	 (type (port-type-constraint port)))
    (format stream "~%~vt(:name ~a :direction sink :port-name ~a :port-type ~a :state ~a)"
	    indent 
	    (funcall value-to-arg name)
	    (funcall value-to-arg port-name)
	    (funcall value-to-arg type)
	    (or state-name name)
	    )
    ))

(defun format-component (stream component indent value-to-arg)
  (let* ((name (name component))
	 (type (task-type component))
	 (properties (properties component)))
    (format stream "~%~vt(:name ~a :type ~a~{ ~s ~a~^~})"
	    indent 
	    (funcall value-to-arg name)
	    (funcall value-to-arg type)
	    (loop for prop in properties collect (funcall value-to-arg prop))
	    )))

(defun format-dataflow (stream dataflow indent)
  (let* ((input-port (input dataflow))
	 (input-port-name (name input-port))
	 (input-component (task input-port))
	 (input-component-name (if (typep input-component 'branch) (name (superior input-component)) (name input-component)))
	 (output-port (output dataflow))
	 (output-port-name (name output-port))
	 (output-component (task output-port))
	 (output-component-name (if (typep output-component 'join) (name (superior output-component)) (name output-component)))
	 (input-branch-name (when (typep input-component 'branch) (name input-component)))
	 (output-join-name (when (typep output-component 'join) (name output-component))))
    (format stream "~%~vt((:component ~a :port ~a~@[ :branch ~a~]) (:component ~a :port ~a~@[ :branch ~a~]))"
	    indent input-component-name input-port-name input-branch-name
	    output-component-name output-port-name output-join-name)))

(defun format-controlflow (stream controlflow indent)
  (let* ((input-component (predecessor controlflow))
	 (input-component-name (if (typep input-component 'branch) (name (superior input-component)) (name input-component)))
	 (input-branch-name (when (typep input-component 'branch) (name input-component)))
	 (output-component (successor controlflow))
	 (output-component-name (if (typep output-component 'join) (name (superior output-component)) (name output-component)))
	 (output-branch-name (when (typep output-component 'join) (name output-component))))
    (format stream "~%~vt((:component ~a~@[ :branch ~a~]) (:component ~a~@[ :branch ~a~]))"
	    indent input-component-name input-branch-name output-component-name output-branch-name)))

		    


;;;	for name = (name c)
;;;	do (typecase c
;;;	     (initial-input
;;;	      (let ((the-port (first (outputs c))))
;;;		(format stream "~%(initial-input ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (final-output
;;;	      (let ((the-port (first (inputs c))))
;;;		(format stream "~%(final-output ~a ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port)
;;;			(branch-name c))))
;;;	     (state-source
;;;	      (let ((the-port (first (outputs c))))
;;;		(format stream "~%(state source ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (state-sink
;;;	      (let ((the-port (first (inputs c))))
;;;		(format stream "~%(state sink ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (core-task-mixin
;;;	      (let ((type (task-type c))
;;;		    (property-list (properties c)))
;;;		(format stream "~%(component ~a ~a ~{~a~^ ~})"
;;;			type name property-list)))
;;;	     
;;;	     ))
;;;    ;; next loop over the components finding dataflows
;;;    (loop for source-task in components
;;;	for source-task-name = (name source-task)
;;;	do (loop for source-port in (outputs source-task)
;;;	       for source-port-name = (name source-port)
;;;	       do (loop for flow in (outgoing-flows source-port)
;;;		      for destination-port = (output flow)
;;;		      for destination-port-name = (name destination-port)
;;;		      for destination-task = (task destination-port)
;;;		      for destination-task-name =  (name destination-task)
;;;		      do (format stream "~%(dataflow (~a ~a) (~a ~a))"
;;;				 source-task-name source-port-name
;;;				 destination-task-name destination-port-name))))
;;;    ;; next loop over vanilla, non-branching components and dump any control
;;;    ;; flows from them
;;;    (loop for source-task in components
;;;	for source-task-name = (name source-task)
;;;	do (typecase source-task 
;;;	     (branching-task
;;;	      (loop for branch in (branches source-task)
;;;		  for branch-name = (name branch)
;;;		  do (loop for control-flow in (outgoing-control-flows branch)
;;;			 for destination-task = (successor control-flow)
;;;			 for destination-task-name = (name destination-task)
;;;			 do (format stream "~%(control-flow (~a ~a) ~a)"
;;;				    source-task-name branch-name destination-task-name))))
;;;	     (basic-task
;;;	      (loop for control-flow in (outgoing-control-flows source-task)
;;;		  for destination-task = (successor control-flow)
;;;		  for destination-task-name = (name destination-task)
;;;		  do (format stream "~%(control-flow ~a ~a)"
;;;			     source-task-name destination-task-name)))))
;;;    (format stream "~%)")
;;;    ))
