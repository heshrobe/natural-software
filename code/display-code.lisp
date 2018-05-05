;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(In-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The Port-Map
;;;  The way the CAD grapher works is a bit different from the vanilla grapher
;;;  It follows the pattern of first generating the nodes by tracing the child relation
;;;  and drawing (into a record) each node.
;;;  However, when it wants to draw edges, the pattern is different because each node
;;;  potentially has several ports.
;;;  So when the arc-drawer is called, it is passed a "Collector function" which is then 
;;;  called for each edge that connects the parent and child.  The collector is called with a bunch
;;;  of arguments, including the actual <x,y> location of the source and destination ports.
;;;  In order to know where these are, the drawing function has to stash the 
;;;  information for later use by the arc drawer.  This is the *port-map*, a hash-table
;;;  indexed by the node, which is bound during the whole graphing process.
;;;  tasks, the arc drawer has to map over the branches or joins 
;;;  Each entry include a list of three things:
;;;   1) The Input-Port-Map:  A list of triples: port, x, y treated as an alist
;;;       the locations of the input ports of a task 
;;;       for a joining task there shouldn't be an entry, but rather one for each join
;;;   2) The Output-Port-Map: A list of triples: port, x, y treated as an alist
;;;      similar to input-port-map except for the output ports of the task
;;;      for branching tasks there shouldn't be an entry, but rather one for each branch
;;;   3) The box height
;;;   
;;;
;;;  This is further complicated by the fact that the ports might actually be output ports
;;;  of a branch or input ports of a join.  Branches have output-ports + the control-port
;;;  and joins have input ports + the control port.
;;;  So during both component drawing and arc drawing it needs to iterate over branches/joins
;;;  and their output/input ports.
;;;  The arc-drawer function is called with 2 components, so for branching-tasks and joining
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *graph-generation-separation* 0)

(defun display-task (task stream)
  (let ((*the-top-level-task* task)
	(*port-map* (make-hash-table)))
    (declare (special *the-top-level-task* *port-map*))
    (when task
      (clim:format-graph-from-roots
       ;; collect those tasks that have nothing flowing into them
       (loop for child in (children task)
	     when (is-initial-task child)
	     collect child)
       #'display-task-component
       #'task-component-children
       :stream stream
       :graph-type :cad
       ;; actually for CAD grapher this is irrelevant
       :center-nodes t
       :merge-duplicates t
       :generation-separation *graph-generation-separation*
       :arc-drawer #'task-network-arrow-drawer)
      ))
  (let* ((selected-objects (selected-objects clim:*application-frame*))
	 (guy-to-center-on (first selected-objects)))
    (when (and guy-to-center-on (eql (superior guy-to-center-on) task))
      (center-display-pane-on-task clim:*application-frame* guy-to-center-on)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Producing the children
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod task-component-children ((the-port port))
  (declare (special *the-top-level-task*))
  (let ((parent-task (task the-port))
	(direction (direction the-port)))
    (when (eql *the-top-level-task* parent-task)
      (case direction
	(input (loop for flow in (outgoing-flows the-port)
		     for destination-port = (output flow)
		     for parent = (task destination-port)
		     collect parent))
	(output nil)))))

(defmethod task-component-children ((the-task has-output-ports-mixin))
  (declare (special *the-top-level-task*))
  (let ((answers nil))
    (loop for the-port in (outputs the-task)
	do (loop for flow in (outgoing-flows the-port)
	       for destination-port = (output flow)
	       for parent = (task destination-port)
	       when (typep parent 'join) do (setq parent (superior parent))
	       do (pushnew (if (and (eql (direction destination-port) 'output)
				    (eql parent *the-top-level-task*))
			       destination-port
			     parent)
			   answers)))
    (nreverse answers)))

(defmethod task-component-children :around ((the-task has-successors-mixin))
  (nconc (call-next-method)
	 (let ((answers nil))
	   (loop for cflow in (outgoing-control-flows the-task)
	       for successor = (successor cflow)
	       when (typep successor 'join) do (setq successor (superior successor))
	       do (pushnew  successor answers))
	   (nreverse answers))))

(defmethod task-component-children ((the-task join-mixin))
  (task-component-children (superior the-task)))

(defmethod task-component-children ((the-task has-branches-mixin))
  (loop for branch in (branches the-task)
      append (task-component-children branch)))

;;; Final-outputs can have successors but no outputs and they aren't a final-object
;;; anymore. So this base method must be here for the :around above to call
(defmethod task-component-children ((the-task final-output)) nil)
(defmethod task-component-children ((the-task path-end)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying each component
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *graphing-text-style* (clim:make-text-style :fix :bold :very-small))
(defparameter *show-input-type-constraints* nil)
(defparameter *show-input-names* t)
(defparameter *show-output-type-constraints* nil)
(defparameter *show-output-names* t)
(defparameter *branch-target-minimum-wire-size* 0)
(defparameter *control-port-length* 30)
(defparameter *minimum-task-name-width* 45)
(defparameter *meta-info-ink* clim:+blue+)
(defparameter *mininum-input-port-line-length* 20)
(defparameter *mininum-output-port-line-length* 20)

(defun set-display-controls (&optional (stream *standard-output*))
  (clim:accepting-values (stream :own-window t)
    (setq *show-input-names* (clim:accept 'boolean :prompt "Show input names" :stream stream :default *show-input-names*))
    (terpri stream)
    (setq *show-output-names* (clim:accept 'boolean :prompt "Show output names" :stream stream :default *show-output-names*))
    (terpri stream)
    (setq *show-input-type-constraints* (clim:accept 'boolean :prompt "Show input types" :stream stream :default *show-input-type-constraints*))
    (terpri stream)
    (setq *show-output-type-constraints* (clim:accept 'boolean :prompt "Show output types" :stream stream :default *show-output-type-constraints*))
    (terpri stream)))

(define-design-editor-command (com-set-display-controls :name t)
    ()
  (set-display-controls))


(defgeneric box-size (task stream))

(defclass box-size-description ()
  ((width :accessor width :initarg :width :initform 0)
   (height :accessor height :initarg :height :initform 0)
   (record :accessor record :initarg :record :initform nil)
   (joins-width :accessor joins-width :initarg :joins-width :initform 0)
   (join-heights :accessor join-heights :initarg :join-heights :initform nil)
   (join-records :accessor join-records :initarg :join-records :initform nil)
   (branches-width :accessor branches-width :initarg :branches-width :initform 0)
   (branch-heights :accessor branch-heights :initarg :branch-heights :initform nil)
   (branch-records :accessor branch-records :initarg :branch-records :initform nil)
   ))

(defmethod display-task-component ((task core-task-mixin) stream)
  (let ((*input-port-map* nil)
	(*output-port-map* nil)
	(first-input-port-position 0)
	(first-output-port-position 0)
	(input-width 0)
	)
    (declare (special *port-map* *input-port-map* *output-port-map*))
    (let* ((descriptor (box-size task stream))
	   (box-width (width descriptor))
	   (box-height (height descriptor))
	   (record (record descriptor))
	   (joins-width (joins-width descriptor))
	   (join-heights (join-heights descriptor))
	   (join-records (join-records descriptor))
	   (branch-heights (branch-heights descriptor))
	   (branch-records (branch-records descriptor)))
      (let* ((output-record
	      (clim:with-output-as-presentation (stream task 'basic-task
							:single-box t
							:allow-sensitive-inferiors t)
		(typecase task
		  ((or initial-input final-output path-end state-source state-sink constant)
		   (clim:draw-oval* stream (+ joins-width (floor box-width 2)) (floor box-height 2)
				    (floor box-width 2) (floor box-height 2) :filled nil))
		  (otherwise
		   (clim:draw-rectangle* stream joins-width 0 (+ joins-width box-width) box-height :filled nil)))
		(set-record-in-box-middle record joins-width 0 (+ joins-width box-width) box-height)
		)))
	(setf (output-record task) output-record)
	(multiple-value-setq (first-input-port-position input-width) (draw-input-ports task stream 0 0 box-height))
	(setq input-width (max input-width (draw-joins task stream 0 box-height joins-width join-heights join-records)))
	(setq first-output-port-position (draw-output-ports task stream (+ joins-width box-width) box-height 0))
	;; these two types of things only have inputs or output and always return 0
	;; for the first position of the the other, so this compensates for that
	(typecase task
	  ((or initial-input state-source constant)
	   (setq first-input-port-position first-output-port-position))
	  ((or path-end final-output state-sink)
	   (setq first-output-port-position first-input-port-position)))
	(draw-branches task stream box-width box-height branch-heights branch-records)
	(setf (gethash task *port-map*) (list *input-port-map* *output-port-map* box-height))
	(clim:with-bounding-rectangle* (left top right bottom) output-record
	  (declare (ignore top bottom))
	  (let ((fiducial-center-offset-x (+ input-width joins-width (floor (- right left) 2)))
		(fiducial-center-offset-y (min first-input-port-position first-output-port-position)))
	    ;; CAD grapher allows you to return a point that's used for centering 
	    ;; (rather than the bounding box which might be skewed by text labels on the ports)
	    ;; The first value must be the keyword :fiducial, otherwise it's ignored.
	    (values :fiducial fiducial-center-offset-x fiducial-center-offset-y left right joins-width)))))))


;;; Box-size figures out how large a box needs to be including both the size of the text
;;; and the number of input/output ports and the sizes of branches or joins
(defmethod box-size ((task core-task-mixin) stream)
  (multiple-value-bind (title-width title-height record) (box-title-and-type-size stream task)
    (let* ((box-width (round (* 1.1 title-width)))
	   (number-of-slots (determine-number-of-wire-slots task))
	   (normal-line-height (clim:stream-line-height stream))
	   (small-line-height (clim:stream-line-height stream *graphing-text-style*))
	   (box-height (max title-height (+ 2 (* (+ number-of-slots 2) (+ normal-line-height (* .5  small-line-height)))))))
      (make-instance 'box-size-description
	:width box-width
	:height box-height
	:record record))))

(defun box-title-and-type-size (stream task)
  (let ((record (clim:with-output-recording-options (stream :draw nil :record t)
		  (format-box-title-and-type stream task))))
    (clim:with-bounding-rectangle* (left top right bottom) record
      (values (- right left) (- bottom top) record))))

(defun format-box-title-and-type (stream task)
  (let* ((task-name (typecase task
		      (entry-point "Entry Point")
		      (exit-point "Exit Point")
		      (final-output "Final Output")
		      (path-end "Path End")
		      (initial-input "initial Input")
		      (otherwise (string (name task)))))
	 (state-element? (eql (task-type task) 'state))
	 ;; (name-size (clim:text-size stream task-name ::text-style *graphing-text-style*))
	 (task-type (typecase task
		      (entry-point (string (name task)))
		      (exit-point (string (name task)))
		      (path-end (string (name task)))
		      (final-output (string (name task)))
		      (initial-input (string (name task)))
		      (otherwise (string (task-type task))))))
    (clim:with-drawing-options (stream :ink (if (selected? task) clim:+red+ clim:+black+))
      (clim:with-output-centered (stream :move-cursor nil)
	(clim:with-text-style (stream *graphing-text-style*)
	  ;; In case this code is used for joins and branches (it's not)
	  ;; the task-type will be nil and we don't want to 
	  ;; print that out.
	  (clim:filling-output (stream :fill-width *minimum-task-name-width* :break-characters '(#\-))
	    (write-string task-name stream)
	    (when (join-name task)
	      (terpri stream)
	      (format stream "join: ~a" (join-name task)))
	    (when (branch-name task)
	      (terpri stream)
	      (format stream "branch: ~a" (branch-name task)))
	    (when state-element?
	      (terpri stream)
	      (format stream "STATE: ~a" (state-name task)))
	    (when (and task-type (not state-element?))
	      (clim:with-drawing-options (stream :ink *meta-info-ink*)
		(terpri stream)
		(write-string task-type stream)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Calculating sizes of branches
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass branch-box-size-description ()
  ((branch-height :accessor branch-height :initarg :branch-height :initform 0)
   (branch-width :accessor branch-width :initarg :branch-width :initform 0) 
   (branch-record :accessor branch-record :initarg :branch-record :initform 0)
   ))

(defmethod box-size :around ((task has-branches-mixin) stream)
  (let ((core-descriptor (call-next-method)))
    (loop for branch in (branches task)
	for branch-descriptor = (box-size branch stream)
	for branch-height = (branch-height branch-descriptor)
	collect branch-height into branch-heights
	collect (branch-record branch-descriptor) into branch-records
	sum branch-height into total-height
	maximize (branch-width branch-descriptor) into branches-width
	finally (setf (height core-descriptor) (max (height core-descriptor) total-height)
		      (branches-width core-descriptor) branches-width
		      (branch-heights core-descriptor) branch-heights
		      (branch-records core-descriptor) branch-records)
		(return core-descriptor))))

(defmethod box-size ((branch branch) stream)
  (multiple-value-bind (name-string-width name-string-height record)
      (fracture-text (string (name branch)) :max-width 30 :break-characters '(#\-) :stream stream ::text-style *graphing-text-style*)
    (let* ((number-of-slots (determine-number-of-wire-slots branch))
	   (normal-line-height (clim:stream-line-height stream))
	   (small-line-height (clim:stream-line-height stream *graphing-text-style*)) 
	   (box-width (round (* 1.1 name-string-width)))
	   (box-height (max name-string-height (+ 2 (* (+ number-of-slots 2) (+ normal-line-height (* .5  small-line-height)))))))
      (make-instance 'branch-box-size-description
	:branch-width box-width
	:branch-height box-height
	:branch-record record))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Calculating sizes of joins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass join-box-size-description ()
  ((join-height :accessor join-height :initarg :join-height :initform 0)
   (join-width :accessor join-width :initarg :join-width :initform 0) 
   (join-record :accessor join-record :initarg :join-record :initform 0)
   ))

(defmethod box-size :around ((task has-joins-mixin) stream)
  (let ((core-descriptor (call-next-method)))
    (loop for join in (joins task)
	for join-descriptor = (box-size join stream)
	for join-height = (join-height join-descriptor)
	collect join-height into join-heights
	collect (join-record join-descriptor) into join-records
	sum join-height into total-height
	maximize (join-width join-descriptor) into join-widths
	finally (setf (height core-descriptor) (max (height core-descriptor) total-height)
		      (joins-width core-descriptor) (+ (joins-width core-descriptor) join-widths)
		      (join-records core-descriptor) join-records
		      (join-heights core-descriptor) join-heights)
		(return core-descriptor))))

(defmethod box-size ((join join) stream)
   (multiple-value-bind (name-string-width name-string-height record)
      (fracture-text (string (name join)) :max-width 30 :break-characters '(#\-) :stream stream ::text-style *graphing-text-style*)
     (let* ((number-of-slots (determine-number-of-wire-slots join))
	    (normal-line-height (clim:stream-line-height stream))
	    (small-line-height (clim:stream-line-height stream *graphing-text-style*)) 
	    (box-width (round (* 1.1 name-string-width)))
	    (box-height (max name-string-height (+ 2 (* (+ number-of-slots 2) (+ normal-line-height (* .5  small-line-height)))))))
       (make-instance 'join-box-size-description
	 :join-width box-width
	 :join-height box-height
	 :join-record record))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dealing with the ports and their "wires"
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod is-target-of-branch ((task has-predecessors-mixin))
  (not (null (incoming-control-flows task))))

;;; default method
(defmethod is-source-of-branch ((branch branch)) t)

(defmethod is-source-of-branch ((task core-task-mixin)) nil)

(defmethod is-source-of-branch ((task has-successors-mixin))
  (not (null (outgoing-control-flows task))))

(defmethod determine-number-of-wire-slots ((task core-task-mixin))
  (let ((n-inputs (length (inputs task)))
	(n-outputs (length (outputs task))))
    (when (is-target-of-branch task) (incf n-inputs))
    (when (is-source-of-branch task) (incf n-outputs))
    (max n-inputs n-outputs))) 

(defgeneric draw-input-ports (task stream x-pos start-y end-y &optional width))

(defmethod draw-input-ports ((task has-joins-mixin) stream x-pos start-y end-y &optional width)
  (declare (ignore stream x-pos start-y end-y width))
  (values 0 0))

(defmethod draw-input-ports ((input initial-object-mixin) stream x start-y end-y &optional width)
  (declare (ignore stream x start-y start-y end-y width))
  (values 0 0))

(defvar *control-port* "<C>")

;;; Note that for a joining-task, this is called once for each join

(defmethod draw-input-ports ((task input-side-mixin) stream x-pos start-y end-y &optional width)
  (declare (special *input-port-map*))
  (let* ((branch-target? (is-target-of-branch task))
         (box-height (- end-y start-y))
         (inputs (inputs task))
         (n-true-inputs (length inputs))
	 (n-inputs (if branch-target? (1+ n-true-inputs) n-true-inputs))
	 ;; (line-height (clim:stream-line-height stream))
	 (small-line-height (clim:stream-line-height stream *graphing-text-style*))
	 (pitch (floor box-height (1+ n-inputs)))
	 (width (max *mininum-input-port-line-length* (or width (width-of-input-ports task stream))))
	 )
    ;; OK there's a special case when there are no inputs to the task, but you're a branch target.  Sigh!!
    (when (and branch-target? (null inputs))
      (let ((y-pos (+ start-y (floor box-height 2))))
	(clim:draw-arrow* stream (- x-pos width) y-pos 0 y-pos :line-dashes t)
	(push (list *control-port* (- x-pos width) y-pos) *input-port-map*)))
    (loop for input in inputs
	for name = (name input)
	for type-constraint-string = (type-constraint-string input)
	for y-pos from (+ start-y pitch) by pitch
	for i from 0
	do (when (and branch-target? (or (= n-inputs 2) (= i (floor n-inputs 2))))
	     ;; (clim:draw-text* stream *control-port*  (- width) y-pos)
	     (clim:draw-arrow* stream (- x-pos width) y-pos 0 y-pos :line-dashes t)
	     (push (list *control-port* (- x-pos width) y-pos) *input-port-map*)
	     (incf y-pos pitch))
	   (clim:with-output-as-presentation (stream input 'input-port :single-box t :allow-sensitive-inferiors nil)
	     (when *show-input-names*
	       (clim:draw-text* stream (string name) (- x-pos width) y-pos :text-style *graphing-text-style*))
	     (when (and type-constraint-string *show-input-type-constraints*)
	       (clim:with-text-style (stream *graphing-text-style*)
		 (clim:draw-text* stream type-constraint-string (- x-pos width) (+ y-pos small-line-height))))
	     (apply #'clim:draw-line* stream (- x-pos width) (1- y-pos) x-pos (1- y-pos) (drawing-options input)))
	   (push (list input (- x-pos width) (1- y-pos)) *input-port-map*))
    (values pitch width)))

(defmethod drawing-options ((thing t)) nil)

(defmethod drawing-options ((thing could-be-problematic-mixin))
  (if (is-problematic? thing)
      `(:ink ,clim:+red+ :line-thickness 3 :line-cap-shape :square)
    ()))

(defmethod width-of-input-ports ((task input-side-mixin) stream)
  (let* ((inputs (inputs task))
	 (branch-target? (is-target-of-branch task))
	 (width (loop for input in inputs
		    when *show-input-type-constraints*
		    maximize (clim:text-size stream (type-constraint-string input) ::text-style *graphing-text-style*)
		    when *show-input-names*
		    maximize (clim:text-size stream (string (name input)) ::text-style *graphing-text-style*))))
    (if branch-target?
	(max *branch-target-minimum-wire-size* width)
      width)))

(defmethod width-of-input-ports :around ((task has-joins-mixin) stream)
  (loop for join in (joins task)
      maximize (width-of-input-ports join stream)))

(defgeneric draw-output-ports (task stream x-pos box-height box-base &optional max-string-length))

(defmethod draw-output-ports ((output final-object-mixin) stream x-pos box-height box-base &optional max-string-length)
  (declare (ignore stream x-pos box-height box-base max-string-length))
  (values 0))

(defmethod draw-output-ports ((task has-branches-mixin) stream x-pos box-height box-base &optional max-string-length)
  (declare (ignore x-pos box-height stream box-base max-string-length))
  (values 0))

(defmethod draw-output-ports ((task path-end) stream x-pos box-height box-base &optional max-string-length)
  (declare (ignore x-pos box-height stream box-base max-string-length))
  (values 0)
  )

;;; a final output can have an outgoing control flow but it has no outputs.
;;; A special case for that (which used to be here for state sinks which now do have outputs)
(defmethod draw-output-ports ((task final-output) stream x-pos box-height box-base &optional max-string-length)
  (declare (special *output-port-map* max-string-length))
  (let* ((branch-source? (or (is-source-of-branch task) (typep task 'branch)))
	 (n-outputs (if branch-source? 1 0))
	 (pitch (floor box-height (1+ n-outputs)))
	 (width *control-port-length*))
    (flet ((draw-control-flow (y-pos)
	     (when branch-source?
	       (clim:draw-arrow* stream x-pos y-pos (+ x-pos width -10) y-pos :line-dashes t)
	       (clim:draw-circle* stream (+ x-pos width -5) y-pos 5))
	     (push (list *control-port* (+ width x-pos) y-pos) *output-port-map*)
	     ))
      ;; special case for a component with no outputs
      ;; but a control flow to some other component
      ;; don't know if that makes sense but this is defensive code in case.
      (when branch-source?
	(draw-control-flow (+ box-base pitch)))
      )
    pitch))

(defmethod draw-output-ports ((task output-side-mixin) stream x-pos box-height box-base &optional max-string-length)
  (declare (special *output-port-map*))
  (let* ((outputs (outputs task))
	 (branch-source? (or (is-source-of-branch task) (typep task 'branch)))
	 (n-true-outputs (length outputs))
	 (n-outputs (if branch-source? (1+ n-true-outputs) n-true-outputs))
	 (small-line-height (clim:stream-line-height stream *graphing-text-style*))
	 (pitch (floor box-height (1+ n-outputs)))
	 (width (or max-string-length
		    (max *control-port-length*
			 *mininum-output-port-line-length*
			 (loop for output in outputs
			     when *show-output-type-constraints*
			     maximize (clim:text-size  stream (type-constraint-string output) :text-style *graphing-text-style*)
			     when *show-output-names*
			     maximize (clim:text-size stream (string (name output)) :text-style *graphing-text-style*))))))
    (flet ((draw-control-flow (y-pos)
	     (cond
	      ((and branch-source? (null (outgoing-control-flows task)))
	       (clim:draw-arrow* stream x-pos y-pos (+ x-pos width -10) y-pos :line-dashes t)
	       (clim:draw-circle* stream (+ x-pos width -5) y-pos 5))
	      (t
	       (clim:draw-line* stream x-pos y-pos (+ x-pos width) y-pos :line-dashes t)))
	     (push (list *control-port* (+ width x-pos) y-pos) *output-port-map*)
	     ))
      ;; special case for a component with no outputs
      ;; but a control flow to some other component
      ;; don't know if that makes sense but this is defensive code in case.
      (when (and branch-source? (= n-true-outputs 0))
	(draw-control-flow (+ box-base pitch)))
      (loop for output in outputs
	  for name = (name output)
	  for type-constraint-string = (type-constraint-string output)
	  for y-pos from (+ pitch box-base) by pitch
	  for i from 0
	  do (when (and branch-source? (or (= n-outputs 2) (= i (floor n-outputs 2))))
	       (draw-control-flow y-pos)
	       (incf y-pos pitch)) 
	     (clim:with-output-as-presentation (stream output 'output-port :single-box t :allow-sensitive-inferiors nil)
	       (when *show-output-names*
		 (clim:draw-text* stream (string name) x-pos y-pos :text-style *graphing-text-style*))
	       (when (and type-constraint-string *show-output-type-constraints*)
		 (clim:with-text-style (stream *graphing-text-style*)
		   (clim:draw-text* stream type-constraint-string x-pos (+ y-pos small-line-height))))
	       ;; (format *error-output* "~%Line ~a ~d ~d ~d ~d" task x-pos (1- y-pos) (+ width x-pos 1) (1- y-pos))
	       (apply #'clim:draw-line* stream x-pos (1- y-pos) (+ width x-pos) (1- y-pos) (drawing-options output)))
	     (push (list output (+ x-pos width) (1- y-pos)) *output-port-map*)))
    pitch))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing joins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric draw-joins (task stream x-pos box-height joins-width join-heights join-records))

(defmethod draw-joins (basic-task stream x-pos box-height joins-width join-heights join-records)
  (declare (ignore basic-task stream x-pos box-height joins-width join-heights join-records))
  (values 0))

(defmethod draw-joins ((task has-joins-mixin) stream x-pos box-height joins-width join-heights join-records)
  (let* ((joins (joins task))
         (n-joins (length joins))
         (pitch (floor box-height n-joins))
	 (width-of-input-ports (width-of-input-ports task stream)))
    (loop for join in joins
	for record in join-records
	for join-number from 1
	for y-pos = 0 then next-y-pos
	for next-y-pos = (+ y-pos pitch)
	for join-height in join-heights
	maximize (multiple-value-bind (first-input-position input-width) 
		     (draw-join join stream x-pos joins-width y-pos (if (= join-number n-joins) box-height next-y-pos)
				join-height width-of-input-ports record)
		   (declare (ignore first-input-position))
		   input-width)
		 )))

(defmethod draw-join ((task join-mixin) stream start-x end-x start-y end-y join-height width-of-input-ports record)
  (declare (ignore join-height))
  (let (first-input-position input-width)
    (clim:with-output-as-presentation (stream task 'join
					      :single-box t
					      :allow-sensitive-inferiors t)
      (clim:draw-rectangle* stream start-x start-y end-x end-y :filled nil)
      (set-record-in-box-middle record start-x start-y end-x end-y)
      (let ((*input-port-map* nil))
	(declare (special *input-port-map* *port-map*))
	(multiple-value-setq (first-input-position input-width)
	  (draw-input-ports task stream start-x start-y end-y width-of-input-ports))
	 (setf (gethash task *port-map*) (list *input-port-map* nil 0))))
    (values first-input-position input-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing Branches
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric draw-branches (task stream x-pos box-height branch-heights branch-records))

(defmethod draw-branches ((task core-task-mixin) stream x-pos box-height branch-heights branch-records)
  (declare (ignore stream x-pos box-height branch-heights branch-records))
  (values))

(defmethod draw-branches ((task has-branches-mixin) stream xpos box-height branch-heights branch-records)
  (declare (special *port-map*))
  (let* ((branches (branches task))
	 (n-branches (length branches))
	 (pitch (floor box-height n-branches))
	 ;; (half-pitch (floor pitch 2))
	 (width (loop for branch-record in branch-records maximize (clim:with-bounding-rectangle* (left top right bottom) branch-record
												  (declare (ignore top bottom))
												  (abs (- left right)))))
	 (xpos-plus-width (+ xpos width))
	 (max-string-width (max *control-port-length*
				(loop for branch in branches
				    maximize (loop for output in (outputs branch)
						 when *show-output-type-constraints*
						 maximize (clim:text-size stream (type-constraint-string output) ::text-style *graphing-text-style*)
						 maximize (clim:text-size stream (string (name output)) ::text-style *graphing-text-style*))))))
    (loop for branch in branches 
	for record in branch-records
	for branch-number from 1
	;; for name = (name branch)
	;; for string = (string name) 
	for ypos from 0 by pitch
	for ypos-plus-pitch = (if (= branch-number n-branches) box-height (+ ypos pitch))
	for branch-height in branch-heights
	do (clim:with-output-as-presentation (stream branch 'branch
						     :single-box t
						     :allow-sensitive-inferiors t)
	     (clim:draw-rectangle* stream xpos ypos xpos-plus-width ypos-plus-pitch :filled nil)
	     (set-record-in-box-middle record xpos ypos xpos-plus-width ypos-plus-pitch)
	     (clim:tree-recompute-extent record)
	     (clim:replay record stream)
	     (let ((*output-port-map* nil))
	       (declare (special *output-port-map*))
	       (draw-output-ports branch stream (+ xpos width) branch-height ypos max-string-width)
	       (setf (gethash branch *port-map*) (list nil *output-port-map* box-height)))
	       )
	   )))

(defun set-record-in-box-middle (record left top right bottom)
  (clim:with-bounding-rectangle* (rleft rtop rright rbottom) record
    (let* ((record-half-width (/ (abs (- rright rleft)) 2))
	   (record-half-height (/ (abs (- rbottom rtop)) 2))
	   (half-height (/ (abs (- top bottom)) 2))
	   (half-width (/ (abs (- left right)) 2)))
      (clim:output-record-set-position record (- (+ left half-width) record-half-width) (- (+ top half-height) record-half-height)))))
				 
(defmethod display-task-component ((the-thing has-name-mixin) stream)
  (clim:surrounding-output-with-border (stream :shape (drawing-shape the-thing))
    (clim:with-output-as-presentation (stream the-thing (common-lisp:type-of the-thing) 
					      :single-box t
					      :allow-sensitive-inferiors nil)
      (clim:with-text-style (stream *graphing-text-style*)
	(write-string (string (name the-thing)) stream)
	;; (terpri stream)
	(write-string (string (task-type the-thing)) stream))
      )))

(defmethod drawing-shape ((the-join join-mixin)) :rectangle)
(defmethod drawing-shape ((the-port port)) :oval)
(defmethod drawing-shape ((the-branch branch-mixin)) :drop-shadow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interacting with the CAD Grapher's wiring sytem
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *debug-arrow-drawer* nil)

(defmethod task-network-arrow-drawer (collector from-object to-object x1 y1 x2 y2)
  ;; (declare (ignore from-object to-object))
  ;; (error "I don't think this method should ever get invoked")
  (when *debug-arrow-drawer*
    (format *error-output* "~%Collector call 1 ~a ~a ~d ~d ~d ~d" from-object to-object x1 y1 x2 y2))
  (when (and (typep from-object 'core-task-mixin)
	     (typep to-object 'core-task-mixin))    
    (funcall collector x1 y1 x2 y2)))

(defmethod task-network-arrow-drawer (collector 
				      (source-port port)
				      (destination-task has-input-ports-mixin)
				      x1 y1 x2 y2)
  (declare (special *port-map*))
  (error "I don't think this method should ever get invoked")
  (let* ((destination-task-port-map (gethash destination-task *port-map*)))
    (destructuring-bind (destination-task-input-map destination-task-output-map output-height) destination-task-port-map
      (declare (ignore destination-task-output-map output-height))
      (loop for flow in (outgoing-flows source-port)
	  for output-port = (output flow)
	  for destination-task = (task output-port)
	  do 
	    (cond ((eql destination-task destination-task)
		   (let* ((out-entry (assoc output-port destination-task-input-map))
			  (real-y2 (+ y2 (third out-entry))))
		     (when *debug-arrow-drawer* 
		       (format *error-output* "~%Collector-call 2 ~a ~a ~d ~d ~d ~d" source-port destination-task x1 y1 x2 real-y2))
		     (funcall collector x1 y1 x2 real-y2))))))))

(defmethod task-network-arrow-drawer (collector 
				      (source-task has-output-ports-mixin)
				      (destination-port port)
				      x1 y1 x2 y2)
  (declare (special *port-map*))
  (error "I don't think this method should ever get invoked")
  (let* ((source-task-port-map (gethash source-task *port-map*)))
    (destructuring-bind (source-task-input-port-map source-task-output-port-map input-height) source-task-port-map
      (declare (ignore source-task-input-port-map input-height))
      (loop for source-port in (outputs source-task)
	  do (loop for flow in (outgoing-flows source-port)
		 when (eql (output flow) destination-port)
		 do (let* ((entry (assoc source-port source-task-output-port-map))
			   (real-y1 (+ y1 (third entry))))
		      (when *debug-arrow-drawer*
			(format *error-output* "~%Collector call 3 ~a ~a ~d ~d ~d ~d" source-task destination-port x1 real-y1 x2 y2))
		      (funcall collector x1 real-y1 x2 y2)))))))

;;; map down to the branches
(defmethod task-network-arrow-drawer (collector 
				      (source-task has-branches-mixin)
				      (destination-task core-task-mixin)
				      x1 y1 x2 y2)
  (loop for branch in (branches source-task)
      do (task-network-arrow-drawer collector branch destination-task x1 y1 x2 y2)
	 ))

;;; map down to the joins
(defmethod task-network-arrow-drawer (collector 
				      (source-task output-side-mixin)
				      (destination-task has-joins-mixin)
				      x1 y1 x2 y2)
  (loop for join in (joins destination-task)
      do (task-network-arrow-drawer collector source-task join x1 y1 x2 y2)))

;;; At this point, the source and destination are both either join, branch or task
;;; whichever is the most specific.
(defmethod task-network-arrow-drawer (collector 
				      (source-task output-side-mixin)
				      (destination-task input-side-mixin)
				      x1 y1 x2 y2)
  (declare (special *port-map*))
  (let* ((source-task-port-map (gethash source-task *port-map*))
	 (destination-task-port-map (gethash destination-task *port-map*)))
    (destructuring-bind (source-task-input-port-map source-task-output-port-map input-height) source-task-port-map
      (declare (ignore source-task-input-port-map input-height))
      (destructuring-bind (output-task-input-port-map output-task-output-port-map output-height) destination-task-port-map
	(declare (ignore output-task-output-port-map output-height))
	(loop for source-port in (outputs source-task)
	    do (loop for flow in (outgoing-flows source-port)
		   for destination-port = (output flow)
		   for his-task = (task destination-port)
		   when (eql his-task destination-task)
		   do (let* ((source-entry (assoc source-port source-task-output-port-map))
			     (real-y1 (+ y1 (third source-entry)))
			     (destination-entry (assoc destination-port output-task-input-port-map))
			     (real-y2 (+ y2 (third destination-entry))))
			(when *debug-arrow-drawer* 
			  (format *error-output* "~%Collector call 4a ~a ~a ~d ~d ~d ~d" source-task destination-task x1 real-y1 x2 real-y2))
			(funcall collector x1 real-y1 x2 real-y2 (drawing-options flow)))))))))

(defmethod task-network-arrow-drawer (collector (from-object final-output) to-object x1 y1 x2 y2)
  ;;; This keeps method 1 from getting invoked on final-objects; they
  ;;; aren't output-side-mixin but are has-successors-mixin which 
  ;;; means the next method gets invoked as the after but requires
  ;;; a primary which defaults to method 1 without this.
  (declare (ignore collector to-object x1 y1 x2 y2))
  (values)
  )

(defmethod task-network-arrow-drawer (collector (from-object path-end) to-object x1 y1 x2 y2)
  ;;; This keeps method 1 from getting invoked on final-objects; they
  ;;; aren't output-side-mixin but are has-successors-mixin which 
  ;;; means the next method gets invoked as the after but requires
  ;;; a primary which defaults to method 1 without this.
  (declare (ignore collector to-object x1 y1 x2 y2))
  (values)
  )

;;; this guy handles control flows for non-branching tasks
(defmethod task-network-arrow-drawer :after (collector 
					     (source-task has-successors-mixin)
					     (destination-task input-side-mixin)
					     x1 y1 x2 y2)
  (declare (special *port-map*))
  (let* ((source-task-port-map (gethash source-task *port-map*))
	 (destination-task-port-map (gethash destination-task *port-map*)))
    (destructuring-bind (source-task-input-port-map source-task-output-port-map input-height) source-task-port-map 
      (declare (ignore source-task-input-port-map input-height))
      (destructuring-bind (output-task-input-port-map output-task-output-port-map output-height) destination-task-port-map
	(declare (ignore output-task-output-port-map output-height))
	(loop for cflow in (outgoing-control-flows source-task)
	    for successor = (successor cflow)
	    when (eql successor destination-task)
	    do (let* ((source-entry (assoc *control-port* source-task-output-port-map))
		      (real-y1 (+ y1 (third source-entry)))
		      (destination-entry (assoc *control-port* output-task-input-port-map))
		      (real-x2 (+ x2 (second destination-entry)))
		      (real-y2 (+ y2 (third destination-entry))))
		 (when *debug-arrow-drawer* 
		   (format *error-output* "~%Collector call 4b ~a ~a ~d ~d ~d ~d ~d" source-task destination-task x1 real-y1 x2 real-x2 real-y2))
		 (funcall collector x1 real-y1 x2 real-y2 '(:line-dashes t))))))))

(defun fracture-text (string &key (stream *standard-output*) (max-width 30) (break-characters '(#\- #\space)) (text-style *graphing-text-style*))
  (setq string (string string))
  (let ((record (clim:with-new-output-record (stream)
		  (clim:with-output-recording-options (stream :draw nil :record t)
		    (clim:filling-output (stream :fill-width max-width :break-characters break-characters)
		      (clim:with-text-style (stream text-style)
			(fresh-line stream)
			(write-string string stream)))))))
    (clim:with-bounding-rectangle* (left top right bottom) record
      (let ((width (abs (- right left)))
	    (height (abs (- top bottom))))
	(values width height record)))))

(defun test-me (string1 string2 size &optional (stream *standard-output*))
  (clim:filling-output (stream :fill-width size :break-characters '(#\-))
    (write-string string1 stream)
    (terpri stream)
    (write-string string2 stream)))
