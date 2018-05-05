;;; -*- Mode: Common-lisp; Package: Natsoft; readtable: Joshua -*-

(in-package :natsoft)

(tell [context-include catalano "Catalano.Imaging.FastBitmap"])
(tell [context-include catalano "Catalano.Imaging.Filters.*"])
(tell [context-include catalano "javax.swing.JOptionPane"])


(defdata-type java-frame
    :super-types ()
    )

(defdata-type null
    :super-types ()
    )

(defdata-type java-abstract-frame 
    :union (java-frame null)
    )

(defdata-type java-image 
    :super-types (image)
    :other-assertions ([allocation-code java-image (:allocate fast-bit-map) :java])
    )

(defdata-type fast-bitmap
    :super-types (java-image)
    )

(defdata-type java-icon
    :super-types (java-image)
    )

(defdata-type pathname
    :super-types ())

;;; Read Image is just an allocate of a fast bit map 
;;; with one argument.  
;;; should add a transformation for this.

(deftask read-image
    :parameters (framework)
    :dont-expand nil
    :interface ((:inputs (file string))
                (:outputs (image java-image)))
    )

(deftask make-fast-bitmap
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (file pathname))
                (:outputs (image java-image)))
    )

;;; This is the more abstract version that only takes 1 arg, the image It has a
;;; parameter saying what to label the display.  it needs a refinement to go to
;;; the actual showMessageDialog The refinement also includes a to-icon step to
;;; convert from java-image to java iconz

(deftask display-image
    :parameters (framework label)
    :dont-expand nil
    :interface ((:inputs (the-image java-image)))
    )

;;; This is an actual java facility that takes four arguments
;;; We will need a reduction from display-image to this
;;; A plan that involves this and a convert-to-icon method
(deftask display-dialog
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs 
                 (class-name java-class-name)
                 (parent java-abstract-frame)
                 (message java-icon)
                 (label string)
                 (type dialog-type)))
    :other-assertions ([framework-mapping display-dialog catalano show-message-dialog])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'display-dialog)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(defdata-type java-class-name
    :super-types ()
    :parameters (name)
    )

(defdata-type dialog-type 
    :super-types ())

(defdata-type plain-message-dialog-type
    :super-types (dialog-type)
    ;; :other-assertions ([allocation-code plain-message-dialog-type JOptionPane :java])
    )

;;; do I want types for rgb-image and grayscale-image?
(deftask convert-to-rgb
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (image java-image))
                ;; works by side effect
                ;; (:outputs (rgb-image java-image))
                )
    :other-assertions ([framework-mapping convert-to-rgb catalano to-r-g-b])
    )


(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'convert-to-rgb)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

;;; like to rgb works by side effect
(deftask convert-to-grayscale
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (image java-image)))
    :other-assertions ([framework-mapping convert-to-grayscale catalano to-grayscale])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'convert-to-grayscale)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

;;; does produce an output
(deftask convert-to-icon
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (image java-image))
                (:outputs (icon java-icon))
                )
    :other-assertions ([framework-mapping convert-to-icon catalano to-icon])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'convert-to-icon)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(defdata-type image-processing-filter
    ;; I guess there's no notation for saying that a type is abstract
    ;; and never implemented directly
    ;; :abstract t
    )


;;; This is the abstract apply-filter.  It refines to 
;;; a two step plan of making the filter and then applying
(deftask filter-image
    :super-types ()
    :parameters (framework filter-name)
    :interface ((:inputs (image java-image))))

;;; This is the actual java method
(deftask apply-filter
    :super-types ()
    :dont-expand t
    :parameters (framework)
    :interface ((:inputs (filter image-processing-filter)
                         (image java-image)))
    :other-assertions ([framework-mapping apply-filter catalano Apply-in-place])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'apply-filter)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

;;; This is the compound task
;;; read, display, filter, display
(deftask display-and-filter-image
    :super-types ()
    :parameters (framework filter-type)
    :interface ((:inputs (the-file string)))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Canonical-arguments, Reductions and Cliche's
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display and filter image 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defmethod canonical-arguments-for ((type (eql 'display-and-filter-image)) the-displayer)
  (flet ((get-one-property (property-name)
           (ask `[property-value-of ,the-displayer ,property-name ?answer]
                #'(lambda (just) 
                    (declare (ignore just)) 
                    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (let ((filter-type (get-one-property 'filter-type))
          (the-framework (get-one-property 'framework)))
      (list filter-type the-framework)))
  )

(defreduction build-the-filter (display-and-filter-image (?filter-type ?the-framework))
  :reduce-to (read-and-display-image-plan ?the-framework ?filter-type)
  )

(defcliche read-and-display-image-plan (?the-framework ?filter-type)
  ;; :constants ((:name PATHNAME :type PATHNAME :value "sample20.png"))
  :initial-inputs ((:name the-file :type string))
  :components ((:name READ-IMAGE :type read-image :FRAMEWORK ?THE-FRAMEWORK)
               (:name TO-RGB :type CONVERT-TO-RGB :FRAMEWORK ?THE-FRAMEWORK)
               (:name SHOW-INITIAL :type DISPLAY-IMAGE :FRAMEWORK ?THE-FRAMEWORK :LABEL "Original")
               (:name TO-GRAY :type CONVERT-TO-GRAYSCALE :FRAMEWORK ?THE-FRAMEWORK)
               (:name FILTER :type FILTER-IMAGE :FRAMEWORK ?THE-FRAMEWORK :FILTER-NAME ?filter-type)
               (:name SHOW-FINAL :type DISPLAY-IMAGE :FRAMEWORK ?THE-FRAMEWORK :LABEL "Result")
               )
  :dataflows (((:component the-file :port the-file) (:component read-image :port file))
              ((:component READ-IMAGE :port image) (:component TO-RGB :port IMAGE))
              ((:component READ-IMAGE :port image) (:component TO-GRAY :port IMAGE))
              ((:component READ-IMAGE :port image) (:component SHOW-INITIAL :port THE-IMAGE))
              ((:component READ-IMAGE :port image) (:component FILTER :port IMAGE))
              ((:component READ-IMAGE :port image) (:component SHOW-FINAL :port THE-IMAGE))
              ;; ((:component PATHNAME :port PATHNAME) (:component READ-IMAGE :port FILE))
              )
  :control-flows (
                  ((:component TO-RGB) (:component SHOW-INITIAL))
                  ((:component SHOW-INITIAL) (:component TO-GRAY))
                  ((:component TO-GRAY) (:component FILTER))
                  ((:component FILTER) (:component SHOW-FINAL))                  
                  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apply filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defmethod canonical-arguments-for ((type (eql 'filter-image)) the-displayer)
  (flet ((get-one-property (property-name)
           (ask `[property-value-of ,the-displayer ,property-name ?answer]
                #'(lambda (just) 
                    (declare (ignore just)) 
                    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (let ((filter-type (get-one-property 'filter-name))
          (the-framework (get-one-property 'framework)))
      (list filter-type the-framework)))
  )

(defreduction filter-image-to-apply-filter (filter-image (?filter-type ?the-framework))
  :reduce-to (apply-filter-for-real ?filter-type ?the-framework)
    )

(defcliche apply-filter-for-real (?filter-type ?framework)
  :initial-inputs ((:name IMAGE :type JAVA-IMAGE))
  :components ((:name MAKE-FILTER :type ALLOCATE :OBJECT-TYPE ?FILTER-TYPE :FRAMEWORK ?framework :local t)
               (:name DO-IT :type APPLY-FILTER :FRAMEWORK ?framework))
  :dataflows (((:component IMAGE :port IMAGE) (:component DO-IT :port IMAGE))
              ((:component MAKE-FILTER :port NEW-OBJECT) (:component DO-IT :port FILTER))
              )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod canonical-arguments-for ((type (eql 'display-image)) the-displayer)
  (flet ((get-one-property (property-name)
           (ask `[property-value-of ,the-displayer ,property-name ?answer]
                #'(lambda (just) 
                    (declare (ignore just)) 
                    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (let ((the-label (get-one-property 'label))
          (the-framework (get-one-property 'framework)))
      (list the-label the-framework)))
  )

(defreduction display-image-through-dialog (display-image (?the-label ?the-framework))
  :reduce-to (display-image-as-dialog ?the-label ?the-framework)
  )


(defcliche display-image-as-dialog (?label ?framework)
  :initial-inputs ((:name THE-IMAGE :type JAVA-IMAGE))
  :constants ((:Name make-class :type java-class-name :value "JOptionPane" :local t)
              (:name MAKE-LABEL :TYPE STRING :value ?label :local t)
              (:name MAKE-NULL :TYPE null :value NULL :local t)
              (:name MAKE-DIALOG-TYPE :TYPE dialog-type :value "JOptionPane.PLAIN_MESSAGE" :local t)
              )
  :components ((:name DO-DIALOG :type DISPLAY-DIALOG :FRAMEWORK ?framework)
               (:name MAKE-ICON :type CONVERT-TO-ICON :FRAMEWORK ?framework)
               )
  :dataflows (((:component make-class :port make-class) (:component Do-dialog :port class-name))
              ((:component MAKE-LABEL :port make-label) (:component DO-DIALOG :port LABEL))
              ((:component MAKE-NULL :port make-null) (:component DO-DIALOG :port PARENT))
              ((:component MAKE-DIALOG-TYPE :port make-dialog-type) (:component DO-DIALOG :port TYPE))
              ((:component MAKE-ICON :port ICON) (:component DO-DIALOG :port MESSAGE))
              ((:component THE-IMAGE :port THE-IMAGE) (:component MAKE-ICON :port IMAGE))
              )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read-Image
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod canonical-arguments-for ((type (eql 'read-image)) the-displayer)
  (flet ((get-one-property (property-name)
                           (ask `[property-value-of ,the-displayer ,property-name ?answer]
                                #'(lambda (just) 
                                    (declare (ignore just)) 
                                    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (let ((the-framework (get-one-property 'framework)))
      (list the-framework)))
  )

(defreduction read-image-through-allocate-fast-bitmap (read-image (?the-framework))
  :reduce-to (read-image-as-allocate ?the-framework)
  )

(defcliche read-image-as-allocate (?the-framework)
  :initial-inputs ((:name file :type string))
  :Final-outputs ((:name image :type java-image))
  :components ((:name READ-IMAGE :type allocate :object-type Fast-Bitmap :FRAMEWORK ?THE-FRAMEWORK :inputs (pathname string)))
  :dataflows (((:component file :port file) (:component read-image :port pathname))
              ((:component READ-IMAGE :port new-object) (:component image :port IMAGE))
              )
  )

;;; Need stuff for reducing read-image to fast-bitmap

                                                          
#|
Real thing:     JOptionPane.showMessageDialog(null, fb.toIcon(), "Original image", JOptionPane.PLAIN_MESSAGE);
What I Produce: JOPtionPane.showMessageDialog(null, theImage.toIcon(), "inital display", JOPtionPane.PLAIN_MESSAGE);
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NLP support
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule recognize-read-image-request (:backward)
  :then [is-appropriate-response |read_in| ?main handle-read-image-request ()]
  :if [and [subject-of ?main ? |you|]
           [object-of ?main ? |image|]
           [as-subject ?main ?qualifier]
           [relation-of ?qualifier ? |has_modifier|]
           [object-of ?qualifier ? |first|]
           ])

(defmethod handle-read-image-request ((parser core-parser-mixin) main)
  (declare (ignore main))
  (initiate-pipeline 'read-image 'read-image)
  )

(defrule recognize-convert-image-request (:backward)
  :then [is-appropriate-response |convert| ?main handle-convert-image-request (?goal)]
  :if [and [subject-of ?main ? |you|]
           [object-of ?main ? |image|]
           [as-subject ?main ?qualifier]
           [relation-of ?qualifier ? |has_modifier|]
           [object-of ?qualifier ? |next|]
           [as-subject ?main ?destination]
           [relation-of ?destination ? |to|]
           [object-of ?destination ? ?goal]           
           ]
  )

(defmethod handle-convert-image-request ((parser core-parser-mixin) main image-goal)
  (declare (ignore main))
  (ecase (convert-start-string-to-lisp-atom image-goal)
    (grayscale (attach-to-pipeline 'convert-to-grayscale :task-name 'to-gray))
    (color (attach-to-pipeline 'convert-to-rgb :task-name 'to-color))))

(defrule recognize-display-request (:backward)
  :then [is-appropriate-response |display| ?main handle-display-image-request (?label)]
  :if [and [subject-of ?main ? |you|]
           [object-of ?main ?image |image|]
           [as-subject ?main ?qualifier]
           [relation-of ?qualifier ? |has_modifier|]
           [object-of ?qualifier ? |next|]
           [as-subject ?image ?image-property-clause]
           [relation-of ?image-property-clause ? |has_property|]
           [object-of ?image-property-clause ? ?label]
           ]
  )

(defmethod handle-display-image-request ((parser core-parser-mixin) main label)
  (declare (ignore main))
  (attach-to-pipeline 'display-image 
                      :task-name 'show-image
                      :additional-properties `((:label ,(string label)))
                      ))

(defrule recognize-filter-request (:backward)
  :then [is-appropriate-response |filter| ?main handle-filter-image-request (?type)]
  :if [and [subject-of ?main ? |you|]
           [object-of ?main ? |image|]
           [as-subject ?main ?qualifier]
           [relation-of ?qualifier ? |has_modifier|]
           [object-of ?qualifier ? |next|]
           [as-subject ?main ?using]
           [relation-of ?using ? |using|]
           [object-of ?using ? ?type]
           ]
  )

(defmethod handle-filter-image-request ((parser core-parser-mixin) main type-of-filter)
  (declare (ignore main))
  (ecase (convert-start-string-to-lisp-atom type-of-filter)
    (threshold-filter
     (attach-to-pipeline 'filter-image
                        :task-name 'filter
                        :additional-properties `((:filter-name bradley-local-threshold))
                        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Aids for building an image processing pipeline
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pipeline-name-counter* 0)

(defun find-initiating-task (design &optional (initial-input-acceptable? nil))
  (let ((initial-input (loop for task in (children design)
                           for inputs = (inputs task)
                           when (null inputs)
                           return task)))
    (if initial-input-acceptable?
        initial-input
      (let* ((output (first (outputs initial-input)))
             (flow (first (outgoing-flows output))))
        (task (output flow))))))

(defun attach-to-pipeline (task-type &key (task-name (smash task-type (incf *pipeline-name-counter*)))
                                          (initial-input-acceptable? nil) 
                                          (do-control-flow? t)
                                          (additional-properties nil)
                                          )
  (let* ((design (design-in-progress *design-editor*))
         (start-of-pipeline (find-initiating-task design initial-input-acceptable?))
         (source-port (first (outputs start-of-pipeline)))
         (end-of-pipeline (first (find-final-tasks design))))
    (let* ((new-object (add-component *design-editor* 
                                      task-name
                                      task-type
                                      `((:framework catalano) ,@additional-properties)
                                      ))
           (destination-port (first (inputs new-object))))
      (add-dataflow source-port destination-port)
      (when do-control-flow?
        (make-control-flow end-of-pipeline new-object)))))

(defun initiate-pipeline (task-type task-name)
  (let* ((design (design-in-progress *design-editor*)))
    (add-initial-input design 'file 'file 'string)
    (attach-to-pipeline task-type :task-name task-name 
                        :initial-input-acceptable? t
                        :do-control-flow? nil)
    ))

(defun show-pipeline (file)
  (declare (ignore file))
  (excl:run-shell-command "~/Research-projects/Muse/ImageDemo/java -classpath ~/Research-proojects/Muse/ImageDemo/SampleApplication.jar:~/Research-projects/MUSE/ImageDemo/ Pipeline ~/Research-projects/MUSE/ImageDemo/picture-of-me.jpg")
  )

(defun mine-filter-types (&key 
                          (filter-package-name "Catalano.Imaging.Filters")
                          (pathname "natsoft:code;filter-type-defs.lisp"))
  (let* ((filter-package (intern-package-descriptor filter-package-name))
         (classes (retrieve-classes-with-package-name filter-package)))
    (with-open-file (out pathname :direction :output :if-exists :supersede :if-doesn-not-exist :create)
      (format out ";;; -*- Mode: Common-lisp; Package: Natsoft; readtable: Joshua -*-")
      (format out "~2%(in-package :natsoft)~2%")
      (loop for class in classes
          for class-name = (json:simplified-camel-case-to-lisp (java-class-name class))
          unless (find #\$ class-name :test #'char-equal)
          do (format out "~2%(defdata-type ~a :super-types (image-processing-filter))"
                     class-name)))))
         