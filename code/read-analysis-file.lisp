;;; -*- Mode: Common-lisp; Package: Natsoft; readtable: Joshua -*-

(in-package :natsoft)

(eval-when (:compile-toplevel :load-toplevel)
  (unless (find-package :json)
    (ql:quickload "cl-json"))
  )

(setq json:*json-array-type* 'vector)

#+Allegro
(eval-when (:compile-toplevel :load-toplevel)
  (setq excl:*additional-logical-pathname-name-chars* '(#\_ #\*)))

(defparameter *package-descriptor-ht* (make-hash-table :test #'equal))
(defparameter *package-names* nil)
(defparameter *class-descriptor-ht* (make-hash-table :test #'equal))
(defparameter *method-descriptor-ht* (make-hash-table :test #'equal))
(defparameter *cluster-number* 0)

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate-model java-class-description-mixin
      ()
    (cf-mixin)))

(define-predicate-method (certainty-factor java-class-description-mixin) () 1)

(defun explode-string (string delim)
  (loop for last-pos = 0 then (1+ next-pos)
        for next-pos = (position delim string :start last-pos)
        collect (subseq string last-pos next-pos)
      until (null next-pos))
  )

(defun smash (symbol number) (intern (format nil "~a-~a" symbol number)))

(defun explode-class-name (string)
  (explode-string string #\.))

(defun read-json-file (pathname)
  (let ((json:*json-symbols-package* nil))
    (json:decode-json-from-source (pathname pathname))))

(eval-when (:compile-toplevel :load-toplevel)
(define-predicate has-field (class field-name)
  (java-class-description-mixin default-predicate-model)
  ))

(defun get-class-to-field (&optional (pathname #p"rp:muse;analyses;class_to_field.json"))
  (let ((mappings (rest (assoc 'mappings (read-json-file pathname)))))
    (loop for entry across mappings
        for classes = (rest (assoc 'class entry))
        for fields = (rest (assoc 'fields entry))
        do (loop for class-name across classes
               for class-descriptor = (intern-class-descriptor class-name)
               do (loop for field across fields
                      do (tell `[has-field ,class-descriptor ,field]))))))

(eval-when (:compile-toplevel :load-toplevel)
(define-predicate field-mentions-word (field word)
  (java-class-description-mixin default-predicate-model)
  ))

(defun get-field-to-word (&optional (pathname #p"rp:muse;analyses;word_to_field.json"))
  (let ((mappings (rest (assoc 'mappings (read-json-file pathname)))))
    (loop for entry across mappings
        for words = (rest (assoc 'label entry))
        for fields = (rest (assoc 'fields entry))
        do (loop for word across words
               do (loop for field across fields
                      do (tell `[field-mentions-word ,field ,word]))))))

(eval-when (:compile-toplevel :load-toplevel)
(define-predicate java-subclass (subtype supertype)
  (java-class-description-mixin default-predicate-model)))

;;; This file also has information about which fields are present in each class
;;; and which methods are present.  This seems more complete that what is in the 
;;; class_to_field file which doesn't seem to mention any of the physics engines
;;; Maybe that should be extracted as well?
(defun get-subclass-info (&optional (pathname #p"rp:muse;analyses;class_info.json"))
  (let ((class-info (rest (assoc 'classinfo (read-json-file pathname)))))
    (loop for entry across class-info
        for super = (intern-class-descriptor (rest (assoc 'super entry)))
        for sub = (intern-class-descriptor (rest (assoc 'name entry)))
        for methods = (rest (assoc 'methods entry))
        do (loop for method across methods 
               for method-name = (rest (assoc 'methodname method))
                                 ;; note that this interns the method descriptor
               do (parse-method-descriptor method-name))
        do (tell `[java-subclass ,sub ,super]))                   
    ))

(defun class-to-fields (class)
  (let ((answer nil))
    (ask* `[has-field ,class ?field]
          (push ?field answer))
    answer))

(defun class-to-words (class)
  (ask* `[has-field ,class ?field]
        (let ((compound-name (concatenate 'string class "." ?field)))
          (ask* `[field-mentions-word ,compound-name ?word]
                (print ?word)))))

(clim-env:define-lisp-listener-command (com-retrieve-fields :name t)
    ((class 'class-descriptor))
  (clim:format-textual-list (class-to-fields class ) #'princ))

(define-object-type cluster
    :slots ((keywords :set-valued t :initarg :keywords)))

(clim:define-presentation-method clim:present (cluster (type cluster) stream (view clim:textual-view) &key)
  (princ (role-name cluster) stream))

(clim:define-presentation-method clim:accept ((type cluster) stream (view clim:textual-view) &key)
 (let* ((token (clim:read-token stream))
        (cluster (follow-path (list (intern (string-upcase token))))))
  (when cluster (return-from clim:accept cluster))
  (clim:input-not-of-required-type token type)))

(defun explode-cluster (string) (explode-string string #\;))

(eval-when (:compile-toplevel :load-toplevel)
(define-predicate in-cluster (class cluster)
  (java-class-description-mixin default-predicate-model)
  ))

;;; I'm still not sure what the cluster file is telling us
;;; particularly the labels field
(defun get-clusters (&optional (pathname #p"rp:muse;analyses;clusters.json"))
  (let ((clusters (rest (assoc 'mappings (read-json-file pathname)))))
    (loop for entry across clusters
        for cluster-name = (smash 'cluster (incf *cluster-number*))
        for labels = (aref (rest (assoc 'labels entry)) 0)
        for label-components = (loop for string in (delete "" (explode-cluster labels) :test 'string-equal)
                                   collect (intern (string-upcase string)))
        for cluster = (make-object 'cluster :keywords label-components :name cluster-name)
        for type-strings = (rest (assoc 'types entry))
        do (loop for type-string across type-strings
               for interned-type = (intern-class-descriptor type-string)
               do (tell `[in-cluster ,interned-type ,cluster])))))

(define-predicate-method (clear java-class-description-mixin) (&optional clear-database undefrule)
  (declare (ignore undefrule))
  (when clear-database
    (setq *cluster-number* 0 *package-names* nil)
    (clrhash *package-descriptor-ht*)
    (clrhash *class-descriptor-ht*)
    (clrhash *method-descriptor-ht*)))

(define-object-type package-descriptor
    :slots ((java-package-name :initarg :java-package-name)
            (java-package-full-name :initarg :java-package-full-name)
            (parent-package :initarg :parent-package :initform nil)
            (child-packages :set-valued t :initarg :child-packages :Initform nil)
            (java-classes :set-valued t :Initform nil))
    )

(defmethod print-object ((p package-descriptor) stream)
  (format stream "~a" (java-package-full-name p)))

(defun intern-package-descriptor (package-name-string)
  (let* ((last-dot-position (position #\. package-name-string :test #'char-equal :from-end t))
         (last-token (if last-dot-position
                         (subseq package-name-string (1+ last-dot-position))
                       package-name-string
                       ))
         (entry (gethash package-name-string *package-descriptor-ht*)))
    (unless entry
      (let ((name (intern (string-upcase last-token))))
        (setq entry (make-object 'package-descriptor
                                 :name package-name-string
                                 :java-package-name name
                                 :java-package-full-name package-name-string))
        (pushnew name *package-names*)      
        (setf (gethash package-name-string *package-descriptor-ht*) entry)
        (when last-dot-position
          (let* ((parent-string (subseq package-name-string 0 last-dot-position))
                 (parent (intern-package-descriptor parent-string)))
            (tell `[value-of (,parent child-packages) ,entry])
            (tell `[value-of (,entry parent-package) ,parent])))))
    entry))

(clim-env:define-lisp-listener-command (com-graph-packages :name t)
    ((package 'package-descriptor))
  (let ((stream *standard-output*))
    (terpri stream)
    (clim:format-graph-from-root 
     package
     #'(lambda (package stream)
         (present-nicely package 'package-descriptor stream))
     #'child-packages
      )))

(defun retrieve-classes-in-package (package-descriptor)
  (let ((answer nil))
    (ask* `[and [object-type-of ?class class-descriptor]
                [value-of (?class java-package)  ,package-descriptor]]
          (push ?class answer))
    answer))

(defun map-subpackages (package-descriptor function)
  (labels ((next-level (p)
             (funcall function p)
             (loop for subp in (child-packages p) 
                 do (next-level subp))))
    (next-level package-descriptor)))

(defun retrieve-classes-under-package (package-descriptor)
  (let ((answer nil))
    (flet ((do-one-package (p)
             (loop for c in (retrieve-classes-in-package p)
                 do (push c answer))))
      (map-subpackages package-descriptor #'do-one-package))
    answer))


(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate framework-has-topic (framework topic)  
    (java-class-description-mixin default-predicate-model))
  (define-predicate package-for-framework (package-descriptor framework)  
    (java-class-description-mixin default-predicate-model))
  )

(defun get-framework-info (&optional (pathname "rp:muse;analyses;framework-to-package.lisp"))
  (with-open-file (stream pathname :direction :input)
    (loop for entry = (read stream nil 'eof)
        until (eql entry 'eof)
        for (framework package-string . topics) = entry
        for package-descriptor = (intern-package-descriptor package-string)
        do (tell `[package-for-framework ,package-descriptor ,framework])
           (loop for topic in topics 
               do (tell `[framework-has-topic ,framework ,topic])))))

(clim-env:define-lisp-listener-command (com-retrieve-package :name t)
    ((token `(member ,@*package-names*) :prompt "A package name"))
  (ask* `[and [object-type-of ?package package-descriptor]
              [value-of (?package java-package-name) ,token]]
        (terpri)
        (present-nicely ?package 'package-descriptor)))

(clim:define-presentation-method clim:accept ((type package-descriptor) stream (view clim:textual-view) &rest args)
  (clim:completing-from-suggestions (stream  :partial-completers '(#\-))
                                    (maphash #'(lambda (key value)
                                                 (clim:suggest key value))
                                              *package-descriptor-ht*)))

(clim-env:define-lisp-listener-command (com-retrieve-subpackages :name t)
    ((p 'package-descriptor))
  (loop for sub-p in (child-packages p)
      do (terpri)
         (present-nicely sub-p 'package-descriptor)))

(defun is-subpackage (sub parent)
  (loop for next-parent = (parent-package sub) then (parent-package next-parent)
      while next-parent
      when (eql next-parent parent) return t))


(define-object-type class-descriptor
    :slots ((java-class-name :initarg :java-class-name)
            (java-package :initarg :java-package)
            (java-methods :initform nil :set-valued t))
    )

;;; This now prints as #c< ... the descriptor ... >
(defmethod print-object ((c class-descriptor) stream)
  (when *print-readably* (format stream "#c<"))
  (format stream "~@[~a.~]~a"
          (java-package c)
          (java-class-name c))
  (when *print-readably* (format stream ">"))
  )

;;; Which is then read back by this.
;;; Really should peek char to make sure it's a < and > pair
(defun read-class-descriptor (stream c n)
  (declare (ignore c n))
  (let* ((separators (list #\space #\>)))
    (read-char stream)
    (let ((token (clim:with-delimiter-gestures (separators :override t) (clim:read-token stream))))
      (read-char stream)
      (intern-class-descriptor token))))

(set-dispatch-macro-character #\# #\c #'read-class-descriptor ji::*joshua-readtable*)

(clim:define-presentation-method clim:accept ((type class-descriptor) stream (view clim:textual-view) &rest args)
  (let* ((separators (list #\space))
         (token (clim:with-delimiter-gestures (separators :override t)
                  (clim:read-token stream))))
    (intern-class-descriptor token)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate class-has-name (descriptor name)
    (java-class-description-mixin default-predicate-model)))

(defun intern-class-descriptor (class-name-string)
  (let* ((last-dot (position #\. class-name-string :from-end t :test #'char-equal))
         (class-name (if last-dot (subseq class-name-string (1+ last-dot)) class-name-string))
         (package-name (if last-dot (subseq class-name-string 0 last-dot) nil))
         (package (when package-name (intern-package-descriptor package-name)))
         (class-descriptor (gethash (list class-name package) *class-descriptor-ht*)))
    (unless class-descriptor
      (setq class-descriptor (make-object 'class-descriptor
                               :name (concatenate 'string class-name "-" package-name)
                               :java-class-name class-name
                               :java-package package))
      (setf (gethash (list class-name package) *class-descriptor-ht* ) class-descriptor)
      (when package (tell `[value-of (,package java-classes) ,class-descriptor]))
      (tell `[class-has-name ,class-descriptor ,(intern class-name)])
      (loop for token in (explode-string (json:simplified-camel-case-to-lisp class-name) #\-)
          do (tell `[class-has-name ,class-descriptor ,(intern token)]))
      )
    class-descriptor))

;;; For example:
;;; "<org.dyn4j.dynamics.joint.PrismaticJoint: double getJointSpeed()>"
(define-object-type method-descriptor
    :slots ((java-method-name :initarg :java-method-name)
            ;; a list of class-descriptors
            (argument-types :initarg :argument-types)
            ;; a class-descriptor
            (return-type :initarg :return-type)
            ;; a class-descriptor
            (java-class :initarg :java-class)))

(defmethod print-object ((m method-descriptor) stream)
  (format stream "<~a: ~a ~a~a>"
          (java-class m) 
          (return-type m) (java-method-name m) (argument-types m)))

(defun intern-method-descriptor (method-name argument-type-strings class-name return-type-string)
  (let* ((class-descriptor (intern-class-descriptor class-name))
         (return-type (intern-class-descriptor return-type-string))
         (argument-types (loop for arg-type in argument-type-strings collect (intern-class-descriptor arg-type)))
         (entry (gethash (list method-name argument-types class-descriptor return-type) *method-descriptor-ht*)))
    (unless entry
      (setq entry (make-object 'method-descriptor
                               :name (concatenate 'string method-name "-" class-name)
                               :java-class class-descriptor
                               :return-type return-type
                               :java-method-name method-name
                               :argument-types argument-types))
      (tell `[value-of (,class-descriptor java-methods) ,entry])
      (setf (gethash (list method-name argument-types class-descriptor return-type) *method-descriptor-ht* ) entry))
    entry))

(clim:define-presentation-method clim:accept ((type method-descriptor) stream (view clim:textual-view) &rest args)
  (let* ((separators (list #\>))
         (token (clim:with-delimiter-gestures (separators :override t)
                  (clim:read-token stream))))
    (parse-method-descriptor token)))

(defun parse-method-descriptor (string)
  (flet ((not-whitespace (char) (not (clim-utils:whitespace-char-p char)))
         (comma-or-close-paren (char) (member char '(#\, #\)))))
    (let* ((colon-position (position #\: string :test #'char-equal))
           (class-name (subseq string 1 colon-position))
           (next-non-whitespace (position-if #'not-whitespace string :start (1+ colon-position)))
           (whitespace-after-that (position-if #'clim-utils:whitespace-char-p string :start next-non-whitespace))
           (return-type (subseq string next-non-whitespace whitespace-after-that))
           (method-name-begin (position-if #'not-whitespace string :start whitespace-after-that))
           (method-name-end (position #\(  string :start method-name-begin))
           (method-name (subseq string method-name-begin method-name-end))
           (argument-types (loop for start-pos = (1+ method-name-end) then (1+ next-pos)
                               for next-pos = (position-if #'comma-or-close-paren string :start start-pos)
                               collect (subseq string start-pos next-pos) into args
                               when (char-equal (aref string next-pos) #\))
                               return args)))                                                      
      (intern-method-descriptor method-name argument-types class-name return-type))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate method-similarity (method-1-descriptor method-2-descriptor degree-of-similarity)
    (java-class-description-mixin default-predicate-model)
    ))

(defun present-nicely (thing presentation-type &optional (stream *standard-output*))
  (clim:with-output-as-presentation (stream thing 'clim:expression
                                            :single-box t
                                            :allow-sensitive-inferiors t)
    (clim:present thing presentation-type :stream stream)))

(clim-env:define-lisp-listener-command (com-process-analyses :name t)
    (&key (clean-database-first? 'boolean :default t))
  (when clean-database-first? (clear))
  (format t "~%Getting framework info")
  (get-framework-info)
  (format t "~%Getting sub-class info")
  (get-subclass-info)
  (format t "~%Getting cluster info")
  (get-clusters)
  (format t "~%Getting class to field info")
  (get-class-to-field)
  (format t "~%Getting class to work info")
  (get-field-to-word)
  (format t "~%Getting similarity info")
  (process-similarity-files)
  (values))

(defun process-similarity-files (&optional (directory #p"rp:muse;analyses;method-similarity;*.json"))
  (loop for pathname in (directory directory)
      do (make-similarity-assertions pathname)))

(defparameter *similarity-threshhold* .25)

(defun make-similarity-assertions (&optional (pathname #p"rp:muse;analyses;react_result.json"))
  (let ((json (read-similarity-file pathname)))
    (loop for (method-descriptor1 . vector) in json
        do (loop for entry across vector
               do (loop for (method-descriptor2 . similarity) in entry
                      when (>= similarity *similarity-threshhold*)
                      do (tell `[method-similarity ,method-descriptor1 ,method-descriptor2 ,similarity]
                               :justification `((:premise ,similarity))
                               ))))))

(defun read-similarity-file (&optional (pathname #p"rp:muse;analyses;react_result.json"))
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'parse-method-descriptor))
    (json:decode-json-from-source (pathname pathname))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun get-all-frameworks () 
    (let ((answer nil))
      (ask* [package-for-framework ?pd ?framwork] (pushnew ?framwork answer))
      answer)))

(defun get-package-of-framework (framework) 
  (ask* `[package-for-framework ?pd ,framework]
        (return-from get-package-of-framework ?pd)))

(defun get-framework-of-package (package)
  (ask* `[package-for-framework ,package ?framework]
        (return-from get-framework-of-package ?framework)))

(defun get-framework-of-class (class)
  (get-framework-of-package (java-package class)))

(defun get-frameworks-for-topic (topic)
  (let ((answer nil))
    (ask* `[framework-has-topic ?framework ,topic]
          (pushnew ?framework answer))
    (sort answer #'string-lessp)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun get-all-topics ()
    (let ((answer nil))
      (ask* `[framework-has-topic ?framework ?topic]
            (pushnew ?topic answer))
      answer)))

(clim:define-presentation-type-abbreviation framework () `(member ,@(get-all-frameworks)))
(clim:define-presentation-type-abbreviation topic () `(member ,@(get-all-topics)))

(clim-env:define-lisp-listener-command (com-retrieve-classes :name t)
    (&key (package 'package-descriptor)
          (method-name 'string)
          (name 'symbol)
          (topic 'topic)
          (framework `framework))
  (let ((classes nil) (something-done nil))
    (when topic
      (loop for framework in (get-frameworks-for-topic topic)
          for his-classes = (retrieve-classes-under-package (get-package-of-framework framework))
          do (setq classes (union classes his-classes)))
      (setq something-done t))
    (when framework
      (if something-done
          (setq classes (intersection classes (retrieve-classes-under-package (get-package-of-framework framework))))
        (setq classes (retrieve-classes-under-package (get-package-of-framework framework)) something-done t))
        )
    (when package 
      (if something-done
          (setq classes (intersection classes (retrieve-classes-with-package-name package)))
        (setq classes (retrieve-classes-with-package-name package) something-done t)))
    (when method-name
      (if something-done
          (setq classes (intersection classes (retrieve-classes-with-method-name method-name)))
        (setq classes (retrieve-classes-with-method-name method-name) something-done t)))
    (when name
      (if something-done
          (setq classes (intersection classes (retrieve-classes-with-class-name name)))
        (setq classes (retrieve-classes-with-class-name name))))
    (loop for class in (sort classes #'string-lessp :key #'java-class-name)
        do (terpri *standard-output*)
           (present-nicely class 'class-descriptor *standard-output*)))
  )

(clim:define-gesture-name :show-class :pointer-button (:left :super))

(clim:define-presentation-to-command-translator package-to-class
    (package-descriptor com-retrieve-classes clim-env::lisp-listener
                       :gesture :show-class
                       :echo nil)
  (object)
  (list :package object))

(clim-env:define-lisp-listener-command (com-retrieve-class-of-method)
    ((method 'method-descriptor))
  (terpri *standard-output*)
  (present-nicely (java-class method) 'class-descriptor))

(clim:define-presentation-to-command-translator method-to-class
    (method-descriptor  com-retrieve-class-of-method clim-env::lisp-listener
                       :gesture :show-class
                       :echo nil)
  (object)
  (list object))

(clim-env:define-lisp-listener-command (com-retrieve-superclass :name t)
    ((class 'class-descriptor))
  (ask* `[java-subclass ,class ?parent]
        (present-nicely ?parent 'class-descriptor *standard-output*)))

(clim-env:define-lisp-listener-command (com-retrieve-subclasses :name t)
    ((class 'class-descriptor))
  (let ((answer nil))
    (ask* `[java-subclass ?child ,class] (push ?child answer))
    (loop for child in (sort answer #'string-lessp :key #'java-class-name)
        do (present-nicely child 'class-descriptor *standard-output*)
           (terpri *standard-output*))))
  
(clim-env:define-lisp-listener-command (com-retrieve-cluster :name t)
    (&key (class 'class-descriptor)
          (name 'symbol))
  (let ((answer (cond (class (retrieve-classes-in-same-cluster class))
                      (name (retrieve-cluster name))))
        (stream *standard-output*))
    (terpri stream)
    (if answer
        (clim:formatting-table (stream)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream) (write-string "Cluster" stream))
            (clim:formatting-cell (stream) (write-string "Keywords" stream))
            (clim:formatting-cell (stream) (write-string "Class" stream)))
          (loop for (cluster classes) in answer
              do (loop for class in classes
                     for first = t then nil
                     do (clim:formatting-row (stream)
                          (clim:formatting-cell (stream) 
                            (when first (clim:present cluster 'cluster :stream stream)))
                          (clim:formatting-cell (stream) 
                            (when first
                              (clim:format-textual-list (keywords cluster) #'princ :stream stream)))
                          (clim:formatting-cell (stream) (present-nicely class 'class-descriptor stream))))))
      (format stream "~%No applicable clusters~%"))))

(defun retrieve-cluster (keyword)
  (let ((answer nil))
    (ask* `[and [object-type-of ?cluster cluster]
                [value-of (?cluster keywords) ,keyword]
                [in-cluster ?class-descriptor ?cluster]]
          (let ((entry (assoc ?cluster answer)))
            (Unless entry
                    (setq entry (list ?cluster nil))
                    (push entry answer))
          (pushnew ?class-descriptor (second entry))))
    answer))

(defun retrieve-classes-in-same-cluster (class-descriptor)
  (let ((answer nil))
    (ask* `[in-cluster ,class-descriptor ?cluster]
          (let ((entry (assoc ?cluster answer)))
            (Unless entry
                    (setq entry (list ?cluster nil))
                    (push entry answer))
          (ask* [in-cluster ?other-class ?cluster]
                (pushnew ?other-class (second entry)))))
    answer))

(defun retrieve-classes-with-class-name (name)
  (let ((answer nil)) 
   (ask* `[class-has-name ?descriptor ,name]
          (push ?descriptor answer))
    answer
  ))

(defun retrieve-classes-with-method-name (name)
  (let ((answer nil))  
    (ask* `[and [object-type-of ?x method-descriptor]
                [value-of (?x java-method-name) ,name]
                [value-of (?x java-class) ?class]
                ]
          (push ?class answer))
    answer))

(clim-env:define-lisp-listener-command (com-retrieve-methods :name t)
    (&key (method-name 'string)
          (class 'class-descriptor)
          )
  (let ((methods (if method-name
                     (retrieve-methods-with-name method-name)
                   (retrieve-methods-of-class class))))
    (setq methods (sort methods #'string-lessp :key #'java-method-name))
    (loop for method in (sort methods #'string-lessp :key #'java-method-name)
        do (terpri *standard-output*)
           (present-compactly method *standard-output*))))

(clim:define-gesture-name :show-methods :pointer-button (:left :super))

(clim:define-presentation-to-command-translator click-to-retrieve-method
    (class-descriptor com-retrieve-methods clim-env::lisp-listener
                      :gesture :show-methods)
  (object)
  (list :class object))

(defun retrieve-methods-with-name (name)
  (let ((answer nil))  
    (ask* `[and [object-type-of ?x method-descriptor]
                [value-of (?x java-method-name) ,name]]
          (push ?x answer))
    answer))

(defun retrieve-methods-of-class (class-descriptor)
  (java-methods class-descriptor))

(defun retrieve-classes-with-package-name (package)
  (let ((answer nil))    
    (ask* `[and [object-type-of ?x class-descriptor]
                [value-of (?x java-package) ,package]]
          (push ?x answer))
    answer))

(clim-env:define-lisp-listener-command (com-retrieve-similar-methods :name t)
    ((method 'method-descriptor))
  (let ((stream *standard-output*)
        (methods (retrieve-similar-methods method)))
    (cond
     (methods
      (terpri stream)
      (write-string "Methods Similar to " stream)
      (terpri stream)
      (present-compactly method stream)
      (write-char #\: stream)
      (terpri stream)
      (terpri stream)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream) (write-string "Method" stream))
          (clim:formatting-cell (stream) (write-string "Strength" stream)))
        (loop for (method strength) in methods
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream)
                   (present-compactly method stream))
                 (clim:formatting-cell (stream)
                   (format stream "~7,3f" strength))
                 ))))
     (t
      (format stream "~%No similar methods")))
      ))

(defmethod present-compactly ((m method-descriptor) &optional (stream *standard-output*))
  (let* ((class (java-class m))
         (class-name (java-class-name class))
         (return-type (return-type m))
         (return-type-name (java-class-name return-type))
         (method-name (java-method-name m))
         (argument-types (argument-types m)))
  (clim:with-output-as-presentation (stream m 'method-descriptor)
    (clim:with-output-as-presentation (stream class 'class-descriptor)
      (write-string class-name stream))
    (write-char #\. stream)
    (write-string method-name stream)
    (write-char #\( stream)
    (loop for argument-type in argument-types
        for first = t then nil
        for argument-name = (java-class-name argument-type)
        when (not first) do (write-string ", " stream)
        do (clim:with-output-as-presentation (stream argument-type 'class-descriptor)
             (write-string argument-name stream)))
    (write-string ") -> " stream)
    (clim:with-output-as-presentation (stream return-type 'class-descriptor)
      (write-string return-type-name stream)))))

(clim:define-gesture-name :show-similar :pointer-button (:left :meta))

(clim:define-presentation-to-command-translator method-to-retrieve-similar-methods
    (method-descriptor com-retrieve-similar-methods clim-env::lisp-listener
                       :gesture :show-similar
                       :echo nil)
  (object)
  (list object))

(defun retrieve-similar-methods (name)
  (let ((answer nil))
    (ask* `[method-similarity ,name ?who ?how-much]
          (push (list ?who ?how-much) answer))
    (ask* `[method-similarity ?who ,name ?how-much]
          (push (list ?who ?how-much) answer))
    (setq answer (sort answer #'> :key #'second))
    answer))

(clim-env:define-lisp-listener-command (com-show-method-similarities :name t)
    (&key (how-many '(integer 0) :default 100) (starting-at '(integer 0) :default 0)
          (threshold '(float 0 1) :default 0)
          (topic 'topic))
  (let ((stream *standard-output*)
        (queue (retrieve-all-similarities topic threshold)))
    (terpri stream)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream) (write-string "Method 1" stream))
        (clim:formatting-cell (stream) (write-string "Method 2" stream))
        (clim:formatting-cell (stream) (write-string "Similarity" stream)))
      (loop for i below starting-at do (heap-remove queue))
      (loop for i below how-many
          for entry = (heap-remove queue)
          until (null entry)
          for (m1 m2 s) = entry
          do (clim:formatting-row (stream)
               (clim:formatting-cell (stream) (present-compactly m1 stream))
               (clim:formatting-cell (stream) (present-compactly m2 stream))
               (clim:formatting-cell (stream) (format stream "~7,5f" s)))))
    (terpri stream)
    (values)))

(defun retrieve-all-similarities (&optional topic threshold)
  (let ((queue (make-heap :predicate #'>))
        (ht (make-hash-table :test #'equal))
        (frameworks (when topic (get-frameworks-for-topic topic))))
    (flet ((method-is-in-frameworks (method-descriptor)
             (or (null frameworks)
                 (let* ((class-descriptor (java-class method-descriptor))
                        (package-descriptor (java-package class-descriptor))
                        (framework (get-framework-of-package package-descriptor)))
                   (member framework frameworks)))))
      (ask* `[method-similarity ?who1 ?who2 ?how-much]
            (when (or (null threshold) (> ?how-much threshold))
              (when (or (method-is-in-frameworks ?who1)
                        (method-is-in-frameworks ?who2))
                (unless (gethash (list ?who2 ?who1) ht)
                  (setf (gethash (list ?who2 ?who1) ht) t)                
                  (heap-insert queue (list ?who1 ?who2 ?how-much) ?how-much))))))
    queue))

(clim-env:define-lisp-listener-command (com-retrieve-framework :name t)
    (&key (class 'class-descriptor) (method 'method-descriptor))
  (let ((framework (cond
                    (class (let* ((package (java-package class))
                                  (framework (get-framework-of-package package)))
                             framework))
                    (method (let* ((class (java-class method))
                                   (package (java-package class))
                                   (framework (get-framework-of-package package)))
                              framework)))))
    (if framework
        (clim:present framework 'framework)
      (format t "~%No known framework"))))


(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate class-in-framework (class framework)
    (java-class-description-mixin default-predicate-model)))

(defrule class-framework (:backward)
  then [class-in-framework ?class ?framework]
  if [and [package-for-framework ?p ?framework]
          [object-type-of ?p package-descriptor]
          [object-type-of ?class class-descriptor]
          [value-of (?class java-package) ?p]
          ])

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate classes-share-name (c1 c2 name)
    (java-class-description-mixin default-predicate-model))
  (define-predicate classes-share-method (c1 c2 method1 method2)
    (java-class-description-mixin default-predicate-model))
  (define-predicate classes-share-cluster (c1 c2 cluster)
    (java-class-description-mixin default-predicate-model))
  )

(defparameter *not-useful-words* 
    (loop for name in '(shape abstract scene collision convex mesh data mass type feature)
        collect name
        collect (intern (string-capitalize (string name)))))

(defun make-framework-assertions (answers)
  (loop for (name . alist) in answers
      do (loop for (f1 classes1) = (first alist) then (pop alist)
             until (null f1)
             do (loop for c1 in classes1
                    do (loop for (nil classes2) in (rest alist)
                           do (loop for c2 in classes2
                                  do (tell `[classes-share-name ,c1 ,c2 ,name])
                                     (check-method-similarity c1 c2)
                                     (check-shared-cluster c1 c2)
                                     ))))))

(defun check-method-similarity (class1 class2)
  (ask* `[value-of (,class1 java-methods) ?method]
        (ask `[method-similarity ?method ?other-method ?certainty]
             #'(lambda (just)
                 (let ((pred (ask-database-predication just)))
                   (ask* `[value-of (?other-method java-class) ,class2]
                         (tell `[classes-share-method ,class1 ,class2 ?method ?other-method]
                               :justification `((method-similarity ,?certainty) (,pred))))))))
  )

(defun check-shared-cluster (c1 c2)
  (ask `[in-cluster ,c1 ?cluster]
       #'(lambda (just1)
           (ask `[in-cluster ,c2 ?cluster]
                 #'(lambda (just2)
                     (tell `[classes-share-cluster ,c1 ,c2 ?cluster]
                           :justification `(shared-cluster (,(ask-database-predication just1)
                                                            ,(ask-database-predication just2)))))))))

(defun map-frameworks (topic)
  (let ((frameworks (get-frameworks-for-topic topic))
        (name-to-classes (make-hash-table)))
    (loop for framework in frameworks
        for package = (get-package-of-framework framework)
        do (loop for class in (java-classes package)
               do (ask* `[class-has-name ,class ?name]
                        (unless (member ?name *not-useful-words*)
                          (let* ((name-entry (gethash ?name name-to-classes))
                                 (framework-entry (assoc framework name-entry)))
                            (unless framework-entry 
                              (setq framework-entry (list framework nil))
                              (push framework-entry name-entry)
                              (setf (gethash ?name name-to-classes) name-entry))
                            (push class (second framework-entry)))))))
    (values (sort frameworks #'string-lessp)
            (sort 
             (loop for entry being the hash-values of name-to-classes using (hash-key name)
                 when (rest entry) 
                 collect (cons name (sort entry #'string-lessp :key #'first)))
             #'string-lessp
              :key #'first
              ))))

(clim-env:define-lisp-listener-command (com-map-frameworks :name t)
    ((topic 'topic))
  (multiple-value-bind (frameworks answers) (map-frameworks topic)
    (make-framework-assertions answers)
    (let ((stream *standard-output*))
      (terpri stream)
      (clim:formatting-table (stream :move-cursor t :record-type 'clim:ruled-table-output-record)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream) (format stream "Shared~%Name"))
          (loop for framework in frameworks
              do (clim:formatting-cell (stream) 
                   (princ framework stream)
                   (terpri stream)
                   (present-nicely (get-package-of-framework framework) 'package-descriptor stream))))
        (loop for (name . alist) in answers
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream) (present-nicely name 'symbol stream))
                 (loop for framework in frameworks
                     for his-entry = (second (assoc framework alist))
                     do (clim:formatting-cell (stream) 
                          (loop for class in his-entry
                              for first = t then nil
                              do (when (not first) (terpri stream))
                                 (clim:with-output-as-presentation (stream class 'class-descriptor)
                                   (write-string (java-class-name class) stream)))))))
        ))))
  
;;; We will canonicalize this by always have class-1 be in a frawework that
;;; alphabetically before that of the framework of class-2
(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate same-concept (class-1 class-2) (cf-mixin default-predicate-model)))

(defun class-ordered-p (c1 c2)
  (let ((class-name-1 (java-class-name c1))
        (class-name-2 (java-class-name c2)))
    (if (string-equal class-name-1 class-name-2)
        (string-lessp (string (get-framework-of-package (java-package c1))) 
                      (string (get-framework-of-package (java-package c2))))
      (string-lessp class-name-1 class-name-2))))

(define-predicate-method (insert same-concept :around) ()
  (with-statement-destructured (c1 c2) self
    (if (class-ordered-p c1 c2)
        (call-next-method)
      (call-next-method `[same-concept ,c2 ,c1]))))

(defrule same-name-implies-same-concept (:forward :certainty .5)
  :if [and [classes-share-name ?c1 ?c2 ?name] (not (eql ?c1 ?c2))]
  :then [same-concept ?c1 ?c2]
  )

(defrule same-method-implies-same-concept (:forward :certainty .7)
  :if [and [classes-share-method ?c1 ?c2 ?m1 ?m2] (not (eql ?c2 ?c2))]
  :then [same-concept ?c1 ?c2]
  )

(defrule superclass-similarity-from-subclasses (:forward :certainty .4)
  :if [and [same-concept ?c1 ?c2]
           [java-subclass ?c1 ?c1-super]
           [java-subclass ?c2 ?c2-super]]
  :then [same-concept ?c1-super ?c2-super]
  )

(defrule share-clusters-implies-same-concept (:forward :certainty .8)
  :if [and [classes-share-cluster ?c1 ?c2 ?cluster] (not (eql ?c2 ?c2))]
  :then [same-concept ?c1 ?c2]
  )

;;; Need a rule saying that if something is a sub-class of something else
;;; then it's negative evidence for similarity
(defun cluster-concepts (topic &optional (min-certainty .4))
  (let ((counter 0)
        (mapping (make-hash-table))
        (reverse-mapping (make-hash-table)))
    (map-frameworks topic)
    (flet ((make-new-cluster () (intern (format nil "CONCEPT-~d" (incf counter)))))
      (ask [same-concept ?c1 ?c2]
           #'(lambda (just)
               (when (> (certainty-factor (ask-database-predication just)) min-certainty)
                 (let ((c1-cluster (gethash ?c1 mapping))
                       (c2-cluster (gethash ?c2 mapping)))
                   (cond
                    ((and c1-cluster c2-cluster)
                     (unless (eql c1-cluster c2-cluster)
                       ;; Merge c2 into c1, first forward c2 to c1 in forward map
                       (setf (gethash ?c2 mapping) c1-cluster)
                       ;; next set c1-cluster to the union of the two
                       (setf (gethash c1-cluster reverse-mapping) (union (gethash c1-cluster reverse-mapping)
                                                                         (gethash c2-cluster reverse-mapping)))
                       ;; and remove the c2-cluster from the reverse-mapping
                       (remhash c2-cluster reverse-mapping)
                       ))
                    (c1-cluster
                     (setf (gethash ?c2 mapping) c1-cluster)
                     (Pushnew ?c2 (gethash c1-cluster reverse-mapping)))
                    (c2-cluster
                     (setf (gethash ?c1 mapping) c2-cluster)
                     (pushnew ?c1 (gethash c2-cluster reverse-mapping)))
                    (t (setq c1-cluster (make-new-cluster))
                       (setf (gethash ?c1 mapping) c1-cluster)
                       (pushnew ?c1 (gethash c1-cluster reverse-mapping))
                       (setf (gethash ?c2 mapping) c1-cluster) 
                       (Pushnew ?c2 (gethash c1-cluster reverse-mapping)))))))))
    (values reverse-mapping mapping)))

(defun display-concept-cluster (topic &optional (min-certainty .4) (stream *standard-output*))
  (let ((mapping (cluster-concepts min-certainty))
        (frameworks (get-frameworks-for-topic topic)))
    (terpri stream)
    (clim:formatting-table (stream :move-cursor t :record-type 'clim:ruled-table-output-record)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream) (write-string "Framework" stream))
        (loop for framework in frameworks
            do (clim:formatting-cell (stream) (clim:present framework 'framework :stream stream))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream) (write-string "Package" stream))
        (loop for framework in frameworks
            do (clim:formatting-cell (stream) 
                 (present-nicely (get-package-of-framework framework) 'package-descriptor stream))))
      (loop for entry being the hash-values of mapping
          when (loop for class in entry when (member (get-framework-of-class class) frameworks) return t)
          do (clim:formatting-row (stream)
               (clim:formatting-cell (stream) (write-string "" stream))
               (loop for framework in frameworks
                   do (clim:formatting-cell (stream)
                        (loop for class in entry
                            for first = t then nil
                            when (eql framework (get-framework-of-class class))
                            do (unless first
                                 (terpri stream))
                               (clim:with-output-as-presentation (stream class 'class-descriptor)
                                 (write-string (string (java-class-name class)) stream))))))))
    (values)))
                                            
(clim-env:define-lisp-listener-command (com-show-concept-clusters :name t)
    ((topic 'topic)
     &key (minimum-certainty '(float 0 1) :default .4)
          (to-file 'clim:pathname))
  (map-frameworks topic)
  (if to-file
      (with-open-file (f to-file :direction :output :if-does-not-exist :create :if-exists :supersede)
        (clim:with-output-to-postscript-stream (stream f)
          (display-concept-cluster topic minimum-certainty stream)))
    (display-concept-cluster topic minimum-certainty)))

(defun collect-same-concepts (topic &optional (min-certainty .4))
  (let ((frameworks (get-frameworks-for-topic topic))
        (answers nil))
    (ask [same-concept ?c1 ?c2]
         #'(lambda (just)
             (let ((certainty (certainty-factor (ask-database-predication just))))
               (when (and (> certainty min-certainty)
                          (member (get-framework-of-class ?c1) frameworks)
                          (member (get-framework-of-class ?c2) frameworks))
                 (push (list ?c1 ?c2 certainty) answers)))))
    (setq answers (sort answers #'> :key #'third))
    answers))

(clim-env:define-lisp-listener-command (com-show-similar-classes :name t)
    ((topic 'topic)
     &key (minimum-certainty '(float 0 1) :default .4)
     (to-file 'clim:pathname))
  (map-frameworks topic)
  (flet ((display-similarities (stream)
           (clim:formatting-table (stream :move-cursor t :record-type 'clim:ruled-table-output-record)
             (loop for (c1 c2 certainty) in (collect-same-concepts topic minimum-certainty)
                 do (clim:formatting-row (stream)
                      (clim:formatting-cell (stream)
                         (clim:with-output-as-presentation (stream c1 'class-descriptor)
                           (write-string (string (java-class-name c1)) stream)))
                      (clim:formatting-cell (stream)
                         (clim:with-output-as-presentation (stream c2 'class-descriptor)
                           (write-string (string (java-class-name c2)) stream)))
                      (clim:formatting-cell (stream)
                        (princ certainty stream)))))))                                        
  (if to-file
      (with-open-file (f to-file :direction :output :if-does-not-exist :create :if-exists :supersede)
        (clim:with-output-to-postscript-stream (stream f)
          (display-similarities stream)))
    (display-similarities *standard-output*))))