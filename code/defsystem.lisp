;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

(load "~/quicklisp/setup.lisp")

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (code-dir (pathname-directory loading-file))
	 (home-dir (butlast code-dir))
	 (wild-dir (append home-dir (list :wild-inferiors))))
    (let ((home-directory (make-pathname :directory home-dir
					 :host host
					 :device device))
	  (code-directory (make-pathname :directory code-dir
					 :host host
					 :device device))
	  (wild-directory (make-pathname :directory wild-dir
					 :host host 
					 :device device
					 :type :wild
					 :name :wild
					 :version :unspecific)))
      (setf (logical-pathname-translations "natsoft")
	`(("code;*.*" ,code-directory)
	  ("home;*.*" ,home-directory)
	  ("**;*.*"   ,wild-directory)
	  ))
      (with-open-file (F #P"natsoft:home;my-logical-pathnames.lisp" :direction :output :if-exists :supersede)
	(format f "~%;;; natsoft")
	(format f "~2%~s" "Natsoft")
	(loop for (a b) in (logical-pathname-translations "natsoft")
	  do (format f "~%'(~s ~s)" (namestring a) (namestring b)))
	(terpri f)
	)      
      (pushnew (namestring (truename #P"natsoft:home;my-logical-pathnames.lisp"))
	       (logical-pathname-translations-database-pathnames)
	       :test #'string-equal)
      )))

(defsystem start-interface
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   ("package-definition" (:module-class separate-destination-module)) 
   ("http-interface" (:module-class separate-destination-module)) 
   ("parse-interface" (:module-class separate-destination-module))))

(defsystem core-natsoft
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   start-interface
   ("utilities" (:module-class separate-destination-module))
   ("predicate-definitions" (:module-class separate-destination-joshua-module))
   ("basic-types" (:module-class separate-destination-joshua-module))
   ("task-descriptions" (:module-class separate-destination-joshua-module))
   ("refinement" (:module-class separate-destination-joshua-module))
   ("reductions-and-cliches" (:module-class separate-destination-joshua-module))
   ("canonical-arguments" (:module-class separate-destination-joshua-module))
   ("codegen" (:module-class separate-destination-joshua-module))
   ("propagation-stage" (:module-class separate-destination-joshua-module))
   ("sort-stage" (:module-class separate-destination-joshua-module))
   ("topological-sort" (:module-class separate-destination-module))
   ("generation-stage" (:module-class separate-destination-joshua-module))
   ("java-pretty-printer" (:module-class separate-destination-joshua-module))
   ("core-facts" (:module-class separate-destination-joshua-module))
   ("learned-stuff" (:module-class data-module))
   ))

(defsystem UI-natsoft
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   ("command-or-form" (:module-class separate-destination-module))
   ("design-editor" (:module-class separate-destination-joshua-module))
   ("display-code" (:module-class separate-destination-joshua-module))
   ("dump-buffer" (:module-class separate-destination-joshua-module))
   ("ui-support" (:module-class separate-destination-joshua-module))
   ("physics-ontology" (:module-class separate-destination-joshua-module))
   ("image-processing-ontology" (:module-class separate-destination-joshua-module))
   ("read-analysis-file" (:module-class separate-destination-joshua-module))
   ))

(defsystem natsoft
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   start-interface 
   core-natsoft
   UI-natsoft
   ))