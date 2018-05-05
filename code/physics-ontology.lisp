;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

; box1 = new SimulationBody()		
; box1.addFixture(Geometry.createRectangle(0.2, 0.2)) 
; box1.translate(0.0, 0.0)		
; box1.setLinearVelocity(0.5, 0.0)	
; box1.setMass(MassType.NORMAL)		
; this.world.addBody(box1)		


(defdata-type time
    :super-types (number)
    )

(Defdata-type linear-dimension
    :super-types (number)
    )

(defdata-type mass
    :super-types (Number)
    )

(defdata-type location
    :super-types (data-structure)
    :parameters ((x linear-dimension)
                 (y linear-dimension))
    :other-assertions ([allocation-code location (make-instance 'location) :lisp])
    )

(defdata-type linear-velocity
    :super-types (number)
    )

(defdata-type velocity
    :super-types (data-structure)
    :parameters ((x linear-velocity)
                 (y linear-velocity))
    :other-assertions ([allocation-code velocity (make-instance 'velocity) :lisp])
    )

(defdata-type shape
    :super-types (data-structure)
    )

(defdata-type rectangle
    :super-types (shape)
    :other-assertions ([allocation-code rectangle (make-instance 'rectangle) :lisp])
    )

(defdata-type simulation-object 
    :super-types (data-structure)
    :parts ((shape shape)
	    (mass number)
	    (velocity velocity)
	    (location location))
    :other-assertions ([allocation-code simulation-object (make-instance 'simulation-object) :lisp]
                       [allocation-code simulation-object (:allocate simulation-object) :java]
                       )
    )

(defdata-type simulation-world
    :super-types (data-structure)
    :parts ((objects (set simulation-object))
	    (gravity number))
    :other-assertions ([allocation-code simulation-world (make-instance 'simulation-world) :lisp]
                       [allocation-code simulation-world (:allocate simulation-world) :java]
                       )
    )

(defdata-type simulation-environment
    :super-types (data-structure)
    :parts ((world simulation-world)
	    (time-step time)
	    (focus location))
    )

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate framework-mapping (ontic-name framework framework-name)))

(deftask add-shape
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (the-object simulation-object)
			 (the-shape shape)))
    :other-assertions ([framework-mapping add-shape dyn4j add-fixture]))

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'add-shape)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(deftask set-velocity
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (the-object simulation-object)
			 (the-velocity velocity)))
    :other-assertions ([framework-mapping set-velocity dyn4j set-linear-velocity]))

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'set-velocity)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(deftask set-location
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (the-object simulation-object)
                         (the-location location)))
    :other-assertions ([framework-mapping set-location dyn4j translate]
		       [framework-mapping set-location jmonkey set-local-translation])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'set-location)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(deftask set-mass
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (the-object simulation-object)
                         (the-mass mass)))
    :other-assertions ([framework-mapping set-mass dyn4j set-mass])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'set-mass)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(deftask add-object
    :parameters (framework)
    :dont-expand t
    :interface ((:inputs (the-world simulation-world)
                         (the-object simulation-object))
                )
    :other-assertions ([framework-mapping add-object dyn4j add-body]
                       [framework-mapping add-object jmonkey attach-child])
    )

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'add-object)) (language (eql :java)))
  (framework-version-of-method task-type (getf (properties task) :framework))
  )

(defun framework-version-of-method (ontic-name framework)
  (let ((answer ontic-name))
    (ask* `[framework-mapping ,ontic-name ,framework ?framework-name]
          (setq answer ?framework-name))
    answer))


(deftask add-object-to-world
    :parameters (framework)
    :interface ((:inputs (location location)
                         (velocity velocity)
                         (mass mass)
                         (shape shape)
                         (the-world simulation-world)
                         (the-object simulation-object)))
    )


(defmethod canonical-arguments-for ((type (eql 'add-object-to-world)) the-thing)
  (let ((framework nil))
    (ask* `[property-value-of ,the-thing framework ?answer]
	  (setq framework ?answer))
  (list framework)))

(defreduction add-object (add-object-to-world (?framework))
  :reduce-to (add-object-to-world ?framework))

(defcliche add-object-to-world (?framework)
  :initial-inputs (
		   (:name LOCATION :type LOCATION)
		   (:name VELOCITY :type VELOCITY)
                   (:name MASS :type MASS)
		   (:name Shape :type shape)
                   (:name the-world :type simulation-world)
                   (:name the-object :type simulation-object)
                   )
  :components (
               (:name ADD-OBJECT :type ADD-OBJECT :Framework ?framework)
               (:name SET-MASS :type SET-MASS :Framework ?framework)
               (:name SET-LOCATION :type SET-LOCATION :Framework ?framework)
               (:name SET-VELOCITY :type SET-VELOCITY :Framework ?framework)
	       (:name set-shape :type add-shape :Framework ?framework)
               )
  :dataflows (
              ((:component the-OBJECT :port THE-OBJECT) (:component ADD-OBJECT :port THE-OBJECT))
              ((:component the-OBJECT :port THE-OBJECT) (:component SET-LOCATION :port THE-OBJECT))
              ((:component THE-OBJECT :port THE-OBJECT) (:component SET-VELOCITY :port THE-OBJECT))
              ((:component THE-OBJECT :port THE-OBJECT) (:component SET-MASS :port THE-OBJECT))
	      ((:component THE-OBJECT :port THE-OBJECT) (:component SET-shape :port THE-OBJECT))
              ((:component THE-WORLD :port THE-WORLD) (:component ADD-OBJECT :port THE-WORLD))
              ((:component LOCATION :port LOCATION) (:component SET-LOCATION :port THE-LOCATION))
              ((:component VELOCITY :port VELOCITY) (:component SET-VELOCITY :port THE-VELOCITY))
              ((:component MASS :port MASS) (:component SET-MASS :port THE-MASS))
	      ((:component shape :port shape) (:component set-shape :port the-shape))
              )
  )

(deftask make-world
    :parameters (framework)
    :interface ((:inputs (loc-1 location)
                         (loc-2 location)
                         (vel-1 velocity)
                         (vel-2 velocity)
			 (shape-1 shape)
			 (shape-2 shape)
                         (mass mass)))
    )

(defmethod canonical-arguments-for ((type (eql 'make-world)) the-thing)
  (let ((framework nil))
    (ask* `[property-value-of ,the-thing framework ?answer]
	  (setq framework ?answer))
    (list framework)))

(defreduction make-world (make-world (?framework))
  :reduce-to (implement-make-world ?framework))

(defcliche implement-make-world (?framework)
  :initial-inputs (
                   (:name LOC-1 :type LOCATION)
                   (:name LOC-2 :type LOCATION)
                   (:name VEL-1 :type VELOCITY)
                   (:name VEL-2 :type VELOCITY)
                   (:name shape-1 :type shape)
                   (:name shape-2 :type shape)
                   (:name MASS :type MASS)
                   )
  :state-elements (
                   (:name OBJECT-1 :direction source :port-name OBJECT-1 :port-type SIMULATION-OBJECT :state OBJECT-1)
                   (:name OBJECT-2 :direction source :port-name OBJECT-2 :port-type SIMULATION-OBJECT :state OBJECT-2)
                   (:name THE-WORLD :direction source :port-name THE-WORLD :port-type SIMULATION-WORLD :state THE-WORLD)
                   )
  :components ((:name NEW-OBJECT-1 :type ALLOCATE :OBJECT-TYPE SIMULATION-OBJECT :framework ?framework)
               (:name NEW-OBJECT-2 :type ALLOCATE :OBJECT-TYPE SIMULATION-OBJECT :framework ?framework)
               (:name NEW-WORLD :type ALLOCATE :OBJECT-TYPE SIMULATION-WORLD :framework ?framework)
               (:name ADD-1 :type ADD-OBJECT-TO-WORLD :framework ?framework)
               (:name ADD-2 :type ADD-OBJECT-TO-WORLD :framework ?framework)
               )
  :dataflows (
              ((:component LOC-1 :port LOC-1) (:component ADD-1 :port LOCATION))
              ((:component LOC-2 :port LOC-2) (:component ADD-2 :port LOCATION))
              ((:component VEL-1 :port VEL-1) (:component ADD-1 :port VELOCITY))
              ((:component VEL-2 :port VEL-2) (:component ADD-2 :port VELOCITY))
              ((:component shape-1 :port shape-1) (:component ADD-1 :port shape))
              ((:component shape-2 :port shape-2) (:component ADD-2 :port shape))
              ((:component MASS :port MASS) (:component ADD-1 :port MASS))
              ((:component MASS :port MASS) (:component ADD-2 :port MASS))
              ((:component NEW-OBJECT-1 :port NEW-OBJECT) (:component OBJECT-1 :port INITIAL-OBJECT-1))
              ((:component NEW-OBJECT-2 :port NEW-OBJECT) (:component OBJECT-2 :port INITIAL-OBJECT-2))
              ((:component OBJECT-1 :port OBJECT-1) (:component ADD-1 :port THE-OBJECT))
              ((:component OBJECT-2 :port OBJECT-2) (:component ADD-2 :port THE-OBJECT))
              ((:component THE-WORLD :port THE-WORLD) (:component ADD-1 :port THE-WORLD))
              ((:component THE-WORLD :port THE-WORLD) (:component ADD-2 :port THE-WORLD))
              ((:component NEW-WORLD :port NEW-OBJECT) (:component THE-WORLD :port INITIAL-THE-WORLD))
              )
  )

(tell [context-package dyn4j "org.dyn4j.samples"])

(tell [context-include dyn4j "java.awt.Graphics2D"])
(tell [context-include dyn4j "org.dyn4j.dynamics.World"])
(tell [context-include dyn4j "org.dyn4j.geometry.Geometry"])
(tell [context-include dyn4j "org.dyn4j.geometry.MassType"])

(defparameter +goldenrod+ (clim:find-named-color "goldenrod" (clim:Frame-manager-palette (clim:find-frame-manager))))
(defparameter +forest-green+ (clim:find-named-color "forest-green" (clim:Frame-manager-palette (clim:find-frame-manager))))
(defparameter +red+ clim:+red+)
(defparameter +blue+ clim:+blue+)
(defparameter +magenta+ clim:+magenta+)
(defparameter +black+ clim:+black+)

(eval-when (:compile-toplevel :load-toplevel)
(define-predicate physics-framework (name))
(define-predicate child-of (physics-framework shape-class))
(define-predicate equivalence-class-for (shape-class class))
(define-predicate color-for (thing color-name)))

(defun clear-physics-world ()
  (untell [physics-framework ?])
  (untell [child-of ? ?])
  (untell [equivalence-class-for ? ?])
  (untell [color-for ? ?]))
  
(defun init-physics-world ()
  (clear-physics-world)
  (tell [physics-framework :jrect])
  (tell [child-of :jrect collision-shape])
  (tell [child-of collision-shape capsule-shape])
  (tell [equivalence-class-for capsule-shape capsule-eq-class])
  (tell [child-of collision-shape cylinder-shape])
  (tell [equivalence-class-for cylinder-shape cylinder-eq-class])
  (tell [child-of collision-shape box-shape])
  (tell [equivalence-class-for box-shape box-eq-class])

  (tell [physics-framework :jmonkey])
  (tell [child-of :jmonkey mesh])
  (tell [child-of mesh sphere])
  (tell [child-of mesh cylinder])
  (tell [equivalence-class-for cylinder cylinder-eq-class])
  (tell [child-of mesh abstract-box])
  (tell [child-of abstract-box strip-box])
  (tell [child-of abstract-box box])
  (tell [equivalence-class-for box box-eq-class])

  (tell [physics-framework :dyn4j])
  (tell [child-of :dyn4j abstract-shape])
  (tell [child-of abstract-shape capsule])
  (tell [equivalence-class-for capsule capsule-eq-class])
  (tell [child-of abstract-shape polygon])
  (tell [child-of abstract-shape circle])

  (tell [physics-framework :ode4j])
  (tell [child-of :ode4j d-geom])
  (tell [child-of d-geom d-capsule])
  (tell [equivalence-class-for d-capsule capsule-eq-class])
  (tell [child-of d-geom d-cylinder])
  (tell [equivalence-class-for d-cylinder cylinder-eq-class])
  (tell [child-of d-geom d-box])
  (tell [equivalence-class-for d-box box-eq-class])

  (tell `[color-for box-eq-class ,+blue+])
  (tell `[color-for cylinder-eq-class ,+magenta+])
  (tell `[color-for capsule-eq-class ,+forest-green+])
  )

(defun is-physics-framework (name)
  (ask* `[physics-framework ,name]
	(return-from is-physics-framework t))
  nil)

(defun physics-frameworks ()
  (let ((answer nil))
    (ask* [physics-framework ?name]
	  (pushnew ?name answer))
    answer))

(defun children-of (name)
  (let ((answer nil))
    (ask* `[child-of ,name ?child]
	  (pushnew ?child answer))
    answer))

(defun equiv-class-of (name)
    (ask* `[equivalence-class-for ,name ?name]
	  (return-from equiv-class-of ?name))
    'undefined)

(defun color-for (name)
  (ask* `[color-for ,name ?color]
	(return-from color-for ?color))
  +black+)

(defun graph-ontology (the-frame name &optional (stream *standard-output*))
  (declare (ignore the-frame))
  (clim:format-graph-from-root
     name
     #'(lambda (object stream) 
	 (if (is-physics-framework object)
	     (clim:surrounding-output-with-border (stream :shape :oval :ink clim:+black+ :line-thickness 4)
	       (clim:draw-text* stream (string object) 0 0 :text-style '(nil :italic :very-large)))
	   (let ((color (color-for (equiv-class-of object))))
	     (clim:surrounding-output-with-border (stream :shape :rectangle :ink color :line-thickness 4)
	       (clim:draw-text* stream  (string (javaize-class-name object)) 0 0 
				:text-style '(nil :bold :very-large)
				:ink color)))))
     #'(lambda (object) (children-of object))
     :orientation :vertical
     :arc-drawer #'(lambda (stream from-node to-node x1 y1 x2 y2 &rest drawing-options) 
		     (declare (ignore from-node to-node))
		     (clim:draw-arrow* stream x2 y2 x1 y1))
     :center-nodes t
     :stream stream)
  (values))

(defun the-hack (the-frame &optional (stream *standard-output*))
  (clim:window-clear stream)
  (terpri stream)
  (let ((frameworks (physics-frameworks)))
    (when frameworks
      (clim:formatting-table (stream :x-spacing '(30 :point) :y-spacing '(20 :point))
	(clim:formatting-row (stream)
	  (clim:formatting-cell (stream :align-x :center)
	    (graph-ontology the-frame :jrect stream)
	    )
	  (clim:formatting-cell (stream :align-x :center)
	    (graph-ontology the-frame :jmonkey stream)))
	(clim:formatting-row (stream)
	  (clim:formatting-cell (stream :align-x :center)
	    (graph-ontology the-frame :dyn4j stream)
	    )
	  (clim:formatting-cell (stream :align-x :center)
	    (graph-ontology the-frame :ode4j stream))))))
  (values))

(defun query-about-relationships (object-1 object-2)
  (let ((gen (generate-query-about-relationship object-1 object-2)))
    (vocalize-generator gen :vocalize? t :wait? t)))

(defun generate-query-about-relationship (object-1 object-2)
  (let* ((generator (make-instance 'question-generator))
	 (object-1-entity (intern-entity generator (find-plural-of-noun object-1) :definite? nil :singular? nil))
	 (object-2-entity (intern-entity generator (find-plural-of-noun object-2) :definite? nil :singular? nil))
	 (relationship (intern-entity generator "relationship"))
	 (what (intern-entity generator "what"))
	 (verb (intern-verb generator "is"))
	 (between-1 (intern-new-instance generator "between"))
	 (between-2 (intern-new-instance generator "between")))
    (create-main-clause generator relationship verb what :is-question? t)
    (intern-and-add-texp generator (list relationship between-1 object-1-entity))
    (intern-and-add-texp generator (list relationship between-2 object-2-entity))
    generator))

(define-design-editor-command (com-show-design-view :name t)
    ()
  (setf (clim:frame-current-layout *design-editor*) 'normal)
  )

(define-design-editor-command (com-show-ontology-view :name t)
    ()
  (clear-physics-world)
  (setf (clim:frame-current-layout *design-editor*) 'alt)
  )

(define-design-editor-command (com-show-ontology-alignment :name t)
    ()
  (do-ontology-hack))

(defun do-ontology-hack ()  
  (init-physics-world)
  (let ((display-pane-name 'alt-display))
    (clim:redisplay-frame-pane *design-editor* display-pane-name)
    (vocalize-generator (build-introductory-statement) :vocalize? t :wait? t)
    (see-if-equiv 'sphere 'capsule)
    (clim:redisplay-frame-pane *design-editor* display-pane-name)
    (sleep 4)
    (vocalize-text "I have inferred these additional equivalences" :vocalize? t :wait? t)
    (add-new-equivalences 'collision-shape 'd-geom 'mesh)
    ))

(eval-when (:compile-toplevel :load-toplevel)
  (define-predicate relationship-type-and-args-for-parse (parse relationship-type arg1 arg2)))

(defun see-if-equiv (concept-1 concept-2)
  (query-about-relationships concept-1 concept-2)
  (let ((response (parse-text (get-response))) 
	(equiv-class (equiv-class-of concept-2))
	(rel nil)
	(arg1 nil)
	(arg2 nil))
    (ask* `[relationship-type-and-args-for-parse ,(get-main response) ?rel ?arg1 ?arg2]
	  (setq rel (convert-start-string-to-lisp-atom ?rel)
		arg1 (convert-start-string-to-lisp-atom ?arg1)
		arg2 (convert-start-string-to-lisp-atom ?arg2)))
    (when (eql rel 'specialization)
      (setq rel 'generalization)
      (rotatef arg1 arg2))
    (when (eql rel 'generalization)
      (change-equivalence-class-of concept-1 equiv-class))))

(defun add-new-equivalences (&rest shapes)
  (let ((name 'super-class))
    (loop for shape in shapes
	do (tell `[equivalence-class-for ,shape ,name]))
    (tell `[color-for ,name ,+red+])))

(defrule rel-1-from-parse (:backward)
  :then [relationship-type-and-args-for-parse ?main ?rel ?arg1 ?arg2]
  :if [and [relation-of ?main ? |is-a|]
	   [object-of ?main ?rel-entity ?rel]
	   [subject-of ?main ? ?arg1]
	   [as-subject ?rel-entity ?related-to-clause]
	   [relation-of ?related-to-clause ? |related-to|]
	   [object-of ?related-to-clause ? ?arg2]])


(defrule rel-2-from-parse (:backward)
  :then [relationship-type-and-args-for-parse ?main ?rel ?arg1 ?arg2]
  :if [and [relation-of ?main ? |is-a|]
	   [subject-of ?main ? ?arg1]
	   [object-of ?main ?object ?arg2]
	   [as-subject ?object ?degenerate-clause]
	   [object-of ?degenerate-clause ? |degenerate|]
	   [unify ?rel specialization]]
  )

(defun change-equivalence-class-of (entry new-class)
  (untell `[equivalence-class-for ,entry ?])
  (tell `[equivalence-class-for ,entry ,new-class])
  )

(defun build-introductory-statement ()
  (multiple-value-bind (generator main) (build-object-equivalent-statement nil)
    (build-belief-statement main t generator)))

(defun build-belief-statement (believed-clause &optional main? (generator (make-instance 'question-generator)))
  (let ((believe (intern-new-instance generator "believe"))
	(somebody (intern-constant generator "somebody")))
    (values generator
	    (if main?
		(create-main-clause generator somebody believe believed-clause :is-passive? t)
	      (intern-and-add-texp generator (list somebody believe believed-clause))))))

(defun build-object-equivalent-statement (main? &optional (generator (make-instance 'question-generator)))
  (multiple-value-bind (items color) (generate-items-with-same-color generator)
    (let ((equivalent (intern-constant generator "equivalent")))
      (let ((is-a-clause (add-is-a-clause generator items equivalent main?)))
      (add-property generator items "equivalent")
      (add-property generator color "the_same")
      (values generator is-a-clause)
      ))))

(defun generate-items-with-same-color (generator)
  (let ((items (intern-entity generator "items" :definite? nil :singular? nil))
	(color (intern-entity generator "color" :definite? 'null :singular? t)))
    (add-with-clause generator items color)
    (values items color)))

(defun add-is-a-clause (generator subject object &optional main?)
  (let ((is-a (intern-new-instance generator "is")))
    (if main?
	(create-main-clause generator subject is-a object)
      (intern-and-add-texp generator (list subject is-a object))
    )))

(defun add-with-clause (generator subject object)
  (let ((with (intern-new-instance generator "with")))
    (intern-and-add-texp generator (list subject with object))))

(defun add-property (generator thing property)
  (let ((interned-property (intern-constant generator property))
	(has-property (intern-new-instance generator "has_property")))
    (intern-and-add-texp generator (list thing has-property interned-property))))

(defrule show-ontology-handler (:backward)
  :then [is-appropriate-response |show| ?main handle-show-ontology ()]
  :if [and [subject-of ?main ? |you|]
	   [object-of ?main ? |ontology|]
	   ]
  )

(defmethod handle-show-ontology ((parser core-parser-mixin) texp)
  (declare (ignore texp))
  (do-ontology-hack))