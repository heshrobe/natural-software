k;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 2:
;;; Generating code after symbolic execution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is for non-primitive tasks
;;; The primitive ones all have their own methods
;;; Compound joining tasks will need a method that overrides this one

(defmethod code-for-top-level-task (top-level-task top-level-arguments the-code (language (eql :lisp))
                                    &key  state-variable-code streaming? &allow-other-keys)
  (let ((name (name top-level-task))
        (outputs (program-outputs top-level-task)))
    (If state-variable-code
        `(defun ,name ,(loop for arg in top-level-arguments collect (top-level-arg-code-for arg language))
           (let ,state-variable-code
             ,@the-code
             ,@(unless streaming?
                 `(,@outputs))))
        `(defun ,name ,(loop for arg in top-level-arguments collect (top-level-arg-code-for arg language))
           ,@the-code
           ,@(unless streaming?
               `(,@outputs)))
        )))

;;; Fix: Need to add state variable bindings in method
(defmethod code-for-top-level-task (top-level-task top-level-arguments the-code (language (eql :java))
                                    &key state-variable-code streaming? generation-controller &allow-other-keys)
  ;; Fix: Will eventually get used  
  (declare (ignore streaming?))
  (let* ((name (javaize-identifier (name top-level-task)))
         (outputs (loop for output in (program-outputs top-level-task) 
		      if (symbolp output)
		      collect (identifier-for-language output language)
		      else collect output))
         (include-main? (if generation-controller (include-main? generation-controller) nil))
         (class-name (if generation-controller (javaize-class-name (class-name generation-controller)) '|Main|))
         (raw-super-class (when generation-controller (super-class generation-controller)))
         (super-class (when raw-super-class (javaize-class-name raw-super-class)))
         (methods nil))
    (push `(:method :name ,name 
                    :static? ,(when generation-controller (static? generation-controller))
                    :result ,(loop for output in (outputs top-level-task) collect (port-type-constraint output))
                    :protections ,(when generation-controller (protections generation-controller))
                    :args ,(loop for top-level-arg in top-level-arguments collect (top-level-arg-code-for top-level-arg language))
                    :outputs ,outputs
                    :state-variable-code ,state-variable-code
                    :body ,the-code)
          methods)
    (when include-main?
      (push `(:method :name |main| 
                      :protections (|public|)
                      :static? t
                      :result |void|
                      :args (|String[] args|)
                      :body ((:application ,name nil
                                           ,@(loop for i below (length top-level-arguments)
                                                 collect `(:array-access |args| ,i))))
                      )
            methods))   
    `(:class-definition :visible? t 
                        :context ,(when generation-controller (context generation-controller))
                        :super-class ,super-class
                        :name ,class-name
                        :methods ,methods)))

;;; I'm not even sure if this will show up in the current approach
;;; However, even if it does, there's nothing to do
;;; because all of its subtasks will be on the queue in the right order
(defmethod form-for ((task implementation) (task-type t) (language t))
  *defer-token*
  )

(defmethod form-for ((task path-end) (task-type t) (language t))
  *defer-token*
  )

;;; Completion tasks are only there to make sure the sorted order comes
;;; out right so they don't generate code.
(defmethod form-for ((task completion-task) (task-type t) (langauge t))
  *defer-token*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Normal tasks without special methods
;;;
;;; when a normal task wants to make a form it need the
;;; values of its inputs.  This simple method tests
;;; whether the input has been bound (because of a let introduction
;;; motivated by multiple consumers) and if so return the bound variable name
;;; otherwise the form for that input.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Default case, just complain that you shouldn't get here
(defmethod form-for (task task-type language)
  (break "Why am I generating code for ~a ~a ~a" task task-type language)
  (values nil))

;;; We want final outputs to be in the processing stream
;;; so that we can impose ordering constraints around them
;;; but they aren't really consumers of the token, so 
;;; we need to make binding decisions around the count of 
;;; consumers other than final outputs
(defun count-useful-consumers (token)
  (loop for consumer in (consumers token)
      count (not (typep consumer 'final-output))))

(defmethod value-to-use ((token value-token-mixin) language)
  (cond
   ((> (count-useful-consumers token) 1)
    (identifier-for-language (value token) language))
   ((symbolp (form token))
    (identifier-for-language (form token) language))
   (t (form token))))

(defmethod value-to-use ((token allocate-symbolic-value-token) (language (eql :java)))
  (identifier-for-language (value token) language)
  )

(defmethod identifier-for-language (identifier (language (eql :lisp))) identifier)
(defmethod identifier-for-language (identifier (language (eql :java))) 
  (cond
   ((symbolp identifier) `(:variable ,identifier))
   ((eql (first identifier) :variable) identifier)
   (t
    (destructuring-bind (op . values) identifier
      (cons op (loop for sub-op in values collect (identifier-for-language sub-op language)))))))

;;; For a bound variable of any type you use the variable name
(defmethod value-to-use ((token bound-variable-mixin) language) 
  (identifier-for-language (value token) language))

(defmethod top-level-argument-for-language (token language)
  (identifier-for-language token language))

(defmethod top-level-argument-for-language ((token top-level-argument) (language (eql :java)))
  (let ((value (value token)))
    (cond ((symbolp value) (identifier-for-language value language))
	  ((is-viewpoint? value) (identifier-for-language (second value) language))
	  (t (break "Other type of top-level argument in java ~a" value)))))

;;; Notice that this will only get called if no more specific method
;;; is appropriate.  So all the enumerators and other built-ins modeing primitives have
;;; their own appropriate methods.  This is just for a vanilla computations.
;;; This does apply to left, right, add-to-set etc.

(defgeneric generate-opaque-task-code (task task-type language))

(defmethod form-for ((task task-interface-mixin) (task-type t) (language t))
  (if (or (primitive? task) (dont-expand task))
      ;; generate the form for evaluating this guy
      (generate-opaque-task-code task task-type language)
    ;; otherwise, I guess nothing should happen?
    *defer-token*
    ))

;;; This is split into an around method that is language independent
;;; and methods for each language to build the form and/or the binding code
(defgeneric binding-code-for (output-variable type form body-code language))


(defmethod generate-opaque-task-code :around ((task output-side-mixin) (task-type t) (language t))
  (let* ((form (call-next-method))
	 (simplified-form (simplified-form form))
	 ;; (viewpoint? (is-viewpoint? simplied-form))
	 (abstract-op? (is-abstract-statement? simplified-form))
	 )
    (if (outputs task)
	(let* ((output-port (first (outputs task))) 
	       (output-token (symbolic-token output-port))
	       (output-variable (value output-token))
	       (output-variable-type (type-constraint output-token))
	       (count (count-useful-consumers output-token))
	       (make-binding? (> count 1)))
	  (cond 
	   ;; if the form is an abstract form
	   ;; then don't include it in the output
	   ;; and stick it into the "form" field of the output token
	   ((not (eql simplified-form form))
	    (when (listp form)
	      (setq simplified-form (concretize-for-langauge simplified-form (first simplified-form) language)))
	    ;; Fix? Is there a side-effect on shared data issue here?
	    (setf (form output-token) simplified-form
		  (value output-token) simplified-form)
	    (cond
	     ((zerop count) (form output-token))
	     ((or abstract-op? (= 1 count)) *defer-token*)
	     (make-binding?
	      (setf (form output-token) output-variable
		    (value output-token) output-variable)
	      (let ((body-code (sub-gobble (code language task) code)))
		(binding-code-for output-variable output-variable-type simplified-form body-code language)))))
	   ;; if we don't have to bind a variable
	   ;; then we just make the form for this guy.
	   ;; If he has no conumers, then he's just for effect
	   ;; and we return his form
	   ((= 0 count) 
	    (setf (Form output-token) form)
	    form)
	   ;; He has exactly one consumer
	   ;; squirrel away his code in the appropriate place
	   ;; and wait for the one consumer to get processed.
	   ((= 1 count)
	    (setf (form output-token) form)
	    *defer-token*)
	   ;; otherwise we need to make a binding form
	   (make-binding?
	    (let ((body-code (sub-gobble (code language task) code)))
	      (binding-code-for output-variable output-variable-type form body-code language)))))
      ;; if this thing is a dead end, just return its code
      form)))


;;; An opaque task that has branches is sort of funny
;;; the branches are there but in general we wouldn't know what to do with them
;;; so we just gobble them and throw them away.

(defmethod generate-opaque-task-code :around ((task has-branches-mixin) (task-type t) (language t))
  (let ((form (call-next-method)))
    ;; this is being done for effect
    (gobble-branches task language)
    form
  ))

(defmethod binding-code-for (output-variable type form body-code (language (eql :lisp)))
  (declare (ignore type))
  `(let ((,output-variable ,form))
     ,@body-code))

(defmethod binding-code-for (output-variable type form body-code (language (eql :java)))
  `(:bind  ,(identifier-for-language output-variable language) ,type ,form ,@body-code))

(defmethod top-level-arg-code-for (port (language (eql :lisp)))
  (declare (ignore type))
  (name port))

(defmethod top-level-arg-code-for (port (language (eql :java)))
  (let* ((token (symbolic-token port))
	 (name (top-level-argument-for-language token language))
	 (type (type-constraint token)))
  `(:top-level-arg ,name ,type)))

;;; Build a form to apply this guy's function to its arguments
;;; this is the language dependent part

(defgeneric function-name-to-use (task task-type language))

(defmethod function-name-to-use ((task task-interface-mixin) (task-type t) (language t))
  (let ((recursive-call-name (getf (properties task) :recursive-call-name)))
    (or recursive-call-name task-type)))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type t) (language (eql :lisp)))
  (cons (function-name-to-use task task-type language)
	(loop for input in (inputs task)
	    for token = (symbolic-token input)
	    for form = (value-to-use token language)
	    collect form)))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type t) (language (eql :java)))
  `(:application ,(function-name-to-use task task-type language)
		 ,@(loop for input in (inputs task)
		       for token = (symbolic-token input)
		       for form = (value-to-use token language)
		       collect form)))

;;; Fix: need to switch to gobble framework
;;; why is this wired to more and empty branches?
;;; that seems peculiar to enumerators.

; (defmethod generate-opaque-task-code ((task has-branches-mixin) language)
;   ;; a special case for generators with an exit branch
;   (let* ((branches (branches task))
; 	 (more-branch (find 'more branches :key #'name))
; 	 (empty-branch (find 'empty branches :key #'name)))
;     (cond 
;      ((and (= (length branches) 2) more-branch empty-branch)
;       (let* ((more-output-port (first (outputs more-branch)))
; 	     (more-output-token (symbolic-token more-output-port))
; 	     (output-variable (value more-output-token))
; 	     (more-output-consumers (consumers more-output-token))
; 	     (output-form (form more-output-token))
; 	     (downstream-code (downstream-code more-output-port language)))
; 	(if (rest more-output-consumers)
; 	    `((let ((,output-variable ,output-form))
; 		,@downstream-code))
; 	  (list output-form))
; 	)))))

;;; This is about making something a recursive internal function.
(defmethod form-for :around ((task task-interface-mixin) (task-type t) (language (eql :lisp)))
  (let* ((introduce-labels (getf (properties task) :introduce-labels))
         (local-bindings (getf (properties task) :local-bindings))
         (state-variable-code (when local-bindings (build-local-state-source-initializations task language)))
         )
    (when local-bindings
      ;; Make the form field of the initial-input tokens
      ;; be the same as those of the interface

      (loop for input-port in (inputs task)
          for token = (symbolic-token input-port)
          for form = (form token)
          for corresponding-port = (corresponding-port input-port)
          for corresponding-token = (symbolic-token corresponding-port)
          do (setf (form corresponding-token) form)))
    (if (or introduce-labels local-bindings)
        (sub-gobble (answer language task)
                    ;; only create the binding if we're a sub-task
                    ;; a top level task has a design (a composite-task, but not an implementation)
                    ;; as its superior
                    ;; FixMe: This is lisp specific but is in a method for all languages.
                    (when introduce-labels
                      (setq answer
                        `(labels ((,introduce-labels ,(loop for port in (inputs task) collect (name port))
                                    ,@answer))
                           (,introduce-labels ,@(loop for input in (inputs task)
                                                    for token = (symbolic-token input)
                                                    for value = (simplified-form (value token))
                                                    collect value))
                           )))
                    (when state-variable-code
                      (setq answer `(let ,state-variable-code ,@answer)))
                    answer)
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Form-for methods for the inputs, outputs, entries, exit, states
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; here go all the specific method for final-output entry-point exit-point, etc.

(defmethod form-for ((task initial-input) (task-type (eql 'input)) (language t))
  *defer-token*)

(defmethod form-for ((final-output final-output) (task-type (eql 'output)) (language t))
  *defer-token*)

(defmethod form-for ((entry-point entry-point) (task-type (eql 'entry-point)) (language t))
  *defer-token*)

(defmethod form-for ((exit-point exit-point) (task-type t) (language t))
  *defer-token*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant Values
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod value-to-use ((token constant-value-token) language)
  (declare (ignore language))
  (form token))

(defmethod form-for ((task constant-value) (task-type (eql 'constant)) (language (eql :lisp)))
  (let* ((output-port (first (outputs task)))
	 (output-token (symbolic-token output-port))
	 (output-type (type-constraint output-token))
	 (output-value (value-for-type-in-language (value output-token) output-type language)))
    (setf (form output-token) output-value)
    *defer-token*))

(defmethod form-for ((task constant-value) (task-type (eql 'constant)) (language (eql :java)))
  (let* ((output-port (first (outputs task)))
         (output-token (symbolic-token output-port))
         (output-type (type-constraint output-token))
         (output-value (value-for-type-in-language (value output-token) output-type language)))
    (setf (form output-token) `(:constant ,output-value ,output-type))
    *defer-token*
    ))

(defmethod value-for-type-in-language (value (type t) (language t)) value)
(defmethod value-for-type-in-language ((value (eql 'true)) (type (eql 'boolean)) (language (eql :lisp))) t)
(defmethod value-for-type-in-language ((value (eql 'true)) (type (eql 'boolean)) (language (eql :java))) '|true|)
(defmethod value-for-type-in-language ((value (eql 'false)) (type (eql 'boolean)) (language (eql :lisp))) nil)
(defmethod value-for-type-in-language ((value (eql 'false)) (type (eql 'boolean)) (language (eql :java))) '|false|)
(defmethod value-for-type-in-language ((value symbol) (type (eql 'symbol)) (language (eql :lisp))) `',value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allocate State
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fix
(defmethod form-for ((task task-interface-mixin) (task-type (eql 'allocate)) (language t))
  (let* ((output-port (first (outputs task)))
         (type (port-type-constraint output-port))
         (output-token (symbolic-token output-port))
         ;; (input-ports (inputs task))
         ;; (input-tokens (mapcar #'symbolic-token input-ports))
         ;; (input-values (mapcar #'value input-tokens))
         )
    (cond
     ((and (eql language :lisp) (listp type) (eql (first type) 'temporal-sequence))
      *defer-token*)
     ((eql language :lisp)
      (setf (form output-token) (allocation-code task type language))
      *defer-token*
      ))))

;;;    (case language
;;;      (:lisp *defer-token*)
;;;      (:java
;;;       ;; Fix: What's the right way to avoid allocating things that aren't real types
;;;       (if (and (listp type) (eql (first type) 'temporal-sequence))
;;;	   *defer-token*
;;;       `(:allocate ,(value output-token) ,type ,@input-values))))
;;;    ))

;;; If it's a local state-source then we need to emit bindings
;;; I guess local ones are done magically at the beginning in code gen.
(defmethod form-for ((task state-source)  (task-type (eql 'state)) (language t))
  (let* ((output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (output-variable (value output-token))
	 )
    (setf (form output-token) output-variable)
    *defer-token*))

(defmethod form-for ((task state-sink) (task-type (eql 'state)) (language (eql :lisp)))
  (let* ((state-name (state-name task))
	 (new-value-port (first (inputs task)))
	 (new-value-token (symbolic-token new-value-port))
	 (output-port (first (outputs task)))
	 (output-token (symbolic-token output-port))
	 ; Fix: Kludge because unsure whether it should be code or form
	 (new-value-code (value-to-use new-value-token language)))
    (setf (form output-token) (value output-token))
    (if (and (listp new-value-code) (eql (first new-value-code) '1+))
	`(incf ,(second new-value-code))
      (simplified-form `(setq ,state-name ,new-value-code)))))

(defmethod form-for ((task state-sink) (task-type (eql 'state)) (language (eql :java)))
  (let* ((state-name (state-name task))
	 (new-value-port (first (inputs task)))
	 (new-value-token (symbolic-token new-value-port))
	 ;; Fix: Kludge because unsure whether it should be code or form
	 (new-value-code (value-to-use new-value-token language)))
    (let* ((simplified-form `(setq ,state-name ,new-value-code))
	   (state-name (second simplified-form))
	   (form (third simplified-form)))
      (when (symbolp state-name)      
	(setq state-name (identifier-for-language state-name language)))
      ;; special case a = a + 1
      (if (and (listp form) (eql (first form) :count))
	  `(:increment ,(second form))
	`(:assignment ,state-name ,form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; 
;;; Numeric operations
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'add)) (language (eql :lisp)))
  '+)

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'subtract)) (language (eql :lisp)))
  '-)

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'multiply)) (language (eql :lisp)))
  '*)

(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'divide)) (language (eql :lisp)))
  '/)

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'count)) (language (eql :lisp)))
  `(1+ ,(let* ((input (second (inputs task)))
	      (token (symbolic-token input))
	      (form (value-to-use token language)))
	  form)))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'add)) (language (eql :java)))
  (form-java-binary-op '+ task))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'subtract)) (language (eql :java)))
  (form-java-binary-op '- task))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'multiply)) (language (eql :java)))
  (form-java-binary-op '* task))

(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'divide)) (language (eql :java)))
  (form-java-binary-op '/ task))
  
(defun form-java-binary-op (operator task)  
  `(:binary-op ,operator ,@(loop for input in (inputs task)
		       for token = (symbolic-token input)
		       for form = (value-to-use token :java)
		       collect form)))


(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'count)) (language (eql :java)))
  `(:count ,(let* ((input (second (inputs task)))
		   (token (symbolic-token input))
		   (form (value-to-use token language)))
	      form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; 
;;; Print
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defmethod function-name-to-use ((task task-interface-mixin) (task-type (eql 'print)) (language (eql :lisp)))
  'print)

;;; This needs to work at this level because it's a static method and you need to
;;; provide both the function and the object
(defmethod generate-opaque-task-code ((task task-interface-mixin) (task-type (eql 'print)) (language (eql :Java)))
  (let* ((input-port (first (inputs task)))
	 (input-token (symbolic-token input-port))
	 (input-value (value-to-use input-token language)))
    `(:application |println| |System.out| ,input-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Primitive Branching tasks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod form-for ((task primitive-branching-task) (task-type t) (language t))
  ;; This is done to make sure that (value-to-use ...) works appropriately inside
  ;; processed-branch-condition
  (loop for branch in (branches task)
      do (loop for output in (outputs branch)
             for token = (symbolic-token output)
             for value = (value token)
             do (setf (form token) value)))
  (gobble-branches task language))

(defmethod gobble-branches ((task has-branches-mixin) (language t))
  (let ((branches-code (loop for branch in (branches task)
                           for branch-in-queue = (pop *all-tasks*)
                           for condition = (processed-branch-condition (substituted-branch-condition branch-in-queue) language)
                           for his-code = (form-for branch-in-queue (task-type branch) language)
                           when (and his-code (not (eql his-code *defer-token*))) 
                           collect (cons condition his-code))))
    (let ((final-code (build-branch-code (reverse branches-code) language)))
      final-code)))

(defmethod processed-branch-condition (form (language (eql :lisp)))
  (labels ((do-one-token (token op-position)
	     (cond
	      ((listp token)
	       (loop for thing in token
		   for first = t then nil
		   collect (do-one-token thing first)))
	      ((typep token 'value-token-mixin)
	       (let ((value (value-to-use token language)))
		 (if (and op-position (symbolp value))
		     (do-one-token value t)
		   value)))
	      ((eql token :otherwise) 't)
	      ((symbolp token)
	       (if op-position 
		   (language-version-of-predicate token language)
		 token))
	      (t token))))
    (do-one-token form nil)))

;;; Fix: really to get the form and then figure out its arity
;;; and then build either unary comparison or binary-comparison

(defmethod language-version-of-predicate ((predicate t) (language t)) predicate)
(defmethod language-version-of-predicate ((predicate (eql 'less-than)) (language t)) '<)

(defmethod processed-branch-condition (form (language (eql ::java)))
  (labels ((do-one-token (token)
	     (cond
	      ((listp token)
	       `(:binary-comparison ,(language-version-of-predicate (first token) language)
				    ,@(loop for thing in (rest token) collect (do-one-token thing))))
	      ((typep token 'value-token-mixin)
	       (let ((value (value-to-use token language)))
		 (cond
		  ((numberp value)
		   `(:constant ,value))
		  ((and (listp value) (member (first value) '(:variable :constant)))
		   value)
		  ((and (listp value) (eql (first value) :application))
		   value)
		  (t
		   `(:application ,@value)))))
	      ((eql token :otherwise)
	       `(:if-terminator))
	      (t token))))
    (do-one-token form)))

;;; If we require that every branch has a matching join (as we should)
;;; then the terminator can be a bit simpler

(defmethod form-for ((branch branch) (task-type (eql 'branch)) (language t))
  (let* ((parent-task (superior branch)))
    ;; For a non-primitive branching task, something in the implementation
    ;; must be branching and it's going to get to this branch and keep going
    ;; so in that case we don't need to sub-gobble.
    ;; I guess we need to do this because the branch itself might be a dead end
    ;; but it's already popped off  *all-tasks*
    (cond
     ((and (downstream-tasks branch)
	   (or (primitive? parent-task) (dont-expand parent-task)))
      (sub-gobble (code language branch) (if (null code) *defer-token* code)))
     (t *defer-token*))))

(defmethod build-branch-code (branches-code (language (eql :lisp)))
  (if (rest branches-code)
      `(cond ,@branches-code)
    (let ((only-pair (first branches-code)))
      `(when ,(first only-pair) ,@(rest only-pair)))))

;;; Pretty printer can figure out the best java
;;; version I guess.
(defmethod build-branch-code (branches-code (language (eql :java)))
  `(:conditional ,@(loop for (test . body) in branches-code
		       collect `(,test ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Joins
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod form-for ((task join) (task-type (eql 'join)) (language t))
  *defer-token*)

(defmethod form-for ((task has-joins-mixin) (task-type t) (language t))
  *defer-token*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; proxies for things downstream from joins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod form-for ((task proxy-task) (task-type t) (language t))
  (let ((real-task (real-task task))
	(join (join task)))    
    (let ((base-form (form-for real-task task-type language)))
      (if (eql base-form *defer-token*)
	  *defer-token*
	(dejoin-form base-form join language)))))

(defun dejoin-form (base-form join language)
  (labels ((do-one (token)
	     (cond ((atom token) token)
		   ((and (listp token) 
			 (listp (first token))
			 (typep (first (first token)) 'join))
		    (value-to-use (cdr (assoc join token)) language))
		   ((listp token)
		    (loop for sub-token in token collect (do-one sub-token)))
		   (t token))))
    (do-one base-form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector related stuff
;;;  includes accessor, length, assignment, range-constructor
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note: I thought this should be an around method, but was wrong.
;;; If it were, it would wrap the base case (i.e. (task-type t) (language t))
;;; which is the opposite of what was desired.  Here's it's just a normal method
;;; but the call-next-method in the second clause of the COND has the effect of 
\;;; doing the normal case for vanilla application forms if we're not in the special case of array accessing
;;; vs Vector (i.e. object-oriented) accessing. 
;;; We still need the same kind of special casing for array assignment.
(defmethod generate-opaque-task-code ((task task-interface) (task-type (eql 'vector-accessor)) (language (eql :lisp)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 ;; (vector-type (type-constraint vector-token))
	 (vector-value (value vector-token))
	 (index-port (port-named 'input 'index task))
	 (index-token (symbolic-token index-port)))
    (cond
     ((or (is-viewpoint? vector-value) (is-abstract-statement? vector-value))
      ;; if the equivalence used aref instead of element we wouldn't need this
      `(element ,(value-to-use vector-token language) ,(value-to-use index-token language)))
     (t (call-next-method)))))

(defmethod generate-opaque-task-code ((task task-interface) (task-type (eql 'vector-accessor)) (language (eql :java)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector-type (type-constraint vector-token))
	 (vector-value (value vector-token))
	 (index-port (port-named 'input 'index task))
	 (index-token (symbolic-token index-port)))
    (cond
     ((or (is-viewpoint? vector-value) (is-abstract-statement? vector-value))
      ;; if the equivalence used aref instead of element we wouldn't need this
      `(element ,(value-to-use vector-token language) ,(value-to-use index-token language)))
     ((and (listp vector-type) (eql (first vector-type) 'array))
      `(:array-access ,(value-to-use vector-token language) ,(value-to-use index-token language)))
     (t (call-next-method)))))

(defmethod generate-opaque-task-code ((task task-interface) (task-type (eql 'vector-length)) (language (eql :lisp)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 ;; (vector-type (type-constraint vector-token))
	 (vector-value (value vector-token)))
    (cond
     ((or (is-viewpoint? vector-value) (is-abstract-statement? vector-value))
      ;; if the equivalence used aref instead of element we wouldn't need this
      `(length ,(value-to-use vector-token language)))
     (t (call-next-method)))))

(defmethod generate-opaque-task-code ((task task-interface) (task-type (eql 'vector-length)) (language (eql :java)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector-type (type-constraint vector-token))
	 (vector-value (value vector-token)))
    (cond
     ((or (is-viewpoint? vector-value) (is-abstract-statement? vector-value))
      ;; if the equivalence used aref instead of element we wouldn't need this
      `(length ,(value-to-use vector-token language)))
     ((and (listp vector-type) (eql (first vector-type) 'array))
      `(:array-length ,(value-to-use vector-token language)))
     (t (call-next-method)))))

(defmethod function-name-to-use ((task task-interface) (task-type (eql 'vector-accessor)) (language (eql :lisp)))
  'aref)

(defmethod function-name-to-use ((task task-interface) (task-type (eql 'vector-accessor)) (language (eql :java)))
  '|get|)
				      
(defmethod function-name-to-use ((task task-interface) (task-type (eql 'vector-length)) (language (eql :lisp)))
  'length)

(defmethod function-name-to-use ((task task-interface) (task-type (eql 'vector-length)) (language (eql :java)))
  (let* ((vector-port (port-named 'input 'vector task))
	 (vector-token (symbolic-token vector-port))
	 (vector-type (type-constraint vector-token)))
    (cond
     ((and (listp vector-type) (eql (first vector-type) 'array))
      '|length|)
     (t '|size|))))

(defmethod function-name-to-use ((task task-interface) (type (eql 'vector-push)) (language (eql :lisp)))
  'vector-push)

(defmethod function-name-to-use ((task task-interface) (type (eql 'vector-push)) (language (eql :java)))
  '|add|)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Makimng the result of abstract operator simplification language specific
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric concretize-for-langauge (form operator language))

;;; generally this is a no-op
(defmethod concretize-for-langauge (form operator language) 
  (declare (ignore operator language))
  form)

(defmethod concretize-for-langauge (form (operator (eql 'array-dimension)) (language (eql :java)))
  `(:array-length ,@(rest form)))

(defmethod concretize-for-langauge (form (operator (eql 'aref)) (language (eql :java)))
  `(:array-access ,@(rest form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerator (and generator) related things
;;;  take put truncate
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fix Where's the Java Method for this?
(defmethod form-for ((task task-interface) (task-type (eql 'take)) (language t))
  (let* ((input-port (port-named 'input 'sequence-data task))
         (input-token (symbolic-token input-port))
	 (input-tokens-input-token (when (typep input-token 'temporal-sequence-output-value) (input-token input-token)))
         (input-variable (value input-token))
         (sequence-type (first (type-constraint input-token)))
         (output-port (first (outputs task)))
         (output-token (symbolic-token output-port))
         (output-variable (value output-token)))
    ;; I don't remember why this setf is there, but in the case
    ;; that our output token is the result of a put-take flow sequence
    ;; out output token is that the result token of whatever operation
    ;; was put into the sequence and which case we shouldn't be mucking
    ;; with it's form which the actual operation has already produced.
    (unless (eql output-token input-tokens-input-token)
      (setf (form output-token) (value output-token)))
    (case sequence-type
      (stream
       ;; Fix: needs to be language independent
       ;; the variable might have been converted to viewpoint of something
       ;; to we need this check
       ;; Check: Can this issue occur elsewhere?
       `(loop for ,(if (is-viewpoint? output-variable) (form output-token) output-variable) = (dequeue ,input-variable)
            do ,@(sub-gobble (code language task) code)))
      (t *defer-token*))))

(defmethod form-for ((task task-interface) (task-type (eql 'truncate)) (language (eql :lisp)))
  (let* ((input-port (port-named 'input 'data task))
	 (input-token (symbolic-token input-port))
	 (input-variable (value input-token))
	 (return-value (getf (properties task) :return-value?)))
    (if return-value
	`(return ,input-variable)
      `(return)
      )))

(defmethod form-for ((task task-interface) (task-type (eql 'truncate)) (language (eql :java)))
  (let* ((input-port (port-named 'input 'data task))
	 (input-token (symbolic-token input-port))
	 (input-variable (value input-token))
	 (return-value (getf (properties task) :return-value?)))
    (declare (ignore return-value input-variable))
    `(:break |abnormal-exit|)))

(defmethod form-for ((task task-interface) (task-type (eql 'put)) (language (eql :lisp)))
  (let* ((data-input (port-named 'input 'data task))
	 (data-token (symbolic-token data-input))
	 (sequence-input (port-named 'input 'sequence task))
	 (sequence-token (symbolic-token sequence-input))
	 (sequence-type (type-constraint sequence-token))
	 )
    ;; probably the output token should encode whether 
    ;; the put actually does something in a better way than this
    (if (and (listp sequence-type) (eq (first sequence-type) 'stream))
	`(enqueue ,(value data-token) ,(value sequence-token))
      *defer-token*)))


(defmethod form-for ((task task-interface) (task-type (eql 'put)) (language (eql :java)))
  (let* ((output (first (outputs task)))
	 (output-token (symbolic-token output))
	 (output-form (form output-token))
	 )
    ;; probably the output token should encode whether 
    ;; the put actually does something in a better way than this
    (if (and output-form (eql (first output-form) 'enqueue))
	output-form
      *defer-token*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enumerators and Range Constructors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A range constructor is only used (at the moment)
;;; in conjunction with a single index enumerator
;;; We need to set up the upper-bound and lower-bound in the output-token
;;; but we generate no code of our own.
;;; isn't this also done in the propagation stage?  Does it need to be done twice? 
(defmethod form-for ((task task-interface) (type (eql 'range-constructor)) (language t))
  (let* ((lower-bound-port (port-named 'input 'lower-bound task))
	 (lower-bound-token (symbolic-token lower-bound-port))
	 (lower-bound-value (value-to-use lower-bound-token language))
	 (upper-bound-port (port-named 'input 'upper-bound task))
	 (upper-bound-token (symbolic-token upper-bound-port))
	 (upper-bound-value (value-to-use upper-bound-token language))
	 (output-port (port-named 'output 'the-range task))
	 (output-token (symbolic-token output-port))
	 )
    (setf (lower-bound output-token) lower-bound-value
	  (upper-bound output-token) upper-bound-value)
    *defer-token*))

;;; A place constructor used (at the moment)
;;; to package up a "bounded pointer" i.e. an object and offset
;;; such as a vector and its index 
;;; We need to set up the container and offset in the output-token
;;; but we generate no code of our own.

(defmethod form-for ((task task-interface) (type (eql 'place-constructor)) (language t))
  (let* ((container-port (port-named 'input 'container task))
	 (container-token (symbolic-token container-port))
	 (container-value (value-to-use container-token language))
	 (offset-port (port-named 'input 'offset task))
	 (offset-token (symbolic-token offset-port))
	 (offset-value (value-to-use offset-token language))
	 (output-port (port-named 'output 'the-place task))
	 (output-token (symbolic-token output-port))
	 )
    (setf (container output-token) container-value
	  (offset output-token) offset-value)
    *defer-token*))

;;; Fix add equivalent methods for java generation
(defmethod generate-opaque-task-code ((task task-interface) (type (eql 'fetch)) (language (eql :lisp)))
  (let* ((place-port (port-named 'input 'the-place task))
	 (place-token (symbolic-token place-port)))
    (simplified-form `(element ,(container place-token) , (offset place-token)))))

(defmethod generate-opaque-task-code ((task task-interface) (type (eql 'fetch-object)) (language (eql :lisp)))
  (let* ((place-port (port-named 'input 'the-place task))
	 (place-token (symbolic-token place-port)))
    (simplified-form (container place-token))))

(defmethod generate-opaque-task-code ((task task-interface) (type (eql 'assign)) (language (eql :lisp)))
  (let* ((place-port (port-named 'input 'the-place task))
	 (place-token (symbolic-token place-port))
	 (new-value-port (port-named 'input 'new-value task))
	 (new-value-token (symbolic-token new-value-port))
	 (accessor-form `(element ,(container place-token) ,(offset place-token))))
    `(setf ,(simplified-form accessor-form)
       ,(value-to-use new-value-token language))))

;;; Dead code at the moment
(defmethod is-a-dead-end ((proxy proxy-task)) (is-a-dead-end (real-task proxy)))

(defmethod is-a-dead-end (task)
  (cond ((eql (task-type task) 'truncate) t)
	((and (typep task 'output-side-mixin)
	      (or (null (outputs task))
		  (loop for output in (outputs task) always (null (outgoing-flows output))))
	      (null (outgoing-control-flows task)))
	 t)
	(:otherwise nil)))

(defmethod form-for ((task primitive-enumerator) (task-type (eql 'index-enumerator)) (language t))
  (let* ((more-branch (branch-named 'more task))
	 (more-output-port (first (outputs more-branch)))
	 (more-output-port-token (symbolic-token more-output-port))
	 (index (value more-output-port-token))
	 (input-port (first (inputs task)))
	 (input-token (symbolic-token input-port))
	 (lower-bound (lower-bound input-token))
	 (upper-bound (upper-bound input-token)))
    ;; The next thing in the queue ought to be two branches and their bodies
    ;; so if we start gobbling we'll get the branch code
    (setf (form more-output-port-token) index)
    (multiple-value-bind (more-branch-code empty-branch-code) (gobble-more-and-empty-branch-codes task language)
      (build-index-enumerator-code-for-language task language index lower-bound upper-bound more-branch-code empty-branch-code)
    )))

(defparameter *trace-enumerator-gobbling* nil)
(defmethod gobble-more-and-empty-branch-codes (searching-task language)
  (declare (ignore searching-task))
  (let ((more-branch-code nil)
	(empty-branch-code nil))
    (labels ((gobble-branch ()
	       (let* ((next-guy (pop *all-tasks*))
		      (branch-name (when next-guy (name next-guy))))
		 (case branch-name
		   (more (setq more-branch-code (let ((*trace-gobble* *trace-enumerator-gobbling*))
						  (sub-gobble (code language next-guy) code))))
		   (empty (setq empty-branch-code (sub-gobble (code language next-guy) code)))
		   (t (error "Where's the other branch")))
		 )))
      (gobble-branch)
      (gobble-branch))
    (values more-branch-code empty-branch-code)))

(defmethod build-index-enumerator-code-for-language ((task primitive-enumerator) (language (eql :lisp)) 
						   index lower-bound upper-bound more-branch-code empty-branch-code)
  (let ((how-to-handle-finally (getf (properties task) :finally-option)))
    `(loop for ,index from ,lower-bound below ,upper-bound
	 do ,@more-branch-code
	    ,@(when empty-branch-code 
		(case how-to-handle-finally
		  (:empty ())
		  (:return `(finally (return ,@empty-branch-code)))
		  (:for-effect `(finally ,@empty-branch-code)))
		))))

;;; (loop-variable &key init refresh terminate body)
(defmethod build-index-enumerator-code-for-language ((task primitive-enumerator) (language (eql :java)) 
						   index lower-bound upper-bound more-branch-code empty-branch-code)
  ;; This is to prevent a newline being printed
  (let ((how-to-handle-finally (getf (properties task) :finally-option)))
    (when (eql (first upper-bound) :application)
      (setq upper-bound (cons :application (rest upper-bound))))
    `(:for (:variable ,index)
	   :init ,lower-bound
	   :refresh (:increment (:variable ,index))
	   :terminate (:binary-comparison < (:variable ,index) ,upper-bound)
	   :body ,more-branch-code
	   :exit-code ,(case how-to-handle-finally
			 (:empty ())
			 ;; Fix: this is probably wrong
			 (:return `(:return ,@empty-branch-code))
			 (:effect empty-branch-code))				
	   :abnormal-exit? ,(abnormal-exit? task)
	   )))

(defmethod form-for ((task primitive-enumerator) (task-type (eql 'list-enumerator)) (language t))
  (let* ((more-branch (branch-named 'more task))
	 (more-output-port (first (outputs more-branch)))
	 (more-output-port-token (symbolic-token more-output-port))
	 (element (value more-output-port-token))
	 (input-port (first (inputs task)))
	 (the-list-token (symbolic-token input-port))
	 (the-list (value-to-use the-list-token language)))
    (multiple-value-bind (more-branch-code empty-branch-code) (gobble-more-and-empty-branch-codes task language)
      (build-list-enumerator-code language element the-list more-branch-code empty-branch-code))))

(defmethod build-list-enumerator-code ((language (eql :lisp)) element the-list more-branch-code empty-branch-code)
  `(loop for ,element in ,the-list
       do ,@more-branch-code
	  ,@(when empty-branch-code 
	      `(finally ,@empty-branch-code))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set related stuff
;;;   Add-to-Set
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fix:
(defmethod function-name-to-use ((task task-interface) (type (eql 'add-to-set)) (language (eql :lisp)))
  (if (getf (properties task) :unique?) `adjoin `cons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List related things
;;;  left, right, first, rest
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fix

(defmethod function-name-to-use ((task task-interface) (type (eql 'right)) (language (eql :lisp)))
  'rest)

(defmethod function-name-to-use ((task task-interface) (type (eql 'left)) (language (eql :lisp)))
  'first)

(defmethod function-name-to-use ((task task-interface) (type (eql 'first)) (language (eql :lisp)))
  'first)

(defmethod function-name-to-use ((task task-interface) (type (eql 'rest)) (language (eql :lisp)))
  'rest)



