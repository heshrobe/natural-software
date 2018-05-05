;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Describing things using Joshus predicates
;;;  This is a more free-form type and expressive type of thing than deftask
;;;  and it will always be possible to build elements like the
;;;  above by calling the basic-task primitives that deftask
;;;  expands into or to generate a deftask if desired
;;;
;;;  Note that we also need to describe data structures
;;;  as well as computations so we'll need something besides deftask
;;;  anyhow
;;;
;;;  The convention is that things ending with -of refer to instance
;;;                                beginning with has- refer to types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :natsoft)

;;; just so we can find all types
(define-predicate is-type (type) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; the subclassing relationships
;;; direct sub type
(define-predicate subtype (sub-type super-type) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; New-type is an abbreviation for definition typically.
;;; Anything of type New-Type is of type definition and vice versa.
(define-predicate definition (new-type definition) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; inherited sub type
(define-predicate is-of-type (sub-type super-type) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate union-member (sub-type super-type) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; We might possibly want something like definition-of: [definition-of bit-vector (vector bit)]

;;; We definitely want something like can-be-viewed as:
;;; For example: An array can be viewed as a vector of columns where a column is a vector (or as a vector of rows)
;;; That's the easy part, that more complicated part is explaining the mapping
;;; So when viewing an array as a vector of columns: Each column is a pair of the array and a column index
;;; The way in which the column is a vector is that (element column-j i) = (element array i j)
;;; where column-j is just (array j).  Similar for rows.
;;; This also makes clear that we need to have something that describes the operations and observation possible on an
;;; object.  For vectors that includes element-of (an observer) and set-element (an operation).  Operations are described by their pre
;;; and post-observations (co-algebraic description).

(define-predicate can-be-viewed-as (type target-type name-of-viewpoint) (ltms:ltms-predicate-model))
(define-predicate viewpoint-equivalence (viewpoint-of-something value) (ltms:ltms-predicate-model))
(define-predicate abstract-operator (operator-name) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate is-viewpoint (viewpoint-name operator-name) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; These describe the class not the instance
(define-predicate has-property (procedure-type property-name) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate typical-property-value-of (procedure-type property-name value) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; See branch and join related stuff below
;;; Basically any procedure can split at its output.   Then it has a set of output-ports for each branch.
;;; We have a separate predicate for branch-port descriptions
(define-predicate has-port (procedure-type direction-or-branch-name port-name governing-parameter) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; This is a property that applies to the values at a port other than the port-type-constraint
;;; but it appears that this isn't used
;;; (define-predicate has-port-property (procedure-type direction-or-branch-name port-name property) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; nor this
;;; (define-predicate port-property-computable (procedure-type direction port-name property) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate has-branch-port (procedure-type branch-name port-name governing-parameter) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-join-port  (procedure-type join-name port-name governing-parameter) (no-variables-in-data-mixin ltms:ltms-predicate-model))


;;; the data type can be a pattern, where when applied to an instance, elements that are properties of the type
;;; are replaced by the corresponding concrete properties of the instance.
(define-predicate has-data-type (procedure-type direction-or-branch-or-join-name port-name data-type) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-dependent-data-subtype (procedure-type direction-or-branch-or-join-name port-name property-name parameter-name)
  (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate has-component (procedure-type name sub-procedure) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-dataflow (procedure-type from-component-name from-port-name to-component-name to-port-name) 
  (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-controlflow (procedure-type from-component-name from-branch-name to-component-or-join-name) 
  (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; These allow specification of pre- and post-conditions.
;;; I'm assuming that input and output ports are referred to by (input name) (output name) when there's an ambiguity
(define-predicate has-precondition (procedure-type condition) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-postcondition (procedure-type condition) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; Any task can terminate with branches each of which has a control port as well as data ports
;;; For static descriptions we need to specify that there are branches with names
(define-predicate has-branch (procedure-type branch-name) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate has-branch-condition (procedure-type branch-name condition) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; joins don't have conditions
(define-predicate has-join (procedure-type join-name) (no-variables-in-data-mixin ltms:ltms-predicate-model))

;;; for example you can observe the (i j) element of an array
(define-predicate has-observation (data-type operation qualifiers) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate has-part (data-type component type-constraint) (no-variables-in-data-mixin ltms:ltms-predicate-model))


;;; These describe the instance not the type
;;; relates an object to its type

(define-predicate type-of (component type) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate component-of (computation component) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate port-of (computation-or-branch port) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate branch-of (component branch) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate join-of (component join) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate dataflow-from (flow from-port to-port) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate controlflow-from (flow from-branch to-task) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate branch-name-of (final-output name) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate join-name-of (entry-point name) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate property-value-of (thing property-name value) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; For exmaple the rate of the data port of the camera is 8 images per second
(define-predicate port-property-value-of (port property-name value) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; for example the values of the output port of the imager is required to be of type image
(define-predicate port-type-constraint (port type-constraint) (no-variables-in-data-mixin ltms:ltms-predicate-model))
;;; for example the i-j element of this array is currently 22
(define-predicate observation-value-of (observation-name object qualifiers value) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate typical-value-of (port value) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate typical-element-of (container value) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate precondition-of (component precondition) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate postcondition-of (component postcondition) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate less-than (thing1 thing2) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate identical (thing1 thing2) (no-variables-in-data-mixin ltms:ltms-predicate-model))
(define-predicate equals (thing1 thing2) (no-variables-in-data-mixin ltms:ltms-predicate-model))