;;; -*- Mode: Common-lisp; Package: Natsoft -*-

(deftask sorted-list-test
    :parameters ()
    :interface ((:inputs (the-list (list integer)))
		(:outputs (the-value boolean))))

(defmethod canonical-arguments-for ((type (eql 'sorted-list-test)) the-thing)
  (declare (ignore the-thing))
  nil)

(defreduction sorted-list-test-reduce (sorted-list-test ())
  :reduce-to (for-all-cliche))

(defcliche for-all-cliche ()
  :initial-inputs (
                   (:name THE-LIST :type (LIST INTEGER))
                   )
  :final-outputs (
                  (:name THE-VALUE :type BOOLEAN)
                  )
  :constants (
              (:name TRUE :type BOOLEAN :value TRUE)
              (:name FALSE :type BOOLEAN :value FALSE)
              )
  :state-elements (
                   (:name LAST :direction source :port-name LAST :port-type INTEGER :state last)
                   (:name NEXT-LAST :direction sink :port-name LAST :port-type INTEGER :state last)
                   (:name INITIAL-LAST :direction sink :port-name LAST :port-type INTEGER :state last)
                   (:name RETURN-VALUE :direction source :port-name VALUE :port-type BOOLEAN :state value)
                   (:name VALUE :direction sink :port-name VALUE :port-type BOOLEAN :state value)
                   )
  :components (
               (:name ENUM :type LIST-ENUMERATOR :ELEMENT-TYPE INTEGER)
               (:name TAKE :type TAKE :INPUT-CONTAINER-TYPE TEMPORAL-SEQUENCE :ELEMENT-TYPE INTEGER)
               (:name TR :type TRUNCATE :ELEMENT-TYPE (TEMPORAL-SEQUENCE INTEGER))
               (:name TEST :type LESS-THAN-TEST)
               (:name FIRST :type FIRST :OUTPUT-TYPE INTEGER :INPUT-TYPE (LIST INTEGER))
               (:name REST :type REST :OUTPUT-TYPE (LIST INTEGER) :INPUT-TYPE (LIST INTEGER))
               )
  :dataflows (
              ((:component ENUM :port LIST-ELEMENTS :branch MORE) (:component TAKE :port SEQUENCE-DATA))
              ((:component ENUM :port LIST-ELEMENTS :branch MORE) (:component TR :port DATA))
              ((:component TAKE :port DATA) (:component NEXT-LAST :port LAST))
              ((:component TAKE :port DATA) (:component TEST :port NUMBER-2))
              ((:component FIRST :port THE-PART) (:component INITIAL-LAST :port LAST))
              ((:component REST :port THE-PART) (:component ENUM :port THE-LIST))
              ((:component LAST :port LAST) (:component TEST :port NUMBER-1))
              ((:component RETURN-VALUE :port VALUE) (:component THE-VALUE :port THE-VALUE))
              ((:component TRUE :port TRUE) (:component VALUE :port VALUE))
              ((:component FALSE :port FALSE) (:component RETURN-VALUE :port INITIAL-VALUE))
              ((:component THE-LIST :port THE-LIST) (:component FIRST :port THE-CONTAINER))
              ((:component THE-LIST :port THE-LIST) (:component REST :port THE-CONTAINER))
              )
  :control-flows (
                  ((:component ENUM :branch EMPTY) (:component VALUE))
                  ((:component TEST :branch NO) (:component TR))
                  ((:component FIRST) (:component ENUM))
                  )
  )

