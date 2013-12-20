(in-package :lisp-unit2)

;;; Assert macros

(defmacro assert-eq (expected form &rest extras)
  "Assert whether expected and form are EQ."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
  "Assert whether expected and form are EQL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
  "Assert whether expected and form are EQUAL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
  "Assert whether expected and form are EQUALP."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equalp))

(defmacro assert-error (condition form &rest extras)
  "Assert whether form signals condition."
  `(expand-assert :error ,form (expand-error-form ,form)
                  ,condition ,extras))

(defmacro assert-warning (condition form &rest extras)
  (alexandria:with-unique-names (signalled)
    `(let (,signalled)
      (handler-bind ((warning #'(lambda (c)
                                  (when (eql (type-of c) ,condition)
                                    (setf ,signalled T)
                                    (muffle-warning c)))))
        ,form)
      (assert-true ,signalled
       "Expected condition" ,condition ,@extras))))

(defmacro assert-no-warning (condition form &rest extras)
  (alexandria:with-unique-names (error)
    `(let (,error)
      (handler-bind ((warning #'(lambda (c)
                                  (when (eql (type-of c) ,condition)
                                    (setf ,error c)))))
        ,form)
      (assert-false ,error
       "Undesired condition signaled" ,error ,condition ,@extras))))

(defmacro assert-expands (expansion form &rest extras)
  "Assert whether form expands to expansion."
  `(expand-assert :macro ,form
                  (expand-macro-form ,form nil)
                  ',expansion ,extras))

(defmacro assert-false (form &rest extras)
  "Assert whether the form is false."
  `(expand-assert :result ,form ,form nil ,extras))

(defmacro assert-equality (test expected form &rest extras)
  "Assert whether expected and form are equal according to test."
  `(expand-assert :equal ,form ,form ,expected ,extras :test ,test))

(defmacro assert-prints (output form &rest extras)
  "Assert whether printing the form generates the output."
  `(expand-assert :output ,form (expand-output-form ,form)
                  ,output ,extras))

(defmacro assert-true (form &rest extras)
  "Assert whether the form is true."
  `(expand-assert :result ,form ,form t ,extras))

(defmacro expand-assert (type form body expected extras &key (test '#'eql))
  "Expand the assertion to the internal format."
  `(internal-assert ,type ',form
    (lambda () ,body)
    (lambda () ,expected)
    (expand-extras ,extras)
    ,test
    :full-form '(,type ,expected ,form)))

(defmacro expand-error-form (form)
  "Wrap the error assertion in HANDLER-CASE."
  `(handler-case ,form
     (condition (error) error)))

(defmacro expand-output-form (form)
  "Capture the output of the form in a string."
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output*
             (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defmacro expand-macro-form (form env)
  "Expand the macro form once."
  `(let ((*gensym-counter* 1))
     (macroexpand-1 ',form ,env)))

(defmacro expand-extras (extras)
  "Expand extra forms."
  `(lambda ()
     (list ,@(mapcan (lambda (form) (list `',form form)) extras))))

(defgeneric assert-result (type test expected actual)
  (:documentation
   "Return the result of the assertion."))

(defgeneric record-failure (type form actual expected extras test)
  (:documentation
   "Record the details of the failure."))

(defclass failure-result ()
  ((unit-test :accessor unit-test :initarg :unit-test :initform *unit-test*)
   (form :accessor form :initarg :form :initform nil)
   (actual :accessor actual :initarg :actual :initform nil)
   (expected :accessor expected :initarg :expected :initform nil)
   (extras :accessor extras :initarg :extras :initform nil)
   (test :accessor test :initarg :test :initform nil))
  (:documentation
   "Failure details of the assertion."))

(defun %record-failure (class form actual expected extras test)
  "Return an instance of the failure result."
  (make-instance class
                 :form form
                 :actual actual
                 :expected expected
                 :extras extras
                 :test test))

(defclass equal-result (failure-result)
  ()
  (:documentation
   "Result of a failed equal assertion."))

(defmethod assert-result ((type (eql :equal)) test expected actual)
  "Return the result of an equal assertion."
  (and
   (<= (length expected) (length actual))
   (every test expected actual)))

(defmethod record-failure ((type (eql :equal))
                           form actual expected extras test)
  "Return an instance of an equal failure result."
  (%record-failure 'equal-result form actual expected extras test))

(defclass error-result (failure-result)
  ()
  (:documentation
   "Result of a failed error assertion."))

(defmethod assert-result ((type (eql :error)) test expected actual)
  "Return the result of an error assertion."
  (declare (ignore test))
  (or
   (eql (car actual) (car expected))
   (typep (car actual) (car expected))))

(defmethod record-failure ((type (eql :error))
                           form actual expected extras test)
  "Return an instance of an error failure result."
  (%record-failure 'error-result form actual expected extras test))

(defclass macro-result (failure-result)
  ()
  (:documentation
   "Result of a failed macro expansion assertion."))

(defun %expansion-equal (form1 form2)
  "Descend into the forms checking for equality."
  (let ((item1 (first form1))
        (item2 (first form2)))
    (cond
     ((and (null item1) (null item2)))
     ((and (listp item1) (listp item2))
      (and
       (%expansion-equal item1 item2)
       (%expansion-equal (rest form1) (rest form2))))
     ((and (symbolp item1) (symbolp item2))
      (and
       (string= (symbol-name item1) (symbol-name item2))
       (%expansion-equal (rest form1) (rest form2))))
     (t (and
         (equal item1 item2)
         (%expansion-equal (rest form1) (rest form2)))))))

(defmethod assert-result ((type (eql  :macro)) test expected actual)
  "Return the result of a macro assertion."
  (declare (ignore test))
  (%expansion-equal (first expected) (first actual)))

(defmethod record-failure ((type (eql :macro))
                           form actual expected extras test)
  "Return an instance of a macro failure result."
  (%record-failure 'macro-result form actual expected extras test))

(defclass boolean-result (failure-result)
  ()
  (:documentation
   "Result of a failed boolean assertion."))

(defmethod assert-result ((type (eql :result)) test expected actual)
  "Return the result of a result assertion."
  (declare (ignore test))
  (logically-equal (car actual) (car expected)))

(defmethod record-failure ((type (eql :result))
                           form actual expected extras test)
  "Return an instance of a boolean failure result."
  (%record-failure 'boolean-result form actual expected extras test))

(defclass output-result (failure-result)
  ()
  (:documentation
   "Result of a failed output assertion."))

(defmethod assert-result ((type (eql :output)) test expected actual)
  "Return the result of an output assertion."
  (declare (ignore test))
  (string=
   (string-trim '(#\newline #\return #\space) (car actual))
   (car expected)))

(defmethod record-failure ((type (eql :output))
                           form actual expected extras test)
  "Return an instance of an output failure result."
  (%record-failure 'output-result form actual expected extras test))

(define-condition assertion-pass (condition)
  ((unit-test :accessor unit-test :initarg :unit-test :initform nil)
   (assertion :accessor assertion :initarg :assertion :initform nil)))

(define-condition assertion-fail (condition)
  ((unit-test :accessor unit-test :initarg :unit-test :initform nil)
   (assertion :accessor assertion :initarg :assertion :initform nil)
   (failure :accessor failure :initarg :failure :initform nil)))

(defun internal-assert
    (type form code-thunk expected-thunk extras test &key full-form)
  "Perform the assertion and record the results."
  (let* ((actual (multiple-value-list (funcall code-thunk)))
         (expected (multiple-value-list (funcall expected-thunk)))
         (result (assert-result type test expected actual)))
    (with-simple-restart (abort "Cancel this assertion")
      (if result
          (signal 'assertion-pass
                  :unit-test *unit-test*
                  :assertion (or full-form form))
          (signal
           'assertion-fail
           :unit-test *unit-test* :assertion (or full-form form)
           :failure (record-failure
                     type full-form actual expected
                     (when extras (funcall extras)) test))))
    ;; Return the result
    result))