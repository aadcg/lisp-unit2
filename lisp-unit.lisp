;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.


How to use
----------

1. Read the documentation at:
   https://github.com/OdonataResearchLLC/lisp-unit/wiki

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS :ALL)
to clear any previously defined tests.

3. Load this file.

4. (use-package :lisp-unit)

5. Load your code file and your file of tests.

6. Test your code with (RUN-TESTS '(test-name1 test-name2 ...)) or
simply (RUN-TESTS :ALL) to run all defined tests.

A summary of how many tests passed and failed will be printed.

NOTE: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

|#

;;; Packages

(in-package :cl-user)

(defpackage :lisp-unit
  (:use :common-lisp :iter)
  ;; Print parameters
  (:export :*print-summary*
           :*print-failures*
           :*print-errors*)
  ;; Forms for assertions
  (:export :assert-eq
           :assert-eql
           :assert-equal
           :assert-equalp
           :assert-equality
           :assert-prints
           :assert-expands
           :assert-true
           :assert-false
           :assert-error)
  ;; Functions for managing tests
  (:export :define-test
           :list-tests
           :test-code
           :test-documentation
           :remove-tests
           :run-tests
           :use-debugger)
  ;; Functions for managing tags
  (:export :list-tags
           :tagged-tests
           :remove-tags
           :run-tags)
  ;; Functions for reporting test results
  (:export :test-names
           :failed-tests
           :error-tests
           :missing-tests
           :print-failures
           :print-errors
           :summarize-results)
  ;; Functions for extensibility via signals
  (:export :test-run-complete
           :results)
  ;; Utility predicates
  (:export :logically-equal :set-equal))

(in-package :lisp-unit)
(cl-interpol:enable-interpol-syntax)

(defvar *log-level* 1)

(defun %ts (&optional (time (get-universal-time)))
  "returns a date as {y}{mon}{d}-{h}{min}{s}, defaults to get-universal-time
   intended for use in datestamping filenames
  "
  (multiple-value-bind ( s min h d mon y  )
      (decode-universal-time time)
    (format nil "~d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"  y mon d h min s)))

(defun %log (message &key (level 0))
  (when (<= *log-level* level)
    (format *test-log-stream* message)))

(defmacro %log-around ((message &key (level 0))&body body)
  `(unwind-protect
    (progn
      (%log #?"${ (%ts) } START ${,message}" :level ,level)
      ,@body)
    (%log #?"${ (%ts) }   END ${,message}" :level ,level)))

(defvar *test-stream* *standard-output*)
(defvar *test-log-stream* *test-stream*)

;;; Global counters


;;; Global options

(defparameter *print-summary* t
  "Print a summary of the pass, fail, and error count if non-nil.")

(defparameter *print-failures* t
  "Print failure messages if non-NIL.")

(defparameter *print-errors* t
  "Print error messages if non-NIL.")

;;; Global unit test database

(defparameter *test-db* (make-hash-table :test #'eq)
  "The unit test database is simply a hash table.")

(defun null-tests-warning-report (null-tests-warning stream)
  "Write the null-tests-warning to the stream."
  (format stream "No tests defined for package ~A."
          (tests-package-name null-tests-warning)))

(define-condition null-tests-warning (simple-warning)
  ((name
    :type string
    :initarg :name
    :reader tests-package-name))
  (:report null-tests-warning-report))

(defun package-table (package &optional create)
  (let ((packobj (find-package package)))
    (cond
     ((gethash packobj *test-db*))
     (create
      (setf (gethash packobj *test-db*) (make-hash-table)))
     (t (warn 'null-tests-warning :name (package-name package))))))

(defmacro with-package-table ((table
                               &optional (package *package*) create)
                              &body body)
  "Execute the body only if the package table exists."
  (let ((gtable (gensym "TABLE-")))
    `(let* ((,gtable (package-table ,package ,create))
            (,table ,gtable))
       (when (hash-table-p ,gtable) ,@body))))

;;; Global tags database

(defparameter *tag-db* (make-hash-table :test #'eq)
  "The tag database is simply a hash table.")

(defun null-tags-warning-report (null-tags-warning stream)
  "Write the null-tags-warning to the stream."
  (format stream "No tags defined for package ~A."
          (tags-package-name null-tags-warning)))

(define-condition null-tags-warning (simple-warning)
  ((name
    :type string
    :initarg :name
    :reader tags-package-name))
  (:report null-tags-warning-report))

(defun package-tags (package &optional create)
  "Return the tags DB for the package."
  (let ((packobj (find-package package)))
    (cond
     ((gethash packobj *tag-db*))
     (create
      (setf (gethash packobj *tag-db*) (make-hash-table)))
     (t (warn 'null-tags-warning :name (package-name package))))))

(defmacro with-package-tags ((table
                              &optional (package *package*) create)
                             &body body)
  "Execute the body only if the package tags exists."
  (let ((gtable (gensym "TABLE-")))
    `(let* ((,gtable (package-tags ,package ,create))
            (,table ,gtable))
       (when (hash-table-p ,gtable) ,@body))))

;;; Unit test definition

(defvar *unit-test* nil
  "The currently executing unit test")

(defclass unit-test ()
  ((name :accessor name :initarg :name :initform nil)
   (doc :accessor doc :initarg :doc :initform nil)
   (code :accessor code :initarg :code :initform nil
         :documentation "The forms to produce the fn")
   (tags :accessor tags :initarg :tags :initform nil))
  (:documentation
   "Organize the unit test documentation and code."))

(defgeneric test-thunk-name (test)
  (:method ((u unit-test))
    (test-thunk-name (name u)))
  (:method ((u symbol))
    (symbol-munger:english->lisp-symbol
     (list u 'test-thunk)
     (symbol-package u))))

(defmethod test-thunk ((u unit-test))
  (compile-unit-test u)
  (symbol-function (test-thunk-name u)))

(defmethod %compile ((u unit-test))
  (%log-around (#?"Compiling Test: ${ (name u) }" :level 0)
    (compile (test-thunk-name u)
             `(lambda (),(doc u)
               (%log-around (#?"Running Test: ${ (name u) }" :level 1)
                 ,@(code u))))))


(defun test-name-error-report (test-name-error stream)
  "Write the test-name-error to the stream."
  (format stream "Test name ~S is not of type ~A."
          (type-error-datum test-name-error)
          (type-error-expected-type test-name-error)))

(define-condition test-name-error (type-error)
  ()
  (:default-initargs :expected-type 'symbol)
  (:report test-name-error-report)
  (:documentation
   "The test name error is a type error."))

(defun valid-test-name (name)
  "Signal a type-error if the test name is not a symbol."
  (if (symbolp name)
      name
      (error 'test-name-error :datum name)))

(defmethod install-test ((u unit-test) &optional (package *package*))
  (%compile u)
  (with-package-tags (tags package t)
    (iter (for tag in (tags u))
      (push (name u) (gethash tag tags))))
  (with-package-table (tests package t)
    (setf (gethash (name u) tests) u)))

(defgeneric uninstall-test (test)
  (:method ((u symbol)
            &aux (package (symbol-package u)))
    (ignore-errors (fmakunbound u))
    (ignore-errors (fmakunbound (test-thunk-name u)))
    (with-package-table (tests package) (remhash u tests))
    (with-package-tags (tags package)
      (iter (for (tag tests) in tags)
        (setf (gethash tag tags) (remove u tests)))))
  (:method ((u unit-test))
    (uninstall-test (name u))))


(defmacro define-test (name (&rest tags) &body body)
  `(let ((unit-test
          (make-instance 'unit-test
           :name ',name
           :doc ,(when (stringp (first body)) (first body))
           :tag '(,@tags)
           :code '(,@body))))
    (install-test unit-test)
    (defun ,name ()
      (%run-test unit-test))))

;;; Manage tests

(defun list-tests (&optional (package *package*))
  "Return a list of the tests in package."
  (with-package-table (table package)
    (loop for test being each hash-value in table
          collect test)))

(defun test-documentation (name &optional (package *package*))
  "Return the documentation for the test."
  (with-package-table (table package)
    (let ((unit-test (gethash name table)))
      (if (null unit-test)
          (warn "No test ~A in package ~A."
                name (package-name package))
          (doc unit-test)))))

(defun test-code (name &optional (package *package*))
  "Returns the code stored for the test name."
  (with-package-table (table package)
    (let ((unit-test (gethash name table)))
      (if (null unit-test)
          (warn "No test ~A in package ~A."
                name (package-name package))
          (code unit-test)))))

(defun remove-tests (&optional (names :all) (package *package*))
  "Remove individual tests or entire sets."
  (if (eq :all names)
      (if (null package)
          (clrhash *test-db*)
          (progn
            (remhash (find-package package) *test-db*)
            (remhash (find-package package) *tag-db*)))
      (progn
        ;; Remove tests
        (with-package-table (table package)
          (loop for name in names
                unless (remhash name table) do
                (warn "No test ~A in package ~A to remove."
                      name (package-name package))))
        ;; Remove tests from tags
        (with-package-tags (tags package)
          (loop for tag being each hash-key in tags
                using (hash-value tagged-tests)
                do
                (setf
                 (gethash tag tags)
                 (set-difference tagged-tests names)))))))

;;; Manage tags

(defun list-tags (&optional (package *package*))
  "Return a list of the tags in package."
  (with-package-tags (table package)
    (loop for tag being each hash-key in table collect tag)))

(defun tagged-tests (&optional (tags :all) (package *package*))
  "Return a list of the tests associated with the tags."
  (with-package-tags (table package)
    (remove-duplicates
     (iter (for tag in (case tags
                         (:all (list-tags package))
                         (t (alexandria:ensure-list tags))))
       (appending (gethash tag table))))))

(defun remove-tags (&optional (tags :all) (package *package*))
  "Remove individual tags or entire sets."
  (if (eq :all tags)
      (if (null package)
          (clrhash *tag-db*)
          (remhash (find-package package) *tag-db*))
      (with-package-tags (tag-table package)
        (loop for tag in tags
              unless (remhash tag tag-table) do
              (warn "No tag ~A in package ~A to remove."
                    tag (package-name package))))))

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
                    ,test))

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
  ((form
    :initarg :form
    :reader form)
   (actual
    :type list
    :initarg :actual
    :reader actual)
   (expected
    :type list
    :initarg :expected
    :reader expected)
   (extras
    :type list
    :initarg :extras
    :reader extras)
   (test
    :type function
    :initarg :test
    :reader test))
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
       (type form code-thunk expected-thunk extras test)
  "Perform the assertion and record the results."
  (let* ((actual (multiple-value-list (funcall code-thunk)))
         (expected (multiple-value-list (funcall expected-thunk)))
         (result (assert-result type test expected actual)))
    (if result
        (signal 'assertion-pass :unit-test *unit-test* :assertion form)
        (signal 'assertion-fail :unit-test *unit-test* :assertion form
                :failure (record-failure type form actual expected
                                         (when extras (funcall extras)) test)))
    ;; Return the result
    result))

;;; Test results database

(defclass test-results-db ()
  ((database
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader database)
   (pass
    :type fixnum
    :initform 0
    :accessor pass)
   (fail
    :type fixnum
    :initform 0
    :accessor fail)
   (exerr
    :type fixnum
    :initform 0
    :accessor exerr)
   (failed-tests
    :type list
    :initform ()
    :accessor failed-tests)
   (error-tests
    :type list
    :initform ()
    :accessor error-tests)
   (missing-tests
    :type list
    :initform ()
    :accessor missing-tests))
  (:documentation
   "Store the results of the tests for further evaluation."))

(defmethod print-object ((object test-results-db) stream)
  "Print the summary counts with the object."
  (let ((pass (pass object))
        (fail (fail object))
        (exerr (exerr object)))
    (format
     stream "#<~A Total(~D) Passed(~D) Failed(~D) Errors(~D)>~%"
     (class-name (class-of object))
     (+ pass fail) pass fail exerr)))

(defun test-names (test-results-db)
  "Return a list of the test names in the database."
  (loop for name being each hash-key in (database test-results-db)
        collect name))

(defun record-result (test-name code results)
  "Run the test code and record the result."
  (let ((result (run-test-thunk test-name code)))
    ;; Store the result
    (setf (gethash test-name (database results)) result)
    ;; Count passed tests
    (when (plusp (pass result))
      (incf (pass results) (pass result)))
    ;; Count failed tests and record the name
    (when (fail result)
      (incf (fail results) (length (fail result)))
      (push test-name (failed-tests results)))
    ;; Count errors and record the name
    (when (exerr result)
      (incf (exerr results))
      (push test-name (error-tests results)))
    ;; Running output
    (when *print-failures* (print-failures result))
    (when *print-errors* (print-errors result))
    (when (or *print-summary* *print-failures* *print-errors*)
      (print-summary result))))

(defmethod print-summary ((results test-results-db) &optional (stream *test-stream*))
  "Print a summary of all results to the stream."
  (let ((pass (pass results))
        (fail (fail results)))
    (format stream "~&Unit Test Summary~%")
    (format stream " | ~D assertions total~%" (+ pass fail))
    (format stream " | ~D passed~%" pass)
    (format stream " | ~D failed~%" fail)
    (format stream " | ~D execution errors~%" (exerr results))
    (format stream " | ~D missing tests~2%"
            (length (missing-tests results)))))

;;; Run the tests
(define-condition missing-test (warning)
  ((test-name :accessor test-name :initarg :test-name :initform nil))
  (:documentation
   "Signaled when a single test is finished."))

(define-condition test-complete ()
  ((run :accessor run :initarg :run :initform nil))
  (:documentation
   "Signaled when a single test is finished."))

(define-condition test-run-complete ()
  ((results
    :type 'test-results-db
    :initarg :results
    :reader results))
  (:documentation
   "Signaled when a test run is finished."))

(defun %package-tests (package)
  (with-package-table (table package)
    (iter (for (name test) in-hashtable table)
      (collect test))))

(defun %package-test (name &optional (package *package*))
  (or (with-package-table (table (symbol-package name))
        (gethash name table))
      (with-package-table (table package)
        (gethash name table))))

(defun %get-tests (&key
                   (tests :all)
                   (tags nil)
                   (package *package*))
  (cond
    ((eql :all tests) (%package-tests package))
    ((eql :all tags) (tagged-tests))
    (t
     (iter (for name in (append (alexandria:ensure-list tests)
                                (tagged-tests tags)))
       (for test = (etypecase name
                     (null nil)
                     (unit-test name)
                     (symbol (%package-test name))))
       (if test
           (collect test)
           (warn 'missing-test :test-name test))))))

(defun run-tests (&key
                  (tests :all)
                  (tags nil)
                  (package *package*)
                  &aux (results (make-instance 'test-results-db)))
  "Run the specified tests in package."
  (handler-bind
      ((missing-test (lambda (c) (push (test-name c) (missing-tests results)))))
    (iter (for test in (%get-tests :tests tests :tags tags :package package))
      (run-test test)
      *most-recent-run*
      ))
  (signal 'test-run-complete :results results)
  (summarize-results results)
  results)

(defvar *most-recent-run* nil)

(defclass unit-test-run ()
  ((unit-test :accessor unit-test :initarg :unit-test :initform nil)
   (return-value :accessor return-value :initarg :return-value :initform nil)
   (start-time :accessor start-time :initarg :start-time :initform (get-universal-time))
   (end-time :accessor end-time :initarg :end-time :initform nil)
   (errors :accessor errors :initarg :errors :initform nil)
   (warnings :accessor warnings :initarg :warnings :initform nil)
   (passed :accessor passed :initarg :passed :initform nil)
   (failed :accessor failed :initarg :failed :initform nil)))

(defmethod print-summary ((run unit-test-run) &optional (stream *test-stream*))
  "Print a summary of the test result."
  (format stream "~&~A: ~S assertions passed"
          (name (unit-test run))
          (length (pass run)))
  (when (failed run)
    (format stream ", ~S failed~%"
            (length (failed run)))
    (mapcar #'print-failures (failed run)))
  (when (errors run)
    (format stream ", ERROR: ~%    ~A~%    ~S" (errors run) (errors run)))
  (format stream "~%~%"))

(defgeneric run-test (test)
  (:method ((n symbol)) (run-test (%get-tests :tests n)))
  (:method ((u unit-test)
            &aux (run (make-instance 'unit-test-run :unit-test u))
            (*unit-test* u))
    (setf *most-recent-run* run)
    (unwind-protect
         (handler-bind
             ((assertion-pass (lambda (c) (push (assertion c) (passed run))))
              (assertion-fail (lambda (c) (push (failure c) (failed run))))
              (error (lambda (c) (push c (errors run))))
              (warning (lambda (c) (push c (warnings run)))))
           (%compile u)
           ;; run the test code
           (setf (return-value run)
                 (funcall (test-thunk u))))
      run
      (setf (end-time run) (get-universal-time))
      (signal 'test-complete :run run))
    run))

;;; Print failures

(defgeneric print-failures (result &optional stream)
  (:documentation
   "Report the results of the failed assertion."))

(defmethod print-failures :around ((result failure-result) &optional
                                   (stream *standard-output*))
  "Failure header and footer output."
  (format stream "~& | Failed Form: ~S" (form result))
  (call-next-method)
  (when (extras result)
    (format stream "~{~& | ~S => ~S~}~%" (extras result)))
  (format stream "~& |~%"))

(defmethod print-failures ((result failure-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Expected ~{~S~^; ~} " (expected result))
  (format stream "~<~% | ~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result error-result) &optional
                           (stream *standard-output*))
  (format stream "~& | ~@[Should have signalled ~{~S~^; ~} but saw~]"
          (expected result))
  (format stream " ~{~S~^; ~}" (actual result)))

(defmethod print-failures ((result macro-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Should have expanded to ~{~S~^; ~} "
          (expected result))
  (format stream "~<~%~:;but saw ~{~S~^; ~}~>" (actual result)))

(defmethod print-failures ((result output-result) &optional
                           (stream *standard-output*))
  (format stream "~& | Should have printed ~{~S~^; ~} "
          (expected result))
  (format stream "~<~%~:;but saw ~{~S~^; ~}~>"
          (actual result)))

(defmethod print-failures ((result test-result) &optional
                           (stream *standard-output*))
  "Print the failed assertions in the unit test."
  (loop for fail in (fail result) do
        (print-failures fail stream)))

(defmethod print-failures ((results test-results-db) &optional
                           (stream *standard-output*))
  "Print all of the failure tests."
  (loop with db = (database results)
        for test in (failed-tests results)
        as result = (gethash test db)
        do
        (print-failures result stream)
        (print-summary result stream)))

;;; Print errors

(defgeneric print-errors (result &optional stream)
  (:documentation
   "Print the error condition."))

(defmethod print-errors ((result test-result) &optional
                         (stream *standard-output*))
  "Print the error condition."
  (let ((exerr (exerr result))
        (*print-escape* nil))
    (when exerr
      (format stream "~& | Execution error:~% | ~W" exerr)
      (format stream "~& |~%"))))

(defmethod print-errors ((results test-results-db) &optional
                         (stream *standard-output*))
  "Print all of the error tests."
  (loop with db = (database results)
        for test in (error-tests results)
        as result = (gethash test db)
        do
        (print-errors result stream)
        (print-summary result stream)))

;;; Useful equality predicates for tests

(defun logically-equal (x y)
  "Return true if x and y are both false or both true."
  (eql (not x) (not y)))

(defun set-equal (list1 list2 &rest initargs &key key (test #'equal))
  "Return true if every element of list1 is an element of list2 and
vice versa."
  (declare (ignore key test))
  (and
   (listp list1)
   (listp list2)
   (apply #'subsetp list1 list2 initargs)
   (apply #'subsetp list2 list1 initargs)))

(pushnew :lisp-unit common-lisp:*features*)
