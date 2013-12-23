;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :lisp-unit2)
(cl-interpol:enable-interpol-syntax)

(defun %ts (&optional (time (get-universal-time)))
  "returns a date as {y}{mon}{d}-{h}{min}{s}, defaults to get-universal-time
   intended for use in datestamping filenames
  "
  (multiple-value-bind ( s min h d mon y  )
      (decode-universal-time time)
    (format nil "~d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"  y mon d h min s)))

(defmacro %log (message &key (level 0))
  (when (<= *log-level* level)
    `(format *test-log-stream* "~&~A ~A~%" (%ts) ,message)))

(defmacro %log-around ((message &key (start-level 1) (end-level 0)) &body body)
  `(unwind-protect
    (progn
      (%log #?" START ${,message}" :level ,start-level)
      ,@body)
    (%log #?"   END ${,message}" :level ,end-level)))

(defclass test-database ()
  ((%tests :accessor %tests :initarg :%tests :initform nil)
   (name-index :accessor name-index :initarg :name-index :initform (make-hash-table))
   (package-index :accessor package-index :initarg :package-index :initform (make-hash-table))
   (tag-index :accessor tag-index :initarg :tag-index :initform (make-hash-table))))

(unless *test-db* (setf *test-db* (make-instance 'test-database)))

(defmethod tests ((db test-database))
  (head (%tests db)))

;;; Global unit test database
(defclass unit-test-control-mixin ()
  ((context-provider
    :accessor context-provider :initarg :context-provider :initform nil
    :documentation "A function that accepts a test thunk and executes
     with a given context (eg: database-connects, html-collectors,
     http-context etc)")
   (data :accessor data :initarg :data :initform nil
         :documentation "Shared data so the context"))
  (:documentation "Helps commonize test construction by providing a
    shared data and dynamic context"))

(defclass unit-test (unit-test-control-mixin)
  ((name :accessor name :initarg :name :initform nil)
   (doc :accessor doc :initarg :doc :initform nil)
   (code :accessor code :initarg :code :initform nil
         :documentation "The forms to produce the fn")
   (tags :accessor tags :initarg :tags :initform nil)
   (most-recent-result :accessor most-recent-result :initarg :most-recent-result :initform nil))
  (:documentation
   "Organize the unit test documentation and code."))

(defun short-full-name (s)
  (typecase s
    ((or assertion-pass assertion-fail test-result test-start)
     (short-full-name (unit-test s)))
    (test-complete
     (short-full-name (unit-test s)))
    (unit-test (short-full-name (name s)))
    (symbol
     (let* ((package (symbol-package s))
            (nick (first (package-nicknames package)))
            (p (or nick (package-name package) "#")))
       #?"${p}::${s}"))))

(defmethod print-object ((o unit-test) s)
  "Print the auto-print-items for this instance."
  (print-unreadable-object (o s :type t :identity t)
    (princ (ignore-errors (short-full-name o)) s)))

(defmethod install-test ((u unit-test)
                         &aux (package (symbol-package (name u)))
                         (db *test-db*))
  (%log #?"Installing test ${u}")
  (uninstall-test u) ;; prevents duplication, does a lot of work :/
  (%compile u)
  (%collect! u (%tests db))
  (setf (gethash (name u) (name-index db)) u)
  (%collect! u (gethash package (package-index db)))
  (iter (for tag in (alexandria:ensure-list (tags u)))
    (%collect! u (gethash tag (tag-index db)))))

(defun %uninstall-name (n &optional tags
                        &aux (db *test-db*)
                        (package (symbol-package n)))
  (%decollect! n (%tests db) :key #'name)
  (remhash n (name-index db))
  (when package
    (setf (gethash package (package-index db))
          (%decollect! n (gethash package (package-index db)) :key #'name)))
  (when tags
    (if (eql t tags)
        (iter (for (tag vals) in-hashtable (tag-index db))
          (setf
           (gethash tag (tag-index db))
           (%decollect! n (gethash tag (tag-index db)) :key #'name)))
        (iter (for tag in (alexandria:ensure-list tags))
          (setf
           (gethash tag (tag-index db))
           (%decollect! n (gethash tag (tag-index db)) :key #'name)))))
  (ignore-errors (fmakunbound n))
  (ignore-errors (fmakunbound (test-thunk-name n))))

(defgeneric uninstall-test (test)
  (:method ((n symbol) &aux (test (first (get-tests :tests n))))
    (%uninstall-name n (or (and test (tags test))
                           t)))
  (:method ((u unit-test) &aux (n (name u)))
    (%log #?"Uninstalling test ${u}")
    (%uninstall-name n (tags u))))

(defun get-tests (&key tests tags package reintern-package
                  &aux (db *test-db*))
  "finds tests by names, tags, and package

   if reintern-package is provided, reintern all symbols provided for tests
   and tags into the reintern-package.  Mostly provided for easing conversion
   of lisp-unit1 test suites
  "
  (%log-around (#?"get-tests:${tests} tags:${tags} package:${package} reintern-package:${reintern-package}"
                :start-level 0)
    (when reintern-package
      (setf tests (%in-package tests reintern-package))
      (setf tags (%in-package tags reintern-package)))
    (cond
      ;; defaults to pulling up all tests in the current package
      ((and (null tests) (null tags) (null package))
       (head (gethash *package* (package-index db))))
      (t
       (remove-duplicates
        (append
         (iter (for p in (alexandria:ensure-list package))
           (appending (head (gethash (find-package p) (package-index db)))))
         (iter (for tag in (alexandria:ensure-list tags))
           (appending (head (gethash tag (tag-index db)))))
         (iter (for name in (alexandria:ensure-list tests))
           (for test = (etypecase name
                         (null nil)
                         (unit-test name)
                         (symbol (gethash name (name-index *test-db*)))))
           (if test
               (collect test)
               (warn 'missing-test :test-name test))))
        :key #'name)))))



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

(defgeneric test-thunk-name (test)
  (:method ((u unit-test))
    (test-thunk-name (name u)))
  (:method ((u symbol))
    (symbol-munger:english->lisp-symbol
     (list u 'test-thunk)
     (symbol-package u))))

(defmethod test-thunk ((u unit-test))
  (%compile u)
  (symbol-function (test-thunk-name u)))

(defmethod %compile ((u unit-test))
  (%log-around (#?"Compiling Test: ${ (name u) }" :start-level 0)
    (compile (test-thunk-name u)
             `(lambda ()
               (declare (optimize (debug 3))) ,@(code u)))))


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

(defun %in-package (n &optional (package *package*))
  (typecase n
    (null n)
    (keyword n)
    ((or string symbol)
     (symbol-munger:english->lisp-symbol
      n package))
    (list (mapcar #'%in-package n))))

(defmacro define-test (name (&key tags context-provider package) &body body)
  "Defines a new test object, test functions and installs the test
   function in the test database

   name: the name of the test and the test-function

   context-provider: a (lambda (fn)...) (or list of) that runs the fn in a dynamic
      context

   tags: symbols that can be used to group tests

   If package is provided, the name and any provided tags are reinterned into
      package before use. Keywords are left alone. This is useful because many
      times tests and the functions they test, coexist (and always could in
      lisp-unit v1, now that we create test functions, we dont want them
      overwriting the original function).

      Note: This should probably not be used (rather opting for
      packaged::symbols), but will be useful when converting from lisp-unit
      1->2.  See also: run-tests, get-tests reintern-package
  "
  (when package
    (setf name (%in-package name package)))
  `(progn
    (install-test
     (make-instance 'unit-test
      :name ',name
      :doc ,(when (stringp (first body)) (first body))
      :tags ,(if package
                 `(%in-package ,tags ,package)
                 tags)
      :code '(,@body)
      :context-provider (combine-contexts ,context-provider)
      ))
    (defun ,name (&key test-context-provider)
      (declare (optimize (debug 3)))
      "Runs this test, this fn is useful to help going to test definitions"
      (%run-test-name ',name :test-context-provider test-context-provider))
    #',name))

;;; Manage tests

(defun list-tests ()
  "Return a list of all tests,
   use get tests to find tests by package tag or name "
  (tests *test-db*))

(defun test-documentation (name)
  "Return the documentation for the test."
  (let ((unit-test (first (get-tests :tests name))))
    (if (null unit-test)
        (warn "No test ~A in package ~A."
              name (symbol-package name))
        (doc unit-test))))

(defun test-code (name)
  "Returns the code stored for the test name."
  (let ((unit-test (first (get-tests :tests name))))
    (if (null unit-test)
        (warn "No test ~A in package ~A."
              name (symbol-package name))
        (code unit-test))))

(defun remove-tests (&optional (tests :all) (tags nil) (package *package*))
  "Remove individual tests or entire sets."
  (let ((tests (get-tests :tests tests :tags tags :package package)))
    (iter (for test in tests)
      (uninstall-test test))))

;;; Manage tags

(defun list-tags ()
  "Return a list of the tags"
  (iter (for (tag list) in-hashtable (tag-index *test-db*))
    (collect tag)))

(defun remove-tags (&optional tags)
  "Remove individual tags or entire sets."
  (if (eq :all tags)
      (setf (tag-index *test-db*) (make-hash-table))
      (iter (for tag in (alexandria:ensure-list tags))
        (remhash tag (tag-index *test-db*)))))

;;; Test results database

(defclass test-results-mixin ()
  #.`((start-time :accessor start-time :initarg :start-time
                  :initform (get-universal-time))
      (end-time :accessor end-time :initarg :end-time :initform nil)
      ;; SORRY want to keep this in sync with the +statuses, with minimal
      ;; shenanigans
      ,@(iter
          (for s in +statuses+)
          (collect `(,s :accessor ,s :initform nil)))))

(defun status ( u )
  (or
   (iter (for s in +statuses+)
     (when (%has? s u)
       (return s)))
   'empty))

(defmethod %has? (status thing
                  &aux (n (len (funcall status thing))))
  (when (< 0 n) n))

(defgeneric run-time (it)
  (:method ((o test-results-mixin))
    (or
     (ignore-errors
      (- (end-time o) (start-time o)))
     -1)))

(defclass test-results-db (test-results-mixin unit-test-control-mixin)
  ((tests :reader tests :initarg :tests :initform nil)
   (results :reader results :initarg :results :initform nil))
  (:documentation
   "Store the results of the tests for further evaluation."))

(defmethod initialize-instance :after
    ((ctl test-results-db) &key &allow-other-keys)
  (setf (slot-value ctl 'results)
        (make-array (len (tests ctl)) :initial-element nil :fill-pointer 0)))

(defclass test-result (test-results-mixin)
  ((unit-test :accessor unit-test :initarg :unit-test :initform *unit-test*)
   (return-value :accessor return-value :initarg :return-value :initform nil)))

(defmethod print-object ((o test-result) s)
  "Print the auto-print-items for this instance."
    (format s "#<RESULT ~A ~A(~d)>" (ignore-errors (short-full-name o))
            (ignore-errors (status o))
            (ignore-errors (len (funcall (status o) o)))))

(defgeneric passed-assertions (it)
  (:method ((n null)) n)
  (:method ((u test-result))
    (passed u))
  (:method ((u test-results-db))
    (iter (for test-result in-vector (results u))
      (while test-result)
      (appending (head (passed-assertions test-result))))))

(defgeneric failed-assertions (it)
  (:method ((n null)) n)
  (:method ((u test-result))
    (failed u))
  (:method ((u test-results-db))
    (iter (for test-result in-vector (results u))
      (while test-result)
      (appending (head (failed-assertions test-result))))))

(defgeneric all-warnings (it)
  (:method ((n null)) n)
  (:method ((u test-result))
    (warnings u))
  (:method ((u test-results-db))
    (iter (for test-result in-vector (results u))
      (while test-result)
      (appending (head (warnings test-result))))))

(defmethod print-object ((o test-results-db) stream)
  "Print the summary counts with the object."
  (print-unreadable-object (o stream :type t :identity t)
    (let ((total (ignore-errors (len (tests o))))
          (passed (ignore-errors (len (passed-assertions o))))
          (failed (ignore-errors (len (failed-assertions o))))
          (errors (ignore-errors (len (errors o))))
          (warnings (ignore-errors (len (all-warnings o)))))
      (format stream "Tests:(~a) Passed:(~a) Failed:(~a) Errors:(~a) Warnings:(~a)"
              total passed failed errors warnings))))

(defun record-result-context (body)
  "as we are finishing a test (ie: it has the right status)
   record the result"
  (unwind-protect (funcall body)
    (record-result *result* *results*)))

(defmethod record-result ((res test-result) (db test-results-db)
                          &aux (status (status res)))
  (vector-push res (results db))
  (funcall (fdefinition `(setf ,status))
           (cons res (funcall status db))
           db))

(defgeneric run-tests (&key tests tags package
                       test-context-provider
                       reintern-package)
  (:documentation
   "Run the specified tests.

    We run all the listed tests, and all tests tagged with tags.  If both test
    and tags are nil (the default), then we run all tests in
    package (defaulting to *package*)

    reintern-package: when looking up tests, first reintern all tests and tags
      in this package. In general this should probably not be used, but is provided
      for convenience in transitioning from lisp-unit 1 to 2 (see: define-test package)
  ")
  (:method :around (&key tests tags package test-context-provider
                    reintern-package)
    (declare (ignorable tests tags package test-context-provider
                        reintern-package))
    (%log-around (#?"Running tests:${tests} tags:${tags} package:${package} context:${test-context-provider}")
      (call-next-method)))
  (:method (&key
            tests tags package test-context-provider reintern-package
            &aux
            (all-tests (get-tests :tests tests
                                  :tags tags
                                  :package package
                                  :reintern-package reintern-package))
            (results (make-instance 'test-results-db :tests all-tests))
            (*results* results))
    (%log #?"Running tests:${all-tests}" :level 0)
    (signal 'all-tests-start :results results)
    (unwind-protect
         (handler-bind ((missing-test
                          (lambda (c) (%collect! (test-name c) (missing results)))))
           (iter (for test in all-tests)
             ;; this calls the test fn so the test source-location is
             ;;  available in stack traces
             (funcall
              (name test)
              :test-context-provider
              (list #'record-result-context test-context-provider))))
      (setf (end-time results) (get-universal-time))
      (signal 'all-tests-complete :results results))
    results))

(defun combine-contexts (&rest contexts)
  "Takes a list of nils and contexts and combines them into a single context
   (or null of no contexts were provided)"
  (labels ((%combine-2-contexts (c0 c1)
             (lambda (body-fn)
               (funcall c0 (lambda () (funcall c1 body-fn)))))
           (%make (contexts &aux (c (first contexts)) (them (rest contexts)))
             (cond
               ((and (null c) (null them) nil))
               ((null them) c)
               (t (%make (cons (%combine-2-contexts c (first them))
                               (rest them)))))))
    (%make (alexandria:flatten contexts))))

(defun do-contexts (body-fn &rest contexts)
  "runs the body-fn inside of contexts, the last context will be the outermost
   all nils in contexts are ignored"
  (let ((c (apply #'combine-contexts contexts)))
    (if c
        (funcall c body-fn)
        (funcall body-fn))))

#| ;; TODO: make this a test

|#

(defun %run-test-name (u &key test-context-provider
                         &aux (test (first (get-tests :tests u))))
  (if test
      (%run-test test :test-context-provider test-context-provider)
      (warn 'missing-test :test-name u)))

(defun %run-test
    (u &key test-context-provider
       &aux
       (result (setf (most-recent-result u)
                     (make-instance 'test-result :unit-test u)))
       (*unit-test* u)
       (*result* result))
  (check-type u unit-test)
  ;; todo: clear context-provider, data? so that it must be set via signal?
  ;; possibly in an unwind-protect region
  (signal 'test-start :unit-test u)
  (with-simple-restart (continue "Continue running the next test")
    (unwind-protect
         (handler-bind
             ((assertion-pass (lambda (c) (%collect! (assertion c) (passed result))))
              (assertion-fail (lambda (c) (%collect! (failure c) (failed result))))
              (error (lambda (c)
                       (%collect! c (errors result))
                       (unless *debugger-hook*
                         (return-from %run-test result))))
              (warning (lambda (c) (%collect! c (warnings result)))))
           ;; run the test code
           (setf (return-value result)
                 (do-contexts (test-thunk u)
                   (context-provider u)
                   test-context-provider)))
      (setf (end-time result) (get-universal-time))
      (signal 'test-complete :result result)))
  result)

;; This is written this way so that erroring test fns show up in the
;; stack and then can easily goto-definition
(defgeneric run-test (test &key test-context-provider)
  (:method ((n symbol) &key test-context-provider )
    (funcall n :test-context-provider test-context-provider))
  (:method :around ((u symbol) &key test-context-provider)
    (declare (ignorable u test-context-provider))
    (%log-around (#?"Running Test:${ u } context:${test-context-provider}")
      (call-next-method)))
  (:method ((u unit-test) &key test-context-provider)
    (run-test (name u) :test-context-provider test-context-provider)))

(pushnew :lisp-unit2 common-lisp:*features*)


#|
Copyright (c) 2013 Russ Tyndall, Acceleration.net
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
|#