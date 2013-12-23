(in-package :lisp-unit2)
(cl-interpol:enable-interpol-syntax)

(defvar *test-stream* *standard-output*)
(defvar *test-log-stream* *test-stream*)
(defvar *unit-test* nil
  "The currently executing unit test (bound in %run-test, ie every test
  function)" )
(defvar *results* nil "The current results database (bound in run-tests)")
(defvar *result* nil "The current test result  (bound in %run-test)")

(defparameter *log-level* 5)

(defvar *test-db* nil
  "The unit test database is a list of tests and some hashtable indexes")

(defparameter +statuses+
  '(errors failed warnings passed missing empty)
  "List of statuses in order of priority for categorizing test runs")
