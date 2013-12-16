(in-package :cl-user)

(handler-bind
    ;; fixes sbcl SUPER warnings that prevent automatic fasl loading
    ((warning (lambda (c)
                (format *error-output* "~A~%~S" c c)
                (muffle-warning c))))
  (defpackage :lisp-unit
    (:use :common-lisp :iter)
    ;; Print parameters
    (:export :*print-summary*
     :*print-failures*
             :*print-errors*)
    ;; Forms for assertions
    (:export
     :assert-eq :assert-eql :assert-equal :assert-equalp
     :assert-equality :assert-prints :assert-expands :assert-true
     :assert-false :assert-error)
    ;; Functions for managing tests
    (:export
     ;; statuses
     :errors :failed :warnings :passed :missing :empty
     :define-test
     :list-tests
     :get-tests
     :test-code
     :test-documentation
     :remove-tests
     :run-tests
     :run-test
     :use-debugger)
    ;; Functions for managing tags
    (:export
     :list-tags
     :tagged-tests
     :remove-tags)
    ;; Functions for reporting test results
    (:export
     :with-summary
     :with-summary-context
     :test-names
     :print-summary
     :print-status-summary
     :summarize-results
     :with-tap-summary
     :with-tap-context
     :write-tap
     :write-tap-to-file)
    ;; Functions for extensibility via signals
    (:export #:assertion-pass #:assertion-fail
             #:all-tests-start #:all-tests-complete
             #:test-start #:test-complete
     :results)
    ;; Utility predicates
    (:export :logically-equal :set-equal))

  (defpackage :lisp-unit-asserts
    (:import-from :lisp-unit
     :assert-eq :assert-eql :assert-equal :assert-equalp
     :assert-equality :assert-prints :assert-expands :assert-true
     :assert-false :assert-error)
    (:export
     :assert-eq :assert-eql :assert-equal :assert-equalp
     :assert-equality :assert-prints :assert-expands :assert-true
     :assert-false :assert-error))
  )