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
     :define-test
     :list-tests
     :test-code
     :test-documentation
     :remove-tests
     :run-tests
     :run-test
     :use-debugger)
    ;; Functions for managing tags
    (:export :list-tags
     :tagged-tests
             :remove-tags
     :run-tags)
    ;; Functions for reporting test results
    (:export
     :with-summary
     :test-names
     :failed
     :error
     :passed
     :missing
     :print-failures
     :print-errors
     :summarize-results)
    ;; Functions for extensibility via signals
    (:export :test-run-complete
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