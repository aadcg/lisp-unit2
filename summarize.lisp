;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
(in-package :lisp-unit)
(cl-interpol:enable-interpol-syntax)

(defun with-summary-context (body-fn)
  (handler-bind
      ((test-start
         (lambda (c) (format *test-stream* "~%Starting: ~A" (name (unit-test c)))))
       (all-tests-complete (lambda (c) (print-summary (results c))))
       (test-complete (lambda (c) (print-summary (result c)))))
    (funcall body-fn)))

(defmacro with-summary (() &body body)
  `(with-summary-context (lambda () ,@body)))

(defmethod print-summary ((o test-results-db))
  "Print a summary of all results to the stream."
  (let ((total (length (test-names o)))
        (passed (length (passed-assertions o)))
        (failed (length (failed-assertions o)))
        (errors (length (errors o)))
        (warnings (length (all-warnings o)))
        (empty (length (empty o)))
        (missing (length (missing o))))
    (format *test-stream* "~%Test Summary (~D tests ~D sec)~4I~%" total (run-time o))
    (format *test-stream* "  | ~D assertions total~%" (+ passed failed))
    (format *test-stream* "  | ~D passed~%" passed)
    (format *test-stream* "  | ~D failed~%" failed)
    (format *test-stream* "  | ~D execution errors~%" errors)
    (format *test-stream* "  | ~D warnings~%" warnings)
    (format *test-stream* "  | ~D empty~%" empty)
    (format *test-stream* "  | ~D missing tests~%~0I" missing)))


(defmethod print-summary ((run test-result))
  "Print a summary of the test result."
  (format *test-stream* "~%~A - ~A (~ds) : ~S assertions passed"
          (name (unit-test run)) (status run)
          (- (end-time run) (start-time run))
          (length (passed run)))
  (when (failed run)
    (format *test-stream* ", ~S failed~%" (length (failed run)))
    (print-failures run))
  (when (errors run) (print-errors run))
  (format *test-stream* "~%")
  run)

;;; Print failures

(defgeneric print-failures (result)
  (:documentation "Report the results of the failed assertion.")

  (:method  ((results test-results-db))
    (iter (for res in (errors results))
      (print-summary res)))

  (:method ((result test-result) )
    (iter (for fail in (failed result))
      (print-failures fail)))

  (:method :around ((result failure-result))
    (format *test-stream* "~%| Failed Form: ~S" (form result))
    (call-next-method)
    (when (extras result)
      (format *test-stream* "~{~%| ~S => ~S~}~%" (extras result))))

  (:method ((result failure-result))
    (format *test-stream* "~%| Expected ~{~S~^; ~} " (expected result))
    (format *test-stream* "~<~% | ~:;but saw ~{~S~^; ~}~>" (actual result)))

  (:method ((result error-result))
    (format *test-stream* "~%| ~@[Should have signalled ~{~S~^; ~} but saw~]"
            (expected result))
    (format *test-stream* " ~{~S~^; ~}" (actual result)))

  (:method ((result macro-result))
    (format *test-stream* "~%| Should have expanded to ~{~S~^; ~} "
            (expected result))
    (format *test-stream* "~<~%~:;but saw ~{~S~^; ~}~>" (actual result)))

  (:method ((result output-result))
    (format *test-stream* "~%| Should have printed ~{~S~^; ~} "
            (expected result))
    (format *test-stream* "~<~%~:;but saw ~{~S~^; ~}~>"
            (actual result))))

;;; Print errors

(defgeneric print-errors (result)
  (:documentation "Print the error condition.")
  (:method ((result test-result))
    (let ((errors (errors result)))
      (when errors
        (format *test-stream* "~%| Execution error:~%| ~a~%| ~s" errors errors))))
  (:method ((results test-results-db))
    (iter (for res in (errors results))
      (print-summary res))))