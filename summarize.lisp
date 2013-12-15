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

(defparameter *status-prefix* "~%  |  ")

(defun %out (s &rest args
               &aux (prefix (alexandria:ensure-list *status-prefix*)))
  (format *test-stream* "~?~?" (first prefix) (rest prefix) s args))

(defgeneric print-summary (o)
  (:documentation "Print a summary of all results to the stream.")
  (:method ((o test-results-db))
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
  (:method  ((run test-result))
    (format *test-stream* "~%~A - ~A (~ds) : ~S assertions passed"
            (name (unit-test run)) (status run)
            (- (end-time run) (start-time run))
            (length (passed run)))
    (iter (for s in '(errors failed warnings))
      (print-status-summary run s))
    (format *test-stream* "~%")
    run)
  (:method :around ((result failure-result))
    (%out "Failed Form: ~S" (form result))
    (call-next-method)
    (when (extras result)
      (iter (for e in (extras result))
        (%out "~S => ~S")))
    (%out ""))

  (:method ((result failure-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (%out "Expected ~{~S~^; ~} " (expected result))
    (format *test-stream*
            "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result)))
  (:method ((result error-result))
    (%out "~@[Should have signalled ~{~S~^; ~} but saw~]"
            (expected result))
    (%out "~{~S~^; ~}" (actual result)))

  (:method ((result macro-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (%out "Should have expanded to ~{~S~^; ~} " (expected result))
    (format *test-stream*
            "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result)))

  (:method ((result output-result)
            &aux (prefix (alexandria:ensure-list *status-prefix*)))
    (format *test-stream* "~%| Should have printed ~{~S~^; ~} "
            (expected result))
    (format *test-stream* "~<~?~:;but saw ~{~S~^; ~}~>"
            (first prefix) (rest prefix)
            (actual result)))
  (:method ((w warning))
    (%out "WARNING: ~A" w)
    (%out "~S" w)
    (%out ""))
  (:method ((e error))
    (%out "ERROR: ~A" e)
    (%out "~S" e)
    (%out "")))

(defgeneric print-status-summary (object status)
  (:method ((db test-results-db) s &aux (objs (funcall s db)))
    (when objs
      (iter (for o in objs)
        (print-status-summary o s))))
  (:method ((o test-result) s &aux (objs (funcall s o)))
    (when objs
      (%out "~A (~D) --------------" s (length objs))
      (iter (for o in objs)
        (print-summary o))
      (%out " --------------"))))