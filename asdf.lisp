(in-package :lisp-unit2)

#+asdf
(defun get-system-list (start &aux list)
  (labels ((do-it (sys-name)
             (let* ((sys (asdf/system:find-system sys-name))
                    (deps (slot-value sys 'asdf/component:sideway-dependencies)))
               ;; ensures that all dependencies are before things that depend on them
               (setf list (delete sys-name list :test #'equal))
               (push sys-name list)
               (mapc #'do-it deps))))
    (mapc #'do-it (alexandria:ensure-list start))
    list))

#+asdf
(defgeneric test-asdf-system-recursive (sys-name &key ignore-systems)
  (:method (sys-name &key ignore-systems &aux out)
    (with-test-results (:collection-place out
                        :summarize? t)
      (iter (for system in (get-system-list sys-name))
        (unless (member system ignore-systems)
          (format *test-stream* "~% asdf:test-system: ~A~%" system)
          (asdf:test-system system))))
    out))