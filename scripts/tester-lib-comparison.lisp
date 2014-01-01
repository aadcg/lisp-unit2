(in-package :lisp-unit2)
(cl-interpol:enable-interpol-syntax)

;; List pulled by cross-referencing http://www.cliki.net/Test%20Framework
;; with ql-dist
(defparameter +test-systems+
  '(:lisp-unit :lisp-unit2
    :xptest :stefil :clunit :lift :fiveam :ptester :eos :cl-test-more
    :cl-quickcheck :rt :hu.dwim.stefil :testbild :unit-test :xlunit))

(defun collect-systems ()
  (let ((ht (make-hash-table)))
    (iter (for system in (ql-dist:provided-systems t))
      (iter (for s in +test-systems+)
        (when (member s (ql-dist:required-systems system)
                      :test #'(lambda (x y) (string-equal (string x) (string y))))
          (push system (gethash s ht)))))
    ht))

(defparameter +test-systems-required-from+ (collect-systems))

(defun print-statistics ()
  (format *test-stream* "~:{~%~A (~D)~}~%"
          (sort
           (iter (for (k v) in-hashtable +test-systems-required-from+)
             (collect (list k (len v))))
           #'> :key #'second)))