(in-package :lisp-unit2-tests)

(define-test collect/decollect (:tags '(utils))
  (let (list)
    (iter (for i from 0 to 5)
      (lisp-unit2::%collect! i list))
    (assert-equal '(0 1 2 3 4 5) (lisp-unit2::head list))
    (lisp-unit2::%decollect! 0 list)
    (assert-equal '(1 2 3 4 5) (lisp-unit2::head list))
    (lisp-unit2::%decollect! 5 list)
    (lisp-unit2::%collect! 6 list)
    (assert-equal '(1 2 3 4 6) (lisp-unit2::head list))
    (lisp-unit2::%decollect! 3 list)
    (lisp-unit2::%collect! 7 list)
    (assert-equal '(1 2 4 6 7) (lisp-unit2::head list))
    (lisp-unit2::%collect-new! 7 list)
    (assert-equal '(1 2 4 6 7) (lisp-unit2::head list))
    ))

(defparameter *context-stack* nil)

(defun test-body-thunk ()
  (assert-equal '(test-context-2 test-context-1) *context-stack*))
(defun test-context-1 (body-fn)
  (assert-equal '() *context-stack*)
  (let ((*context-stack* (cons 'test-context-1 *context-stack*)))
    (funcall body-fn)))
(defun test-context-2 (body-fn)
  (assert-equal '(test-context-1) *context-stack*)
  (let ((*context-stack* (cons 'test-context-2 *context-stack*)))
    (funcall body-fn)))

(define-test combine-contexts (:tags '(utils contexts))
  (lisp-unit2::do-contexts
    #'test-body-thunk nil #'test-context-1 nil nil #'test-context-2 nil nil ))

(define-test %form-equal (:tags '(utils asserts))
  (macrolet ((fe? (x y) `(assert-true (lisp-unit2::%form-equal ,x ,y)))
             (nfe? (x y) `(assert-false (lisp-unit2::%form-equal ,x ,y))))
    (fe? 'a 'a)
    (nfe? 'a 'b)
    (nfe? :a 'a)
    (nfe? '#:a 'a)
    (fe? '#:a '#:a)
    (fe? '1 '1)
    (fe? 1 1)
    (fe? "1" "1")
    (nfe? "1" #\1)
    (fe? '((1 () 2) :a) '((1 () 2) :a))
    (fe? '((1 () 2) (a b) ())
         '((1 () 2) (a b) ()))
    (nfe? '((1 () 2) (a b) ())
          '((1 () 2) (a b)))))

(define-test test-error-assertions (:tags '(asserts errors))
  (handler-bind ((lisp-unit2:assertion-pass
                   (lambda (c)
                     (assert-false T "Error was not thrown, shouldnt have passed assertion")
                     (abort c)))
                 (lisp-unit2:assertion-fail
                   (lambda (c)
                     (assert-true "Error was not thrown and we correctly failed")
                     (abort c))))
    (assert-error 'error 'foo))
  (handler-bind ((lisp-unit2:assertion-pass
                   (lambda (c)
                     (assert-true "Error was thrown and we correctly caught it")
                     (abort c)))
                 (lisp-unit2:assertion-fail
                   (lambda (c)
                     (assert-false T "Error was thrown, should have passed assertion")
                     (abort c))))
    (assert-error 'error (error "foo"))))

(define-test test-no-error-assertions (:tags '(asserts errors))
  (handler-bind
      ((lisp-unit2:assertion-pass
         (lambda (c)
           (assert-true "Error was not thrown and we correctly didnt see an error")
           (abort c)))
       (lisp-unit2:assertion-fail
         (lambda (c)
           (assert-false T "Error was not thrown, should have passed no-error assertion")
           (abort c))))
    (assert-no-error 'error 'foo))
  (handler-bind
      ((lisp-unit2:assertion-pass
         (lambda (c)
           (assert-false T "Error was thrown, shouldnt have passed no-error assertion")
           (abort c)))
       (lisp-unit2:assertion-fail
         (lambda (c)
           (assert-true "Error was not thrown and we correctly didnt see an error")
           (abort c))))
    (assert-no-error 'error (error "foo"))))