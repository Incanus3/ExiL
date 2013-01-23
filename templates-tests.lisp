(in-package :core-tests)

(deftestsuite core-tests (tests) ())

(deftestsuite templates-tests (core-tests)
  ((template (make-template 'test-template '((slot :default 5))))
   (object (make-instance 'exil-core::template-object :tmpl-name 'test-template))))

(addtest (templates-tests)
  test-variable-p
  (ensure (variable-p '?a))
  (ensure (not (variable-p 'a))))

#|
(addtest (templates-tests)
  test-has-slot-p
  (print object)
  (ensure (has-slot-p object 'slot))
  (ensure (not (has-slot-p object 'no-slot))))
|#
