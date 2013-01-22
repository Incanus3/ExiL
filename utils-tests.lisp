(in-package :exil-utils)
(ql:quickload "lift")
(use-package :lift)
(setf *test-describe-if-not-successful?* t)

(deftestsuite utils-tests () ())

(addtest (utils-tests)
  test-intern
  (ensure-same 'ABC (intern "abc")))

(addtest (utils-tests)
  test-string-append
  (ensure-same "abcdef" (string-append "abc" "def")))

(addtest (utils-tests)
  test-symbol-name
  (ensure-same "ABC" (symbol-name 'abc))
  (ensure-same "abc" (symbol-name "abc")))

(addtest (utils-tests)
  test-symbol-append
  (ensure-same 'ABCDEF (symbol-append "ABC" 'def)))

(addtest (utils-tests)
  test-to-keyword
  (ensure-same :abc (to-keyword 'abc))
  (ensure-same :abc (to-keyword "abc")))

(addtest (utils-tests)
  test-from-keyword
  (ensure-same 'abc (from-keyword :abc)))

(addtest (utils-tests)
  test-weak-symbol-equal-p
  (ensure (weak-symbol-equal-p 'exil-utils:intern 'cl:intern)))

(addtest (utils-tests)
  test-subsets
  (let ((subsets (subsets '(1 2))))
    (dolist (i '(nil (1) (2) (1 2)))
      (ensure (member i subsets :test #'equalp))
      (setf subsets (delete i subsets :test #'equalp)))
    (ensure-null subsets)))

(addtest (utils-tests)
  test-assoc-value
  (let ((alist '((a . 1))))
    (ensure-same 1 (assoc-value 'a alist))
    (ensure-same nil (assoc-value 'b alist))
    (setf (assoc-value 'a alist) 2)
    (ensure-same 2 (assoc-value 'a alist))
    (ensure-error (setf (assoc-value 'b alist) 3))))

(print (run-tests :suite 'utils-tests))
