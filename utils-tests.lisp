(in-package :exil-utils)
;(ql:quickload "lift")
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
  (let ((alist (list (cons 'a 1))))
    (ensure-same 1 (assoc-value 'a alist))
    (ensure-same nil (assoc-value 'b alist))
    (setf (assoc-value 'a alist) 2)
    (ensure-same 2 (assoc-value 'a alist))
    (ensure-error (setf (assoc-value 'b alist) 3))))

(addtest (utils-tests)
  test-assoc-key
  (let ((alist (list (cons 'a 1))))
    (ensure-same 'a (assoc-key 1 alist))
    (ensure-same nil (assoc-key 2 alist))))

(addtest (utils-tests)
  test-cpl-assoc-val
  (let ((cpl-list (list (list 'a 1))))
    (ensure-same 1 (cpl-assoc-val 'a cpl-list))
    (ensure-same nil (cpl-assoc-val 'b cpl-list))
    (setf (cpl-assoc-val 'a cpl-list) 2)
    (ensure-same 2 (cpl-assoc-val 'a cpl-list))
    (ensure-error (setf (cpl-assoc-val 'b cpl-list) 2))))

(addtest (utils-tests)
  test-to-list
  (ensure-same '(a) (to-list 'a))
  (ensure-same '(a) (to-list '(a)))
  (ensure-same () (to-list ())))

(addtest (utils-tests)
  test-to-list-of-lists
  (ensure-same '((a) (b :default 10))
               (to-list-of-lists '(a (b :default 10)))))



(print (run-tests :suite 'utils-tests))
