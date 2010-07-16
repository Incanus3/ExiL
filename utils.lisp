(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various utilities

(defun intern (string &optional (package *package*))
  (cl:intern (string-upcase string) package))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-append (&rest strings)
    (apply #'concatenate 'string strings)))

(defgeneric symbol-name (symbol)
  (:documentation "For symbol returns its name, for string just return itself")
  (:method ((symbol symbol)) (cl:symbol-name symbol))
  (:method ((string string)) string))

(defun symbol-append (&rest symbols)
  (intern (apply #'string-append (mapcar #'symbol-name symbols))))
;; (symbol-append "test-" 'symbol) => TEST-SYMBOL

;; following 2 definitions enable the use of square ([]) parentheses to append
;; the contained strings, this will be useful for managing strings longer
;; than one row
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     (let ((string-list (read-delimited-list #\] stream t)))
       (apply #'string-append string-list)))))
;; ["abc" "def"] => "abcdef"

;; following 2 functions may seem redundant (and they are)
;; but they're names tell much more about the purpose of such call
(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern symbol :keyword))
;; (to-keyword 'a) => :a

(defun from-keyword (key &optional (package *package*))
  "get sybol from key"
  (intern key package))
;; (from-keyword :a) => a

(defmacro mac-exp (&body body)
  `(pprint (macroexpand-1 ',@body)))

;; more like sublists - doesn't care about duplicities in the input list
;; i just don't like the name sublists, that's why
(defun subsets (list)
  (cl:assert (<= (length list) 20)
	  ()
	  ["subsets: Can't generate subsets of list longer then 20,"
			 "not enough memory!"])
  (case (length list)
    (0 ())
    (1 (list () list))
    (otherwise (let ((subsets (subsets (rest list))))
		 (append subsets (mapcar (lambda (x) (cons (first list) x))
					 subsets))))))
;; (subsets '(1 2)) = ((1 2) (1) (2) ())

(defun assoc-value (key alist)
  (cdr (assoc key alist)))
;; (assoc-value 'b '((a . 1) (b . 2))) => 2

(defun assoc-key (value alist)
  (car (rassoc value alist)))
;; (assoc-key 2 '((a . 1) (b . 2))) => b

(defun (setf assoc-value) (value key plist)
  (setf (cdr (assoc key plist)) value))
;; (defvar alist '((a . 1) (b . 2)))
;; (setf (assoc-value 'a alist) 3)
;; alist => '((a . 3) (b . 2))

(defun to-list (x)
  (if (listp x)
      x
      (list x)))
;; (to-list '(a)) => (a)
;; (to-list 'a) => (a)

(defun to-list-of-lists (list)
  (mapcar #'to-list list))
;; (to-list-of-lists '(a (b :default 10))) => ((a) (b :default 10))

;; like pushnew, but returns the test-equivalent object, which actualy resides
;; in the place (if there already was a test-equivalent object, returns it)
;; i could shadow the pushnew from common-lisp package and name this just
;; pushnew, but since this one is 2x slower, i'll keep both of them and
;; use this only when appropriate
(defmacro my-pushnew (item place &key (test '#'equalp) (key '#'identity))
  `(progn
     (pushnew ,item ,place :test ,test :key ,key)
     (find (funcall ,key ,item) ,place :test ,test :key ,key)))

(defun class-slot-value (class-name slot-name)
  (slot-value (make-instance class-name) slot-name))