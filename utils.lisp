(in-package :exil-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various general purpose utilities

;; insures that (intern "abc") equals (intern "ABC")
(defun intern (string &optional (package *package*))
  "create symbol from string"
  (cl:intern (string-upcase string) package))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-append (&rest strings)
    "append two strings"
    (apply #'concatenate 'string strings)))

(defgeneric symbol-name (symbol)
  (:documentation "For symbol return its name, for string just return itself")
  (:method ((symbol symbol)) (cl:symbol-name symbol))
  (:method ((string string)) string))

(defun symbol-append (&rest symbols)
  "concatenate several symbols"
  (intern (apply #'string-append (mapcar #'symbol-name symbols))))
;; (symbol-append "test-" 'symbol) => TEST-SYMBOL

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

(defgeneric weak-symbol-equal-p (sym1 sym2)
  (:documentation "Test if the symbol name is equal, omits the package name")
  (:method ((sym1 symbol) (sym2 symbol))
    (equalp (to-keyword sym1) (to-keyword sym2)))
  (:method (sym1 sym2) nil))

(defmacro mac-exp (&body body)
  "shortcut for calling macroexpand-1"
  `(pprint (macroexpand-1 ',@body)))

;; more like sublists - doesn't care about duplicities in the input list
;; i just don't like the name sublists, that's why
(defun subsets (list)
  "get a list of all subsets of given list"
  (cl:assert (<= (length list) 20)
	  ()
	  (string-append "subsets: Can't generate subsets of list longer then 20,"
			 "not enough memory!"))
  (case (length list)
    (0 ())
    (1 (list () list))
    (otherwise (let ((subsets (subsets (rest list))))
		 (append subsets (mapcar (lambda (x) (cons (first list) x))
					 subsets))))))
;; (subsets '(1 2)) = ((1 2) (1) (2) ())

(defun assoc-value (key alist)
  "get value from assoc-list according to the key"
  (cdr (assoc key alist)))
;; (assoc-value 'b '((a . 1) (b . 2))) => 2

(defun (setf assoc-value) (value key alist)
  "set value in assoc-list according to the key"
  (setf (cdr (assoc key alist)) value))

(defun assoc-key (value alist)
  "get key from assoc-list according to the value"
  (car (rassoc value alist)))
;; (assoc-key 2 '((a . 1) (b . 2))) => b

(defun to-list (x)
  "when given an atom, returns list containing it, when given a list, just returns it"
  (if (listp x)
      x
      (list x)))
;; (to-list '(a)) => (a)
;; (to-list 'a) => (a)

(defun to-list-of-lists (list)
  "collects return value of to-list on each element"
  (mapcar #'to-list list))
;; (to-list-of-lists '(a (b :default 10))) => ((a) (b :default 10))

;; like pushnew, but returns the test-equivalent object, which actualy resides
;; in the place (if there already was a test-equivalent object, returns it)
;; i could shadow the pushnew from common-lisp package and name this just
;; pushnew, but since this one is 2x slower, i'll keep both of them and
;; use this only when appropriate
(defmacro my-pushnew (item place &key (test '#'equalp) (key '#'identity))
  "slightly altered pushnew"
  `(progn
     (pushnew ,item ,place :test ,test :key ,key)
     (find (funcall ,key ,item) ,place :test ,test :key ,key)))

(defmacro ext-pushnew (item place &key (test '#'equalp) (key '#'identity))
  "like pushnew, but as a second value returns, whether the list was actualy altered"
  (let ((length (gensym "length"))
	(new-list (gensym "new-list")))
    `(let ((,length (length ,place))
	   (,new-list (pushnew ,item ,place :test ,test :key ,key)))
       (values ,new-list (not (= (length ,new-list) ,length))))))

(defmacro push-end (item list)
  `(progn (if ,list (nconc ,list (cons ,item nil)) (setf ,list (list ,item)))
	  ,list))

(defmacro pushnew-end (item list &key (key '#'identity) (test '#'equalp))
  (string-append "pushes the item at the end of list, it it's not yet in the list"
		 "as a second value, returns whether the list was actualy altered")
  `(if (find ,item ,list :key ,key :test ,test)
       (values ,list nil)
       (values (push-end ,item ,list) t)))

(defmacro ext-delete (item place &key (test '#'equalp) (key '#'identity))
  "like delete, but as a second value returns, whether the list was actualy altered"
  (let ((length (gensym "length"))
	(new-list (gensym "new-list")))
    `(let ((,length (length ,place))
	   (,new-list (delete ,item ,place :test ,test :key ,key)))
       (values ,new-list (not (= (length ,new-list) ,length))))))

(defmacro diff-delete (item sequence &key (test '#'equalp) (key '#'identity))
  "like delete, but as a second value returns list of deleted items"
  (let ((new-list (gensym "new-list"))
	(deleted (gensym "deleted"))
	(elem (gensym "elem")))
    `(let (,new-list ,deleted)
       (dolist (,elem ,sequence (values (nreverse ,new-list) ,deleted))
	 (if (funcall ,test ,item (funcall ,key ,elem))
	     (push ,elem ,deleted)
	   (push ,elem ,new-list))))))

(defmacro push-update (item place &key (test '#'equalp) (key '#'identity))
  "like pushnew, but if there is test-equal atom in the place, replaces it by item"
  `(progn
     (setf ,place (delete ,item ,place :test ,test :key ,key))
     (push ,item ,place)))

(defun class-slot-value (class-name slot-name)
  "get a class-slot value from class-name"
  (slot-value (make-instance class-name) slot-name))

(defun select (list indices)
  "get a list of values from list according to the list of indices"
  (mapcar (lambda (i)
	    (nth i list))
	  indices))