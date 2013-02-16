(in-package :exil-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOLS:

;; ensures that (intern "abc") equals (intern "ABC")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALISTS:

(defun assoc-value (the-key alist &key (key #'identity) (test #'equalp))
  "get value from assoc-list according to the key"
  (cdr (assoc the-key alist :key key :test test)))
;; (assoc-value 'b '((a . 1) (b . 2))) => 2

(defun (setf assoc-value) (value the-key alist &key (key #'identity)
                                                 (test #'equalp))
  "set value in assoc-list according to the key"
  (let ((pair (assoc the-key alist :key key :test test)))
    (if pair
        (setf (cdr pair) value)
        (error "Key ~s not present in ~s" the-key alist))))

(defun assoc-key (value alist)
  "get key from assoc-list according to the value"
  (car (rassoc value alist)))
;; (assoc-key 2 '((a . 1) (b . 2))) => b

(defun alistp (list)
  "is the list an assoc-list?"
  (every (lambda (elem)
           (and (listp elem) (= (length elem) 2)))
         list))

(defun alist-equal-p (alist1 alist2)
  (and (= (length alist1) (length alist2))
       (every (lambda (x)
                (equalp (assoc (car x) alist2) x))
              alist1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLISTS:

(defun every-couple% (predicate list)
  "applies 2-parameter predicate to every couple of items in the list,
   returns true if all the return values are true"
  (when (evenp (length list))
    (iter (with lst-copy = (copy-list list))
          (while lst-copy)
          (for first = (pop lst-copy))
          (for second = (pop lst-copy))
          (unless (funcall predicate first second) (return nil))
          (finally (return t)))))

;; TODO: check if this needs to be in eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun plistp (list)
    "is the list a property list?"
    (every-couple% (lambda (key val)
                    (declare (ignore val))
                    (keywordp key))
                  list)))

(defmacro doplist ((key val plist &optional (retval nil)) &body body)
  "iterates over plist setting key and val variables for each iteration"
  (let ((sym-plist (gensym "plist")))
    `(let ((,sym-plist (copy-list ,plist)))
       (cl:assert (plistp ,plist) ()
                  "doplist: ~A not a plist" ,plist)
       (do ((,key (pop ,sym-plist) (pop ,sym-plist))
            (,val (pop ,sym-plist) (pop ,sym-plist)))
           ((not ,key) ,retval)
         ,@body))))

(defun plist-every (pred plist)
  (doplist (key val plist t)
    (unless (funcall pred key val) (return nil))))

(defun plist-equal-p (plist1 plist2)
  (and (= (length plist1) (length plist2))
       (plist-every (lambda (key val)
                      (equalp val (getf plist2 key)))
                    plist1)))

;; returns plist key for given value
(defun rgetf (plist value)
  (doplist (key val plist)
    (when (equalp val value)
      (return key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISTS:

;; more like sublists - doesn't care about duplicities in the input list
;; i just don't like the name sublists, that's why
(defun subsets (list)
  "get a list of all subsets of given list"
  (cl:assert (<= (length list) 20)
             ()
             (string-append "subsets: Can't generate subsets of list longer"
                            "then 20, not enough memory!"))
  (case (length list)
    (0 ())
    (1 (list () list))
    (otherwise (let ((subsets (subsets (rest list))))
                 (append subsets (mapcar (lambda (x) (cons (first list) x))
                                         subsets))))))
;; (subsets '(1 2)) = ((1 2) (1) (2) ())

(defun to-list (x)
  "when given an atom, returns list containing it,
   when given a list, just returns it"
  (if (listp x)
      x
      (list x)))
;; (to-list '(a)) => (a)
;; (to-list 'a) => (a)

(defun to-list-of-lists (list)
  "collects return value of to-list on each element"
  (mapcar #'to-list list))
;; (to-list-of-lists '(a (b :default 10))) => ((a) (b :default 10))

(defmacro ext-pushnew (item place &key (test '#'equalp) (key '#'identity))
  "like pushnew, but as a second value returns,
   whether the list was actualy altered"
  (let ((length (gensym "length"))
        (new-list (gensym "new-list")))
    `(let ((,length (length ,place))
           (,new-list (pushnew ,item ,place :test ,test :key ,key)))
       (values ,new-list (not (= (length ,new-list) ,length))))))

(defmacro push-end (item list)
  "pushes item at the end of the list"
  `(progn (if ,list (nconc ,list (cons ,item nil)) (setf ,list (list ,item)))
          ,list))

(defmacro pushnew-end (item list &key (key '#'identity) (test '#'equalp))
  "pushes the item at the end of list, if it's not yet in the list
   as a second value, returns whether the list was actualy altered"
  `(if (find ,item ,list :key ,key :test ,test)
       (values ,list nil)
       (values (push-end ,item ,list) t)))

(defmacro ext-delete (item place &key (test '#'equalp) (key '#'identity))
  "like delete, but as a second value returns,
   whether the list was actualy altered"
  (let ((length (gensym "length"))
        (new-list (gensym "new-list")))
    `(let ((,length (length ,place))
           (,new-list (delete ,item ,place :test ,test :key ,key)))
       (values ,new-list (not (= (length ,new-list) ,length))))))

(defmacro diff-remove (item sequence &key (test '#'equalp) (key '#'identity))
  "like delete, but as a second value returns list of deleted items"
  (let ((new-list (gensym "new-list"))
        (deleted (gensym "deleted"))
        (elem (gensym "elem")))
    `(let (,new-list ,deleted)
       (dolist (,elem ,sequence)
         (if (funcall ,test ,item (funcall ,key ,elem))
             (push ,elem ,deleted)
             (push ,elem ,new-list)))
       (setf ,sequence (nreverse ,new-list))
       (values ,sequence ,deleted))))

(defmacro push-update (item place &key (test '#'equalp) (key '#'identity))
  "like pushnew, but if there is test-equal atom in the place,
   replaces it by item"
  `(progn
     (setf ,place (delete ,item ,place :test ,test :key ,key))
     (push ,item ,place)))

(defun select (list indices)
  "get a list of values from list according to the list of indices"
  (mapcar (lambda (i)
            (nth i list))
          indices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro mac-exp (&body body)
  "shortcut for calling macroexpand-1"
  `(pprint (macroexpand-1 ',@body)))

(defun hash->list (hash)
  "returns list of all values in the hash"
  (iter (for (key val) in-hashtable hash)
        (collect val)))

(defgeneric weak-equal-p (obj1 obj2)
  (:documentation "ExiL default weak equality predicate")
  (:method (obj1 obj2) (equalp obj1 obj2))
  (:method ((sym1 symbol) (sym2 symbol))
    (equalp (to-keyword sym1) (to-keyword sym2)))
  (:method ((cons1 cons) (cons2 cons))
    (and (weak-equal-p (car cons1) (car cons2))
         (weak-equal-p (cdr cons1) (cdr cons2)))))
