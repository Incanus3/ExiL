(in-package :exil-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOLS:

(defun symbol-name-equal-p (sym1 sym2)
  "returns true if symbol-names are string-equal"
  (string-equal (symbol-name sym1) (symbol-name sym2)))

;; compares symbols or lists of symbols
(defgeneric weak-equal-p (obj1 obj2)
  (:documentation "ExiL default weak equality predicate")
  (:method (obj1 obj2) (equalp obj1 obj2))
  (:method ((sym1 symbol) (sym2 symbol))
    (symbol-name-equal-p sym1 sym2))
  (:method ((cons1 cons) (cons2 cons))
    (and (weak-equal-p (car cons1) (car cons2))
         (weak-equal-p (cdr cons1) (cdr cons2)))))

(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern (symbol-name symbol) :keyword))
;; (to-keyword 'a) => :a

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

;; this probably could be done better with defsetf/define-setf-expander
;; sets value if key present, otherwise adds the cons
(defmacro add-assoc-value (the-key alist value)
  (let ((key-sym (gensym "key"))
        (value-sym (gensym "value")))
    `(let ((,key-sym ,the-key)
           (,value-sym ,value))
       (if (assoc-value ,key-sym ,alist)
           (setf (assoc-value ,key-sym ,alist) ,value-sym)
           (push (cons ,key-sym ,value-sym) ,alist)))))

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

(defun plistp (list)
  "is the list a property list?"
  (every-couple% (lambda (key val)
                   (declare (ignore val))
                   (keywordp key))
                 list))

(defmacro doplist ((key val plist &optional (retval nil)) &body body)
  "iterates over plist setting key and val variables for each iteration"
  (let ((sym-plist (gensym "plist")))
    `(let ((,sym-plist (copy-list ,plist)))
       (iter (for ,key = (pop ,sym-plist))
             (for ,val = (pop ,sym-plist))
             (while ,key)
             ,@body
             (finally (return ,retval))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISTS:

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

(defun numbered-map (fun list &optional (start 1))
  (iter (for i :upfrom start)
	(for item in list)
	(collect (list i (funcall fun item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash->list (hash)
  "returns list of all values in the hash"
  (iter (for (key val) :in-hashtable hash)
        (collect val)))
