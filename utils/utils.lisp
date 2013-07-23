(in-package :exil-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOLS:

(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern (symbol-name symbol) :keyword))
;; (to-keyword 'a) => :a

(defun gensymedp (symbol)
  (not (symbol-package symbol)))
;; (gensymedp 'abc) => nil
;; (gensymedp '#:abc) => t
;; (gensymedp (gensym "abc") => t

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))
;; (string-append "a" "b" "c") => "abc"

(defgeneric symname (object)
  (:method ((symbol symbol)) (symbol-name symbol))
  (:method ((string string)) (string-upcase string)))
;; (symname 'a) => "A"
;; (symname "a") => "A"

(defun symbol-append (&rest symbols)
  (intern (apply #'string-append (mapcar #'symname symbols))))
;; (symbol-append "copy-" 'facts) => copy-facts

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
(defmacro add-assoc-value (the-key alist value &key (test '#'equal))
  (let ((key-sym (gensym "key"))
        (value-sym (gensym "value"))
	(test-sym (gensym "test")))
    `(let ((,key-sym ,the-key)
           (,value-sym ,value)
	   (,test-sym ,test))
       (if (assoc-value ,key-sym ,alist :test ,test-sym)
           (setf (assoc-value ,key-sym ,alist :test ,test-sym) ,value-sym)
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

(defun list-difference (list1 list2 &key (test #'equal))
  (iter (for item :in list1)
	(unless (find-if (lambda (item2)
			   (funcall test item item2))
			 list2)
	  (collect item))))

;; finds element of list for which func returns true, returning the element
;; and the results of application of func to the element (as subsequent values)
(defun find-if-func-result (func list)
  (iter (for item :in list)
	(for results = (multiple-value-list (funcall func item)))
	(when (first results)
	  (return (values-list (cons item results))))))

;; trees
(defun tree-find-all-if (pred tree)
  (if (listp tree)
      (mapcan (lambda (subtree)
		(tree-find-all-if pred subtree)) tree)
      (when (funcall pred tree) (list tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HASH-TABLES

(defun hash-values (hash)
  "returns list of all values in the hash"
  (iter (for (key val) :in-hashtable hash)
        (collect val)))

(defun hash-keys (hash)
  (iter (for (key val) :in-hashtable hash)
	(collect key)))

(defun hash->alist (hash)
  (let (alist)
    (maphash (lambda (key val)
	       (add-assoc-value key alist val :test (hash-table-test hash)))
	     hash)
    alist))

(defun map-hash-table (fun hash)
  "returns new hash with fun applied to each value"
  (let ((new-hash (make-hash-table :test (hash-table-test hash))))
    (maphash (lambda (key val) (setf (gethash key new-hash) (funcall fun val)))
	     hash)
    new-hash))

(defun copy-hash-table (hash)
  (map-hash-table #'identity hash))

(defun hash-equal-p (hash1 hash2 &key (test #'equal))
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (equalp (hash-table-test hash1) (hash-table-test hash2))
       (iter (for (key val) :in-hashtable hash1)
	     (always (funcall test val (gethash key hash2))))))

(defun partition-hash (list fun &key (test 'equal))
  (let ((partition (make-hash-table :test test)))
    (dolist (item (reverse list))
      (let ((result (funcall fun item)))
	(if (gethash result partition)
	    (push item (gethash result partition))
	    (setf (gethash result partition) (list item)))))
    partition))

(defun partition (list fun &key (test 'equal))
  (hash->alist (partition-hash list fun :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETS

(defun set-equal-p (set1 set2 &key (test #'eql))
  (null (set-exclusive-or set1 set2 :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTING OUTPUT

(defun fresh-format (stream control-string &rest args)
  (fresh-line stream)
  (apply #'format stream control-string args))

(defun fresh-princ (object &optional stream)
  (fresh-line stream)
  (princ object stream)
  nil)
