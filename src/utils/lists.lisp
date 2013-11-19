(in-package :exil-utils)

(defun last1 (list)
  (car (last list)))

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

;; this is very inefficient
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

;; not used
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
