(in-package :exil-utils)

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
