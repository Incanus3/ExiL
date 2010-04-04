(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various utilities

(defun intern (string &optional (package *package*))
  (cl:intern (string-upcase string) package))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-append (&rest strings)
    (apply #'concatenate 'string strings)))

(defgeneric my-symbol-name (symbol)
  (:documentation "For symbol returns its name, for string just return itself")
  (:method ((symbol symbol)) (symbol-name symbol))
  (:method ((string string)) string))

(defun symbol-append (&rest symbols)
  (intern (apply #'string-append (mapcar #'my-symbol-name symbols))))

;; following 2 definitions enables the use of [] parentheses to append
;; the containded strings, this will be useful for managing strings longer
;; than one row
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     (let ((string-list (read-delimited-list #\] stream t)))
       (apply #'string-append string-list)))))

;; following 2 functions may seem redundant (and they are)
;; but they're names tell much more about the purpose of such call
(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern symbol :keyword))

(defun from-keyword (key &optional (package *package*))
  "get sybol from key"
  (intern key package))

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
(defun assoc-value (key plist)
  (cdr (assoc key plist)))

(defun to-list (x)
  (if (listp x)
      x
      (list x)))

(defun to-list-of-lists (list)
  (mapcar #'to-list list))