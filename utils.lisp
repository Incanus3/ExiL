(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various utilities

(defun intern (string &optional (package *package*))
  (cl:intern (string-upcase string) package))

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

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
       (apply #'concatenate 'string string-list))))
)

(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern (symbol-name symbol) :keyword))

(defmacro mac-exp (&body body)
  `(pprint (macroexpand-1 ',@body)))

;; more like sublists - doesn't care about duplicities in the input list
;; i just don't like the name sublists, that's why

(defun subsets (list)
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
