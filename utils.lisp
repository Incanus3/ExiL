(in-package :exil)

(defun cap-intern (string &optional (package *package*))
  (intern (string-upcase string) package))

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

(defmethod my-symbol-name ((symbol symbol))
  (symbol-name symbol))

(defmethod my-symbol-name ((string string))
  string)  

(defun symbol-append (&rest symbols)
  (cap-intern (apply #'string-append (mapcar #'my-symbol-name symbols))))

(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern (symbol-name symbol) :keyword))

(defmacro mac-exp (&body body)
  `(pprint (macroexpand-1 ',@body)))