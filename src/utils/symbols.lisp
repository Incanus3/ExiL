(in-package :exil-utils)

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
