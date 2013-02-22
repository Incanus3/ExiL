(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

;; TODO: this shouldn't take function, instead it should seem more like
;; a function definition - it should take name and body, pass the body to parser
;; to create the strategy (which will probably still be just a function)
;; and then store the strategy in the environment under given name
; public
(defmacro defstrategy (name function)
  "define new strategy"
  `(add-strategy ',name ,function))

; public
(defmacro setstrategy (name)
  "set strategy to use"
  `(set-strategy ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

; public
(defmacro defrule (name &body body)
  "define rule"
  `(add-rule *current-environment*
             (parse-rule *current-environment* ',name ',body)))

; public
(defmacro undefrule (name)
  "undefine rule"
  `(rem-rule *current-environment* ',name))

;; TODO: printing functionality should be moved to core
(defun ppdefrule% (name)
  (let ((rule (find-rule *current-environment* name)))
    (format t "(defrule ~A~{~%  ~A~}~%  =>~{~%  ~S~})"
            name (conditions rule) (activations rule))))

; public
(defmacro ppdefrule (name)
  "pretty-print rule definition"
  `(ppdefrule% ',name))
