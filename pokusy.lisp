(in-package :exil)

(deffacts blah
	(b1 on b2)
	(b1 on b3)
	(b1 color red)
	(b2 on table)
	(b2 left-of b3)
	(b2 color blue)
	(b3 left-of b4)
	(b3 on table)
	(b3 color red))

(reset)

(defun sumarize-children (node-list)
  (apply #'append (mapcar #'children node-list)))

(defun sumarize-memories (node-list)
  (mapcar #'memory node-list))

(defun print-memories (mem-list)
  (dolist (mem mem-list)
    (format t "~A's items:~%  ~A~%" mem (items mem))))

(defparameter a-memories (list mn1 mn2 mn3))

(defun print-mems ()
  (print-memories memories))

(defun act (n)
  (node-activation alpha-top-node (nth n (facts))))

#|
(trace node-activation)

; (B3 color RED)
(act 0)
(print-mems)

; (B3 on TABLE)
(act 1)
(print-mems)

; (B3 left-of B4)
(act 2)
(print-mems)

; (B2 color BLUE)
(act 3)
(print-mems)

; (B2 left-of B3)
(act 4)
(print-mems)

; (B2 on TABLE)
(act 5)
(print-mems)

; (B1 color RED)
(act 6)
(print-mems)

; (B1 on B3)
(act 7)
(print-mems)

; (B1 on B2)
(act 8)
(print-mems)
|#

;; beta part is working too! hallowed are both parentheses

(deftemplate blah (a (b :default 10)))
(defvar *fact* (make-fact '(blah foo bar)))
(defvar *tmpl-fact* (make-fact '(blah :a 5)))

(defrule two-blocks-left-of-red-one
  (?x on ?y)
  (?y left-of ?z)
  (?z color red)
=>
)

(defvar *rule* (find-rule 'two-blocks-left-of-red-one))
(new-production *rule*)
(print-rete)
(reset-environment)
(defvar *conds* (conditions *rule*))