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



(defrule find-stack-of-two-blocks-to-the-left-of-a-red-block
  (?x on ?y)
  (?y left-of ?z)
  (?z color red)
=>
)

;; Alpha network part
(defparameter *rete* (rete *current-environment*))

(defparameter alpha-top-node (alpha-top-node *rete*))

(defparameter simple-fact-alpha-subtop (gethash (simple-fact-key-name alpha-top-node)
					  (networks alpha-top-node)))

(defparameter mn1 (make-instance 'alpha-memory-node
				 :description "condition: (?x on ?y)"))
(defparameter mn2 (make-instance 'alpha-memory-node
				 :description "condition: (?y left-of ?z)"))
(defparameter mn3 (make-instance 'alpha-memory-node
				 :description "condition: (?z color RED)"))

(defparameter tn0 (make-instance 'simple-fact-test-node
			   :tested-field 0
			   :value '?))
(defparameter tn1 (make-instance 'simple-fact-test-node
			   :tested-field 1
			   :value 'on))
(defparameter tn2 (make-instance 'simple-fact-test-node
			   :tested-field 1
			   :value 'left-of))
(defparameter tn3 (make-instance 'simple-fact-test-node
			   :tested-field 1
			   :value 'color))
(defparameter tn4 (make-instance 'simple-fact-test-node
			   :tested-field 2
			   :value '?
			   :memory mn1))
(defparameter tn5 (make-instance 'simple-fact-test-node
			   :tested-field 2
			   :value '?
			   :memory mn2))
(defparameter tn6 (make-instance 'simple-fact-test-node
			      :tested-field 2
			      :value 'red
			      :memory mn3))

(add-child simple-fact-alpha-subtop tn0)
(add-children tn0 (list tn1 tn2 tn3))
(add-child tn1 tn4)
(add-child tn2 tn5)
(add-child tn3 tn6)

;; alpha-test-nodes have an extra slot for alpha memory
;(add-child tn4 mn1)
;(add-child tn5 mn2)
;(add-child tn6 mn3)

(defun sumarize-children (node-list)
  (apply #'append (mapcar #'children node-list)))

(defun sumarize-memories (node-list)
  (mapcar #'memory node-list))

(defun print-memories (mem-list)
  (dolist (mem mem-list)
    (format t "~A's items:~%  ~A~%" mem (items mem))))

(defparameter a-memories (list mn1 mn2 mn3))

;; beta network part

(defparameter bm1 (make-instance 'beta-memory-node
				 :description "(?x on ?y)"))
(defparameter bm2 (make-instance 'beta-memory-node
				 :description "(?x on ?y) & (?y left-of ?z)"))
(defparameter pn (make-instance 'production-node
				:production 'rule
				:description "(?x on ?y) & (?y left-of ?z) & (?z color RED)"))

(defparameter b-memories (list bm1 bm2 pn))
(defparameter memories (append a-memories b-memories))

(defparameter beta-top-node (make-instance 'beta-top-node
					   :alpha-memory mn1
					   :beta-memory bm1))

(push beta-top-node (beta-top-nodes *rete*))

(defparameter jn1 (make-instance 'beta-join-node
			   :parent bm1
			   :alpha-memory mn2
			   :beta-memory bm2
			   :tests (list (make-test 0 0 2))
			   :description "(?x on ?y) & (?y left-of ?z)"))

(defparameter jn2 (make-instance 'beta-join-node
			   :parent bm2
			   :alpha-memory mn3
			   :beta-memory pn
			   :tests (list (make-test 0 0 2))
			   :description "(?x on ?y) & (?y left-of ?z) & (?z color RED)"))

(add-child mn1 beta-top-node)
(add-child mn2 jn1)
(add-child mn3 jn2)

(add-child bm1 jn1)
(add-child bm2 jn2)

(defun print-mems ()
  (print-memories memories))

(defun act (n)
  (node-activation alpha-top-node (nth n (facts))))



;; alpha part is working!!! hallowed is the left parenthesis

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
  (?x is ?x)
=>
)

(defvar *rule* (find-rule 'two-blocks-left-of-red-one))
(defvar *conds* (conditions *rule*))