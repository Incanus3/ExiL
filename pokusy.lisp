(in-package :exil)

#|
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

;; reset by mel po vymazani prostredi zavest fakta definovana pomoci deffacts
(reset)

(defrule two-blocks-left-of-red-one
  (?x on ?y)
  (?y left-of ?z)
  (?z color red)
=>
)

(assert (B3 color RED))
(print-memories)
(assert (B3 on TABLE))
(print-memories)
(assert (B3 left-of B4))
(print-memories)
;; tenhle fakt vubec neprojde
(assert (B2 color BLUE))
(print-memories)
(assert (B2 left-of B3))
(print-memories)
(assert (B2 on TABLE))
(print-memories)
(assert (B1 color RED))
(print-memories)
(assert (B1 on B3))
(print-memories)
(assert (B1 on B2))
(print-memories)

(retract (b1 on b2))

(undefrule two-blocks-left-of-red-one)

(deftemplate blah (a (b :default 10)))
(defvar *fact* (make-fact '(blah foo bar)))
(defvar *tmpl-fact* (make-fact '(blah :a 5)))

(defvar *rule* (find-rule 'two-blocks-left-of-red-one))
(new-production *rule*)
(print-rete)
(reset-environment)
(defvar *conds* (conditions *rule*))
|#

(defmethod get-alpha-memories ((node alpha-subtop-node))
  (let (memories)
    (labels ((walk-through (node)
	       (if (typep node 'alpha-memory-node)
		   (push node memories)
		   (dolist (child (node-children node))
		     (walk-through child)))))
      (walk-through node)
      memories)))

(defmethod get-beta-memories ((node beta-top-node))
  (let (memories)
    (labels ((walk-through (node)
	       (when (typep node 'beta-memory-node)
		 (push node memories))
	       (dolist (child (children node))
		 (walk-through child))))
      (walk-through node)
      (nreverse memories))))
	  
(defmethod print-memory ((memory memory-node))
  (format t "~A:~%  ~A~%" memory (items memory)))

(defun print-alpha-mems (&optional (rete (rete)))
  (dolist (mem (get-alpha-memories (get-network (alpha-top-node rete))))
    (print-memory mem)))

(defun print-beta-mems (&optional (rete (rete)))
  (dolist (mem (get-beta-memories (beta-top-node rete)))
    (print-memory mem)))

(defun print-memories (&optional (rete (rete)))
  (fresh-line t)
  (terpri t)
  (print-alpha-mems rete)
  (terpri t)
  (print-beta-mems rete))