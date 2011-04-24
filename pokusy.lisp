(in-package :exil-test)

#|
(in-package :exil-user)
;   ROBOTS:
(deftemplate goal (action object from to))
(deftemplate in (object location))

(deffacts world
  (in :object robot :location A)
  (in :object box :location B)
  (goal :action push :object box :from B :to A))

(defrule stop
  (goal :object ?x :to ?y)
  (in :object ?x :location ?y)
  =>
  (halt)
  )

(defrule move
  (goal :object ?x :from ?y)
  (in :object ?x :location ?y)
  (- in :object robot :location ?y)
  (in :object robot :location ?z)
  =>
  (modify (in :object robot :location ?z)
	  (in :object robot :location ?y)))

(defrule push
  (goal :object ?x :from ?y :to ?z)
  (in :object ?x :location ?y)
  (in :object robot :location ?y)
  =>
  (modify (in :object robot :location ?y)
	  (in :object robot :location ?z))
  (modify (in :object ?x :location ?y)
	  (in :object ?x :location ?z)))

(watch facts)
(watch activations)

(reset)
(step)
(print-memories)

(run)
(pprint (facts))

(completely-reset-environment)

POSTUP:
1)
(in :obj robot :loc A)
(in :obj box :loc B)
(goal :act push :obj box :from B :to A)

fire MOVE

2)
(in :obj robot :loc B)
(in :obj box :loc B)
(goal :act push :obj box :from B :to A)

fire PUSH

3)
(in :obj robot :loc A)
(in :obj box :loc A)
(goal :act push :obj box :from B :to A)

|#

;DEBUG:

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
  (assert (?x and ?y left-of ?z)))

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

(deftemplate fact (id attr val))

(defrule two-blocks-left-of-red-one-tmpl
  (fact :id ?x :attr on :val ?y)
  (fact :id ?y :attr left-of :val ?z)
  (fact :id ?z :attr color :val red)
=>
)

(print-rete)

(assert (fact :id b3 :attr color :val red))
(assert (fact :id b3 :attr on :val table))
(assert (fact :id b3 :attr left-of :val b4))
(assert (fact :id b2 :attr color :val blue))
(assert (fact :id b2 :attr left-of :val b3))
(assert (fact :id b2 :attr on :val table))
(assert (fact :id b1 :attr color :val red))
(assert (fact :id b1 :attr on :val b3))
(assert (fact :id b1 :attr on :val b2))

(print (agenda))

(retract (fact :id b1 :attr on :val b2))
(undefrule two-blocks-left-of-red-one-tmpl)

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
  (format t "~A:~%  ~A~%~%" memory (items memory)))

(defun print-alpha-mems (&optional (rete (rete)))
  (let ((nets
	 (loop for net being the hash-values in (networks (alpha-top-node rete))
	    collect net)))
    (dolist (net nets)
      (dolist (mem (get-alpha-memories net))
	(print-memory mem)))))

(defun print-beta-mems (&optional (rete (rete)))
  (dolist (mem (get-beta-memories (beta-top-node rete)))
    (print-memory mem)))

(defun print-memories (&optional (rete (rete)))
  (fresh-line t)
  (terpri t)
  (print-alpha-mems rete)
  (terpri t)
  (print-beta-mems rete))
