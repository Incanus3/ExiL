(in-package :exil-env)

(defparameter env (make-environment))

(add-fact env (make-simple-fact '(in box1 hall)))
(add-fact env (make-simple-fact '(color box1 green)))
(add-fact env (make-simple-fact '(in box2 hall)))
(add-fact env (make-simple-fact '(color box2 blue)))
(add-fact env (make-simple-fact '(size box2 small)))
(add-fact env (make-simple-fact '(in box3 hall)))
(add-fact env (make-simple-fact '(color box3 blue)))
(add-fact env (make-simple-fact '(size box3 big)))

(facts env)

(add-goal env (make-simple-pattern '(in ?object hall)))
(add-goal env (make-simple-pattern '(color ?object blue)))
(add-goal env (make-simple-pattern '(size ?object big)))

(goals env)

#|
(back-step env)
|#
