(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass functional-integration-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

(defmethod set-up ((tests functional-integration-tests))
;;  (reset-environments)
  (complete-reset)
  (unwatch :all))

(def-test-method test-environments
    ((tests functional-integration-tests) :run nil)
  "env modifiers take symbols"
  (defenvf 'test)
  (setenvf 'test)
  "current-environment always returns keyword"
  (assert-equal (current-environment) :test))

(def-test-method test-environments-keywords
    ((tests functional-integration-tests) :run nil)
  "env modifiers take keywords"
  (defenvf :test)
  (setenvf :test)
  (assert-equal (current-environment) :test))

(def-test-method test-environments-mixed
    ((tests functional-integration-tests) :run nil)
  "keywords and symbols are interchangeable"
  (defenvf 'test)
  (setenvf :test)
  (assert-equal (current-environment) :test))

(def-test-method test-environments-not-defined
    ((tests functional-integration-tests) :run nil)
  "nothing happends when environment not defined"
  ;; TODO: show warning
  (setenvf :test)
  (assert-equal (current-environment) :test))


(def-test-method test-watchers ((tests functional-integration-tests) :run nil)
  (watchf 'facts)
  (assert-true (watchedpf 'facts))

  (unwatchf 'facts)
  (assert-false (watchedpf 'facts)))

(def-test-method test-watchers-keywords
    ((tests functional-integration-tests) :run nil)
  (watchf :facts)
  (assert-true (watchedpf :facts))

  (unwatchf :facts)
  (assert-false (watchedpf :facts)))


(def-test-method test-templates ((tests functional-integration-tests) :run nil)
  (deftemplatef 'in '(object (location :default hall)))
  (assert-equal (find-template 'in) '(:in ((:object) (:location :default hall))))
  (assert-equal (templates) '(:in))

  (undeftemplatef 'in)
  (print (templates))
  (assert-false (find-template 'in))
  (assert-false (templates)))

(def-test-method test-templates-keywords
    ((tests functional-integration-tests) :run nil)
  (deftemplatef :in '(object location))
  (assert-equal (find-template :in) '(:in ((:object) (:location))))
  (assert-equal (templates) '(:in))

  (undeftemplatef :in)
  (assert-false (find-template :in))
  (assert-false (templates)))


(def-test-method test-facts ((tests functional-integration-tests) :run nil)
  (let ((fact-spec '(in box hall)))
    (assertf fact-spec)
    (assert-equal (facts) (list fact-spec))

    (retractf fact-spec)
    (assert-false (facts)))

  (let ((fact-spec '(:in :object box :location hall)))
    (deftemplatef :in '(object location))

    (assertf fact-spec)
    (assert-equal (facts) (list fact-spec))

    (modifyf fact-spec '(:location kitchen))
    (assert-equal (facts) '((:in :object box :location kitchen)))

    (retractf '(in :object box :location kitchen))
    (assert-false (facts))))


(def-test-method test-goals ((tests functional-integration-tests) :run nil)
  (defgoalf '(in ?box hall))
  (assert-equal (goals) '((in ?box hall))))


(def-test-method test-fact-groups
    ((tests functional-integration-tests) :run nil)
  (let ((fact-specs '((in :object robot :location A)
                      (in :object box :location B)
                      (goal :action push :object box :from B :to A))))
    (deffactsf :world fact-specs)
    (assert-equal (fact-groups) '(:world))
    (assert-equal (find-fact-group :world) (cons :world fact-specs))

    (undeffactsf :world)
    (assert-false (fact-groups))
    (assert-false (find-fact-group :world))))

(def-test-method test-strategies ((tests functional-integration-tests) :run nil)
  (defstrategyf :test #'first)
  (setstrategyf :test)

  (assert-true (find :test (strategies)))
  (assert-equal (current-strategy) :test)
  (assert-equal (find-strategy :test) #'first)

  (undefstrategyf :test)

  (assert-false (find :test (strategies)))
  (assert-equal (current-strategy) :default)
  (assert-false (find-strategy :test))

  (assert-condition 'error (undefstrategy :default)))

(def-test-method test-rules
    ((tests functional-integration-tests) :run nil)
  (defrulef :move
      '((goal :action push :object ?x :from ?y)
        (in :object ?x :location ?y)
        (- in :object robot :location ?y)
        ?robot <- (in :object robot :location ?)
        =>
        (modify ?robot :location ?y)))

  (defrulef :push
      '((goal :action push :object ?x :from ?y :to ?z)
        ?object <- (in :object ?x :location ?y)
        ?robot <- (in :object robot :location ?y)
        =>
        (modify ?robot :location ?z)
        (modify ?object :location ?z)))

  (assert-true (set-equal-p (rules) '(:move :push)))
  (assert-equal (find-rule :move)
                '(:move ((goal :action push :object ?x :from ?y)
                         (in :object ?x :location ?y)
                         (:- in :object robot :location ?y)
                         ?robot :<- (in :object robot :location ?)
                         :=>
                         (modify ?robot :location ?y))))

  (undefrulef :move)

  (assert-equal (rules) '(:push))
  (assert-false (find-rule :move)))

(add-test-suite 'functional-integration-tests)
;;(textui-test-run (get-suite functional-integration-tests))
