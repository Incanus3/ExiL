(in-package :rete-tests)

;; environment mock - will receive add- and remove-match from rete

(defclass env-mock ()
  ((matches :accessor matches :initform ())))

(defmethod exil-env:add-match ((env env-mock) production token)
  (pushnew (exil-env::make-match production token)
           (matches env) :test #'exil-env::match-equal-p))

(defmethod exil-env:remove-match ((env env-mock) production token)
  (setf (matches env) (delete (exil-env::make-match production token)
                              (matches env) :test #'exil-env::match-equal-p)))

(defmethod has-match ((env env-mock) production token)
  (find (exil-env::make-match production token)
        (matches env) :test #'exil-env::match-equal-p))

;; need to move object-makers to core first
(defclass rete-tests (test-case)
  ((env :accessor env)
   (rete :accessor rete)
   (move :reader move
         :initform (make-rule 'move '((goal ?action ?object ?from ?to)
                                      (in ?object ?from)
                                      ())))))

(defmethod set-up ((tests rete-tests))
  (with-slots (env rete) tests
    (setf env (make-instance 'env-mock))
    (setf rete (make-rete env))))

;; rete has four entry-points:
;; add-wme, remove-wme, add-production, remove-production
