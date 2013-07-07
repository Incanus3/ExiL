(in-package :exil-rete)

;; rete stores reference to environment, to be able to call its add-match and
;; remove-match callbacks
(defclass rete () ((alpha-top-node :reader alpha-top-node
                                   :initform (make-instance 'alpha-top-node))
                   (beta-top-node  :accessor beta-top-node
                                   :initform (make-instance 'beta-top-node))
                   (environment :reader environment :initarg :environment)))

(defun make-rete (environment)
  (make-instance 'rete :environment environment))

;; public methods
(defgeneric add-wme (rete wme)
  (:documentation "called when new wme has been added to environment, may
                   result in add-match or remove-match callback"))
(defgeneric rem-wme (rete wme)
  (:documentation "called when wme has been removed from environment, may
                   result in add-match or remove-match callback"))
(defgeneric new-production (rete production)
  (:documentation "creates necessary rete network nodes for new production"))
(defgeneric remove-production (rete production)
  (:documentation "removes production from all beta memory nodes"))

(defmethod add-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (activate (alpha-top-node rete) wme))

(defmethod rem-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (inactivate (alpha-top-node rete) wme))
