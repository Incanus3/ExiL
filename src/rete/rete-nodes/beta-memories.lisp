(in-package :exil-rete)

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
                                    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta memory nodes store (complete or partial) matches (sequences of wmes
;; that match some consecutive rule conditions)
;; botom beta-memory-nodes store productions (rules) that
;; are satisfied when the node is activated (i.e. the network path leading
;; to this node represents testing of all the production's conditions)
;; when these nodes are (in)activated, they signal add-match (remove-match)
;; to the environment, that's also why it stores reference to rete
;; (which stores reference to the environment)
;; children are beta-join-nodes, items are tokens

;; forward declarations
(defgeneric exil-env:add-match (env production token))
(defgeneric exil-env:remove-match (env production token))

(defclass beta-memory-node (beta-node memory-node)
  ((productions :accessor productions
                :initform ())
   (rete :accessor rete :initarg :rete
;         :initform (error "rete has to be specified")
	 )))

(defgeneric environment (node)
  (:documentation "environment in which this rete network is defined"))
(defgeneric broken-match (node token)
  (:documentation "pairs token with each of node's productions and removes
                   this match from the environment"))
(defgeneric add-production (node production)
  (:documentation "adds production that is satisfied when node is activated"))
(defgeneric delete-production (node production)
  (:documentation "deletes production from productions list"))

(defmethod environment ((node beta-memory-node))
  (environment (rete node)))

;; when this is a new token, signal newly matched productions and activate
;; children
(defmethod activate ((node beta-memory-node) (token token))
  ;; when it wasn't already there
  (when (ext-add-item node token #'token-equal-p)
    (dolist (production (productions node))
      (exil-env:add-match (environment node) production token))
    (activate-children node token)))

;; signal to environment that token (combined with any production)
;; is not a valid match any more
(defmethod broken-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (exil-env:remove-match (environment node) production token)))

;; right inactivation - the wme has been removed from working memory
;; remove tokens including it and signal broken matches
(defmethod inactivate :before ((node beta-memory-node) (wme fact))
  (multiple-value-bind (new-items deleted) 
      (diff-remove wme (items node) :test #'included-in-p)
    (setf (items node) new-items)
    (dolist (token deleted)
      (broken-match node token))))

;; left inactivation - some other wme, that matched some of the rule's
;; previous conditions, have been removed from working memory, so tokens
;; that include token are not valid matches any more
;; remove tokens including it and signal broken matches
(defmethod inactivate :before ((node beta-memory-node) (token token))
  (multiple-value-bind (new-list deleted)
      (diff-remove token (items node) :test #'included-in-p)
    (setf (items node) new-list)
    (dolist (item deleted)
      (broken-match node item))))

;; add production to productions and signal complete match for already
;; matched tokens
(defmethod add-production ((node beta-memory-node) (production rule))
  (push-update production (productions node) :test #'name-equal-p)
  (dolist (token (items node))
    (exil-env:add-match (rete node) production token)))

;; remove production from productions
;; exil-env:rem-rule, that calls this (indirectly), also removes matches from
;; activations, so there's no need to signal broken matches
(defmethod delete-production ((node beta-memory-node) (production rule))
  (setf (productions node)
        (delete production (productions node) :test #'name-equal-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dummy top-node
(defclass beta-top-node (beta-memory-node)
  ((items :initform (list (make-empty-token)))
   (rete :initform nil)))
