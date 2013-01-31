(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token constitutes a linked list of facts that meet some rule's conditions,
;; e.g. if token's wme is some fact that meets some rule's third condition
;; then its parent's wme is a fact that meets the rule's second condition, etc.
;; Together with the rule the token forms a match (either partial or complete,
;; if a set of facts is found, that meets all the rule's conditions).
;; By pairing the token's wmes (facts) to the rule's conditions (patterns)
;; we get the variable bindings, that are then used in the rule's activations.
;; WME stands for working memory element, which is RETE's terminology for facts
;; that are matched against some rule's conditions. The RETE network keeps
;; tokens (complete or partial matches) in each of its nodes, so many copies
;; of the same WME may exist at the moment, whereas the environment stores only
;; one instance of the fact.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass token () ((parent :reader parent :initarg :parent
                            :initform (make-empty-token))
                    (wme :reader wme :initarg :wme
                         :initform (error "wme slot has to be specified"))
                    (negative-wmes :initform nil :accessor negative-wmes)))

(defclass empty-token (token) ((wme :initform nil)
                               (parent :initform nil)))

(defun make-empty-token ()
  (make-instance 'empty-token))

(defmethod empty-token-p ((token token))
  (typep token 'empty-token))

(defun make-token (wme &optional (parent (make-empty-token)))
  (make-instance 'token :wme wme :parent parent))

(defgeneric token-equal-p (token1 token2)
  (:documentation "token equality predicate")
  (:method ((token1 empty-token) (token2 empty-token)) t)
  (:method ((token1 token) (token2 empty-token)) nil)
  (:method ((token1 empty-token) (token2 token)) nil)
  (:method ((token1 token) (token2 token))
    (and (exil-equal-p (wme token1) (wme token2))
         (token-equal-p (parent token1) (parent token2)))))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (format stream "~A"
            (iter (for tkn :first token :then (parent tkn))
                  (until (empty-token-p tkn))
                  (collect (wme tkn) at beginning)))))

;; wme of the token n steps back in the hierarchy (or nil, if the parent-chain
;; is not that long
(defmethod previous-wme ((token token) &optional (n 1))
  "gives wme from token n wmes back"
  (dotimes (i n (wme token))
    (setf token (parent token))
    (unless token (return))))

;; is fact included in token's hierarchy wmes?
;; parameter's are ordered this way so that it can be called like this:
;; (delete fact (list of tokens) :test #'included-in-p)
(defmethod included-in-p ((fact fact) (token token))
  (iter (for tkn :first token :then (parent tkn))
        (until (empty-token-p tkn))
        (when (exil-equal-p fact (wme tkn)) (return t))))

;; is included-token included in token's hierarchy?
;; (e.g. is it token-equal to either token or some of its ancestors?)
(defmethod included-in-p ((included-token token) (token token))
  (iter (for tkn :first token :then (parent tkn))
        (while tkn)
        (when (token-equal-p tkn included-token) (return t))))

;; extracts wmes from token, ordered from first ancestor -> token
(defun token->list (token)
  (iter (for tkn :first token :then (parent tkn))
        (until (empty-token-p tkn))
        (collect (wme tkn) :at :beginning)))
