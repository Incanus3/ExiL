(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta join node handles testing of variable binding consistency between
;; current rule condition (matching wmes stored in alpha memory feeding into
;; this node) and previous conditions (matchin tokens stored in beta memory
;; feeding into this node)
;; it can be activated either by the alpha memory when newly added wme matches
;; the current condition, or by the beta memory when newly added wme matches
;; some of the previous conditions, in both cases the other memory is searched
;; for items with consistent variable bindings
;; children are beta memory nodes, there's always just one child

(defclass beta-join-node (beta-node)
  ((alpha-memory :accessor alpha-memory :initarg :alpha-memory
;                 :initform (error "alpha-memory slot has to be specified")
		 )
   (tests :accessor tests :initarg :tests :initform ())))

(defgeneric beta-memory (node)
  (:documentation "the beta memory node this node feeds into"))
(defgeneric perform-join-test (test token wme))
(defgeneric perform-join-tests (test-list token wme))

(defmethod initialize-instance :after ((node beta-join-node) &key rete)
  ;; creates parent beta-memory-node and adds itself as its child
  ;; how does the beta-memory-node's parent get set?
  (add-child node (make-instance 'beta-memory-node :parent node :rete rete)))

(defmethod beta-memory ((node beta-join-node))
  (first (children node)))

;; token doesn't include the wme, that's why there's 1- in the call
;; to previous-wme
(defmethod perform-join-test ((test test) (token token) (wme fact))
  (let ((previous-wme (previous-wme token (1- (previous-condition test)))))
    (when previous-wme
      (equalp (object-slot wme (current-field test))
              (object-slot previous-wme (previous-field test))))))

(defmethod perform-join-tests ((tests list) (token token) (wme fact))
  (dolist (test tests t)
    (unless (perform-join-test test token wme) (return nil))))
    
;; left activation - by parent beta-memory-node
(defmethod activate ((node beta-join-node) (token token))
  (dolist (wme (items (alpha-memory node)))
    (if (perform-join-tests (tests node) token wme)
        (activate-children node (make-token wme token)))))

;; right activation - by alpha-memory-node which feeds into it
(defmethod activate ((node beta-join-node) (wme fact))
  (dolist (token (items (parent node)))
    (if (perform-join-tests (tests node) token wme)
        (activate-children node (make-token wme token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta negative node handles join tests (variable binding consistency tests
;; between rule's conditions) for negated conditions
;; these conditions are satisfied if there's no fact in the working memory
;; congruent with the pattern (with consistent variable bindings)

(defclass beta-negative-node (beta-join-node memory-node)
  ;; negative-wmes holds assoc list of (token matching-wmes)
  ;; when last matching wme is removed from token's list, the negative
  ;; condition is satisfied
  ((negative-wmes :initform () :accessor negative-wmes)))

(defun get-neg-wmes (node token)
  (assoc-value token (negative-wmes node) :test #'token-equal-p))

(defun set-neg-wmes (node token wmes)
  (add-assoc-value token (negative-wmes node) wmes :test #'token-equal-p))

(defun ensure-neg-wmes (node token)
  (unless (get-neg-wmes node token)
    (set-neg-wmes node token ())))

(defun add-neg-wme (node token wme)
  (ensure-neg-wmes node token)
  (pushnew wme (assoc-value token (negative-wmes node) :test #'token-equal-p)
	   :test #'exil-equal-p))

(defun rem-neg-wme (node token wme)
  (ensure-neg-wmes node token)
  (set-neg-wmes node token (delete wme (get-neg-wmes node token)
				   :test #'exil-equal-p)))

;; returns list of wmes (from neg-node's alpha-memory), which have consistent
;; variable bindings with token, i.e. which pass the consistency tests
(defun get-consistent-wmes (node token)
  (remove-if-not (lambda (wme) (perform-join-tests (tests node) token wme))
                 (items (alpha-memory node))))
    
;; left activation
;; when activated by token (previous conditions satisfied), ensure there are
;; no wmes consistent with the token, if not, activate children
;; store the consistent wmes (which are blocking the negated condition from
;; being satisfied) in the token for future use
(defmethod activate ((node beta-negative-node) (token token))
  (let ((bad-wmes (get-consistent-wmes node token)))
    (unless bad-wmes
      (activate-children node (make-token nil token)))
    (set-neg-wmes node token bad-wmes)
    (add-item node token #'token-equal-p)))

;; right activation
;; when activated by wme (from alpha memory) we need to check, whether this
;; breaks any negative condition match (if we find a token consistent with
;; this wme, where no consistent wmes were previously blocking
;; ((negative-wmes token) was empty), we need to signal this broken match
;; to children
(defmethod activate ((node beta-negative-node) (wme fact))
  (dolist (token (items node))
    (when (perform-join-tests (tests node) token wme)
      (unless (get-neg-wmes node token)
        (inactivate-children node (make-token nil token)))
      (add-neg-wme node token wme))))

;; left inactivation only propagates to children

;; right inactivation
;; when inactivated by wme (from alpha memory) we need to check, wheter
;; there's some token previously blocked by this wme (i.e. this is the last
;; wme congruent with the negative condition, with bindings consistent with
;; the rest of token) and in that case signal newly satisfied negative
;; condition to children
(defmethod inactivate ((node beta-negative-node) (wme fact))
  (inactivate-children node wme)
  (setf (items node) (delete wme (items node) :test #'included-in-p))
  (dolist (token (items node))
    (when (and (get-neg-wmes node token)
	       (perform-join-tests (tests node) token wme))
      (rem-neg-wme node token wme)
      (unless (get-neg-wmes node token)
        (activate-children node (make-token nil token))))))
