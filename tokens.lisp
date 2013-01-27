(in-package :exil-rete)

(defclass token () ((parent :reader parent :initarg :parent
                            :initform (make-empty-token))
                    (wme :reader wme :initarg :wme
                         :initform (error "wme slot has to be specified"))
                    (negative-wmes :initform nil :accessor negative-wmes)))

(defclass empty-token (token) ((wme :initform nil)
                               (parent :initform nil)))

(defun make-empty-token ()
  (make-instance 'empty-token))

(defun empty-token-p (token)
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
            (nreverse
             (loop for tkn = token then (parent tkn)
                until (empty-token-p tkn)
                collect (wme tkn))))))

(defmethod previous-wme ((token token) &optional (n 1))
  "gives wme from token n wmes back"
  (dotimes (i n (wme token))
    (setf token (parent token))
    (unless token (return))))

;; it would be more logical if the arguements were switched, but this is
;; more convenient, when i supply this predicate as a test to delete
(defmethod includes-p ((fact fact) (token token))
  (loop for tkn = token then (parent tkn)
     ;; unless tkn do (return nil)
     when (empty-token-p tkn) do (return nil)
     when (exil-equal-p fact (wme tkn)) do
       (return t)))

(defmethod includes-p ((included-token token) (token token))
  (loop for tkn = token then (parent tkn)
     unless tkn do (return nil)
     when (token-equal-p tkn included-token) do
       (return t)))

(defun token->list (token)
  (loop with list = ()
     for tkn = token then (parent tkn)
     when (typep tkn 'empty-token) do (return list)
     do (push (wme tkn) list)))
