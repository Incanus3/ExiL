(in-package :exil-env)

; public
(defclass match () ((rule :initarg :rule :reader match-rule
			  :initform (error "match rule has to be specified"))
		    (token :initarg :token :reader match-token
			   :initform (error "match token has to be specified"))
		    (timestamp :initarg :timestamp :reader timestamp
			       :initform (get-internal-real-time))))

; public
(defun make-match (rule token &optional (timestamp (get-internal-real-time)))
  (make-instance 'match :rule rule :token token :timestamp timestamp))

; public
(defmethod match-equal-p ((match1 match) (match2 match))
  (and (rule-equal-p (match-rule match1)
		     (match-rule match2))
       (token-equal-p (match-token match1)
		      (match-token match2))))

; public
(defmethod print-object ((match match) stream)
  (if *print-escape* 
      (print-unreadable-object (match stream :type t :identity t)
	(format stream "~S" (list (match-rule match)
				  (token->list (match-token match)))))
      (format stream "Activation ~A:~%~A"
	      (name (match-rule match)) (token->list (match-token match))))
  match)