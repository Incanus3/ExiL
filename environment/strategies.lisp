(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies are functions, that implement selecting the activation to be
;; fired from environment's activations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric newer-than-p (match1 match2))

(defmethod newer-than-p ((match1 match) (match2 match))
  (> (timestamp match1)
     (timestamp match2)))

(defun depth-strategy (activations)
  (first (sort activations #'newer-than-p)))

(defun breadth-strategy (activations)
  (first (sort activations (complement #'newer-than-p))))

(defgeneric simpler-than-p (obj1 obj2))

(defmethod simpler-than-p ((rule1 rule) (rule2 rule))
  (< (length (conditions rule1))
     (length (conditions rule2))))

(defmethod simpler-than-p ((match1 match) (match2 match))
  (simpler-than-p (match-rule match1) (match-rule match2)))

(defun simplicity-strategy (activations)
  (first (sort activations #'simpler-than-p)))

(defun complexity-strategy (activations)
  (first (sort activations (complement #'simpler-than-p))))
