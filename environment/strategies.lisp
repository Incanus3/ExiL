(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies are functions, that implement selecting the activation to be
;; fired from environment's agenda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric newer-than-p (match1 match2))
(defgeneric simpler-than-p (obj1 obj2))

(defmethod newer-than-p ((match1 match) (match2 match))
  (> (timestamp match1)
     (timestamp match2)))

(defun depth-strategy (agenda)
  (first (sort agenda #'newer-than-p)))

(defun breadth-strategy (agenda)
  (first (sort agenda (complement #'newer-than-p))))

(defmethod simpler-than-p ((rule1 rule) (rule2 rule))
  (< (length (conditions rule1))
     (length (conditions rule2))))

(defmethod simpler-than-p ((match1 match) (match2 match))
  (simpler-than-p (match-rule match1) (match-rule match2)))

(defun simplicity-strategy (agenda)
  (first (sort agenda #'simpler-than-p)))

(defun complexity-strategy (agenda)
  (first (sort agenda (complement #'simpler-than-p))))
