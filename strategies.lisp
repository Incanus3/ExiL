(in-package :exil-env)

(defmethod newer-than ((match1 match) (match2 match))
  (> (timestamp match1)
     (timestamp match2)))

(defun depth-strategy (agenda)
  (first (sort agenda #'newer-than)))

(defun breadth-strategy (agenda)
  (first (sort agenda (complement #'newer-than))))

(defmethod simpler-than ((rule1 rule) (rule2 rule))
  (< (length (conditions rule1))
     (length (conditions rule2))))

(defmethod simpler-than ((match1 match) (match2 match))
  (simpler-than (match-rule match1) (match-rule match2)))

(defun simplicity-strategy (agenda)
  (first (sort agenda #'simpler-than)))

(defun complexity-strategy (agenda)
  (first (sort agenda (complement #'simpler-than))))
