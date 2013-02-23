(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies are functions, that implement activation comparison by which
;; the activations are sorted before popping the first one, which is than fired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric newer-than-p (match1 match2))
(defgeneric older-than-p (match1 match2))
(defgeneric simpler-than-p (obj1 obj2))
(defgeneric more-complex-than-p (match1 match2))

(defmethod newer-than-p ((match1 match) (match2 match))
  (> (timestamp match1)
     (timestamp match2)))

(defmethod older-than-p ((match1 match) (match2 match))
  (not (newer-than-p match1 match2)))

(defmethod simpler-than-p ((rule1 rule) (rule2 rule))
  (< (length (conditions rule1))
     (length (conditions rule2))))

(defmethod simpler-than-p ((match1 match) (match2 match))
  (simpler-than-p (match-rule match1) (match-rule match2)))

(defmethod more-complex-than-p ((match1 match) (match2 match))
  (not (simpler-than-p match1 match2))) 
