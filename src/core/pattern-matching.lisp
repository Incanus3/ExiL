(in-package :exil-core)

;; this has quadratic complexity, but for short lists, is probabably faster than
;; constructing a hash
(defun every-key-once-p (alist)
  (every (lambda (pair)
	   (= (count (car pair) alist :key #'car) 1))
	 alist))

;; returns cons (var . binding), nil or :mismatch
(defun match-fact-atom (fact-atom pattern-atom)
  (if (variable-p pattern-atom)
      (cons pattern-atom fact-atom)
      (unless (equalp pattern-atom fact-atom)
	:mismatch)))

;; returns cons (var . binding), nil or :mismatch
(defun match-pattern-atom (pattern1-atom pattern2-atom)
  (cond ((variable-p pattern2-atom) (cons pattern2-atom pattern1-atom))
	((variable-p pattern1-atom) (cons pattern1-atom pattern2-atom))
	((not (equalp pattern1-atom pattern2-atom)) :mismatch)))

(defgeneric atom-matcher (object)
  (:method ((fact fact))       #'match-fact-atom)
  (:method ((pattern pattern)) #'match-pattern-atom))

;; matches either simple or template objects using atom-matcher
;; returns list of variable bindings, which may contain the symbol :mismatch
(defgeneric match-against-pattern% (object pattern atom-matcher))

(defmethod match-against-pattern% ((object simple-object) (pattern template-pattern)
                                   matcher)
  (list :mismatch))

(defmethod match-against-pattern% ((object template-object) (pattern simple-pattern)
                                   matcher)
  (list :mismatch))

(defmethod match-against-pattern% ((object simple-object) (pattern simple-pattern)
				     (atom-matcher function))
  (if (= (length (specifier object)) (length (pattern pattern)))
      (mapcar atom-matcher (specifier object) (pattern pattern))
      (list :mismatch)))

(defmethod match-against-pattern% ((object template-object) (pattern template-pattern)
				     (atom-matcher function))
  (if (exil-equal-p (template object) (template pattern))
      (iter (for (slot-name . slot-val) :in (slots pattern))
	    (collect (funcall atom-matcher (object-slot object slot-name) slot-val)))
      (list :mismatch)))

(defun remove-noise (bindings)
  (remove-duplicates (delete nil bindings) :test #'equalp))

;; matches object (fact or pattern) against a pattern, returns two values
;; 1) whether the object matches the pattern
;; 2) variable bindings of the match (or nil when they don't match)
(defmethod match-against-pattern ((object base-object) (pattern pattern))
  (let* ((bindings (remove-noise
                    (match-against-pattern% object pattern (atom-matcher object))))
	 (match-var (match-var pattern)))
    (if match-var (push (cons match-var (description object)) bindings))
    (if (and (not (find :mismatch bindings))
	     (every-key-once-p bindings))
	(values t bindings)
	(values nil nil))))
