(in-package :exil-parser)

(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

; extracts actual conditions from rule conditions list, which can also
; include ?fact <- <condition> statements
; returns list of pairs (<condition> , <match-variable>)
; TODO: add some tests - when ?fact <- not followed by condition definition
; this will crash in most unpredictable ways
(defun extract-conditions (cond-list)
  (iter (for i :upfrom 0)
        (for cond :first (first cond-list) :then (nth i cond-list))
        (while (< i (length cond-list)))
        (if (listp cond)
            (collect (cons cond nil))
            (progn (collect (cons (nth (+ i 2) cond-list) cond))
                   (incf i 2)))))

;; TODO: dodelat kontrolu, zda se vsechny promenne v RHS vyskytuji v LHS
; public
(defmethod parse-rule ((env environment) (name symbol) (body list))
  (when (stringp (first body))
    (pop body)) ;; ignore clips rule header
  (let ((=>-position (position '=> body :test #'weak-equal-p)))
    (assert =>-position ()
            "Rule definition must include '=>'!")
    (let ((conditions (extract-conditions (subseq body 0 =>-position)))
          (activations (subseq body (1+ =>-position))))
      (make-rule (to-keyword name)
                 (mapcar (lambda (condition)
                           (parse-pattern env (car condition)
                                          :match-var (cdr condition)))
                         conditions)
                 activations))))
