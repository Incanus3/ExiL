(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

; public
(defmacro defstrategy (name function)
  "Define strategy"
  `(add-strategy ',name ,function))

; public
(defmacro setstrategy (name)
  "Set strategy to use"
  `(set-strategy ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

; extracts actual conditions from rule conditions list, which can also
; include ?fact <- <condition> statements
; returns list of pairs (<condition> , <match-variable>)
; TODO: add some tests - when ?fact <- not followed by condition definition
; this will crash in most unpredictable ways
(defun extract-conditions% (cond-list)
  (iter (for i :upfrom 0)
        (for cond :first (first cond-list) :then (nth i cond-list))
        (while (< i (length cond-list)))
        (if (listp cond)
            (collect (cons cond nil))
            (progn (collect (cons (nth (+ i 2) cond-list) cond))
                   (incf i 2)))))

;; DODELAT KONTROLU, ZDA SE VSECHNY PROMENNE V RHS VYSKYTUJI V LHS
; public
(defmacro defrule (name &body rule)
  "Define rule"
  (when (stringp (first rule))
    (pop rule)) ;; ignore the clips rule header
  (let* ((=>-position (position '=> rule :test #'weak-equal-p))
         (conditions (extract-conditions% (subseq rule 0 =>-position)))
         (activations (subseq rule (1+ =>-position)))
         (rule-symbol (gensym "rule")))
    (cl:assert =>-position ()
               "rule definition must include =>")
    `(let ((,rule-symbol
            (make-rule
             ',name
             (mapcar (lambda (condition)
                       (make-pattern *current-environment* (car condition)
                                     :match-var (cdr condition)))
                     ',conditions)
             ',activations)))
       (add-rule *current-environment* ,rule-symbol))))

(defun ppdefrule% (name)
  (let ((rule (find-rule *current-environment* name)))
    (format t "(defrule ~A~{~%  ~A~}~%  =>~{~%  ~S~})"
            name (conditions rule) (activations rule))))

; public
(defmacro undefrule (name)
  "Undefine rule"
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

; public
(defmacro ppdefrule (name)
  "pretty prints rule definition"
  `(ppdefrule% ',name))
