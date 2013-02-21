(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

;; TODO: this shouldn't take function, instead it should seem more like
;; a function definition - it should take name and body, pass the body to parser
;; to create the strategy (which will probably still be just a function)
;; and then store the strategy in the environment under given name
; public
(defmacro defstrategy (name function)
  "define new strategy"
  `(add-strategy ',name ,function))

; public
(defmacro setstrategy (name)
  "set strategy to use"
  `(set-strategy ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

;; TODO: parsing the rule is the responsibility of parser
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
  "define rule"
  (when (stringp (first rule))
    (pop rule)) ;; ignore clips rule header
  (let* ((=>-position (position '=> rule :test #'weak-equal-p))
         (conditions (extract-conditions% (subseq rule 0 =>-position)))
         (activations (subseq rule (1+ =>-position)))
         (rule-symbol (gensym "rule")))
    (cl:assert =>-position ()
               "rule definition must include =>")
    `(let ((,rule-symbol
            (make-rule ',name
                       (mapcar (lambda (condition)
                                 (make-pattern *current-environment*
                                               (car condition)
                                               :match-var (cdr condition)))
                               ',conditions)
             ',activations)))
       (add-rule *current-environment* ,rule-symbol))))

; public
(defmacro undefrule (name)
  "undefine rule"
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

(defun ppdefrule% (name)
  (let ((rule (find-rule *current-environment* name)))
    (format t "(defrule ~A~{~%  ~A~}~%  =>~{~%  ~S~})"
            name (conditions rule) (activations rule))))

; public
(defmacro ppdefrule (name)
  "pretty-print rule definition"
  `(ppdefrule% ',name))
