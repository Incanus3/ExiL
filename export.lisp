(in-package :exil)

#|
  ;; not in use
(defvar *environments*
  (let ((table (make-hash-table :test #'equalp)))
    (setf (gethash "default" table)
          (make-instance 'environment))
    table))

  ;; not in use
(defmacro defenv (name &key (redefine nil))
  (let ((sym-name (gensym "sym-name")))
    `(let ((,sym-name (symbol-name ,name)))
       (when (or (not (gethash ,name *environments*))
                 ,redefine)
         (setf (gethash ,name *environments*)
               (make-instance 'environment))))))

  ;; not in use
(defmacro setenv (name)
  (let ((env (gensym "env")))
    `(let ((,env (gethash (symbol-name ,name) *environments*)))
       (when ,env (setf *current-environment* ,env)))))

  ;; public
(defvar *current-environment*
  (gethash "default" *environments*))
|#

(defvar *current-environment*
  (make-instance 'environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defun nonclips-slot-spec-p (slot-spec)
  (and (symbolp (first slot-spec))
       (or (null (rest slot-spec))
           (keywordp (second slot-spec)))))

(defun clips-slot-spec-p (slot-spec)
  (and (weak-equal-p (first slot-spec) 'slot))
  (symbolp (second slot-spec))
  (listp (nthcdr 2 slot-spec)))

(defun slot-spec-p (slot-spec)
  (or (nonclips-slot-spec-p slot-spec)
      (clips-slot-spec-p slot-spec)))

(defun clips-slot->slot-des% (slot-spec)
  (destructuring-bind (slot slot-name &optional (modifiers nil)) slot-spec
    (declare (ignore slot))
    `(,slot-name . (:default ,(second modifiers)))))

(defun nonclips-slot->slot-des% (slot-spec)
  (destructuring-bind (slot-name &key (default nil)) slot-spec
    `(,slot-name . (:default ,default))))

(defun slot->slot-designator% (slot-spec)
  (cond 
    ((nonclips-slot-spec-p slot-spec) (nonclips-slot->slot-des% slot-spec))
    ((clips-slot-spec-p slot-spec) (clips-slot->slot-des% slot-spec))
    (t (error "~A not a valid template slot specifier~%" slot-spec))))

(defun slots->slot-designators% (slots)
  (loop for slot in (to-list-of-lists slots)
     collect (slot->slot-designator% slot)))

;; creates instance of template class with given name and slot specification
;; and pushes it into *templates*.
;; it is to consider whether lambda list (name slots)
;; or (name &body slots) is better
;; for the former possibility, the call is more similar to defclass
;; for the latter, the call is more like defstruct call
; public
(defmacro deftemplate (name &body slots)
  "define a fact template"
  (let ((template (gensym "template")))
    `(let ((,template
            (make-template ',name
                           ',(slots->slot-designators% slots))))
       (add-template *current-environment* ,template))))

; public
(defun facts (&optional (start-index 1)
                (end-index (length (exil-env:facts *current-environment*)))
                (at-most end-index))
  (let ((facts (exil-env:facts *current-environment*)))
    (loop for i from (1- start-index) to (min (1- end-index)
                                              (+ start-index at-most -1))
       collect (nth i facts))))

(defun assert% (fact-spec)
  (add-fact *current-environment* (make-fact *current-environment* fact-spec)))

; public
(defmacro assert (&rest fact-specs)
  "Add fact into working memory"
  (let ((fact-spec (gensym "fact-spec")))
    `(dolist (,fact-spec ',fact-specs)
       (assert% ,fact-spec))))

(defun retract% (fact-specs)
  (let (facts-to-remove)
    (dolist (fact-spec fact-specs)
      (typecase fact-spec
        (list (pushnew (make-fact *current-environment* fact-spec)
                       facts-to-remove))
        (integer (pushnew (nth (1- fact-spec) (facts)) facts-to-remove))
        (t (error "Type ~A not supported by retract" (type-of fact-spec)))))
    (dolist (fact facts-to-remove)
      (rem-fact *current-environment* fact))))

; retract supports either full fact specification e.g. (retract (is-animal duck))
; or number indices (starting with 1) for clips compatitibity.
; It can't support * to retract all facts as clips does, cause this symbol has
; a special meaning in lisp. retract-all does this instead.
; public
(defmacro retract (&rest fact-specs)
  "Remove fact from working memory"
  `(retract% ',fact-specs))

; public
(defun retract-all ()
  (reset-facts *current-environment*))

(defun nonclips-mod-list-p (mod-list)
  (plistp mod-list))

(defun clips-mod-list-p (mod-list)
  (alistp mod-list))

(defun clips->nonclips-mod-list (mod-list)
  (loop for (slot-name new-val) in mod-list
     append (list (to-keyword slot-name) new-val)))

(defun to-mod-spec-list (mod-list)
  (cond
    ((nonclips-mod-list-p mod-list) mod-list)
    ((clips-mod-list-p mod-list) (clips->nonclips-mod-list mod-list))
    (t (error "~A not a valid modify specifier" mod-list))))

;; mod-list is a mapping from slot-name to new value
;; it can be either plist for non-clips syntax of alist for clips syntax
(defun modify% (fact-spec mod-list)
  (let ((mod-fact (make-fact *current-environment* fact-spec)))
    (unless (typep mod-fact 'template-fact)
      (error "modify: ~S is not a template fact specification" fact-spec))
    (modify-fact *current-environment* mod-fact (to-mod-spec-list mod-list))))

;; used as follows:
;; (defrule push
;;   (goal :object ?x :from ?y :to ?z)
;;   ?object <- (in :object ?x :location ?y)
;;   ?robot <- (in :object robot :location ?y)
;;   =>
;;   (modify ?robot :location ?z)
;;   (modify ?object :location ?z))
;; modify works for template-facts ONLY!
;; it doesn't make much sense to use it for simple facts as there are no
;; slot names that could be used in the mod-list
;; CLIPS doesn't support modify for simple-facts either
; public
(defmacro modify (fact-spec &rest mod-list)
  "Replace old-fact by new-fact"
  `(modify% ',fact-spec ',mod-list))

; public
(defun clear ()
  "Delete all facts"
  (reset-environment *current-environment*))

;; DEBUG:
(defun complete-reset ()
  (exil-env::completely-reset-environment *current-environment*))

; public
(defmacro deffacts (name &body descriptions)
  "Create group of facts to be asserted after (reset)"
  (if (stringp (first descriptions)) (pop descriptions))
  `(add-fact-group *current-environment* ',name ',descriptions))

; public
(defmacro undeffacts (name)
  "Delete fact group"
  `(rem-fact-group ',name))

(defun assert-group% (group)
  (format t "~%Asserting fact group ~A" (car group))
  (dolist (desc (cdr group))
    (assert% desc)))

; public
(defun reset ()
  "Clear all facts and add all fact groups"
  (clear)
  (dolist (group (fact-groups *current-environment*))
    (assert-group% group)))

(defun my-position (atom list)
  (position atom list))

; extracts actual conditions from rule conditions list, which can also
; include ?fact <- <condition> statements
; returns list of pairs (<condition> , <match-variable>)
; TODO: add some tests - when ?fact <- not followed by condition definition
; this will just piss itself
(defun extract-conditions% (cond-list)
  (loop for i = 0 then (1+ i)
     for cond = (first cond-list) then (nth i cond-list)
     while (< i (length cond-list))
     if (listp cond)
     collect (cons cond nil)
     else
     collect (cons (nth (+ i 2) cond-list) cond) and
     do (incf i 2)))

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
(defmacro ppdefrule (name)
  "pretty prints rule definition"
  `(ppdefrule% ',name))

; public
(defmacro undefrule (name)
  "Undefine rule"
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

; public
(defmacro defstrategy (name function)
  "Define strategy"
  `(add-strategy ',name ,function))

; public
(defmacro setstrategy (name)
  "Set strategy to use"
  `(set-strategy ',name))

; public
(defun step ()
  "Run inference engine for one turn"
  (when (agenda *current-environment*)
    ;; (format t "~%------------------------------------------------------")
    (activate-rule (select-activation *current-environment*))
    t))

(defvar *exil-running* nil)

; public
(defun halt ()
  "Stop the inference engine"
  (format t "~%Halting")
  (setf *exil-running* nil))

; public
(defun run ()
  "Run the infenece engine"
  (setf *exil-running* t)
  (loop while (and *exil-running* (step))))

; public
(defmacro watch (watcher)
  "Watch selected item (facts, rules, activations)"
  `(progn (if (weak-equal-p ',watcher 'all)
              (watch-all)
              (set-watcher *current-environment* (to-keyword ',watcher)))
          nil))

; public
(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(progn (if (weak-equal-p ',watcher 'all)
              (unwatch-all *current-environment*)
              (unset-watcher *current-environment* (to-keyword ',watcher)))
          nil))
