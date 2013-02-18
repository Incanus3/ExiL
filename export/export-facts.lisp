(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; facts

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact groups

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
