(in-package :exil-gui)

(define-interface facts-gui () ()
  (:panes
   (fact-list list-panel
              :items (facts exil::*current-environment*)
              :reader fact-list)
   (retract-button push-button
                   :text "Retract fact"
                   :callback 'retract-fact
                   :callback-type :interface)
   )
  (:default-initargs :title "ExiL Facts"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod selected-fact ((interface facts-gui))
  (choice-selected-item (fact-list interface)))

(defmethod retract-fact ((interface facts-gui))
  (rem-fact exil::*current-environment* (selected-fact interface))
  (update-lists))

(define-interface templates-gui () ()
  (:panes
   (template-list list-panel
                  :items (hash-values (templates exil::*current-environment*))
                  :reader template-list)
   (retract-button push-button
                   :text "Undefine template"
                   :callback 'undef-template
                   :callback-type :interface)
)
  (:default-initargs :title "ExiL Templates"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod selected-template ((interface templates-gui))
  (choice-selected-item (template-list interface)))

(defmethod undef-template ((interface templates-gui))
  (rem-template exil::*current-environment* (name (selected-template interface)))
  (update-lists))

(define-interface rules-gui () ()
  (:panes
   (rule-list list-panel
              :items (hash-values (rules exil::*current-environment*))
              :reader rule-list)
   (undefrule-button push-button
                     :text "Undefine rule"
                     :callback 'undef-rule
                     :callback-type :interface))
  (:default-initargs :title "ExiL Rules"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod selected-rule ((interface rules-gui))
  (choice-selected-item (rule-list interface)))

(defmethod undef-rule ((interface rules-gui))
  (rem-rule exil::*current-environment* (name (selected-rule interface)))
  (update-lists))

(define-interface agenda-gui () ()
  (:panes
   (agenda-list list-panel
                :items (agenda exil::*current-environment*)
                :reader agenda-list))
  (:default-initargs :title "ExiL Agenda"
   :visible-min-height 300
   :visible-min-width 500))

(define-interface exil-gui ()
  ()
  (:panes
   (facts push-button
          :text "Facts"
          :callback 'show-facts
          :callback-type :none)
   (templates push-button
              :text "Templates"
              :callback 'show-templates
              :callback-type :none)
   (rules push-button
          :text "Rules"
          :callback 'show-rules
          :callback-type :none)
   (agenda push-button
           :text "Agenda"
           :callback 'show-agenda
           :callback-type :none))
  (:layouts 
   (buttons row-layout '(facts templates rules agenda)))
  (:default-initargs :title "ExiL Debug Tools" :auto-menus nil))

(defclass gui-tools ()
  ((main-gui :initform (make-instance 'exil-gui) :accessor main-int)
   (facts :initform (make-instance 'facts-gui) :accessor facts-int)
   (templates :initform (make-instance 'templates-gui) :accessor templates-int)
   (rules :initform (make-instance 'rules-gui) :accessor rules-int)
   (agenda :initform (make-instance 'agenda-gui) :accessor agenda-int)))

(defparameter *exil-gui* (make-instance 'gui-tools))

(defun show-facts ()
  (display (facts-int *exil-gui*)))

(defun show-templates ()
  (display (templates-int *exil-gui*)))

(defun show-rules ()
  (display (rules-int *exil-gui*)))

(defun show-agenda ()
  (display (agenda-int *exil-gui*)))

(defun show-gui ()
  (display (main-int *exil-gui*)))

(defun update-lists ()
  (setf (collection-items (fact-list (facts-int *exil-gui*)))
        (facts exil::*current-environment*))
  (setf (collection-items (template-list (templates-int *exil-gui*)))
        (hash-values (templates exil::*current-environment*)))
  (setf (collection-items (rule-list (rules-int *exil-gui*))) 
        (hash-values (rules exil::*current-environment*)))
  (setf (collection-items (agenda-list (agenda-int *exil-gui*)))
        (agenda exil::*current-environment*))
  nil)

(show-gui)
