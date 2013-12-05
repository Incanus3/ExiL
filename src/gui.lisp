(in-package :exil-gui)

(define-interface facts-gui ()
  ((env :initarg :env :accessor env)
   (main-gui :initarg :main :accessor main-gui))
  (:panes
   (fact-list list-panel
              :reader fact-list)
   (retract-button push-button
                   :text "Retract fact"
                   :callback 'retract-fact
                   :callback-type :interface))
  (:default-initargs :title "ExiL Facts"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod selected-fact ((interface facts-gui))
  (choice-selected-item (fact-list interface)))

(defmethod retract-fact ((interface facts-gui))
  (rem-fact (env interface) (selected-fact interface)))

(defmethod update-list ((interface facts-gui))
  (setf (collection-items (fact-list interface))
        (facts (env interface))))

(defmethod initialize-instance :after ((interface facts-gui) &key)
  (update-list interface))


(define-interface templates-gui ()
  ((env :initarg :env :accessor env)
   (main-gui :initarg :main :accessor main-gui))
  (:panes
   (template-list list-panel
                  :reader template-list)
   (retract-button push-button
                   :text "Undefine template"
                   :callback 'undef-template
                   :callback-type :interface))
  (:default-initargs :title "ExiL Templates"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod selected-template ((interface templates-gui))
  (choice-selected-item (template-list interface)))

(defmethod undef-template ((interface templates-gui))
  (rem-template (env interface) (name (selected-template interface))))

(defmethod update-list ((interface templates-gui))
  (setf (collection-items (template-list interface))
        (exil-env:template-list (env interface))))

(defmethod initialize-instance :after ((interface templates-gui) &key)
  (update-list interface))


(define-interface rules-gui ()
  ((env :initarg :env :accessor env)
   (main-gui :initarg :main :accessor main-gui))
  (:panes
   (rule-list list-panel
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
  (rem-rule (env interface) (name (selected-rule interface))))

(defmethod update-list ((interface rules-gui))
  (setf (collection-items (rule-list interface))
        (exil-env:rule-list (env interface))))

(defmethod initialize-instance :after ((interface rules-gui) &key)
  (update-list interface))


(define-interface agenda-gui ()
  ((env :initarg :env :accessor env))
  (:panes
   (agenda-list list-panel
                :reader agenda-list))
  (:default-initargs :title "ExiL Agenda"
   :visible-min-height 300
   :visible-min-width 500))

(defmethod update-list ((interface agenda-gui))
  (setf (collection-items (agenda-list interface))
        (agenda (env interface))))

(defmethod initialize-instance :after ((interface agenda-gui) &key)
  (update-list interface))


(define-interface exil-gui ()
  ((facts :accessor facts-int)
   (templates :accessor templates-int)
   (rules :accessor rules-int)
   (agenda :accessor agenda-int))
  (:panes
   (facts-button push-button
          :text "Facts"
          :callback 'show-facts
          :callback-type :interface)
   (templates-button push-button
              :text "Templates"
              :callback 'show-templates
              :callback-type :interface)
   (rules-button push-button
          :text "Rules"
          :callback 'show-rules
          :callback-type :interface)
   (agenda-button push-button
           :text "Agenda"
           :callback 'show-agenda
           :callback-type :interface))
  (:layouts
   (buttons row-layout '(facts-button templates-button rules-button agenda-button)))
  (:default-initargs :title "ExiL Debug Tools" :auto-menus nil))

(defmethod show-facts ((gui exil-gui))
  (display (facts-int gui)))

(defmethod show-templates ((gui exil-gui))
  (display (templates-int gui)))

(defmethod show-rules ((gui exil-gui))
  (display (rules-int gui)))

(defmethod show-agenda ((gui exil-gui))
  (display (agenda-int gui)))

(defmethod initialize-instance :after ((gui exil-gui) &key environment)
  (with-slots (facts templates rules agenda) gui
    (setf facts (make-instance 'facts-gui :env environment :main gui)
          templates (make-instance 'templates-gui :env environment :main gui)
          rules (make-instance 'rules-gui :env environment :main gui)
          agenda (make-instance 'agenda-gui :env environment))))

(defmethod make-gui ((env exil-env:environment))
  (make-instance 'exil-gui :environment env))

(defmethod update-lists ((gui exil-gui))
  (with-slots (facts templates rules agenda) gui
    (mapc #'update-list (list facts templates rules agenda))))

(defun show-gui (&optional environment)
  (display (gui (exil:getenv environment))))
