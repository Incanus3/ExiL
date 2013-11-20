(in-package :exil-rete)

(defgeneric format-object (node stream))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format-object node stream)))

(defmethod format-object ((node alpha-top-node) stream))

(defmethod format-object ((node alpha-subtop-node) stream)
  (format stream "| tmpl-name: ~A" (tmpl-name node)))

(defmethod format-object ((node alpha-test-node) stream)
  (format stream "| field: ~A, value: ~A" (tested-field node) (value node)))

(defmethod format-object ((node alpha-memory-node) stream)
  (format stream "| pattern: ~A, items: ~S" (pattern node) (items node)))

(defmethod format-object ((node beta-memory-node) stream)
  (format stream "| productions: ~A, items: ~S"
          (mapcar #'name (productions node)) (items node)))

(defmethod print-object ((test test) stream)
  (with-slots (current-field-to-test previous-condition-number
                                     previous-field-to-test) test
    (if *print-escape*
        (print-unreadable-object (test stream :type t :identity nil)
          (format stream
                  "current-field: ~A, ~A conditions back, previous-field: ~A"
                  current-field-to-test previous-condition-number
                  previous-field-to-test))
        (format stream
                "(curr-field: ~A, ~A conds back, prev-field: ~A)"
                current-field-to-test previous-condition-number
                previous-field-to-test))))

(defmethod format-object ((node beta-join-node) stream)
  (format stream "| tests: ~A" (tests node)))

(defmethod format-object ((node beta-negative-node) stream)
  (format stream "| tests: ~A,~%    items: ~S" (tests node) (items node)))
