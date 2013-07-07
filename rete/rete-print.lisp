(in-package :exil-rete)

(defmethod print-object ((node alpha-subtop-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| tmpl-name: ~A" (tmpl-name node))))

(defmethod print-object ((node alpha-test-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| field: ~A, value: ~A" (tested-field node) (value node))))

(defmethod print-object ((node alpha-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| pattern: ~A" (pattern node))))

(defmethod print-object ((node beta-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| productions: ~S" (productions node))))

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

(defmethod print-object ((node beta-join-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| tests: ~A" (tests node))))

