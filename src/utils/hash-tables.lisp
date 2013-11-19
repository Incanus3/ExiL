(in-package :exil-utils)

(defun hash-values (hash)
  "returns list of all values in the hash"
  (iter (for (key val) :in-hashtable hash)
        (collect val)))

(defun hash-keys (hash)
  (iter (for (key val) :in-hashtable hash)
	(collect key)))

(defun hash->alist (hash)
  (let (alist)
    (maphash (lambda (key val)
	       (add-assoc-value key alist val :test (hash-table-test hash)))
	     hash)
    alist))

(defun map-hash-table (fun hash)
  "returns new hash with fun applied to each value"
  (let ((new-hash (make-hash-table :test (hash-table-test hash))))
    (maphash (lambda (key val) (setf (gethash key new-hash) (funcall fun val)))
	     hash)
    new-hash))

(defun copy-hash-table (hash)
  (map-hash-table #'identity hash))

(defun hash-equal-p (hash1 hash2 &key (test #'equal))
  (and (= (hash-table-count hash1) (hash-table-count hash2))
       (equalp (hash-table-test hash1) (hash-table-test hash2))
       (iter (for (key val) :in-hashtable hash1)
	     (always (funcall test val (gethash key hash2))))))

(defun partition-hash (list fun &key (test 'equal))
  (let ((partition (make-hash-table :test test)))
    (dolist (item (reverse list))
      (let ((result (funcall fun item)))
	(if (gethash result partition)
	    (push item (gethash result partition))
	    (setf (gethash result partition) (list item)))))
    partition))

(defun partition (list fun &key (test 'equal))
  (hash->alist (partition-hash list fun :test test)))
