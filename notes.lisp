;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defmetmplate blah
    (slot a)
  (slot b :default 10))

=>

(let (template (make-instance 'template :name blah
			      :slots ((a . (:default nil))
				      (b . (:default 10)))))
  (setf *templates*
	(cons template
	      (remove-if (lambda (template)
			   (equalp (name template) blah))
			 *templates*))))

(tmpl-fact (blah :a 10))

=>

(make-instance 'template-fact
	       :slots ((a . 10) (b . 10)))