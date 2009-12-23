;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LISP2CSF -*-

#|

DESC: lisp2csf/borrowed.lisp - borrowed code to make lisp2csf work

|#


(in-package :lisp2csf)

;;;; The following is borrowed from Closette (See AMOP)


;;; push-on-end is like push except it uses the other end:

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))


(defun parse-defmethod (args)
  (let ((fn-spec (car args))
        (qualifiers ())
        (specialized-lambda-list nil)
        (body ())
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
	(:qualifiers
	 (if (and (atom arg) (not (null arg)))
	     (push-on-end arg qualifiers)
	     (progn (setq specialized-lambda-list arg)
		    (setq parse-state :body))))
	(:body (push-on-end arg body))))
    (values fn-spec 
            qualifiers
            (extract-lambda-list specialized-lambda-list)
            (extract-specializers specialized-lambda-list)
            (list* 'block
                   (if (consp fn-spec)
                       (cadr fn-spec)
                       fn-spec)
                   body))))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
	 (bv (getf plist ':body-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds 
      ,@(if rv `(&rest ,rv) ())
      ,@(if bv `(&rest ,bv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
	     (intern (symbol-name symbol)
		     (find-package 'keyword)))
           (get-keyword-from-arg (arg)
	     (if (listp arg)
		 (if (listp (car arg))
		     (caar arg)
		     (make-keyword (car arg)))
		 (make-keyword arg))))
    (let ((keys ())			; Just the keywords
          (key-args ())			; Keywords argument specs
          (required-names ())		; Just the variable names
          (required-args ())		; Variable names & specializers
          (specializers ())		; Just the specializers
          (rest-var nil)
	  (body-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
	    (ecase arg
	      (&optional
	       (setq state :parsing-optional))
	      (&rest
	       (setq state :parsing-rest))
	      (&body  ;; hack
	       (setq state :parsing-body))
	      (&key
	       (setq state :parsing-key))
	      (&allow-other-keys
	       (setq allow-other-keys 't))
	      (&environment
	       (setq state :parsing-environment))
	      (&whole
	       (setq state :parsing-whole))
	      (&aux
	       (setq state :parsing-aux)))
	    (case state
	      (:parsing-required 
	       (push-on-end arg required-args)
	       (if (listp arg)
		   (progn (push-on-end (car arg) required-names)
			  (push-on-end (cadr arg) specializers))
		   (progn (push-on-end arg required-names)
			  (push-on-end 't specializers))))
	      (:parsing-optional (push-on-end arg optionals))
	      (:parsing-rest (setq rest-var arg))
	      (:parsing-body (setq body-var arg))
	      (:parsing-environment nil) ;; skip
	      (:parsing-whole nil) ;; skip
	      (:parsing-key
	       (push-on-end (get-keyword-from-arg arg) keys)
	       (push-on-end arg key-args))
	      (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :specializers specializers
             :rest-var rest-var
	     :body-var body-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))

