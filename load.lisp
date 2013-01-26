(in-package :cl-user)

(defvar *path*
  (pathname
   (directory-namestring
    (or *load-truename* *compile-file-truename*))))

;; (print (merge-pathnames "3rd-side/asdf.lisp" *path*))
;; nefunguje uvnitr #-lispworks (progn ... )
(require :asdf) ; (merge-pathnames "3rd-side/asdf.lisp" *path*))

(ql:quickload "xlunit")

#-lispworks (progn
              (push *path* asdf:*central-registry*)
              (asdf:oos 'asdf:load-op :exil))

#+lispworks (progn
              (load (merge-pathnames "defsys.lisp" *path*))
              ;; (load-system :exil)
              (compile-system :exil :load t))

(tests-base:run-suites)
