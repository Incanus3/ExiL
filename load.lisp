(in-package :cl-user)

(defvar *path*
  (pathname
   (directory-namestring
    (or *load-truename* *compile-file-truename*))))

(require :asdf)

(ql:update-all-dists)
(ql:quickload "xlunit")

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note)
                (sb-ext:muffle-conditions style-warning))

#-lispworks (progn
              (push *path* asdf:*central-registry*)
              (asdf:oos 'asdf:load-op :exil))

#+lispworks (progn
              (load (merge-pathnames "defsys.lisp" *path*))
              ;; (load-system :exil)
              (compile-system :exil :load t))

(tests-base:run-suites)
