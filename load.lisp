(in-package :cl-user)

(defvar *path*
  (pathname
   (directory-namestring
    (or *load-truename* *compile-file-truename*))))

(load (merge-pathnames "3rd-side/asdf.lisp" *path*))
(push-end *path* asdf:*central-registry*)
(asdf:oos 'asdf:load-op :exil)
