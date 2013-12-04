(in-package :cl-user)

#+sbcl(declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note))

(defvar *path*
  (pathname
   (directory-namestring
    (or *load-truename* *compile-file-truename*))))

#-quicklisp(load (merge-pathnames "dependencies/quicklisp.lisp" *path*))

(push *path* asdf:*central-registry*)

;(ql:update-client)
;(ql:update-all-dists)

(ql:quickload :exil)

;(ql:quickload :iterate)
;(ql:quickload :xlunit)
;(asdf:operate 'asdf:load-op 'exil)

(exil:defenv default)
(exil:setenv default)
