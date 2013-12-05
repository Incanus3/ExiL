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

#-lispworks(ql:quickload :exil)

#+lispworks(ql:quickload :iterate)
#+lispworks(ql:quickload :xlunit)
#+lispworks(asdf:operate 'asdf:load-op 'exil)

(eval-when (:execute)
  (exil:defenv default)
  (exil:setenv default))
