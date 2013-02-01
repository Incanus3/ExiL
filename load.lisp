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

(tests-base:run-suites)
