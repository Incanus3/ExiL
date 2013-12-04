(in-package :cl-user)

(defvar *path*
  (pathname
   (directory-namestring
    (or *load-truename* *compile-file-truename*))))

(load (merge-pathnames "dependencies/quicklisp.lisp" *path*))

(ql:quickload :iterate)
(ql:quickload :xlunit)

(defvar *base*
  (list
   "packages"
   "utils/symbols" "utils/alists" "utils/plists" "utils/lists"
   "utils/hash-tables" "utils/misc"))

(defvar *core*
  (list
   "core/templates" "core/base-objects" "core/patterns" "core/facts"
   "core/pattern-matching" "core/rules"))

(defvar *rete*
  (list
   "rete/tokens" "rete/rete-nodes/generic-node" "rete/rete-nodes/alpha-top"
   "rete/rete-nodes/alpha-tests-mems" "rete/rete-nodes/beta-tests"
   "rete/rete-nodes/beta-memories" "rete/rete-nodes/beta-joins"
   "rete/rete-class" "rete/create-alpha-net" "rete/create-beta-net"
   "rete/rete-print" "rete/graph-traversal" "rete/rete-equal"
   "rete/rete-copy"))

(defvar *env*
  (list
   "environment/matches" "environment/activate-rule" "environment/strategies"
   "environment/env-class" "environment/env-slots"
   "environment/env-copy" "environment/env-undo"
   "environment/env-watchers" "environment/env-templates" "environment/env-facts"
   "environment/env-strategies" "environment/env-agenda" "environment/env-rules"
   "environment/env-cleanup" "environment/env-inference"
   "environment/env-backward-base" "environment/env-backward-matching"
   "environment/env-backward-inference"))

(defvar *parser*
  (list
   "parser/prs-base" "parser/prs-templates" "parser/prs-facts" "parser/prs-rules"))

(defvar *front-end*
  (list
   "front-end/fe-base" "front-end/fe-facts"
   "front-end/fe-rules" "front-end/fe-execution"))

(defvar *examples*
  (list
   "examples/examples-template" "examples/examples-simple" "examples/examples-clips"))

(defvar *tests*
  (list
   "tests/tst-base" "tests/tst-utils"
   "tests/core/tst-templates" "tests/core/tst-base-objects" "tests/core/tst-patterns"
   "tests/rete/tst-tokens" "tests/rete/tst-rete" "tests/rete/tst-rete-walk"
   "tests/rete/tst-rete-copy-simple" "tests/rete/tst-rete-copy-template"
   "tests/environment/tst-env-slots" "tests/environment/tst-env-simple"
   "tests/environment/tst-env-template" "tests/environment/tst-env-backward"
   "tests/environment/tst-env-copy-simple" "tests/environment/tst-env-copy-template"
   "tests/environment/tst-env-undo" "tests/environment/tst-env-undo2"
   "tests/integration/simple" "tests/integration/template" "tests/integration/clips"
   "tests/integration/undo-redo" "tests/integration/backward"
   "tests/integration/undo-redo-backward""tests/integration/functional"
   "tests/run-tests"))

(defparameter *files*
  (append *base*
          *core*
          *rete*
          *env*
          *parser*
          *front-end*
          ;; *examples*
          ;; *tests*
          #+lispworks(list "gui.lisp")
          ))

(dolist (file *files*)
  (load (merge-pathnames file *path*) :verbose t))

(exil:defenv default)
(exil:setenv default)

#+lispworks(exil-gui:show-gui)
