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
   "core/templates" "core/base-objects" "core/facts" "core/patterns"
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

(defparameter *files* (append *base* *core* *rete* *env* *parser* *front-end*))

(dolist (file *files*)
  (load (merge-pathnames file *path*) :verbose t))
