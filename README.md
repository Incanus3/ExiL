## What's this?
ExiL (Expert System in Lisp) is a **CLIPS-based expert system building tool** written in Common Lisp,
with forward chainging and a very basic backward chaining inference engine. It was developed along my
computer science master's thesis and is meant for **academic purposes**, not for real-case scenerios
(at least yet).

## Supported LISP implementations
I've developed ExiL using **SBCL 1.1.13** (SLIME and Emacs), so that's where it runs best. I've also tested it in
**LispWorks&trade; Personal Edition 6.1.1**, for which I've also created a very basic GUI.

## Usage
In sbcl load the file `load.lisp`, enter the `exil-user` package and you can start using the front-end macros.
In LispWorks loading using quicklisp/ASDF doesn't work - my custom destructuring macros don't get expanded,
so just use `load-manual.lisp`, which compiles and loads each file in turn, avoiding ASDF.

## Documentation
- CLIPS documentation - http://clipsrules.sourceforge.net/OnlineDocs.html
- API reference - https://github.com/Incanus3/ExiL/wiki/API-reference
- more extensive documentation is part of the text part of the thesis (text/bakalarka.pdf), unfortunately it's in Czech

## Examples
- ordered facts - https://github.com/Incanus3/ExiL/blob/master/src/examples/examples-simple.lisp
- templated facts - https://github.com/Incanus3/ExiL/blob/master/src/examples/examples-template.lisp
- templated facts using CLIPS syntax - https://github.com/Incanus3/ExiL/blob/master/src/examples/examples-clips.lisp

## Used tools
- CLIPS - http://clipsrules.sourceforge.net/<br />
  expert system building tool

- ASDF (Another System Definition Facility) - http://common-lisp.net/project/asdf/<br />
  project definition and loading facility (alternative to LispWorks defsystem)

- quicklisp - http://www.quicklisp.org/beta/<br />
  library manager for CL

- iterate - http://common-lisp.net/project/iterate/<br />
  extensible loop alternative with more lispy syntax

- xlunit - http://quickdocs.org/xlunit/<br />
  CLOS based unit testing framework for CL

- sbcl - http://www.sbcl.org/<br />
  open source high performance Common Lisp compiler

- LispWorks&trade; - http://www.lispworks.com/<br />
  integrated cross-platform development tool for ANSI Common Lisp

## What is implemented
- basic fact manipulation - `(assert)`, `(retract)`
  - retract accepts integers (as CLIPS) or fact specifiers, can be even mixed
  - TODO: document symbols and values usable in facts (CLIPS ug page 8)
  - TODO: document case sensitivity and other equivalence issues
- structured (unordered, templated) facts
  - `(modify)`
  - clips and lispy syntax for template and structured fact definition and modify
- facts listing - `(facts)`
- fact group definition - `(deffacts)` + `(reset)`, `(undeffacts)`
- environment cleanup
  - `(clear)` - clears facts, agenda, undo/redo stacks, (goals, backtracking stack - backward chaining)
  - `(reset)` - `(clear)` + activate fact groups
  - `(complete-reset)` - clears all slots - including templates, fact groups, rules
- watchers - `(watch facts)`, `(watch rules)`, `(watch activations)`, `(watch all)`
- rules
  - `(defrule)` - both syntaxes for condition specifiers
  - activations can be any lisp expressions, but beware! they're EVALuated - so they don't see lexical scope
  - `(ppdefrule)`
- agenda listing - `(agenda)`
- forward chaining inference execution - `(run)`, `(step)`, `(halt)`
- backward chaining inference execution - `(defgoal goal-spec)`, `(goals)`, `(back-run)`, `(back-step)`
- functional alternatives - macros taking unquoted names or expressions have functional counterparts
  suffixed by 'f', which can be used by other code to call ExiL functionality

## What isn't implemented
- multislot templates
- template slot types
- function calls in (assert), (retract) aren't evaluated, not even in rule's activations
  - but this can be done using assertf, retractf, ...
- advanced rule syntax:
  - singleton variable?
  - wildcards - $?
  - field constraints - ~, |, &
  - test predicate in rule conditions
  - =( directive in rules
  - (undefrule \*) - wildcard
- only basic strategies implemented
  - new strategies can be defined, but this requires accessing the internal rule representation,
    so it's not very useful at the moment
- state saving and loading - program can be easily saved and loaded by interpreter
- CLIPS string, printing, integer, ... functions - use lisp ones instead
- CLIPS deffunction - use lisp
- rule salience
- incremental reset, refresh
- batch, system
- dribbling
- brakepoints
- (matches)
- CLIPS objects, classes, messages
- didn't care about performance, LispWorks personal edition stack overflows easily
