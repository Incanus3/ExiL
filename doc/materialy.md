## References and information sources + covered topics
1. Introduction to Expert Systems [Peter Jackson]
  * What are expert systems?
    * ES applicability
  * Overview of AI
    * what is AI, where does ES fit in
    * evolution of AI, what led towards ES
      * state space search
        * depth-first, breadth-first, generate-and-test
        * theorem proving
        * forward-chaining and combinatorial explosion
        * heuristic search (search direction strategies)
        * applicability
      * knowledge representation schemes
        * production rules
        * protocol analysis
        * associative nets
        * logical formulas
        * computer understanding
      * separation of knowledge base and reasoning apparatus
        * pros and cons
        * trade-off btw. inference engine simplicity and flexibility of knowledge repr.
  * Knowledge representation
    * historic implementations
      * STRIPS
        - state, operators (preconditions, delete list, add list), goals
        - very simple fact and rule structure - low flexibility, simple inference
        - means-ends analysis - backward chaining
        - matching, substitution, goal selection, conflict resolution
        - uniform representation, problem reduction, heuristic search
      * MYCIN
        - domain specific
        - uncertainty, rule weighting
        - static and dynamic parts of knowledge base
          - knowledge base - static
          - patient database - dynamic
          - working memory - conclusions about patient - doesn't modify knowledge base
        - separation of consultation and knowledge acquisition components
        - more flexible rules - disjunctions in conditions, consequences can be instructions
        - more diferentiated knowledge representation
        -> more complex inference
    * ES evaluation
    * limitations of mentioned systems
  * Symbolic computation
    * representation of state as symbolic structures and inference rules as transformation rules
    * physical symbol systems
    * building blocks - symbols, sets, sequences, tuples
    * LISP and its advantages for sym. comp. (and disadvantages)
    * lisp overview
  * Rule-based systems
    * intelligent behavior based on domain-relevant rules
    * syntax of rules, encoding and manipulation of state, conditions, activations
    * production systems
      - production memory (rules)
      - rule interpreter (inference engine)
      - working memory
      - working memory - data in form of object-attr vecotrs, used to activate rules
        -> initialized from knowledge base
        - doesn't say, what about dynamic rules
    * object-attribute-value triplets, structured facts (object-attribute-value vectors)
    * variables in rules, bindings, consistency, singleton variable
    * CLIPS
      * program structure
      * inference execution
      * controlling inference
        - agenda, termination, conflict resolution strategies (nondeterminism)
      * search space
    * forward X backward chaining, solution generations X recognition
    * meta-rules - advanced reasoning control

2. Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp [Peter Norvig]
  * 1,3 - CL introduction and overview
  * 4 - GPS: implmentation of STRIPS-like backward chaining ES,
    examples, common problems, solutions
  * 5.2,5.3,6.2 - implementation of pattern matching
  * 11 - implementation of basic prolog-like language, backtracking
  * 13.9,13.10 - is CLOS object-oriented?

* CLIPS - expert system building tool - http://clipsrules.sourceforge.net/<br />
  http://clipsrules.sourceforge.net/OnlineDocs.html - documentation

* Practical Common Lisp [Peter Seibel] - http://www.gigamonkeys.com/book/<br />
  step-by-step CL introduction, practical examples

* Common Lisp HyperSpec - http://www.lispworks.com/documentation/HyperSpec/Front/<br />
  ANSI CL reference

* ASDF (Another System Definition Facility) - http://common-lisp.net/project/asdf/<br />
  project definition and loading facility (alternative to LW defsystem)

* quicklisp - http://www.quicklisp.org/beta/<br />
  library manager for CL

* the Iterate project - http://common-lisp.net/project/iterate/<br />
  extensible loop alternative with more lispy syntax

* xlunit - http://quickdocs.org/xlunit/<br />
  CLOS based unit testing framework for CL

* git - http://git-scm.com/<br />
  open source distributed version control system

* <http://en.wikipedia.org/wiki/Lisp_(programming_language)>
* <http://en.wikipedia.org/wiki/Expert_system>
