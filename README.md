## lisp-unit

*lisp-unit* is a Common Lisp library that supports unit testing. It is
an extension of the [library written by Chris Riesbeck][orig]. There
is a long history of testing packages in Lisp, usually called
"regression" testers. More recent packages in Lisp and other languages
have been inspired by [JUnit for Java][JUnit].

Recently longtime users at Acceleration.net felt motivated to refactor
significantly, trying to make some sweeping improvments

[Documentation is located on the project wiki.][wiki]

### Features

* Written in portable Common Lisp
* Loadable with [ASDF][] or [Quicklisp][]
* Simple to define and run tests
* Redfine functions and macros without reloading tests
* Test return values, printed output, macro expansions, and conditions
* Fined grained control over the testing output - by writing different
  summary handlers
* Store all test results in a database object that can be examined
* tests grouped and accessed by name, tag, and package of test name
* Group tests by package for modularity
* Group tests using tags
* Signals for starting and completing test runs (both individually and
  as a group)

#### Features of lisp-unit version 2
* Simplified test retrieval / categorization.
 * Tests are stored by their name's symbol-package (easing the
   complexity of this package vs the package argument)
 * package arguments now just retrieve all tests in a given package
* All output goes to its own stream (and can therefore be manipulated
  at will). *test-stream* defaults to *standard-output* as it always
  has.
* Debugging is controlled by *debug-hook*, continue restart activated
  around test
* Tests are named functions that show up on the stack (so that 
  goto-definition in a stack trace brings you to the test definition
* Contexts can be applied on a test or a run-test(s) to provide common
  setup/tear down locations (eg: provide a database to tests that need
  it)
* Signals used throughout (including to drive current output summaries)
 * assertions passing and failing
 * tests starting and completeing
 * test batches starting and completeing
* Logging used throughout (to ease debugging what is happening)

### Extensions

* Floating point predicates
* [Test Anything Protocol][TAP] output

### How to use lisp-unit

The core definitions of *lisp-unit* may be used by loading the single
file 'lisp-unit.lisp'. To use the extensions, *lisp-unit* must be
loaded using either [Quicklisp][] or [ASDF][].

1. Load (or compile and load) as a single file : `(load "lisp-unit")`.
2. Load using [Quicklisp][] : `(ql:quickload :lisp-unit)`.
3. Load using [ASDF][] : `(asdf:load-system :lisp-unit)`.

## Version 0.9.5 Features

No new features, improved the usability based on user feedback. The
list of tests or tags to the following functions is now optional and
defaults to `:ALL`.

* `(remove-tests [names] [package])`
* `(tagged-tests [tags] [package])`
* `(remove-tags [tags] [package])`
* `(run-tests [names] [package])`
* `(run-tags [tags] [package])`

## Version 1 Remaining Tasks

* (1.0.0) Expanded internal testing.

### Future Features

* Fixtures
* Test Suites
* Benchmarking tools

[orig]: <http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html>
  "Original Lisp Unit"
[wiki]: <https://github.com/OdonataResearchLLC/lisp-unit/wiki>
  "Lisp Unit Wiki"
[JUnit]: <http://www.junit.org> "JUnit"
[Quicklisp]: <http://www.quicklisp.org> "Quicklisp"
[ASDF]: <http://common-lisp.net/project/asdf/> "ASDF"
[TAP]: <http://testanything.org/> "Test Anything Protocol"

## 0.9.5 Acknowledgments

* [Jesse Alama][jessealama] for usability feedback.

[jessealama]: <https://github.com/jessealama> "Jesse Alama"
