## lisp-unit2

*lisp-unit2* is a Common Lisp library that supports unit testing.  It
is a new version of a library of the [lisp-unit library written by
Chris Riesbeck][orig]. There is a long history of testing packages in
Lisp, usually called "regression" testers. More recent packages in
Lisp and other languages have been inspired by [JUnit for
Java][JUnit].

Recently longtime users at Acceleration.net felt motivated to refactor
significantly, attempting to make some broad improvements to the library
while maintaining its benefits and workflow

### Features
* Written in portable Common Lisp
* Loadable with [ASDF] or [Quicklisp]
* Simple to define and run tests
* Redfine functions and macros without reloading tests - tests are
  recompiled before each run and at definition time
* Tests have source-location for going to definition
* Supports testing: return values, printed output, macro expansions,
  and conditions.  Special support for testing floating and rational
  point numbers
* Running a single test returns a test-result object. Running many
  tests returns a test-result-db
* Tests grouped and accessed by name, tag, and package of test name
* Signals for starting and completing test runs (both individually and
  as a group)

#### Differences from lisp-unit version 1
* Simplified test retrieval / categorization.
 * Tests are stored by their name's symbol-package (easing the
   complexity of this package vs the package argument)
 * package arguments now just retrieve all tests in a given package
 * dynamic variable consolidation (eg: one test database with three hashtables
   instead of three dynamic variables containing hashtables)
* All output goes to its own stream (and can therefore be manipulated
  at will). *test-stream* defaults to *standard-output* as it always
  has.
* Debugging is controlled by *debug-hook*, continue restart activated
  around test
* Tests are named functions that show up on the stack (so that 
  goto-definition in a stack trace brings you to the test definition
* Contexts can be applied on a test or a run-test(s) to provide common
  setup/tear down locations (eg: provide a database to tests that need
  it).  Contexts are functions that accept a thunk and apply it inside
  of a dynamic context eg: 
  `(lambda (body-thunk) (let ((*var* 1))(funcall body-thunk)))`
* Signals used throughout (including to drive current output summaries)
 * assertions passing and failing (with abort restart, to cancel
   assertions)
 * tests starting and completeing (with continue restart for resuming
   test run after an errors)
 * test batches starting and completeing
 * Dynamic variables available in signal handlers and all tests
   *unit-test* and *results*
* Logging used throughout (to ease debugging of lisp-unit2) - compile
  time
* TAP output displays better messages about the error and is more
  consistent with the summary output
* Better job of reporting compiler warnings and errors: when defining
  the test (and while running it)
 * Summary output prints test starting messages. Compiler / run-time
   errors printed into the output is obviously tied to the test in
   question. (Previously the results printed after the error messages
   and so it was textually ambiguous to which test the messages
   applied)
* lisp-unit2 is no longer loadable as a single file
* :lisp-unit2 feature is available for conditional compilation
* Better optimized for running both in a build-environment (jenkins
  etc) and from the repl (previously there were many non-standard
  boolean flags for controlling output and debugging).
* test bodies compiled with (debug 3) - TODO: revisit this in light of
  slow tests if that becomes an issue
* Default ordering for tests and results is the definition order
  (instead of random or reversed)
* assert-result -> assert-passes?
* (define-test name &body) became (define-test name (&key-args) &body)

### How to use lisp-unit2

1. Load using [Quicklisp][] : `(ql:quickload :lisp-unit2)` or [ASDF][]
   : `(asdf:load-system :lisp-unit2)`.
2. Define some tests (for best luck define tests in their own package
   by making their name be in a specific package).  By having tests in
   their own package, the test and the fn being tested can share the
   same name. (Tests are compiled to a function named after the test
   that runs it and an object in the test database)

```
(lisp-unit2:define-test my-tests::test-subtraction 
    (:tags '(my-tests::bar))
  (assert-eql 1 (- 2 1)))
```

3. Run all tests in your package

```
;; with summary provides results while the tests are running
(with-summary ()
   (run-tests :package :my-tests))

;; to print the results after the run is complete
(print-summary (run-tests :tags 'my-tests::bar))

;; The difference is in whether or not the output occurs while the
;; tests are running or after all the tests have run

;; to disable the debugger:
(let (*debug-hook*)
  (run-tests :tests 'my-tests::test-subtraction))
```

See the internal test suite for more and better examples (internal-test/*)

#### Test Organization: Names, Tags, and Packages

Tests are organized by their name and by tags.  Both of these are
symbols in some package.  Tests can be retrieved by their name, the
package that their name is in, and any of the tags that reference the
test.

The most common way to retrieve and run unit tests is run-tests which
calls get-tests.

`(lisp-unit2:run-tests &key tests tags package reintern-package)`
`(lisp-unit2:get-tests &key tests tags package reintern-package)`

Both of these functions accept:
 * tests: a single or list of symbols or unit-test objects
 * tags: a single or list of symbols.  All tests in these tags will be
   returned
 * package:  a single or list of packages (names or objects)

If no arguments are provided lisp-unit2 will run all tests in *package*

In some cases, particularly when converting from lisp-unit(1) we need
our tests to be in a different package (because tests are functions in
their name's package). In lisp-unit these tests would not conflict
with a function named the same (because tests were not functions). To
ease conversion, the reintern-package argument will reintern all test
names and tags provided into a different package.  Define test accepts 
a `package` argument that mirrors this functionality.  Suggested usage
is to either have tests be named differently from the functions they test
or to have tests and tags be in an explicitly referenced package, eg:
`(define-test my-tests::test1 (:tags '(my-tests::tag1)) ...)`

Tests are organized into the `*test-db*` which is an instance
`test-database`.  These can be rebound if you needed to write tests
about your test framework (see the internal example-tests).

#### Debugging

Debugging is controlled by `*debugger-hook*` (as is usual in common-lisp).
You can make lisp-unit simply record the error and move on by binding
`*debugger-hook*` to nil around your `run-tests` call.

If you would like to debug failed assertions you can wrap your call in
`with-failure-debugging` or apply the `with-failure-debugging-context`
to the unit-test run.

#### Output and Results

All output is printed to `*test-stream*` (which by default is
`*standard-output*`).  Most forms do not output results by default,
instead returning a result object.  All results objects can be printed
(to `*test-stream*`) by calling `print-summary` on the object in
question.  

`print-summary` prints infomation about passing as well as
failing tests. `print-failure-summary` can be called to print only
messages about failures, warnings, errors and empty tests (empty tests
had no assertions).

When running interactively `with-summary` can provide real-time output
printing start messages and result messages for each test.
`with-assertion-summary` provides even more detailed output printing a
message for each assertion passed or failed.

Test results (from many runs) can be captured using
`with-test-results`.  The arg: `collection-place` will copy all the
results as they arrive into a location of your choosing. The arg:
`summarize?` will print a failure summary of each test-run after all
of the tests are finished running.  This is useful for collecting
separate results for many packages or systems (see
test-asdf-system-recursive).  If no args are provided summarize? is
defaulted to true.

##  Remaining Tasks
*  Expanded internal testing.

### Future Features
* Benchmarking tools

[orig]: <http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html>
  "Original Lisp Unit"
[wiki]: <https://github.com/OdonataResearchLLC/lisp-unit/wiki>
  "Lisp Unit Wiki"
[JUnit]: <http://www.junit.org> "JUnit"
[Quicklisp]: <http://www.quicklisp.org> "Quicklisp"
[ASDF]: <http://common-lisp.net/project/asdf/> "ASDF"
[TAP]: <http://testanything.org/> "Test Anything Protocol"

## 0.2.0 Acknowledgments
* Russ Tyndall - Acceleration.net 

## 0.9.5 Acknowledgments

* [Jesse Alama][jessealama] for usability feedback. 
[jessealama]: <https://github.com/jessealama> "Jesse Alama"
