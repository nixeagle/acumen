(defpackage #:acumen.utils
  (:use :cl :alexandria)
  (:nicknames :acuu))

(in-package :acumen.utils)

(defparameter +test-suite-roots+
  '(ep.tokenize::root wiktionary::root)
  "List of all root suites in acumen for the various packages.")

(defun run-all-tests (&optional (test-runner 'eos::run))
  "Run all tests using TEST-RUNNER.

The default is to run them all without explaining them and return in a
list the results of all tests. Passing in #'eos::run! will cause all tests
to be run and their results explained if they fail."
  (values-list
   (remove-if #'eos::test-passed-p
              (mapcan test-runner +test-suite-roots+))))

(defun run-all-tests! ()
  (run-all-tests #'eos::run!))