#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell,
;;; Leif Andersen
;;; See the accompanying file Copyright for details

(require rackunit
         rackunit/text-ui
         "compiler-test.rkt"
         "helpers.rkt"
         "unit-tests.rkt"
         "with-output-language-tests.rkt"
         "../private/helpers.rkt")

(printf "Running unit tests\n")
(for ([suite (list unit-tests
                   ensure-correct-identifiers
                   maybe-tests
                   maybe-dots-tests
                   maybe-unparse-tests
                   language-dot-support
                   error-messages
                   empty-lists
                   language-node-counter
                   with-output-language-capture)])
  (printf "~a: " (rackunit-test-suite-name suite))
  (run-tests suite))
(printf "Compiler loaded, running all tests (quietly)\n")
(time
  (begin
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)
    (run-all-tests)))
