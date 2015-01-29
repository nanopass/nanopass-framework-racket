#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(require rackunit
         "tests/compiler-test.rkt"
         "tests/helpers.rkt"
         "tests/unit-tests.rkt"
         "nanopass/helpers.rkt")

(printf "Running unit tests\n")
(run-test unit-tests)
(run-test ensure-correct-identifiers)
(run-test maybe-tests)
(run-test maybe-dots-tests)
(run-test maybe-unparse-tests)
(run-test language-dot-support)
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
