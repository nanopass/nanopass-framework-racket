#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for detatils

(require "tests/compiler-test.rkt")
(require "tests/helpers.rkt")
(require "tests/unit-tests.rkt")
(require "nanopass/helpers.rkt")

(printf "Running unit tests\n")
(run-unit-tests)
(run-ensure-correct-identifiers)
(run-maybe-tests)
(run-maybe-dots-tests)
(run-maybe-unparse-tests)
(run-language-dot-support)
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
(exit)
