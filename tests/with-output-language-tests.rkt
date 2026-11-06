#lang racket
;;; Copyright (c) 2000-2013 Andrew W. Keep, R. Kent Dybvig, Leif Andersen
;;; See the accompanying file Copyright for details

;;; Regression tests for issue #43: with-output-language must capture
;;; quasiquote and in-context in a body passed through a macro defined in
;;; another module, without disturbing a quasiquote already captured by an
;;; enclosing nanopass form.
;;;
;;; The macros and their uses live in separate (non-module+) submodules, so
;;; they have distinct module scopes, just like separate files.  Each case is
;;; wrapped in convert-compile-time-error so a capture failure that shows up
;;; as a syntax error is reported as a failing test instead of breaking the
;;; whole file.

(provide with-output-language-capture)

(require rackunit)

(module dsl racket
  (require "../main.rkt")
  (provide (all-defined-out))

  (define-language L
    (terminals (integer (n)))
    (Expr (e) (int n) (add e0 e1))
    (Stmt (s) (expr-stmt e))))

(module dsl-macros racket
  (require "../main.rkt" (submod ".." dsl) syntax/macro-testing)
  (provide (all-defined-out))

  ;; the issue's macro: flips the scopes of the body with syntax-local-introduce
  (define-syntax (make-expr/introduce stx)
    (syntax-case stx ()
      [(_ body) #`(with-output-language (L Expr) #,(syntax-local-introduce #'body))]))

  ;; same, without syntax-local-introduce
  (define-syntax (make-expr stx)
    (syntax-case stx ()
      [(_ body) #'(with-output-language (L Expr) body)]))

  ;; the macro's own quasiquote follows a use-site body form
  (define-syntax (make-expr/own stx)
    (syntax-case stx ()
      [(_ body) #'(with-output-language (L Expr) body `(int 7))]))

  ;; the form of with-output-language with no nonterminal
  (define-syntax (make-untyped stx)
    (syntax-case stx ()
      [(_ body) #'(with-output-language L body)]))

  ;; passes a use-site body through a with-output-language for a different
  ;; nonterminal; a quasiquote already captured at the use site must keep
  ;; the capture it had
  (define-syntax (pass-through-stmt stx)
    (syntax-case stx ()
      [(_ body) #'(with-output-language (L Stmt) (void) body)]))

  ;; uses within the module defining the macros
  (define (same-module/introduce)
    (convert-compile-time-error (make-expr/introduce `(int 1))))
  (define (same-module/plain)
    (convert-compile-time-error (make-expr `(int 1))))
  (define (same-module/own)
    (convert-compile-time-error (make-expr/own 1)))
  (define (same-module/nested)
    (convert-compile-time-error
     (with-output-language (L Expr) (let () (pass-through-stmt `(int 1)))))))

;; a third module whose macros forward to the dsl-macros macros
(module forwarding-macros racket
  (require (submod ".." dsl-macros))
  (provide (all-defined-out))

  (define-syntax-rule (forward-expr body) (make-expr body))
  (define-syntax-rule (forward-own) (make-expr `(int 9))))

(module use racket
  (require "../main.rkt" syntax/macro-testing
           (submod ".." dsl) (submod ".." dsl-macros) (submod ".." forwarding-macros))
  (provide (all-defined-out))

  (define (other-module/introduce)
    (convert-compile-time-error (make-expr/introduce `(int 1))))
  (define (other-module/plain)
    (convert-compile-time-error (make-expr `(int 1))))
  (define (other-module/own)
    (convert-compile-time-error (make-expr/own 1)))
  (define (other-module/nested-unquote)
    (convert-compile-time-error
     (make-expr `(add (int 1) ,(let ([x `(int 2)]) x)))))
  (define (other-module/in-context)
    (convert-compile-time-error (make-expr (in-context Expr `(int 3)))))
  (define (other-module/untyped)
    (convert-compile-time-error (make-untyped (in-context Expr `(int 5)))))
  (define (other-module/forwarded-use-site)
    (convert-compile-time-error (forward-expr `(int 4))))
  (define (other-module/forwarded-own)
    (convert-compile-time-error (forward-own)))
  (define (other-module/nested-expr)
    (convert-compile-time-error
     (with-output-language (L Expr) (pass-through-stmt `(int 1)))))
  (define (other-module/nested-in-context)
    (convert-compile-time-error
     (with-output-language (L Stmt) (pass-through-stmt (in-context Expr `(int 2))))))
  (define (other-module/nested-same-nonterminal)
    (convert-compile-time-error
     (with-output-language (L Stmt) (pass-through-stmt `(expr-stmt (int 3))))))
  (define (other-module/nested-pass)
    (convert-compile-time-error
     (let ()
       (define-pass build : * (x) -> L ()
         (Expr : * (x) -> Expr () (pass-through-stmt `(int 1)))
         (Expr x))
       (build 0)))))

(require (submod "." dsl) (submod "." dsl-macros) (submod "." use))

(define with-output-language-capture
  (test-suite "with-output-language-capture"
    (test-case "same module, syntax-local-introduce"
      (check-pred L-Expr? (same-module/introduce)))
    (test-case "same module, plain body"
      (check-pred L-Expr? (same-module/plain)))
    (test-case "same module, macro's own quasiquote after use-site body"
      (check-pred L-Expr? (same-module/own)))
    (test-case "same module, captured quasiquote through inner Stmt form"
      (check-pred L-Expr? (same-module/nested)))
    (test-case "other module, syntax-local-introduce"
      (check-pred L-Expr? (other-module/introduce)))
    (test-case "other module, plain body"
      (check-pred L-Expr? (other-module/plain)))
    (test-case "other module, macro's own quasiquote after use-site body"
      (check-pred L-Expr? (other-module/own)))
    (test-case "other module, quasiquote nested in unquote"
      (check-pred L-Expr? (other-module/nested-unquote)))
    (test-case "other module, in-context"
      (check-pred L-Expr? (other-module/in-context)))
    (test-case "other module, with-output-language without nonterminal"
      (check-pred L-Expr? (other-module/untyped)))
    (test-case "forwarded through two macros, use-site body"
      (check-pred L-Expr? (other-module/forwarded-use-site)))
    (test-case "forwarded through two macros, middle macro's body"
      (check-pred L-Expr? (other-module/forwarded-own)))
    (test-case "other module, captured quasiquote through inner Stmt form"
      (check-pred L-Expr? (other-module/nested-expr)))
    (test-case "other module, captured in-context through inner Stmt form"
      (check-pred L-Expr? (other-module/nested-in-context)))
    (test-case "other module, captured Stmt quasiquote through inner Stmt form"
      (check-pred L-Stmt? (other-module/nested-same-nonterminal)))
    (test-case "pass clause quasiquote through inner Stmt form"
      (check-pred L-Expr? (other-module/nested-pass)))))
