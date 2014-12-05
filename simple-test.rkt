#lang racket

(require "nanopass.rkt")

(define-language L)
(define-language L1 (terminals (symbol (x))))
(define-language L2 (terminals (symbol (x))) (Expr (e) x (lambda (x) e) (e0 e1)))
(define-parser parse-L2 L2)

(provide L unparse-L L1 unparse-L1 L2 parse-L2 unparse-L2)
