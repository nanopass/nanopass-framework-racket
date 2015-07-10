#lang racket/base
(require (for-syntax racket/base))
(provide syntax->datum-helper)

(define-syntax (syntax->datum-helper x)
  (syntax-case x ()
    [(_ ?stx)
     #`(let ([stx ?stx])
         (unless (syntax? stx)
           (printf "expected syntax, got ~s at ~s\n" stx #'#,x))
         (let f ([stx stx])
           (cond
             [(syntax? stx) (syntax->datum stx)]
             [(list? stx) (map f stx)]
             [(pair? stx) (cons (f (car stx)) (f (cdr stx)))]
             [(null? stx) '()]
             [else (error 'syntax->datum-helper "unexpected type ~a" stx)])))]))
