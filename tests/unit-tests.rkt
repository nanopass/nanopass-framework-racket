#lang racket
;;; Copyright (c) 2000-2013 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(provide unit-tests
         ensure-correct-identifiers
         maybe-tests
         maybe-dots-tests
         language-dot-support
         maybe-unparse-tests
         error-messages)

(require rackunit
         "../private/helpers.rkt"
         "../private/language.rkt"
         "../private/pass.rkt"
         "../private/parser.rkt")

(define primitives '(car cdr cons + - =))
(define primitive? (lambda (x) (memq x primitives)))
(define variable? (lambda (x) (and (symbol? x) (not (primitive? x)))))
(define constant?
  (lambda (x)
    (or (number? x) (boolean? x) (string? x)
        (and (pair? x) (constant? (car x)) (constant? (cdr x))))))

(define-language L0
  (terminals
    (variable (x))
    (constant (c))
    (primitive (pr)))
  (Expr (e)
    (var x)
    (quote c)
    (begin e0 ... e1)
    (if e0 e1 e2)
    (lambda (x ...) e0 ... e1)
    (let ([x e] ...) e0 ... e1)
    (letrec ([x e] ...) e0 ... e1)
    (primapp pr e1 ...)
    (app e0 e1 ...)))

(define-struct var (sym ref set mset) #:prefab #:constructor-name $make-var)

(define make-var (lambda (sym) ($make-var sym #f #f #f)))

(define-language LUNPARSE
  (terminals
    (var (x))         => var-sym
    (constant (c))
    (primitive (pr)))
  (Expr (e body)
    (var x)                                      => x
    (quoted c)                                   => (quote c)
    (seq e0 e1)                                  => (begin e0 e1)
    (if e0 e1 e2)
    (lambda (x ...) e0 ... e1)
    (binding (x ...) (e ...) body0 ... body1)    => (let ([x e] ...) body0 ... body1)
    (recbinding (x ...) (e ...) body0 ... body1) => (letrec ([x e] ...) body0 ... body1)
    (primapp pr e1 ...)                          => (pr e1 ...)
    (app e0 e1 ...)                              => (e0 e1 ...)))

(define-language LBool
  (terminals
    (boolean (b)))
  (Expr (e)
    b))

(define-language LBoolLambda
  (terminals
    (boolean (b))
    (symbol (x)))
  (Expr (e)
    v
    x
    (lambda (x) e)
    (and e0 e1)
    (or e0 e1)
    (not e)
    (e0 e1))
  (Value (v)
         b))

(define unit-tests
  (test-suite "unit-tests"
    (test-case "with-output-language"
      (check-equal?
       '(var a)
       (unparse-L0 (with-output-language L0 (in-context Expr `(var a)))))
      (check-equal?
       '(let ([x '1] [y '2]) (primapp + (var x) (var y)))
       (unparse-L0
        (with-output-language L0
          (in-context Expr
                      `(let ([x (quote 1)] [y (quote 2)])
                         (primapp + (var x) (var y)))))))
      (check-equal?
       '(var a)
       (unparse-L0 (with-output-language (L0 Expr) `(var a))))
      (check-equal?
       '(let ([x '1] [y '2]) (primapp + (var x) (var y)))
       (unparse-L0
        (with-output-language (L0 Expr)
          `(let ([x (quote 1)] [y (quote 2)])
             (primapp + (var x) (var y)))))))

    (test-case "unparse-language"
      (check-equal?
       `(quoted 5)
       (unparse-LUNPARSE
        (with-output-language (LUNPARSE Expr) `(quoted 5))
        #t))

      (check-equal?
       `(seq (quoted 7) (quoted 8))
       (unparse-LUNPARSE
        (with-output-language (LUNPARSE Expr)
          `(seq (quoted 7) (quoted 8)))
        #t))

      (let ([x.0 (make-var 'x.0)])
        (check-equal?
         `(var ,x.0)
         (unparse-LUNPARSE
          (with-output-language (LUNPARSE Expr) `(var ,x.0))
          #t)))

      (let ([x.1 (make-var 'x.1)]
            [x.2 (make-var 'x.2)]
            [y.3 (make-var 'y.3)]
            [x.4 (make-var 'x.4)]
            [zero?.5 (make-var 'zero?.5)]
            [*.6 (make-var '*.6)]
            [f.7 (make-var 'f.7)])
        (check-equal?
         `(recbinding (,zero?.5 ,*.6 ,f.7)
                      ((lambda (,x.1) (primapp = (var ,x.1) (quoted 0)))
                       (lambda (,x.2 ,y.3)
                         (if (app (var ,zero?.5) (var ,x.2))
                             (quoted 0)
                             (if (primapp = (var ,x.2) (quoted 1))
                                 (var ,y.3)
                                 (primapp + (var ,y.3)
                                          (app (var ,*.6)
                                               (primapp - (var ,x.2) (quoted 1))
                                               (var ,y.3))))))
                       (lambda (,x.4)
                         (if (app (var ,zero?.5) (var ,x.4))
                             (quoted 1)
                             (app (var ,*.6) (var ,x.4)
                                  (app (var ,f.7)
                                       (primapp - (var ,x.4) (quoted 1)))))))
                      (app (var ,f.7) (quoted 10)))
         (unparse-LUNPARSE
          (with-output-language (LUNPARSE Expr)
            `(recbinding
              (,zero?.5 ,*.6 ,f.7)
              ((lambda (,x.1) (primapp = (var ,x.1) (quoted 0)))
               (lambda (,x.2 ,y.3)
                 (if (app (var ,zero?.5) (var ,x.2))
                     (quoted 0)
                     (if (primapp = (var ,x.2) (quoted 1))
                         (var ,y.3)
                         (primapp + (var ,y.3)
                                  (app (var ,*.6)
                                       (primapp - (var ,x.2) (quoted 1))
                                       (var ,y.3))))))
               (lambda (,x.4)
                 (if (app (var ,zero?.5) (var ,x.4))
                     (quoted 1)
                     (app (var ,*.6) (var ,x.4)
                          (app (var ,f.7)
                               (primapp - (var ,x.4) (quoted 1)))))))
              (app (var ,f.7) (quoted 10)))) #t)))

      (check-equal?
       '(quote 5)
       (unparse-LUNPARSE
        (with-output-language (LUNPARSE Expr) `(quoted 5))
        #f))

      (check-equal?
       '(begin (quote 7) (quote 8))
       (unparse-LUNPARSE
        (with-output-language (LUNPARSE Expr)
          `(seq (quoted 7) (quoted 8)))
        #f))

      (let ([x.0 (make-var 'x.0)])
        (check-equal?
         'x.0
         (unparse-LUNPARSE
          (with-output-language (LUNPARSE Expr) `(var ,x.0))
          #f)))

      (let ([x.1 (make-var 'x.1)]
            [x.2 (make-var 'x.2)]
            [y.3 (make-var 'y.3)]
            [x.4 (make-var 'x.4)]
            [zero?.5 (make-var 'zero?.5)]
            [*.6 (make-var '*.6)]
            [f.7 (make-var 'f.7)])
        (check-equal?
         '(letrec ([zero?.5 (lambda (x.1) (= x.1 '0))]
                   [*.6 (lambda (x.2 y.3)
                          (if (zero?.5 x.2)
                              '0
                              (if (= x.2 '1)
                                  y.3
                                  (+ y.3 (*.6 (- x.2 '1) y.3)))))]
                   [f.7 (lambda (x.4)
                          (if (zero?.5 x.4)
                              '1
                              (*.6 x.4 (f.7 (- x.4 '1)))))])
            (f.7 '10))
         (unparse-LUNPARSE
          (with-output-language (LUNPARSE Expr)
            `(recbinding
              (,zero?.5 ,*.6 ,f.7)
              ((lambda (,x.1) (primapp = (var ,x.1) (quoted 0)))
               (lambda (,x.2 ,y.3)
                 (if (app (var ,zero?.5) (var ,x.2))
                     (quoted 0)
                     (if (primapp = (var ,x.2) (quoted 1))
                         (var ,y.3)
                         (primapp + (var ,y.3)
                                  (app (var ,*.6)
                                       (primapp - (var ,x.2) (quoted 1))
                                       (var ,y.3))))))
               (lambda (,x.4)
                 (if (app (var ,zero?.5) (var ,x.4))
                     (quoted 1)
                     (app (var ,*.6) (var ,x.4)
                          (app (var ,f.7)
                               (primapp - (var ,x.4) (quoted 1)))))))
              (app (var ,f.7) (quoted 10)))) #f)))
      )

    (test-case "boolean-terminals"
      (let ()
        (define-parser parse-LBool LBool)
        (check-equal? #t (parse-LBool #t)))
      (let ()
        (define-parser parse-LBool LBool)
        (check-equal? #f (parse-LBool #f)))
      (let ()
        (define-parser parse-LBool LBool)
        (with-handlers ([exn:fail? (lambda (x) #t)])
          (check-equal? 'a (parse-LBool 'a))))
      (let ()
        (define-parser parse-LBoolLambda LBoolLambda)
        (check-equal? #t (parse-LBoolLambda #t)))
      (let ()
        (define-parser parse-LBoolLambda LBoolLambda)
        (check-equal? #f (parse-LBoolLambda #f)))
      (let ()
        (define-parser parse-LBoolLambda LBoolLambda)
        (check-equal? 
         '(lambda (x) #f) 
         (unparse-LBoolLambda
          (parse-LBoolLambda '(lambda (x) #f)))))
      (let ()
        (define-parser parse-LBoolLambda LBoolLambda)
        (check-equal? 
         '(lambda (f) (f #f)) 
         (unparse-LBoolLambda
          (parse-LBoolLambda '(lambda (f) (f #f))))))
      (let ()
        (define-parser parse-LBoolLambda LBoolLambda)
        (check-equal? 
         '(lambda (f) (not (f #f)))
         (unparse-LBoolLambda
          (parse-LBoolLambda '(lambda (f) (not (f #f))))))))))

(define datum?
  (lambda (x)
    (or (number? x) (string? x) (symbol? x)
        (and (pair? x) (datum? (car x)) (datum? (cdr x)))
        (and (vector? x) (andmap datum? (vector->list x))))))

(define-language LVAR
  (terminals
    (var (x))
    (primitive (pr))
    (datum (d)))
  (Expr (e)
    (var x)
    (quote d)
    (if e0 e1 e2)
    (begin e0 ... e1)
    (let ([x e] ...) e1)
    (letrec ([x e] ...) e1)
    (app e0 e1 ...)
    (primapp pr e ...)))

(define-pass break-variable : LVAR (ir) -> LVAR ()
  (definitions
    (define var? symbol?))
  (Expr : Expr (ir) -> Expr ()
    [(var ,x) (printf "found var: ~a\n" (var-sym x)) `(var ,x)]))

(define ensure-correct-identifiers
  (test-suite "ensure-correct-identifiers"
    (test-case "accidental-variable?-capture"
      (check-equal?
       (with-output-to-string
         (lambda ()
           (break-variable
            (with-output-language (LVAR Expr)
              `(var ,(make-var 'x))))))
       "found var: x\n"))))

(define-language Lmaybe
  (terminals
    (boolean (b))
    (integer (i)))
  (Exp (e)
    (Int i)
    (Bool b)
    (Bar (maybe i) e)
    (Foo i (maybe e))))

(define-parser parse-Lmaybe Lmaybe)

(define maybe-tests
  (test-suite "maybe-tests"
    (test-case "maybe-parse/unparse"
      (check-equal?
       '(Int 72)
       (unparse-Lmaybe (parse-Lmaybe '(Int 72))))
      (check-equal?
       '(Bool #t)
       (unparse-Lmaybe (parse-Lmaybe '(Bool #t))))
      (check-equal?
       '(Bar 5 (Bool #t))
       (unparse-Lmaybe (parse-Lmaybe '(Bar 5 (Bool #t)))))
      (check-equal?
       '(Bar #f (Bool #t))
       (unparse-Lmaybe (parse-Lmaybe '(Bar #f (Bool #t)))))
      (check-equal?
       '(Foo 5 #f)
       (unparse-Lmaybe (parse-Lmaybe '(Foo 5 #f))))
      (check-equal?
       '(Foo 5 (Foo 4 (Foo 3 #f)))
       (unparse-Lmaybe (parse-Lmaybe '(Foo 5 (Foo 4 (Foo 3 #f))))))
      (check-equal?
       '(Foo 5 (Bar 3 (Foo 1 #f)))
       (unparse-Lmaybe (parse-Lmaybe '(Foo 5 (Bar 3 (Foo 1 #f))))))
      (check-equal?
       '(Foo 5 (Int 3))
       (unparse-Lmaybe (parse-Lmaybe '(Foo 5 (Int 3))))))
    (test-case "maybe-with-output-language/unparse"
      (check-equal?
       '(Int 72)
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Int 72))))
      (check-equal?
       '(Bool #t)
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Bool #t))))
      (check-equal?
       '(Bar 5 (Bool #t))
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Bar 5 (Bool #t)))))
      (check-equal?
       '(Bar #f (Bool #t))
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Bar #f (Bool #t)))))
      (check-equal?
       '(Foo 5 #f)
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Foo 5 #f))))
      (check-equal?
       '(Foo 5 (Foo 4 (Foo 3 #f)))
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Foo 5 (Foo 4 (Foo 3 #f))))))
      (check-equal?
       '(Foo 5 (Bar 3 (Foo 1 #f)))
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Foo 5 (Bar 3 (Foo 1 #f))))))
      (check-equal?
       '(Foo 5 (Int 3))
       (unparse-Lmaybe (with-output-language (Lmaybe Exp) `(Foo 5 (Int 3))))))
    (test-case "maybe-pass"
      (let ()
        (define-pass add-one-int : Lmaybe (ir) ->  Lmaybe ()
          (Exp : Exp (ir) -> Exp ()
            [(Int ,i) `(Int ,(+ i 1))]))
        (and
         (check-equal?
          '(Int 4)
          (unparse-Lmaybe (add-one-int (with-output-language (Lmaybe Exp) `(Int 3)))))
         (check-equal?
          '(Foo 4 (Int 4))
          (unparse-Lmaybe (add-one-int (with-output-language (Lmaybe Exp) `(Foo 4 (Int 3))))))
         (check-equal?
          '(Foo 4 (Foo 5 (Int 3)))
          (unparse-Lmaybe (add-one-int (with-output-language (Lmaybe Exp) `(Foo 4 (Foo 5 (Int 2)))))))
         (check-equal?
          '(Foo 3 #f)
          (unparse-Lmaybe (add-one-int (with-output-language (Lmaybe Exp) `(Foo 3 #f)))))
         (check-equal?
          '(Bar #f (Int 5))
          (unparse-Lmaybe (add-one-int (with-output-language (Lmaybe Exp) `(Bar #f (Int 4))))))))
      (let ()
        (define-pass add-one : Lmaybe (ir) ->  Lmaybe ()
          (Exp : Exp (ir) -> Exp ()
            [(Foo ,i ,[e?]) `(Foo ,(+ i 1) ,e?)]
               [(Bar ,i? ,[e]) `(Bar ,(and i? (+ i? 1)) ,e)]
               [(Int ,i) `(Int ,(+ i 1))]))
        (and
         (check-equal?
          '(Int 4)
          (unparse-Lmaybe (add-one (with-output-language (Lmaybe Exp) `(Int 3)))))
         (check-equal?
          '(Foo 5 (Int 4))
          (unparse-Lmaybe (add-one (with-output-language (Lmaybe Exp) `(Foo 4 (Int 3))))))
         (check-equal?
          '(Foo 5 (Foo 6 (Int 3)))
          (unparse-Lmaybe (add-one (with-output-language (Lmaybe Exp) `(Foo 4 (Foo 5 (Int 2)))))))
         (check-equal?
          '(Foo 4 (Bar 6 (Foo 7 #f)))
          (unparse-Lmaybe (add-one (with-output-language (Lmaybe Exp) `(Foo 3 (Bar 5 (Foo 6 #f)))))))
         (check-equal?
          '(Foo 4 (Bar #f (Foo 7 #f)))
          (unparse-Lmaybe (add-one (with-output-language (Lmaybe Exp) `(Foo 3 (Bar #f (Foo 6 #f))))))))))))

(define-language Lmaybe2
  (terminals
    (boolean (b))
    (integer (i)))
  (Exp (e)
    (Int i)
    (Bool b)
    (Bar (maybe i) ... e)
    (Foo i (maybe e) ...)))

(define-parser parse-Lmaybe2 Lmaybe2)

(define maybe-dots-tests
  (test-suite "maybe-dots-tests"
    (test-case "maybe-parse/unparse"
      (check-equal?
       '(Foo 3)
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 3))))
      (check-equal?
       '(Bar (Int 72))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bar (Int 72)))))
      (check-equal?
       '(Int 72)
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Int 72))))
      (check-equal?
       '(Bool #t)
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bool #t))))
      (check-equal?
       '(Bar 5 (Bool #t))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bar 5 (Bool #t)))))
      (check-equal?
       '(Bar #f (Bool #t))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bar #f (Bool #t)))))
      (check-equal?
       '(Bar #f 1 #f 2 #f 3 (Bool #t))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bar #f 1 #f 2 #f 3 (Bool #t)))))
      (check-equal?
       '(Bar 1 #f 2 #f 3 #f (Bool #t))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Bar 1 #f 2 #f 3 #f (Bool #t)))))
      (check-equal?
       '(Foo 5 #f)
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 5 #f))))
      (check-equal?
       '(Foo 5 #f #f (Bar 3 (Foo 2 #f)) (Bool #t) #f #f (Int 2) #f)
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 5 #f #f (Bar 3 (Foo 2 #f)) (Bool #t) #f #f (Int 2) #f))))
      (check-equal?
       '(Foo 5 (Foo 4 (Foo 3 #f (Bool #t) (Int 3))))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 5 (Foo 4 (Foo 3 #f (Bool #t) (Int 3)))))))
      (check-equal?
       '(Foo 5 (Bar 3 (Foo 1 (Bar 2 (Bool #t)) #f #f)))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 5 (Bar 3 (Foo 1 (Bar 2 (Bool #t)) #f #f))))))
      (check-equal?
       '(Foo 5 (Int 3) (Bool #f))
       (unparse-Lmaybe2 (parse-Lmaybe2 '(Foo 5 (Int 3) (Bool #f))))))
    (test-case "maybe-with-output-language/unparse"
      (check-equal?
       '(Foo 3)
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 3))))
      (check-equal?
       '(Bar (Int 72))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bar (Int 72)))))
      (check-equal?
       '(Int 72)
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Int 72))))
      (check-equal?
       '(Bool #t)
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bool #t))))
      (check-equal?
       '(Bar 5 (Bool #t))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bar 5 (Bool #t)))))
      (check-equal?
       '(Bar #f (Bool #t))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bar #f (Bool #t)))))
      (check-equal?
       '(Bar #f 1 #f 2 #f 3 (Bool #t))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bar #f 1 #f 2 #f 3 (Bool #t)))))
      (check-equal?
       '(Bar 1 #f 2 #f 3 #f (Bool #t))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Bar 1 #f 2 #f 3 #f (Bool #t)))))
      (check-equal?
       '(Foo 5 #f)
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 5 #f))))
      (check-equal?
       '(Foo 5 #f #f (Bar 3 (Foo 2 #f)) (Bool #t) #f #f (Int 2) #f)
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 5 #f #f (Bar 3 (Foo 2 #f)) (Bool #t) #f #f (Int 2) #f))))
      (check-equal?
       '(Foo 5 (Foo 4 (Foo 3 #f (Bool #t) (Int 3))))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 5 (Foo 4 (Foo 3 #f (Bool #t) (Int 3)))))))
      (check-equal?
       '(Foo 5 (Bar 3 (Foo 1 (Bar 2 (Bool #t)) #f #f)))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 5 (Bar 3 (Foo 1 (Bar 2 (Bool #t)) #f #f))))))
      (check-equal?
       '(Foo 5 (Int 3) (Bool #f))
       (unparse-Lmaybe2 (with-output-language (Lmaybe2 Exp) `(Foo 5 (Int 3) (Bool #f))))))
    (test-case "maybe-pass"
      (let ()
        (define-pass add-one-int : Lmaybe2 (ir) ->  Lmaybe2 ()
          (Exp : Exp (ir) -> Exp ()
            [(Int ,i) `(Int ,(+ i 1))]))
        (and
         (check-equal?
          '(Int 4)
          (unparse-Lmaybe2 (add-one-int (with-output-language (Lmaybe2 Exp) `(Int 3)))))
         (check-equal?
          '(Foo 4 (Int 4) (Int 5) (Int 7) #f #f (Int 8))
          (unparse-Lmaybe2 (add-one-int (with-output-language (Lmaybe2 Exp) `(Foo 4 (Int 3) (Int 4) (Int 6) #f #f (Int 7))))))
         (check-equal?
          '(Foo 4 (Foo 5 (Int 3) #f (Int 4) (Int 5)))
          (unparse-Lmaybe2 (add-one-int (with-output-language (Lmaybe2 Exp) `(Foo 4 (Foo 5 (Int 2) #f (Int 3) (Int 4)))))))
         (check-equal?
          '(Foo 3 #f (Int 4))
          (unparse-Lmaybe2 (add-one-int (with-output-language (Lmaybe2 Exp) `(Foo 3 #f (Int 3))))))
         (check-equal?
          '(Bar 3 #f 4 #f (Int 4))
          (unparse-Lmaybe2 (add-one-int (with-output-language (Lmaybe2 Exp) `(Bar 3 #f 4 #f (Int 3))))))))
      (let ()
        (define-pass add-one : Lmaybe2 (ir) ->  Lmaybe2 ()
          (Exp : Exp (ir) -> Exp ()
            [(Foo ,i ,[e?*] ...) `(Foo ,(+ i 1) ,e?* ...)]
            [(Bar ,i?* ... ,[e]) `(Bar ,(map (lambda (i?) (and i? (+ i? 1))) i?*) ... ,e)]
            [(Int ,i) `(Int ,(+ i 1))]))
        (and
         (check-equal?
          '(Int 4)
          (unparse-Lmaybe2 (add-one (with-output-language (Lmaybe2 Exp) `(Int 3)))))
         (check-equal?
          '(Foo 5 (Int 4) (Int 5) (Int 6) #f (Int 8))
          (unparse-Lmaybe2 (add-one (with-output-language (Lmaybe2 Exp) `(Foo 4 (Int 3) (Int 4) (Int 5) #f (Int 7))))))
         (check-equal?
          '(Foo 5 (Foo 6 (Int 3) (Bar 4 3 2 #f 1 (Foo 3 (Int 8) (Int 9)))))
          (unparse-Lmaybe2 (add-one (with-output-language (Lmaybe2 Exp) `(Foo 4 (Foo 5 (Int 2) (Bar 3 2 1 #f 0 (Foo 2 (Int 7) (Int 8)))))))))
         (check-equal?
          '(Foo 4 (Bar 6 #f 8 #f 9 (Foo 7 #f)) (Bool #t) #f)
          (unparse-Lmaybe2 (add-one (with-output-language (Lmaybe2 Exp) `(Foo 3 (Bar 5 #f 7 #f 8 (Foo 6 #f)) (Bool #t) #f)))))
         (check-equal?
          '(Foo 4 (Bar #f (Foo 7 #f)) (Bool #t) #f)
          (unparse-Lmaybe2 (add-one (with-output-language (Lmaybe2 Exp) `(Foo 3 (Bar #f (Foo 6 #f)) (Bool #t) #f))))))))))

(define-language LMaybeNoBool
  (terminals
    (symbol (x))
    (number (n)))
  (Expr (e)
    (foo x (maybe n))
    (bar (maybe e) x)
    (num n)
    (ref x)))

(define-language LMaybeListNoBool
  (terminals
    (symbol (x))
    (number (n)))
  (Expr (e)
    (foo ([x (maybe n)] ...) e)
    (bar (maybe e) ... x)
    (num n)
    (ref x)))

(define maybe-unparse-tests
  (test-suite "maybe-unparse-tests"
    (test-case "maybe-unparse"
      (check-equal? '(foo x 10)
                    (unparse-LMaybeNoBool
                     (with-output-language (LMaybeNoBool Expr)
                       `(foo x 10))))
      (check-equal? '(bar (foo x #f) x)
                    (unparse-LMaybeNoBool
                     (with-output-language (LMaybeNoBool Expr)
                       `(bar (foo x #f) x))))
      (check-equal? '(bar (bar (foo y #f) y) z)
                    (unparse-LMaybeNoBool
                     (with-output-language (LMaybeNoBool Expr)
                       `(bar (bar (foo y #f) y) z))))
      (check-equal? '(bar (bar (bar #f x) y) z)
                    (unparse-LMaybeNoBool
                     (with-output-language (LMaybeNoBool Expr)
                       `(bar (bar (bar #f x) y) z)))))

    (test-case "maybe-unparse-dots"
      (check-equal? '(foo ([x 10] [y 12]) (ref x))
                    (unparse-LMaybeListNoBool
                     (with-output-language (LMaybeListNoBool Expr)
                       `(foo ([x 10] [y 12]) (ref x)))))
      (check-equal? '(foo ([x #f] [y 12] [z #f]) (ref y))
                    (unparse-LMaybeListNoBool
                     (with-output-language (LMaybeListNoBool Expr)
                       `(foo ([x #f] [y 12] [z #f]) (ref y)))))
      (check-equal? '(bar #f #f (num 10) (ref x) #f (foo ([x #f] [y 10] [z 5] [w #f]) (bar #f z)) #f w)
                    (unparse-LMaybeListNoBool
                     (with-output-language (LMaybeListNoBool Expr)
                       `(bar #f #f (num 10) (ref x) #f (foo ([x #f] [y 10] [z 5] [w #f]) (bar #f z)) #f w)))))))

;; tests related to issue #7 on github.com
(define-language LPairs
  (terminals
    (symbol (x))
    (null (n)))
  (Expr (e)
    x
    n
    (e0 . e1)))

(define-parser parse-LPairs LPairs)

(define-pass reverse-pairs : LPairs (p) -> LPairs ()
  (Expr : Expr (p) -> Expr ()
    [(,[e0] . ,[e1]) `(,e1 . ,e0)]))

(define-language LList
  (terminals
    (symbol (x))
    (null (n)))
  (Expr (e)
    x
    n
    (e0 ... . e1)))

(define-parser parse-LList LList)

(define-language LList2
  (terminals
    (symbol (x))
    (null (n)))
  (Expr (e)
    x
    n
    (e0 ... e1)))

(define-pass swap-parts : LList (e) -> LList ()
  (Expr : Expr (e) -> Expr ()
    [(,[e*] ... . ,[e])
     `(,e ,e* ... . ())]))

;; example provided by Simon Stapleton via bug #7
(define-language Lx
  (terminals
    (symbol (x)))
  (Expr (e)
    x
    (lambda (x* ... . x) e)
    (define (x x* ... . x1) e)
    (define x e)))

(define-parser parse-Lx Lx)

(define-pass Px1 : Lx (ir) -> Lx ()
  (Expr : Expr (ir) -> Expr()
    [(define (,x ,x* ... . ,x1) ,[e])
     `(define ,x (lambda (,x* ... . ,x1) ,e))]))

(define language-dot-support
  (test-suite "language-dot-support"
    (test-case "simple-dots"
      (check-equal?
       '()
       (unparse-LPairs (parse-LPairs '())))
      (check-equal?
       'a
       (unparse-LPairs (parse-LPairs 'a)))
      (check-equal?
       '(a)
       (unparse-LPairs (parse-LPairs '(a))))
      (check-equal?
       '(a . b)
       (unparse-LPairs (parse-LPairs '(a . b))))
      (check-equal?
       '(a b c . d)
       (unparse-LPairs (parse-LPairs '(a b c . d))))
      (check-equal?
       '(((a b . c) d e) f . g)
       (unparse-LPairs (parse-LPairs '(((a b . c) d e) f . g))))
      (check-equal?
       '()
       (unparse-LPairs (with-output-language (LPairs Expr) `())))
      (check-equal?
       'a
       (unparse-LPairs (with-output-language (LPairs Expr) `a)))
      (check-equal?
       '(a)
       (unparse-LPairs (with-output-language (LPairs Expr) `(a))))
      (check-equal?
       '(a . b)
       (unparse-LPairs (with-output-language (LPairs Expr) `(a . b))))
      (check-equal?
       '(a b c . d)
       (unparse-LPairs (with-output-language (LPairs Expr) `(a b c . d))))
      (check-equal?
       '(((a b . c) d e) f . g)
       (unparse-LPairs (with-output-language (LPairs Expr) `(((a b . c) d e) f . g))))
      (check-equal?
       '(() . a)
       (unparse-LPairs (reverse-pairs (parse-LPairs '(a)))))
      (check-equal?
       '(b . a)
       (unparse-LPairs (reverse-pairs (parse-LPairs '(a . b)))))
      (check-equal?
       '(((d . c) . b) . a)
       (unparse-LPairs (reverse-pairs (parse-LPairs '(a b c . d)))))
      (check-equal?
       '((g . f) ((() . e) . d) (c . b) . a)
       (unparse-LPairs (reverse-pairs (parse-LPairs '(((a b . c) d e) f . g))))))
    (test-case "dot-after-ellipsis"
      (check-equal?
       '()
       (unparse-LList (parse-LList '())))
      (check-equal?
       'x
       (unparse-LList (parse-LList 'x)))
      (check-equal?
       '(a b c)
       (unparse-LList (parse-LList '(a b c))))
      (check-equal?
       '(a b c . d)
       (unparse-LList (parse-LList '(a b c . d))))
      (check-equal?
       '(((a b) (c d)) e . f)
       (unparse-LList (parse-LList '(((a b) (c d)) e . f))))
      (check-equal?
       '()
       (unparse-LList (with-output-language (LList Expr) `())))
      (check-equal?
       'x
       (unparse-LList (with-output-language (LList Expr) `x)))
      (check-equal?
       '(a b c)
       (unparse-LList (with-output-language (LList Expr) `(a b c))))
      (check-equal?
       '(a b c . d)
       (unparse-LList (with-output-language (LList Expr) `(a b c . d))))
      (check-equal?
       '(((a b) (c d)) e . f)
       (unparse-LList (with-output-language (LList Expr) `(((a b) (c d)) e . f))))
      (check-equal?
       '(() a b c)
       (unparse-LList (swap-parts (with-output-language (LList Expr) `(a b c)))))
      (check-equal?
       '(d a b c)
       (unparse-LList (swap-parts (with-output-language (LList Expr) `(a b c . d)))))
      (check-equal?
       '(f (() (() a b) (() c d)) e)
       (unparse-LList (swap-parts (with-output-language (LList Expr) `(((a b) (c d)) e . f))))))

    (test-case "github-issue-7"
      (check-equal?
       'x
       (unparse-Lx (parse-Lx 'x)))
      (check-equal?
       '(lambda (x . z) x)
       (unparse-Lx (parse-Lx '(lambda (x . z) x))))
      (check-equal?
       '(lambda (x y . z) x)
       (unparse-Lx (parse-Lx '(lambda (x y . z) x))))
      (check-equal?
       '(lambda x x)
       (unparse-Lx (parse-Lx '(lambda x x))))
      (check-equal?
       '(define (x y . z) z)
       (unparse-Lx (parse-Lx '(define (x y . z) z))))
      (check-equal?
       '(define x x)
       (unparse-Lx (parse-Lx '(define x x))))
      (check-equal?
       '(define (l m . n)
          (define g
            (lambda (x . z)
              (lambda (a . b)
                (lambda (c . d)
                  l)))))
       (unparse-Lx (parse-Lx '(define (l m . n)
                                (define g
                                  (lambda (x . z)
                                    (lambda (a . b)
                                      (lambda (c . d)
                                        l))))))))
      (check-equal?
       'x
       (unparse-Lx (with-output-language (Lx Expr) `x)))
      (check-equal?
       '(lambda (x . z) x)
       (unparse-Lx (with-output-language (Lx Expr) `(lambda (x . z) x))))
      (check-equal?
       '(lambda (x y . z) x)
       (unparse-Lx (with-output-language (Lx Expr) `(lambda (x y . z) x))))
      (check-equal?
       '(define (x y . z) z)
       (unparse-Lx (with-output-language (Lx Expr) `(define (x y . z) z))))
      (check-equal?
       '(lambda x x)
       (unparse-Lx (with-output-language (Lx Expr) `(lambda x x))))
      (check-equal?
       '(define x x)
       (unparse-Lx (with-output-language (Lx Expr) `(define x x))))
      (check-equal?
       '(define (l m . n)
          (define g
            (lambda (x . z)
              (lambda (a . b)
                (lambda (c . d)
                  l)))))
       (unparse-Lx (with-output-language (Lx Expr) `(define (l m . n)
                                                      (define g
                                                        (lambda (x . z)
                                                          (lambda (a . b)
                                                            (lambda (c . d)
                                                              l))))))))
      (check-equal?
       '(define f (lambda (x . y) x))
       (unparse-Lx (Px1 (parse-Lx '(define (f x . y) x)))))
      (check-equal?
       '(define g (lambda (x y z . w) w))
       (unparse-Lx (Px1 (parse-Lx '(define (g x y z . w) w)))))
      (check-equal?
       '(define h (lambda (x y . z) (define i (lambda (a b c . d) d))))
       (unparse-Lx (Px1 (parse-Lx '(define (h x y . z) (define (i a b c . d) d))))))
      (check-equal?
       '(define f (lambda x (define g (lambda y x))))
       (unparse-Lx (Px1 (parse-Lx '(define (f . x) (define (g . y) x)))))))))

(define-language LMULTI
  (terminals
    (var (x))
    (primitive (pr))
    (datum (d)))
  (Expr (e)
    (var x)
    (primref pr)
    (quote d)
    (if e0 e1 e2)
    (begin e0 ... e1)
    (let ([x e] ...) e1)
    (letrec ([x le] ...) e)
    (app e0 e1 ...))
  (LambdaExpr (le)
    (lambda (x ...) e)
    (case-lambda cl ...))
  (CaseLambdaClause (cl)
    (clause (x ...) e)))

(define error-messages
  (test-suite "error-messages"
    (test-case "error-regressions"
      (check-exn
        #rx"unexpected constant as pattern, maybe missing unquote?"
        (lambda ()
          (parameterize ([current-namespace (make-base-namespace)])
            (namespace-require "base.rkt")
            (eval '(let ()
                     ;; error reported against racket version of nanopass-framework
                     ;; from Jens Axel Søgaard
                     ;; (github.com/akeep/nanoass-framework-racket issue #9)
                     (define (constant? c) (number? c))
                     (define-language L
                       (terminals
                         (constant (c)))
                       (Expr (e)
                         c))
                     (define (parse v)
                       (with-output-language (L Expr)
                         (cond
                           [(number? v) `,v]
                           [else (error 'parse "got: " v)])))
                     (define-pass add1 : L (e) -> L ()
                       (Expr : Expr (e) -> Expr ()
                         [c (guard (even? c)) (+ c 1)]))
                     (add1 (parse 42))))))))))
