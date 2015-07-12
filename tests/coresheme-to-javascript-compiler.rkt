#lang nanopass
;;; Copyright (c) 2015 Jens Axel Søgaard
;;; See the accompanying file Copyright for details
(require (prefix-in old- mzlib/match) (for-syntax nanopass/base))

;;;
;;; CoreScheme to JavaScript
;;;

;; This compiler compiles a program in CoreScheme (CS)
;; via the Admistrative Normal Form of CoreScheme (ACS)
;; into JavaScript.

;; The compiler uses the Racket version of the Nanopass 
;; framework for writing compilers with many small passes.

;;;
;;; CoreScheme (CS)
;;; 

;; The terms M and values V of Core Scheme are given by:

;;    M ::= V                    ; V in Values
;;       |  (let (x M1) M2)      
;;       |  (if M1 M2 M3)
;;       |  (M M1 ...)
;;       |  (O M1 ...)           ; O in PrimitiveOperations

;;    V ::= c                    ; c in Constants
;;       |  x                    ; x in Variables
;;       |  (λ (x1 ...) M)       

;; Note: In (λ (x1 ...) M) the xi are mutually distinct
;;       and are bound in M.
;;       In (let (x M1) M2) the x is bound in M2.

;; The language CS is almost the same as the one used in
;;     "The Essence of Compiling with Continuations"
;;     by Cormac Flanagan, Amr Sabry, Bruce F. Duba, and, Matthias Felleisen.
;; The only difference is that the full  if  is used rather than  if0.

;;;
;;; Terminals: Primitives, Constants, and, Variables
;;;

;; The representation of terminals is determined here.
;; In this simple compiler symbols are used to represent identifiers
;; (syntax objects would be a better representation choice).

(define primitives '(+ - * / zero?))

(define (Primitive? v)
  (and (memq v primitives) #t))

(define (Constant? v)
  (or (number? v)
      (boolean? v)))

(define (Variable? v)
  (and (symbol? v)
       (not (Primitive? v))))

;;;
;;; CoreScheme CS
;;;

;; Now we are ready to define the language.

(define-language LCS
  (entry Term)            ; a full program is a term
  (terminals
   (Variable  (x))        ; x fulfilles the Variable? predicate
   (Primitive (O))        ; O fulfilles the Primitive? predicate
   (Constant  (c)))       ; c fulfilles the Constant? predicate
  ;; Non-terminals : Value and Term
  (Value (V)
    c                     ; constant
    x                     ; variable reference
    (λ (x1 ...) M))       ; abstraction
  (Term (M)
    V                     ; return
    (let (x M1) M2)       ; bind
    (if M1 M2 M3)         ; branch
    (prim O M1 ...)       ; application of primitive operation
    => (O M1 ...)         ; (unparse applications without the prim prefix)  
    (call M M1 ...)       ; application
    => (M M1 ...)))       ; (unparse applications without the call prefix)


;; The (define-language LCS ...) automatically defines
;; a number of structures representing a program in LCS.

;;;
;;; PARSE (convert S-expression into nanopass structures)
;;;

;; In order to get a value representing an LCS program,
;; we need to convert a program represented as an S-expression
;; into the Nanopass structures.

;; parse is written as a pass from e with type * (i.e. anything) into an LCS-struct.
(define-pass parse : * (S-exp) -> LCS ()
  (definitions 
    (define (Term* Ms) (map Term Ms)))
  ; Term : s-expression -> LCS:Term-struct
  (Value : * (V) -> Value ()
    (with-output-language (LCS Value)
      ;(displayln (list 'parse-Value V))
      (old-match V
        [(? Constant?  c)     `,c]
        [(? Variable?  x)     `,x]
        [('λ (x1 ...) M)      `(λ (,x1 ...) ,(Term M))]
        [_ (error 'parse-Value "got: ~a" V)])))
  (Term : * (M) -> Term ()
    (with-output-language (LCS Term) ; rebinds quasiquote to construct Lsrc records
      ;(displayln (list 'parse-Term M))
      (old-match M                 ; (match is bound to a special nanopass pattern matcher)
        [(? Constant? V)           `,(Value V)]
        [(? Variable? V)           `,(Value V)]
        [('λ (x1 ...) M1)          `,(Value M)]
        [('let (x M1) M2)          `(let (,x ,(Term M1)) ,(Term M2))]
        [('if M1 M2 M3)            `(if ,(Term M1) ,(Term M2) ,(Term M3))]
        [((? Primitive? O) M1 ...) `(prim ,O ,(Term* M1) ...)]
        [(M M1 ...)                `(call ,(Term M) ,(Term* M1) ...)]
        [_ (error 'parse-Term "got: ~a" M)])))
  ; start parsing
  (Term S-exp))

;; The (define-language LCS ...) defines an unparser unpase-LCS
;; from LCS structures to S-expression, which we can use to
;; test our parser.

(module+ test (require rackunit)
  (check-equal? (unparse-LCS (parse '3))                       '3)
  (check-equal? (unparse-LCS (parse '(if0 1 2 3)))             '(if0 1 2 3))
  (check-equal? (unparse-LCS (parse '(+ 1 2)))                 '(+ 1 2))
  (check-equal? (unparse-LCS (parse '(+ 1 2 3)))               '(+ 1 2 3))
  (check-equal? (unparse-LCS (parse '(let (x 1) x)))           '(let (x 1) x))
  (check-equal? (unparse-LCS (parse '((λ (x y) (+ x y) 1 2)))) '((λ (x y) (+ x y) 1 2))))

;;;
;;; α-RENAMING
;;;

; To make the next pass easier to implement we do α-renaming.
; The α-renaming pass makes names unique.
;
; Example:
;   (let (x 1) (let (x 2) x)) is rewritten into (let (x 1) (let (x.1 2) x.2))
;
; The code below uses an environment ρ that maps original identifiers
; into ones used in the output. Primitives are mapped to them selves
; in order not to rename primitives in the output. An unbound identifier
; is mapped to #f by the environment ρ.

; The expression (fresh x ρ) returns a new variable not present in ρ.
; The new name is based in the identifier x.


;; New identifiers are produced by new-var.

(define counter 0)
(define joiner ".")
(define (new-var [prefix "t"])          
  (set! counter (+ counter 1))
  (string->symbol (~a prefix joiner counter)))
(define (reset-counter! [joiner ""])
  (set! counter 0))

;; SYNTAX (letv ((x ...) e) b ...)
;;   Evaluate e and bind the result values to x ... in b ...
;;   Example: (letv ((x y) (values 1 2)) (+ x y)) evaluates to 3
(define-syntax (letv stx)
  (syntax-case stx ()
    [(_letv ((id ...) e) . b)
     (syntax/loc stx
       (let-values ([(id ...) e]) . b))]))

(define (map+fold f Ms ρ)
  ; map&fold : (α β -> α β) (listof α) β -> (listof α) β
  ;    Map f over the elements in the list Ms. 
  ;    The first return value of f becomes elements the elements of the first returned list.
  ;    The second return value of f will be used to map the next element.
  ;    Example:
  ;    > (map+fold (λ (x y) (values (sqr x) (add1 y)))
  ;                '(1 2 3 4) 0)
  ;    '(1 4 9 16)
  ;    4
  (define (f* Ms ρ) 
    (match Ms
      ['()         (values '() ρ)]
      [(cons M Ms) (letv ((M ρ) (f M ρ))
                     (letv ((Ms ρ) (f* Ms ρ))
                       (values (cons M Ms) ρ)))]))
  (f* Ms ρ))

(define-pass α-rename : LCS (M) -> LCS ()
  (definitions
    (define (initial-ρ x)
      (if (Primitive? x) x #f))
    (define (extend ρ original renamed)
      (λ (x) (if (eq? x original) renamed (ρ x))))
    (define (extend* ρ originals renameds)
      (for/fold ([ρ ρ]) ([o originals] [r renameds])
        (extend ρ o r)))
    (define (fresh x ρ [orig-x x])
      (if (ρ x) (fresh (new-var x) ρ x) x))
    (define (fresh* xs ρ)
      (define-values (rev-ys ρ*)
        (for/fold ([ys '()] [ρ ρ]) ([x xs])
          (define y (fresh x ρ x))
          (values (cons y ys) (extend ρ x y))))
      (values (reverse rev-ys) ρ*))
    (define (Term* Ms ρ) (map+fold Term Ms ρ)))
  
  (Value : Value (V ρ) -> Value (ρ)
    [,c                  (values `,c    ρ)]
    [,x                  (begin
                           (define x* (ρ x))
                           (unless x* (error 'α-rename "unbound identifier: ~a" x))
                           (values `,x* ρ))]
    [(λ (,x1 ...) ,M)    (begin
                           ; TODO: error unless all xi are distinct
                           (letv ((x1 ρ) (fresh* x1 ρ))
                             (letv ((M ρ) (Term M ρ))
                               (values `(λ (,x1 ...) ,M) ρ))))])
  
  (Term : Term (M ρ) -> Term (ρ)
    [,V                  (Value V ρ)]
    [(let (,x ,M1) ,M2)  (letv ((M1 ρ) (Term M1 ρ))
                           (letv ((xs ρ) (fresh* (list x) ρ))
                             (define x (first xs))
                             (letv ((M2 ρ) (Term M2 ρ))
                               (values `(let (,x ,M1) ,M2) ρ))))]
    [(if ,M1 ,M2 ,M3)    (letv ((Ms ρ) (Term* (list M1 M2 M3) ρ))
                           (old-match-define (M1 M2 M3) Ms)
                           (values `(if ,M1 ,M2 ,M3) ρ))]
    [(prim ,O ,M1 ...)   (letv ((M1 ρ) (Term* M1 ρ))
                           (values `(prim ,O ,M1 ...) ρ))]
    [(call ,M ,M1 ...)   (letv ((M ρ) (Term M ρ))
                           (letv ((M1 ρ) (Term* M1 ρ))
                             (values `(call ,M ,M1 ...) ρ)))])
  (let-values ([(M ρ) (Term M initial-ρ)])
    M))

;;;
;;; Administrative CoreScheme (ACS)
;;;

;; The next compilation pass is A-normalization.
;; The pass rewrites the α-renamed CS program into
;; so-called A-Normal Form.
;; The idea is to break up complex computations in
;; small pieces and name all intermediate results.
;; Furthermore tail call and other call get different syntax.

;; It is time to define the A-Nomalized form of the language CS.
;; The paper has the grammar:
;;     M ::= (let (x (V V1 ...)) M)
;;        |  (let (x (O V1 ...)) M
;;        ...
;; Nanopass doesn't support two nonterminal-productions with the
;; same keyword (here let), so instead the grammar becomes:
;;     M ::= (let (x B) M)
;;        ...
;;     B ::= (call V V1 ...)
;;        |  (prim O V1 ...)
;; where  call  signals application of an abstraction,
;; and    prim  signals application of a primitive operation.

(define-language LACS
  (entry Term)           ; a full program is a term
  (terminals
   (Variable  (x))
   (Primitive (O))
   (Constant  (c)))
  ; Non-terminals : Value and Term
  (Value (V)
    c                 ; constant
    x                 ; variable reference
    (λ (x1 ...) M))   ; abstraction
  (Application (A)
    (call V V1 ...)   ; call
    (prim O V1 ...))  ; primitive operation
  (BoundTerm (B)
    V
    A)
  (Term (M)
    V                 ; return
    A                 ; tail call (call V V1 ...) or
    ;                 ; prim-op   (prim O V1 ...)
    (let (x B) M)     ; bind
    (if V M1 M2)))    ; branch

;;;
;;; A-NORMALIZATION
;;;

;; See "The Essence of Compiling with Continuations" for
;; an explanation of the transformation itself.

; anormalize : LCS -> LACS
(define (anormalize M)  
  (with-output-language (LACS Term)
    (define (normalize-Term M) (normalize M (λ (x) x)))
    (define (normalize M k)
      (nanopass-case (LCS Term) M
        [(let (,x ,M1) ,M2) (normalize M1 (λ (N1) `(let (,x ,N1) ,(normalize M2 k))))]
        [(if ,M1 ,M2 ,M3)   (normalize-name M1
                              (λ (t) (k `(if ,t ,(normalize-Term M2) ,(normalize-Term M3)))))]
        [(call ,M ,M* ...)  (normalize-name M 
                              (λ(t) (normalize-name* M* 
                                      (λ (t*) (k `(call ,t ,t* ...))))))]
        [(prim ,O ,M* ...)  (normalize-name* M*
                              (λ (t*) (k `(prim ,O ,t* ...))))]
        [,V                 (nanopass-case (LCS Value) V
                              [(λ (,x ...) ,M) (k `(λ (,x ...) ,(normalize-Term M)))]
                              [else            (k V)])]))
    (define (normalize-name M k)
      (normalize M (λ (N)
                     (nanopass-case (LACS Term) N
                       [,V   (k N)]
                       [else (let ([t (new-var)])
                               `(let (,t ,N) ,(k t)))]))))
    (define (normalize-name* M* k)
      (cond
        [(empty? M*) (k '())]
        [else        (normalize-name (car M*) 
                       (λ (t) (normalize-name* (cdr M*) 
                                (λ (t*)(k (cons t t*))))))]))
    (normalize M (λ (x) x))))

(define (test-anormalize s-exp)
  (unparse-LACS (anormalize (parse s-exp))))

(module+ test
  (define (test-anormalize s-exp)
    ; during testing we want to produce the same identifier in
    ; each run, so the the "new identifier counter" is reset here.
    (reset-counter! ".") 
    (unparse-LACS (anormalize (parse s-exp))))
  (check-equal? (test-anormalize '1) 1)
  (check-equal? (test-anormalize 'x) 'x)
  (check-equal? (test-anormalize '(λ (x) 1)) '(λ (x) 1))
  (check-equal? (test-anormalize '(if 1 2 3)) '(if 1 2 3))
  (check-equal? (test-anormalize '(if (+ 1 2) 3 4)) 
                '(let (t.1 (prim + 1 2)) (if t.1 3 4)))
  (check-equal? (test-anormalize '(if 1 (+ 2 3) 4))
                '(if 1 (prim + 2 3) 4))
  (check-equal? (test-anormalize '(if 1 2 (+ 3 4)))
                '(if 1 2 (prim + 3 4)))
  ; From the paper:
  (check-equal? (test-anormalize '(+ (+ 2 2) (let (x 1) (f x))))
                '(let (t.1 (prim + 2 2)) 
                   (let (x 1) 
                     (let (t.2 (call f x)) 
                       (prim + t.1 t.2)))))
  (check-equal? (test-anormalize '((f g) (h x) 3) )
                '(let (t.1 (call f g)) 
                   (let (t.2 (call h x)) 
                     (call t.1 t.2 3))))
  (check-equal? (test-anormalize '(λ (x) (+ (* 2 x) 3)))
                '(λ (x) (let (t.1 (prim * 2 x))
                          (prim + t.1 3)))))

;;;
;;; GENERATING JAVASCRIPT
;;;

; Generation of JavaScript source in text form is done in two stages.
; First  generate-js  builds a tree whose leaves are strings and numbers.
; Then   emit         recurses through the tree and displays the elements in order.

(define-pass generate-js : LACS (M) -> * ()
  (definitions
    (reset-counter!)
    (define (new-vars vs) (for/list ([v vs]) (new-var)))
    (define (~comment . xs) (~a "// " (string-join xs) "\n"))
    (define (~id id)
      ; rewrite a Scheme identifier into a legal JavaScript identifier
      (define substitutions (make-hash '(("_"."__") ("-"."_u") ("?"."_p") ("."."_d") ("/"."_w"))))
      (string-join (for/list ([c (~a id)]) (hash-ref substitutions (~a c) (~a c))) ""))
    (define (~variable x) (~id x))
    (define (~number r)
      (cond [(or (fixnum? r) (inexact? r)) (~a r)]
            [else (error '~number "fixnum or inexact number expected, got ~a" r)]))
    (define (~boolean b)
      (match b [#t "true"] [#f "false"] [_ (error '~boolean "boolean expected, got ~a" b)]))
    (define (~string s) (unless (string? s) (error '~string "string expected, got ~a" s))
      (~a "\"" s "\""))
    (define (~constant c)
      (old-match c
        [(? number?)  (~number c)]
        [(? boolean?) (~boolean c)]
        [(? string?)  (~string c)]
        [_ (error '~constant "number, boolean or string expected, got ~a" c)]))
    (define (~store expr dest)      
      (list (~id dest) "=" expr))  ; no ; in an expression context !
    (define (prim->js-prim O)
      (old-match (assq O `((+ "plus") (- "minus") (* "mult") (/ "div") (zero? ,(~id 'zero?))))
        [(_ js-prim) js-prim]
        [_           (error 'prim->js-prim "no JavaScript implementation of ~a yet" O)]))
    (define (~parens . xs)
      (define open  "(") (define close ")")
      (cond [(andmap string? xs) (~a open (apply string-append xs) close)]
            [else                (list open xs close)]))
    (define (~curly-parens . xs)
      (define open "{") (define close "}")
      (cond [(andmap string? xs) (~a open (apply string-append xs) close)]
            [else                (list open xs close)]))
    (define (~application name args)
      (~parens name (~parens (add-between args ","))))
    (define (~statement . s)
      (append s '(";\n")))
    (define (~var-declaration x e)
      (~statement "var " (~id x) "=" e))
    (define (~undefined) "undefined")
    (define (~return e) (list "return" (~parens e)))
    (define (~for initialization condition final-expression . body)
      (~statement (list "for" (~parens initialization ";" condition ";" final-expression)
                        (~curly-parens "\n" body))))
    (define (~function name formals body+return)
      ((if (equal? name "") values ~statement)
       (list "function " name (~parens (add-between (map ~id formals) ","))
             (~curly-parens "\n"
                            body+return))))
    (define (~lambda formals body to-return)
      (~function "" formals
        (list (~var-declaration to-return (~undefined))
              (~statement body)
              (~return (~id to-return)))))
    (define (~not e) (~parens "!" e))
    (define (~=== e1 e2) (~parens e1 "===" e2))
    (define (~boolify v) (~not (~=== v "false")))
    (define (~ternary v m1 m2)
      (~parens (list (~boolify v) "?" m1 ":" m2)))
    (define (~declare-local v body)
      ; declare a local variable whose scope is body
      (~application (~lambda '()
                             (list (~var-declaration (~id v) (~undefined)) body)
                             (new-var))
                    '())))
  (Value : Value (V) -> * ()
    [,c                (~constant c)]
    [,x                (~variable x)]
    [(λ (,x1 ...) ,M)  (define t (new-var))
                       (~lambda x1 (Term M t) t)])
  
  (Application : Application (A dest) -> * ()
    [(call ,V ,V1 ...) (define js-f (Value V))
                       (define args (map Value V1))
                       ; TODO: Insert check that js-f is an actual function
                       (~store (~application js-f args) dest)]
    [(prim ,O ,V1 ...) (define js-prim (prim->js-prim O))
                       (define args    (map Value V1))
                       (~store (~application js-prim args) dest)])
  
  (BoundTerm : BoundTerm (B x) -> * ()
    [,V                (~statement (~store (Value V) x))]
    [,A                (~statement (Application A x))])
  
  (Term : Term (M dest) -> * ()
    [,V                (~store (Value V) dest)]
    [,A                (Application A dest)]            ; tail call
    [(let (,x ,B) ,M)  (~declare-local x (list (BoundTerm B x) (Term M dest)))]
    [(if ,V ,M1 ,M2)   (~ternary (Value V) (Term M1 dest) (Term M2 dest))])
  
  (define (~arithmetic-primitive name start from-index operator)
    (~function (~id name) '()
      (list (~var-declaration 'r start)
            (~var-declaration 'i from-index)
            (~for (~a "i=" from-index) "i<arguments.length" "i++"
                  "r = r " operator " arguments[i];")
            (~return "r"))))
  (list
   (~comment "JavaScript Primitives")
   (~arithmetic-primitive 'plus  0              0 "+")
   (~arithmetic-primitive 'minus "arguments[0]" 1 "-")
   (~arithmetic-primitive 'mult  1              0 "*")
   (~function (~id 'zero?) '(v) (~return "v===0"))
   (~comment "CoreScheme Program")
   (~statement (Term M "result"))
   "console.log(result)"))

; emit : tree -> void
;   display the elements in the tree in order
(define (emit x)
  (cond
    [(or (number? x) (string? x)) (display x)]
    [(list? x)                    (for-each emit x)]
    [else
     (displayln x)
     (error 'emit "got ~a" x)]))

;;;
;;; RUN
;;;

; In order to run the generated JavaScript program the source
; needs to be saved to a file before a JavaScript engine
; can evaluate the program. Here the JavaScript implementation
; Node is used.

(define (run js-tree [delete-tmp? #t])
  (define tmp (make-temporary-file "tmp~a.js"))
  ; (displayln (path->string tmp))
  (with-output-to-file tmp
    (λ () (emit js-tree))
    #:exists 'replace)
  (begin0
    (node tmp)
    (when delete-tmp?
      (delete-file tmp))))

(define (node path)
  (with-output-to-string
      (λ ()
        (system (string-append "/usr/local/bin/node " " " (path->string path))))))

;;;
;;; EVAL
;;;

(define (eval s-exp)
  (run
   (generate-js
    (anormalize
     (α-rename
      (parse s-exp))))))

;;;
;;; EXAMPLES
;;;

;;; Factorial
;
; There is no define nor letrec in CoreScheme so the following
; recursive computation of factorial uses the Y-combinator.

;; Uncomment to see the generated code:
;; (you may have to adjust the path of the node executable)

#;(emit
   (generate-js
    (anormalize
     (α-rename
      (parse
       '(let (Y (λ (b)
                  ((λ (f) (b (λ (x) ((f f) x))))
                   (λ (f) (b (λ (x) ((f f) x)))))))
          (let (Fact (Y (λ (fact)
                          (λ (n)
                            (if (zero? n)
                                1
                                (* n (fact (- n 1))))))))
            (Fact 5))))))))

;; Uncomment to run the generated code and see the result.
;; 

#;(run
   (generate-js
    (anormalize
     (α-rename
      (parse
       '(let (Y (λ (b)
                  ((λ (f) (b (λ (x) ((f f) x))))
                   (λ (f) (b (λ (x) ((f f) x)))))))
          (let (Fact (Y (λ (fact)
                          (λ (n)
                            (if (zero? n)
                                1
                                (* n (fact (- n 1))))))))
            (Fact 5))))))))
