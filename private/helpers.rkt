#lang racket/base
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(require racket/contract/base)
(provide
  ;; hack
  maybe-syntax->datum

  ;; better error reporting for missing language definitions
  lookup-language

  ;; auxiliary keywords for language/pass definitions
  extends definitions entry terminals nongenerative-id

  ;; predicates for looking for identifiers independent of context
  (contract-out
    [ellipsis? (-> any/c boolean?)]
    [unquote? (-> any/c boolean?)]
    [colon? (-> any/c boolean?)]
    [arrow? (-> any/c boolean?)]
    [plus? (-> any/c boolean?)]
    [minus? (-> any/c boolean?)]
    [double-arrow? (-> any/c boolean?)])
    
  ;; things for dealing with syntax and idetnfieris
  (contract-out
    [check-unique-identifiers (-> (or/c false/c string? symbol?)  string? syntax? (listof syntax?) void)]
    [bound-id-member? (-> identifier? (listof identifier?) boolean?)]
    [bound-id-union (-> (listof identifier?) (listof identifier?) (listof identifier?))])
  partition-syn datum

  ;; things for dealing with language meta-variables
  (contract-out
    [meta-var->raw-meta-var (-> symbol? symbol?)]
    [combine (-> list? any/c list?)]
    [unique-symbol (->* (identifier?) () #:rest (listof identifier?) symbol?)])
    
  ;; convenience syntactic forms
  rec with-values define-who 
    
  ;; source information funtions
  (contract-out [syntax->source-info (-> syntax? string?)])

  ;;; stuff imported from implementation-helpers

  ;; formatting
  format printf pretty-print

  ;; needed to know what code to generate
  (contract-out
    [optimize-level (case->
                      (-> (integer-in 0 3))
                      (-> (integer-in 0 3) any))])

  ;; the base record, so that we can use gensym syntax
  (struct-out nanopass-record)

  ;; failure token so that we can know when parsing fails with a gensym
  (contract-out [np-parse-fail-token (and/c symbol? (not/c symbol-interned?))])

  ;; handy syntactic stuff
  with-racket-quasiquote with-extended-quasiquote extended-quasiquote with-auto-unquote
  (contract-out [list-head (-> list? exact-nonnegative-integer? list?)]))

(require (for-syntax syntax/stx
                     racket/base)
         syntax/srcloc
         racket/splicing
         racket/pretty)

(define maybe-syntax->datum
  (lambda (x)
    (if (syntax? x)
        (syntax->datum x)
        x)))

(define list-head
  (lambda (ls n)
    (let loop ([ls ls] [n n])
      (if (= n 0)
          '()
          (cons (car ls) (loop (cdr ls) (- n 1)))))))

(define-syntax datum
  (syntax-rules ()
    [(_ e) (syntax->datum #'e)]))

(define-syntax with-racket-quasiquote
  (lambda (x)
    (syntax-case x ()
      [(k . body)
       (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)])
         #'(splicing-let-syntax ([quasiquote (syntax-rules () [(_ x) `x])]) . body))])))

(define-syntax extended-quasiquote
  (lambda (x)
    (define gather-unquoted-exprs
      (lambda (body)
        (let f ([body body] [t* '()] [e* '()])
          (syntax-case body (unquote unquote-splicing)
            [(unquote x)
             (identifier? #'x)
             (values body (cons #'x t*) (cons #'x e*))]
            [(unquote-splicing x)
             (identifier? #'x)
             (values body (cons #'x t*) (cons #'x e*))]
            [(unquote e)
             (with-syntax ([(t) (generate-temporaries '(t))])
                (values #'(unquote t) (cons #'t t*) (cons #'e e*)))]
            [(unquote-splicing e)
             (with-syntax ([(t) (generate-temporaries '(t))])
               (values #'(unquote-splicing t) (cons #'t t*) (cons #'e e*)))]
            [(tmpl0 . tmpl1)
             (let-values ([(tmpl0 t* e*) (f #'tmpl0 t* e*)])
               (let-values ([(tmpl1 t* e*) (f #'tmpl1 t* e*)])
                 (values #`(#,tmpl0 . #,tmpl1) t* e*)))]
            [atom (values #'atom t* e*)]))))
    (define build-list
      (lambda (body orig-level)
        (let loop ([body body] [level orig-level])
          (syntax-case body (unquote unquote-splicing)
            [(tmpl0 ... (unquote e))
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (- orig-level 1))])
               (cond
                 [(= level 0) #'(tmpl0 ... (unquote e))]
                 [(= level 1) #'(tmpl0 ... (unquote-splicing e))]
                 [else (let loop ([level level] [e #'e])
                         (if (= level 1)
                             #`(tmpl0 ... (unquote-splicing #,e))
                             (loop (- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... (unquote-splicing e))
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (- orig-level 1))])
               (cond
                 [(= level 0) #'(tmpl0 ... (unquote-splicing e))]
                 [else (let loop ([level level] [e #'e])
                         (if (= level 0)
                             #`(tmpl0 ... (unquote-splicing #,e))
                             (loop (- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... tmpl1 ellipsis)
             (eq? (syntax->datum #'ellipsis) '...)
             (loop #'(tmpl0 ... tmpl1) (+ level 1))]
            [(tmpl0 ... tmpl1)
             (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (- orig-level 1))])
               (let-values ([(tmpl1 t* e*) (gather-unquoted-exprs #'tmpl1)])
                 (when (null? e*)
                   (raise-syntax-error 'extended-quasiquote
                     "no variables found in ellipsis expression" body))
                 (let loop ([level level]
                             [e #`(map (lambda #,t*
                                         (extended-quasiquote
                                           #,tmpl1))
                                    . #,e*)])
                   (if (= level 1)
                       #`(tmpl0 ... (unquote-splicing #,e))
                       (loop (- level 1) #`(apply append #,e))))))]))))
    (define rebuild-body
      (lambda (body level)
        (syntax-case body (unquote unquote-splicing)
          [(unquote e) #'(unquote e)]
          [(unquote-splicing e) #'(unquote-splicing e)]
          [(tmpl0 ... tmpl1 ellipsis)
           (eq? (syntax->datum #'ellipsis) '...)
           (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (+ level 1))])
             #'(tmpl0 ...))]
          [(tmpl0 ... tmpl1 ellipsis . tmpl2)
            (eq? (syntax->datum #'ellipsis) '...)
            (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (+ level 1))]
                           [tmpl2 (rebuild-body #'tmpl2 level)])
              #'(tmpl0 ... . tmpl2))]
          [(tmpl0 ... tmpl1)
            (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) level)]
                           [tmpl1 (rebuild-body #'tmpl1 level)])
              #'(tmpl0 ... tmpl1))]
          [(tmpl0 ... tmpl1 . tmpl2)
            (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ... tmpl1) level)]
                           [tmpl2 (rebuild-body #'tmpl2 level)])
              #'(tmpl0 ... . tmpl2))]
          [other #'other])))
    (syntax-case x ()
      [(k body)
       (with-syntax ([body (rebuild-body #'body 0)])
         #'(quasiquote body))])))

(define-syntax with-extended-quasiquote
  (lambda (x)
    (syntax-case x ()
      [(k . body)
       (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)])
         #'(splicing-let-syntax ([quasiquote (syntax-rules ()
                                      [(_ x) (extended-quasiquote x)])])

             . body))])))

(define-syntax with-auto-unquote
  (lambda (x)
    (syntax-case x ()
      [(k (x* ...) . body)
       (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)])
         #'(splicing-let-syntax ([quasiquote
                          (lambda (x)
                            (define replace-vars
                              (let ([vars (list #'x* ...)])
                                (lambda (b)
                                  (let f ([b b])
                                    (syntax-case b ()
                                      [id (identifier? #'id)
                                       (if (memf (lambda (var) (free-identifier=? var #'id)) vars)
                                           #'(unquote id)
                                           #'id)]
                                      [(a . d) (with-syntax ([a (f #'a)] [d (f #'d)]) #'(a . d))]
                                      [atom #'atom])))))
                            (syntax-case x ()
                              [(_ b)
                               (with-syntax ([b (replace-vars #'b)])
                                 #'`b)]))])
             . body))])))

(define check-unique-identifiers
  (lambda (who msg expr ls)
    (let f ([ls ls])
      (unless (null? ls)
        (let ([id (car ls)] [ls (cdr ls)])
          (cond
            [(memf (lambda (x) (free-identifier=? x id)) ls) =>
             (lambda (rest)
               (raise-syntax-error who msg expr (car rest)))]
            [else (f ls)]))))))

(define-syntax with-values
  (syntax-rules ()
    [(_ p c) (call-with-values (lambda () p) c)]))

(define-syntax rec
  (syntax-rules ()
    [(_ name proc) (letrec ([name proc]) name)]
    [(_ (name . arg) body body* ...)
     (letrec ([name (lambda arg body body* ...)]) name)]))

(define-syntax define-auxiliary-keyword
  (syntax-rules ()
    [(_ name)
     (define-syntax name 
       (lambda (x)
         (raise-syntax-error 'name "misplaced use of auxiliary keyword" x)))]))
  
(define-syntax define-auxiliary-keywords
  (syntax-rules ()
    [(_ name* ...)
     (begin
       (define-auxiliary-keyword name*) ...)]))

(define-auxiliary-keywords extends definitions entry terminals nongenerative-id)

(define-syntax define-who
  (lambda (x)
    (syntax-case x ()
      [(k name expr)
       (with-syntax ([who (datum->syntax #'k 'who)])
         #'(define name (let () (define who 'name) expr)))]
      [(k (name . fmls) expr exprs ...)
       #'(define-who name (lambda (fmls) expr exprs ...))])))

;;; moved from meta-syntax-dispatch.ss and nano-syntax-dispatch.ss
(define combine
  (lambda (r* r)
    (if (null? (car r*))
        r
        (cons (map car r*) (combine (map cdr r*) r))))) 
  
;;; moved from meta-syntax-dispatch.ss and syntaxconvert.ss
(define ellipsis?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x (syntax (... ...)))
             (eq? (syntax->datum x) '...))))) 

(define unquote?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x (syntax unquote))
             (eq? (syntax->datum x) 'unquote)))))

(define unquote-splicing?
  (lambda (x)
    (and (identifier? x) (free-identifier=? x (syntax unquote-splicing)))))

(define plus?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'+)
             (eq? (syntax->datum x) '+)))))

(define minus?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'-)
             (eq? (syntax->datum x) '-)))))

(define double-arrow?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'=>)
             (eq? (syntax->datum x) '=>)))))

(define colon?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #':)
             (eq? (syntax->datum x) ':)))))

(define arrow?
  (lambda (x)
    (and (identifier? x)
         (or (free-identifier=? x #'->)
             (eq? (syntax->datum x) '->)))))

;;; unique-symbol produces a unique name derived the input name by
;;; adding a unique suffix of the form .<digit>+.  creating a unique
;;; name from a unique name has the effect of replacing the old
;;; unique suffix with a new one.
  
(define unique-suffix
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (number->string count))))
  
(define unique-symbol
  (lambda (id . id*)
    (string->symbol
      (string-append
        (foldr
          (lambda (id str) (string-append str ":" (symbol->string (syntax->datum id))))
          (symbol->string (syntax->datum id)) id*)
        "."
        (unique-suffix)))))

; TODO: at some point we may want this to be a little bit more
; sophisticated, or we may want to have something like a regular
; expression style engine where we bail as soon as we can identify
; what the meta-var corresponds to.
(define meta-var->raw-meta-var
  (lambda (sym)
    (let ([s (symbol->string sym)])
      (let f ([i (- (string-length s) 1)])
        (cond
          [(= i -1) sym]
          [(or (char=? #\* (string-ref s i))
               (char=? #\^ (string-ref s i))
               (char=? #\? (string-ref s i)))
           (f (- i 1))]
          [else (let f ([i i])
                  (cond
                    [(= i -1) sym]
                    [(char-numeric? (string-ref s i)) (f (- i 1))]
                    [else (string->symbol (substring s 0 (+ i 1)))]))])))))

(define-syntax partition-syn
  (lambda (x)
    (syntax-case x ()
      [(_ ls-expr () e0 e1 ...) #'(begin ls-expr e0 e1 ...)]
      [(_ ls-expr ([set pred] ...) e0 e1 ...)
       (with-syntax ([(pred ...) 
                      (let f ([preds #'(pred ...)])
                        (if (stx-null? (stx-cdr preds))
                            (if (free-identifier=? (stx-car preds) #'otherwise)
                                (list #'(lambda (x) #t))
                                preds)
                            (cons (stx-car preds) (f (stx-cdr preds)))))])
         #'(let-values ([(set ...)
                          (let f ([ls ls-expr])
                            (if (null? ls)
                                (let ([set '()] ...) (values set ...))
                                (let-values ([(set ...) (f (cdr ls))])
                                  (cond
                                    [(pred (car ls))
                                      (let ([set (cons (car ls) set)])
                                        (values set ...))]
                                    ...
                                    [else (error 'partition-syn 
                                            "no home for ~s"
                                            (car ls))]))))])
             e0 e1 ...))])))
  
(define bound-id-member? 
  (lambda (id id*)
    (and (not (null? id*))
         (or (bound-identifier=? id (car id*))
             (bound-id-member? id (cdr id*)))))) 
  
(define bound-id-union ; seems to be unneeded
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(bound-id-member? (car ls1) ls2) (bound-id-union (cdr ls1) ls2)]
      [else (cons (car ls1) (bound-id-union (cdr ls1) ls2))]))) 
  
(define syntax->source-info
  (lambda (stx)
    (and (source-location-known? stx)
         (source-location->string stx))))

(define np-parse-fail-token (gensym "np-parse-fail-token"))

(define-struct nanopass-record (tag) #:prefab)

(define-who optimize-level
  (make-parameter 2
    (lambda (n)
      (unless (and (integer? n) (<= 0 n 3))
        (error who "invalid optimization level ~s" n))
      n)))

(define lookup-language
  (lambda (who msg lang)
    (call-with-exception-handler
      (lambda (c)
        ;; hack: really, I'd like to just call raise-syntax-error, but that
        ;; causes the error to bubble up in a weird way, since it's
        ;; continuation marks show it as inside the exception handler.
        (exn:fail:syntax
          (if (source-location? lang)
              (format "~a: ~s: ~a\n\tat: ~s"
                (source-location->string lang)
                who msg (syntax->datum lang))
              (format "~s: ~a\n\tat: ~s" who msg (syntax->datum lang)))
          (if (exn? c)
              (exn-continuation-marks c)
              (current-continuation-marks))
          (list lang)))
      (lambda () (syntax-local-value lang)))))
