#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for detatils

(provide
  ;; auxiliary keywords for language/pass definitions
  extends definitions entry terminals nongenerative-id

  ;; predicates for looking for identifiers independent of context
  ellipsis? unquote? colon? arrow? plus? minus? double-arrow? 
    
  ;; things for dealing with syntax and idetnfieris
  all-unique-identifiers? gentemp bound-id-member? bound-id-union
  partition-syn datum 

  ;; things for dealing with language meta-variables
  meta-var->raw-meta-var combine unique-symbol 
    
  ;; convenience syntactic forms
  rec with-values define-who 
    
  ;; source information funtions
  syntax->source-info 

  ;;; stuff imported from implementation-helpers

  ;; formatting
  format printf pretty-print

  ;; library export stuff (needed for when used inside module to
  ;; auto-indirect export things)
  ; indirect-export

  ;; compile-time environment helpers
  ; make-compile-time-value

  ;; code organization helpers
  ; module

  ;; useful for warning items
  ; warningf errorf

  ;; debugging support
  trace-lambda trace-define-syntax trace-let trace-define
          
  ;; needed to know what code to generate
  optimize-level

  ;; the base record, so that we can use gensym syntax
  define-nanopass-record

  ;; failure token so that we can know when parsing fails with a gensym
  np-parse-fail-token

  ;; handy syntactic stuff
  with-implicit with-racket-quasiquote with-extended-quasiquote extended-quasiquote with-auto-unquote
  list-head)

(require racket/fixnum (for-syntax racket/fixnum))
(require syntax/srcloc)
(require (for-syntax syntax/stx))

(define list-head
  (lambda (ls n)
    (let loop ([ls ls] [n n])
      (if (= n 0)
          (list (car ls))
          (cons (car ls) (loop (cdr ls) (- n 1)))))))

(define-syntax datum
  (syntax-rules ()
    [(_ e) (syntax->datum #'e)]))

(define-syntax with-implicit
  (syntax-rules ()
    [(_ (tid id ...) body0 body1 ...)
     (with-syntax ([id (datum->syntax #'tid 'id)] ...)
        body0
        body1 ...)]))

(define-syntax with-racket-quasiquote
  (lambda (x)
    (syntax-case x ()
      [(k . body)
       (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)])
         #'(let-syntax ([quasiquote (syntax-rules () [(_ x) `x])]) . body))])))

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
              (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
                (cond
                  [(fx= level 0) #'(tmpl0 ... (unquote e))]
                  [(fx= level 1) #'(tmpl0 ... (unquote-splicing e))]
                  [else (let loop ([level level] [e #'e])
                          (if (fx= level 1)
                              #`(tmpl0 ... (unquote-splicing #,e))
                              (loop (fx- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... (unquote-splicing e))
              (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
                (cond
                  [(fx= level 0) #'(tmpl0 ... (unquote-splicing e))]
                  [else (let loop ([level level] [e #'e])
                          (if (fx= level 0)
                              #`(tmpl0 ... (unquote-splicing #,e))
                              (loop (fx- level 1) #`(apply append #,e))))]))]
            [(tmpl0 ... tmpl1 ellipsis)
              (eq? (syntax->datum #'ellipsis) '...)
              (loop #'(tmpl0 ... tmpl1) (fx+ level 1))]
            [(tmpl0 ... tmpl1)
              (with-syntax ([(tmpl0 ...) (rebuild-body #'(tmpl0 ...) (fx- orig-level 1))])
                (let-values ([(tmpl1 t* e*) (gather-unquoted-exprs #'tmpl1)])
                  (when (null? e*)
                    (raise-syntax-error 'extended-quasiquote
                      "no variables found in ellipsis expression" body))
                  (let loop ([level level]
                              [e #`(map (lambda #,t*
                                          (extended-quasiquote
                                            #,tmpl1))
                                     . #,e*)])
                    (if (fx= level 1)
                        #`(tmpl0 ... (unquote-splicing #,e))
                        (loop (fx- level 1) #`(apply append #,e))))))]))))
    (define rebuild-body
      (lambda (body level)
        (syntax-case body (unquote unquote-splicing)
          [(unquote e) #'(unquote e)]
          [(unquote-splicing e) #'(unquote-splicing e)]
          [(tmpl0 ... tmpl1 ellipsis)
           (eq? (syntax->datum #'ellipsis) '...)
           (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (fx+ level 1))])
             #'(tmpl0 ...))]
          [(tmpl0 ... tmpl1 ellipsis . tmpl2)
            (eq? (syntax->datum #'ellipsis) '...)
            (with-syntax ([(tmpl0 ...) (build-list #'(tmpl0 ... tmpl1) (fx+ level 1))]
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
         #'(let-syntax ([quasiquote (syntax-rules ()
                                      [(_ x) (extended-quasiquote x)])])

             . body))])))

(define-syntax with-auto-unquote
  (lambda (x)
    (syntax-case x ()
      [(k (x* ...) . body)
       (with-syntax ([quasiquote (datum->syntax #'k 'quasiquote)])
         #'(let-syntax ([quasiquote
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

(define all-unique-identifiers?
  (lambda (ls)
    (and (andmap identifier? ls)
         (let f ([ls ls])
           (if (null? ls)
               #t
               (let ([id (car ls)] [ls (cdr ls)])
                 (and (not (memf (lambda (x) (free-identifier=? x id)) ls))
                      (f ls))))))))

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
    (and (identifier? x) (free-identifier=? x (syntax (... ...)))))) 

(define unquote?
  (lambda (x)
    (and (identifier? x) (free-identifier=? x (syntax unquote)))))

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
      (let f ([i (fx- (string-length s) 1)])
        (cond
          [(fx= i -1) sym]
          [(or (char=? #\* (string-ref s i))
               (char=? #\^ (string-ref s i))
               (char=? #\? (string-ref s i)))
           (f (fx- i 1))]
          [else (let f ([i i])
                  (cond
                    [(fx= i -1) sym]
                    [(char-numeric? (string-ref s i)) (f (fx- i 1))]
                    [else (string->symbol (substring s 0 (fx+ i 1)))]))])))))

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
  
(define gentemp
  (lambda ()
    (car (generate-temporaries '(#'t))))) 
  
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

(define-syntax define-nanopass-record
  (lambda (x)
    (syntax-case x ()
      [(k) (with-syntax ([nanopass-record (datum->syntax #'k 'nanopass-record)]
                         [nanopass-record? (datum->syntax #'k 'nanopass-record?)]
                         [nanopass-record-tag (datum->syntax #'k 'nanopass-record-tag)])
             #'(define-struct nanopass-record (tag) #:prefab))])))

(define-who optimize-level
  (make-parameter 2
    (lambda (n)
      (unless (and (integer? n) (<= 0 n 3))
        (error who "invalid optimization level" n))
      n)))

(define-who trace-current-depth
  (make-parameter 0
    (lambda (n)
      (unless (and (exact-integer? n) (>= n 0))
        (error who "expected non-negative integer" n))
      n)))

(define-who trace-indent-depth
  (make-parameter 10
    (lambda (n)
      (unless (and (exact-integer? n) (> n 0))
        (error who "expected positive integer" n))
      n)))

(define build-trace-prefix-helper
  (lambda (n)
    (for/fold ([s ""]) ([i n]) (string-append s (if (even? i) "|" " ")))))

(define build-trace-prefix
  (lambda ()
    (if (>= (trace-current-depth) (trace-indent-depth))
        (let* ([str (format "[~s]" (trace-current-depth))])
          (string-append
            (build-trace-prefix-helper
              (- (trace-indent-depth) (string-length str)))
            str))
        (build-trace-prefix-helper (trace-current-depth)))))

(define print-trace-entry
  (lambda (x)
    (display (build-trace-prefix))
    (pretty-display x)))

(define print-trace-result
  (lambda (ls)
    (let* ([prefix (build-trace-prefix)]
           [blank-prefix (make-string (string-length prefix) #\space)])
      (let loop ([ls ls] [prefix prefix])
        (unless (null? ls)
          (display prefix)
          (pretty-display (car ls))
          (loop (cdr ls) blank-prefix))))))

(define simple-trace
  (lambda (name f)
    (lambda args
      (parameterize ([trace-current-depth (fx+ (trace-current-depth) 1)])
        (print-trace-entry (cons name args))
        (call-with-values
          (lambda () (apply f args))
          (lambda results
            (print-trace-result results)
            (apply values results)))))))

(define-syntax trace-lambda
  (syntax-rules ()
    [(_ name args body0 body1 ...)
     (simple-trace 'name (lambda args body0 body1 ...))]))

(define-syntax trace-let
  (syntax-rules ()
    [(_ name ([x e] ...) body0 body1 ...)
     ((simple-trace 'name (lambda (x ...) body0 body1 ...)) e ...)]))

(define-syntax trace-define
  (syntax-rules ()
    [(_ (name x ... . r) body0 body1 ...)
     (define name (simple-trace 'name (lambda (x ... . r) body0 body1 ...)))]
    [(_ name e) (define name (simple-trace 'name e))]))

(define-syntax trace-define-syntax
  (syntax-rules ()
    [(_ name transformer)
     (define-syntax name
       (lambda (x)
         (let ([real-result #f])
           (trace-let name ([dummy-x (syntax->datum x)])
             (set! real-result (transformer x))
             (syntax->datum real-result))
           real-result)))]))
