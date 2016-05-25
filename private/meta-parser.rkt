#lang racket/base
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell,
;;; Leif Andersen
;;; See the accompanying file Copyright for details

(provide make-meta-parser rhs-in-context-quasiquote meta-parse-term
         make-quasiquote-transformer make-in-context-transformer
         output-records->syntax parse-cata)

(require racket/syntax
         syntax/stx
         syntax/parse
         (for-template racket/splicing)
         "helpers.rkt"
         "records.rkt"
         "syntaxconvert.rkt"
         "meta-syntax-dispatch.rkt")

(require (for-template racket))

(define (make-meta-parser desc)
  (define (escape-pattern x)
    (syntax-case x (...)
      [... #'(... (... ...))]
      [(a . d) (with-syntax ([a (escape-pattern #'a)]
                             [d (escape-pattern #'d)])
                 #'(a . d))]
      [() #'()]
      [id  (identifier? #'id) #'id]))
  (define ntspec-meta-parsers (make-hasheq))
  (define (make-meta-parse-proc desc tspecs ntspecs ntspec lang-name cata?)
    (define (parse-field m level maybe?)
      (cond
        [(meta-name->tspec m tspecs)
         => (lambda (spec)
              (let f ([level level] [x m])
                (if (= level 0)
                    #`(meta-parse-term '#,(tspec-type spec) #,x #,cata? #,maybe?)
                    #`(map (lambda (x)
                             (if (nano-dots? x)
                                 (make-nano-dots #,(f (- level 1)
                                                      #'(nano-dots-x x)))
                                 #,(f (- level 1) #'x)))
                           #,x))))]
        [(meta-name->ntspec m ntspecs)
         => (lambda (spec)
              (with-syntax ([proc-name (hash-ref ntspec-meta-parsers spec #f)])
                (let f ([level level] [x m])
                  (if (= level 0)
                      #`(proc-name #,x #t #t #,maybe?)
                      #`(map (lambda (x)
                               (if (nano-dots? x)
                                   (make-nano-dots #,(f (- level 1)
                                                        #'(nano-dots-x x)))
                                   #,(f (- level 1) #'x)))
                             #,x)))))]
        [else (raise-syntax-error 'meta-parser "unrecognized meta variable"
                                  (language-name desc) m)]))
    (define make-term-clause
      (lambda (x)
        (lambda (alt)
          #`[(memq (meta-var->raw-meta-var (syntax->datum #,x))
                   (quote #,(tspec-meta-vars (terminal-alt-tspec alt (language-tspecs desc)))))
             (make-nano-meta
              (make-terminal-alt
               #'#,(escape-pattern (alt-syn alt))
               #,(let ([t (alt-pretty alt)])
                   (and t
                        (if (alt-pretty-procedure? alt)
                            #`#'#,(alt-pretty alt)
                            #`#'#,(escape-pattern (alt-pretty alt)))))
               #,(alt-pretty-procedure? alt)
               '#,(terminal-alt-type alt))
              (list (make-nano-unquote #,x)))])))
    (define make-nonterm-unquote
      (lambda (x)
        (lambda (alt)
          #`[(memq (meta-var->raw-meta-var (syntax->datum #,x))
                   (quote #,(ntspec-meta-vars (nonterminal-alt-ntspec alt (language-ntspecs desc)))))
             (make-nano-meta
              (make-nonterminal-alt
               #'#,(escape-pattern (alt-syn alt))
               #,(let ([t (alt-pretty alt)])
                   (and t 
                        (if (alt-pretty-procedure? alt)
                            #`#'#,(alt-pretty alt)
                            #`#'#,(escape-pattern (alt-pretty alt)))))
               #,(alt-pretty-procedure? alt)
               #'#,(nonterminal-alt-name alt))
              (list (make-nano-unquote #,x)))])))
    (define ((make-nonterm-clause x maybe?) alt)
      (let ([spec (meta-name->ntspec (alt-syn alt) ntspecs)])
        (unless spec
          (raise-syntax-error 'meta-parser "unrecognized meta variable"
                              (language-name desc) (alt-syn alt)))
        (with-syntax ([proc-name (hash-ref ntspec-meta-parsers spec)])
          #`(proc-name #,x #f nested? #,maybe?))))
    
    (define ((make-pair-clause stx first-stx rest-stx) alt)
      (make-list-clause stx #f first-stx rest-stx alt))
    (define ((make-null-clause stx) alt)
      (make-list-clause stx #t #f #f alt))

    ; Produce a case statement for matching on a list.
    ;  empty-pair is #t if we are matching on a pair, #f if null.
    ;  Primarily for use in make-pair-clause and make-null-clause
    ; Syntax Boolean (Maybe Syntax) (Maybe Syntax) Syntax -> Syntax
    (define (make-list-clause stx empty-pair? first-stx rest-stx alt)
      (let ([field-pats (pair-alt-pattern alt)])
        (with-syntax ([(field-var ...) (pair-alt-field-names alt)])
          (with-syntax ([(parsed-field ...)
                         (map parse-field
                              (syntax->list #'(field-var ...))
                              (pair-alt-field-levels alt)
                              (pair-alt-field-maybes alt))])
            #`(#,(cond
                   [(pair-alt-implicit? alt)
                    #`(meta-syntax-dispatch #,stx '#,(datum->syntax #'lang-name field-pats))]
                   [empty-pair? #f]
                   [else #`(and (eq? (syntax->datum #,first-stx)
                                     '#,(stx-car (alt-syn alt)))
                                (meta-syntax-dispatch #,rest-stx
                                                      '#,(datum->syntax #'lang-name field-pats)))])
               => (lambda (ls)
                    (cond
                      [(and (= (length ls) 2)
                            (null? (car ls))
                            (syntax? (cadr ls))
                            (syntax-parse (cadr ls)
                              [() #t]
                              [_ #f]))
                       (make-nano-quote (syntax/loc stx '()))]
                      [else
                       (apply
                        (lambda (field-var ...)
                          (make-nano-meta
                           ($make-pair-alt
                            #'#,(escape-pattern (alt-syn alt))
                            #,(let ([t (alt-pretty alt)])
                                (and t 
                                     (if (alt-pretty-procedure? alt)
                                         #`#'#,(alt-pretty alt)
                                         #`#'#,(escape-pattern (alt-pretty alt)))))
                            #,(alt-pretty-procedure? alt)
                            '#,(pair-alt-pattern alt)
                            #,(let ([t (pair-alt-field-names alt)])
                                (and t
                                     #`(list #,@(map (lambda (id) #`#'#,id) t))))
                            '#,(pair-alt-field-levels alt)
                            '#,(pair-alt-field-maybes alt)
                            #,(pair-alt-implicit? alt)
                            #,(pair-alt-tag alt)
                            #'#,(pair-alt-pred alt)
                            #'#,(pair-alt-maker alt)
                            #,(let ([t (pair-alt-accessors alt)])
                                (and t
                                     #`(list #,@(map (lambda (id) #`#'#,id) t))))
                            #'#,(pair-alt-name alt))
                           (list parsed-field ...)))
                       ls)])))))))
    (define (separate-syn ls)
      (let f ([ls ls])
        (cond
          [(null? ls) (values '() '() '() '() '())]
          [else
           (define v (car ls))
           (define-values (pair* pair-imp* term* imp* nonimp*) (f (cdr ls)))
           (if (nonterminal-alt? v)
               (if (has-implicit-alt? (nonterminal-alt-ntspec v (language-ntspecs desc))
                                      (language-ntspecs desc))
                   (values pair* pair-imp* term* `(,v . ,imp*) nonimp*)
                   (values pair* pair-imp* term* imp* `(,v . ,nonimp*)))
               (if (terminal-alt? v)
                   (values pair* pair-imp* `(,v . ,term*) imp* nonimp*)
                   (if (pair-alt-implicit? v)
                       (values pair* `(,v . ,pair-imp*) term* imp* nonimp*)
                       (values `(,v . ,pair*) pair-imp* term* imp* nonimp*))))])))
    (define-values (pair-alt* pair-imp-alt* term-alt* nonterm-imp-alt* nonterm-nonimp-alt*)
      (separate-syn (ntspec-alts ntspec)))
    #;(pretty-print
     (list 'inf pair-alt* pair-imp-alt* term-alt* nonterm-imp-alt* nonterm-nonimp-alt*))
    #`(lambda (stx error? nested? maybe?)
        (or (syntax-case stx ()
              [(unquote id)
               (and (unquote? #'unquote) (identifier? #'id))
               (if nested?
                   (make-nano-unquote #'id)
                   (cond
                     #,@(map (make-term-clause #'#'id) term-alt*)
                     ; TODO: right now we can match the meta for this item, but we
                     ; cannot generate the needed nano-meta because we have no
                     ; alt record to put into it.  (perhaps the current model is
                     ; just pushed as far as it can be right now, and we need to
                     ; rework it.)
                     #,@(map (make-nonterm-unquote #'#'id) nonterm-imp-alt*)
                     #,@(map (make-nonterm-unquote #'#'id) nonterm-nonimp-alt*)
                     [else #f]))]
              [(unquote x)
               (unquote? #'unquote)
               (if nested?
                   (if #,cata?
                       (parse-cata #'x '#,(ntspec-name ntspec) maybe?)
                       (make-nano-unquote #'x))
                   (raise-syntax-error #f "cata unsupported at top-level of pattern" stx))]
              [_ #f])
            #,@(map (make-nonterm-clause #'stx #'maybe?) nonterm-nonimp-alt*)
            (syntax-case stx ()
              [(a . d)
               (cond
                 #,@(map (make-pair-clause #'stx #'#'a #'#'d) pair-alt*)
                 #,@(map (make-pair-clause #'stx #'#'a #'#'d) pair-imp-alt*)
                 [else #f])]
              [()
               (cond
                 #,@(map (make-null-clause #'stx) pair-alt*)
                 #,@(map (make-null-clause #'stx) pair-imp-alt*)
                 [else (make-nano-quote (syntax/loc stx '()))])]
              ; if we find something here that is not a pair, assume it should
              ; be treated as a quoted constant, and will be checked appropriately
              ; by the run-time constructor check
              [atom (make-nano-quote (syntax/loc stx 'atom))])
            #,@(map (make-nonterm-clause #'stx #'maybe?) nonterm-imp-alt*)
            (and error?
                 (raise-syntax-error #f "invalid pattern or template" stx)))))

  (let ([lang-name (language-name desc)]
        [ntspec* (language-ntspecs desc)]
        [tspec* (language-tspecs desc)])
    (with-syntax ([cata? (generate-temporary "cata?")]
                  [(ntspec-id ...) (map ntspec-name ntspec*)]
                  [(parse-name ...)
                   (map (lambda (ntspec)
                          (let ([n (format-id lang-name "meta-parse-~a" (ntspec-name ntspec))])
                            (hash-set! ntspec-meta-parsers ntspec n)
                            n))
                        ntspec*)])
      (with-syntax ([(parse-proc ...)
                     (map (lambda (ntspec)
                            (make-meta-parse-proc desc tspec* ntspec*
                                                  ntspec lang-name #'cata?))
                          ntspec*)]
                    [name (format-id lang-name "meta-parse-~a" lang-name)])
        (quasisyntax/loc lang-name
          (lambda (ntspec-name stx input?)
            (let ([cata? input?])
              (define parse-name parse-proc) ...
              (case ntspec-name
                [(ntspec-id) (parse-name stx #t (not input?) #f)] ...
                [else (error '#,lang-name
                             "unrecognized nonterminal passed to meta parser ~s"
                             ntspec-name)]))))))))


;; used to handle output of meta-parsers
(define (meta-parse-term tname stx* cata? maybe?)
  (syntax-case stx* ()
    [(unquote x)
     (unquote? #'unquote)
     (if (and cata? (not (identifier? #'x)))
         (parse-cata (syntax/loc stx* x) tname maybe?)
         (make-nano-unquote (syntax/loc stx* x)))]
    [(a . d)
     (raise-syntax-error 'meta-parse-term
                         "invalid pattern or template" stx*)]
    [stx
     ; treat everything else we find as ,'foo because if we don't
     ; `(primapp void) is interpreted as:
     ; `(primapp #<procedure void>)
     ; instead we want it to treat it as:
     ; `(primapp ,'void)
     ; which is how it would have to be written without this.
     ; Note that we don't care what literal expression we find here
     ; because at runtime it will be checked like every other element
     ; used to construct the output record, and anything invalid will
     ; be caught then. (If we check earlier, then it forces us to use
     ; the terminal predicates at compile-time, which means that can't
     ; be in the same library, and that is a bummer for other reasons,
     ; so better to be flexible and let something invalid go through
     ; here to be caught later.)
     (make-nano-quote (syntax/loc stx* 'stx))]))

;; used in the input meta parser to parse cata syntax
;; TODO: support for multiple input terms.
(define parse-cata
  ; should be more picky if nonterminal is specified--see 10/08/2007 NOTES
  (lambda (x itype maybe?)
    (define (serror) (raise-syntax-error 'define-pass "invalid cata syntax" x))
    (define (s0 stuff)
      (syntax-parse stuff
        #:datum-literals (: ->)
        [(: . stuff) (s2 #f #'stuff)]
        [(-> . stuff) (s4 #f #f '() #'stuff)]
        [(e . stuff) (s1 #'e #'stuff)]
        [() (make-nano-cata itype x #f #f '() maybe?)]
        [_ (serror)]))
    (define (s1 e stuff)
      (syntax-parse stuff
        #:datum-literals (: ->)
        [(: . stuff) (s2 e #'stuff)]
        [(-> . stuff)
         #:when (identifier? e)
         (s4 #f (list e) '() #'stuff)]
        [(expr . stuff)
         ; it is pre-mature to check for identifier here since these could be input exprs
         #;(and (identifier? #'id) (identifier? e))
         #:when (identifier? e)
         (s3 #f (list #'expr e) #'stuff)]
        [() #:when (identifier? e) (make-nano-cata itype x #f #f (list e) maybe?)]
        [_ (serror)]))
    (define (s2 f stuff)
      (syntax-parse stuff
        #:datum-literals (->)
        [(-> . stuff)
         (s4 f #f '() #'stuff)]
        [(id:id . stuff)
         (s3 f (list #'id) #'stuff)]
        [_ (serror)]))
    (define (s3 f e* stuff)
      (syntax-parse stuff
        #:datum-literals (->)
        [(-> . stuff)
         (s4 f (reverse e*) '() #'stuff)]
        [(e . stuff)
         ; this check is premature, since these could be input expressions
         #;(identifier? #'id)
         (s3 f (cons #'e e*) #'stuff)]
        [()
         ; now we want to check if these are identifiers, because they are our return ids
         #:when (andmap identifier? e*)
         (make-nano-cata itype x f #f (reverse e*) maybe?)]
        [_ (serror)]))
    (define (s4 f maybe-inid* routid* stuff)
      (syntax-case stuff ()
        [(id . stuff)
         (identifier? #'id)
         (s4 f maybe-inid* (cons #'id routid*) #'stuff)]
        [() (make-nano-cata itype x f maybe-inid* (reverse routid*) maybe?)]
        [_ (serror)]))
    (syntax-case x ()
      [(stuff ...) (s0 #'(stuff ...))])))

;; used in the output of the input metaparser and in the output of
;; define-pass
(define (rhs-in-context-quasiquote id type omrec ometa-parser body)
  (if type
      (with-syntax ([quasiquote #;(syntax-local-introduce (datum->syntax id 'quasiquote))
                                (datum->syntax id 'quasiquote)]
                    [in-context (datum->syntax id 'in-context)])
        (quasisyntax/loc id
          (splicing-let-syntax ([quasiquote
                                 '#,(make-quasiquote-transformer id type omrec ometa-parser)]
                                [in-context
                                 '#,(make-in-context-transformer id omrec ometa-parser)])
            #,body)))
      (with-syntax ([in-context (datum->syntax id 'in-context)])
        (quasisyntax/loc id
          (splicing-let-syntax ([in-context
                                 '#,(make-in-context-transformer id omrec ometa-parser)])
            #,body)))))

;; Done to do allow a programmer to specify what the context for
;; their quasiquote is, incase it is different from the current
;; expression.
;; bug fix #8 (not sure what this refers to)
(define ((make-in-context-transformer pass-name omrec ometa-parser) x)
  (syntax-parse x
    [(k ntname stuff ...)
     #:with quasiquote (datum->syntax (syntax/loc x k) 'quasiquote)
     (quasisyntax/loc x
       (splicing-let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                             (quasisyntax/loc x k) #'ntname
                                             omrec ometa-parser)])
         stuff ...))]))

;; Used to make quasiquote transformers in the in-context transformer
;; and in the normal right hand side transformer in do-define-pass and
;; make-rhs
(define ((make-quasiquote-transformer pass-name ntname omrec ometa-parser) x)
  (syntax-parse x
    [(_ stuff)
     (with-handlers ([exn:fail:syntax?
                      (lambda (x) (raise x))]
                     [exn:fail?
                      (lambda (x) (raise-syntax-error
                                   #f
                                   (format "unrecognized nonterminal in language ~a"
                                           (syntax->datum (language-name omrec)))
                                   pass-name
                                   ntname))])
       ;(printf "stuff: ~a~n~n" #'stuff)
       (output-records->syntax pass-name ntname omrec ometa-parser
                               (ometa-parser (syntax->datum ntname) (syntax/loc x stuff) #f)))
     #;(let ([stx #f])
         (trace-let quasiquote-transformer ([t (syntax->datum #'stuff)])
                    (let ([t0 (ometa-parser (syntax->datum ntname) #'stuff #f)])
                      ;(printf "ometa-parser result: ~s\n" t0)
                      (let ([t (output-records->syntax pass-name ntname omrec ometa-parser
                                                       t0)])
                        (set! stx t)
                        (syntax->datum t))))
         stx)]))

;; helper function used by the output metaparser in the meta-parsing
;; two step
;; TODO:
;; - defeated (for now) at getting rid of the unnecessary bindings.  still convinced this is possible
;;       and to be fixed.
;; - we are using bound-id-union to append lists of variables that are unique by construction
;;       (unless I am misreading the code) this is pointless.
;; - we are mapping over the field-names to find the specs for the fields. this seems waistful in a
;;       small way (building an unnecessary list) and a big way (lookup something that could be
;;       cached)
;; - we are always building the checking version of the pair-alt constructor here, but could probably
;;       be avoiding that.
(define (output-records->syntax pass-name ntname omrec ometa-parser rhs-rec)
  ;; TODO: add 'near' version when passed a list that might have syntax in it.
  (define (id->msg id)
    (cond
      [(= (optimize-level) 3) #f]
      [(and (syntax? id) (syntax->source-info id))
       => (lambda (si) (format "expression ~s ~a" (syntax->datum id) si))]
      [(syntax? id)(format "expression ~s" (syntax->datum id))]
      [else (format "expression ~s" id)]))
  (define (process-nano-fields elt* spec* binding)
    (cond
      [(null? elt*) (values '() '() '() binding)]
      [else
       (define-values (elt elt-id elt-var* binding*)
         (process-nano-elt (car elt*) (car spec*) binding))
       (let-values ([(elt* elt*-id elt*-var* binding*)
                     (process-nano-fields (cdr elt*) (cdr spec*) binding*)])
         (values
          (cons elt elt*) (cons elt-id elt*-id) (bound-id-union elt-var* elt*-var*) binding*))]))
  (define (process-nano-dots orig-elt spec binding*)
    ; ought to check that each of var* are bound to proper lists
    ; and that they have the same lengths
    (let-values ([(elt id var* binding*) (process-nano-elt (nano-dots-x orig-elt) spec binding*)])
      (if (null? var*)
          ; TODO: store original syntax object in nano-dots record and use it here
          (raise-syntax-error (syntax->datum pass-name)
                              "no variables within ellipsis pattern"
                              (let f ([elt (nano-dots-x orig-elt)])
                                (cond
                                  [(nano-meta? elt) (map f (nano-meta-fields elt))]
                                  [(nano-quote? elt) (cadr (nano-quote-x elt))]
                                  [(nano-unquote? elt) (nano-unquote-x elt)]
                                  [(nano-cata? elt) (nano-cata-syntax elt)]
                                  [(list? elt) (map f elt)]
                                  [else elt])))
          (values
           (if (null? (cdr var*))
               (let ([t (car var*)])
                 (if (eq? t elt)
                     t
                     #`(map (lambda (#,t) #,elt) #,t)))
               #`(map (lambda #,var* #,elt) #,@var*))
           id var* binding*))))
  (define (process-nano-list elt* spec binding*)
    (let f ([elt* elt*] [binding* binding*])
      (if (null? elt*)
          (values #''() '() '() binding*)
          (let ([elt (car elt*)] [elt* (cdr elt*)])
            (if (nano-dots? elt)
                (if (null? elt*)
                    (process-nano-dots elt spec binding*)
                    (let-values ([(elt elt-id elt-var* binding*)
                                  (process-nano-dots elt spec binding*)])
                      (let-values ([(elt* elt*-id* elt*-var* binding*) (f elt* binding*)])
                        (values #`(append #,elt #,elt*)
                                (cons elt-id elt*-id*)
                                (bound-id-union elt-var* elt*-var*)
                                binding*))))
                (let-values ([(elt elt-id elt-var* binding*) (process-nano-elt elt spec binding*)])
                  (let-values ([(elt* elt*-id* elt*-var* binding*) (f elt* binding*)])
                    (values #`(cons #,elt #,elt*)
                            (cons elt-id elt*-id*)
                            (bound-id-union elt-var* elt*-var*)
                            binding*))))))))
  (define (process-nano-meta x binding*)
    (let ([prec-alt (nano-meta-alt x)])
      (let-values ([(field* id* var* binding*)
                    (process-nano-fields (nano-meta-fields x)
                                         (map (lambda (x) (find-spec x omrec))
                                              (pair-alt-field-names prec-alt))
                                         binding*)])
        (values
         #`(#,(pair-alt-maker prec-alt) '#,pass-name #,@field* #,@(map id->msg id*))
         #f var* binding*))))
  (define (process-nano-elt elt spec binding*)
    #|
    (displayln elt)
    (displayln spec)
    (displayln binding*)
    (newline)
|#
    (cond
      [(nano-meta? elt)
       #;(assert (pair-alt? (nano-meta-alt elt)))
       (process-nano-meta elt binding*)]
      [(nano-quote? elt)
       (let ([x (nano-quote-x elt)]) (values x x '() binding*))]
      [(nano-unquote? elt)
       (let ([x (nano-unquote-x elt)])
         (with-syntax ([expr
                        (if (ntspec? spec)
                            ; TODO: when we eventually turn these processors into named entities
                            ; (either directly with meta define, define-syntax or some sort of
                            ; property, replace this with the appropriate call.  In the meantime this
                            ; should allow us to remove some of our in-contexts
                            (with-syntax ([quasiquote (datum->syntax pass-name 'quasiquote)])
                              #`(splicing-let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                                                      pass-name (spec-type spec)
                                                                      omrec ometa-parser)])
                                  #,x))
                            x)]
                       [tmp (car (generate-temporaries '(x)))])
           (values #'tmp x (list #'tmp) (cons #'(tmp expr) binding*))))]
      [(list? elt) (process-nano-list elt spec binding*)]
      [else (values elt elt '() binding*)]))
  (let-values ([(elt id var* binding*)
                (process-nano-elt
                 rhs-rec (nonterm-id->ntspec 'define-pass ntname (language-ntspecs omrec)) '())])
    #|
    (displayln "elt")
    (pretty-print rhs-rec)
    (pretty-print elt)
    (displayln id)
    (displayln var*)
    (displayln binding*)
    (newline)
|#
    (quasisyntax/loc pass-name
      (let #,binding* #,elt))))
