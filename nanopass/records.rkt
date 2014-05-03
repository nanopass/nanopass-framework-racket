#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for detatils

(provide
  nonterm-id->ntspec define-nanopass-record
  (contract-out
    [find-spec (-> identifier? language? (or/c tspec? ntspec?))]
    [nonterminal-meta? (-> identifier? (listof ntspec?) (or/c false/c (listof identifier?)))]
    [nano-alt->ntspec (-> alt? (listof ntspec?) any)]
    [nonterm-id->ntspec? (-> identifier? (listof ntspec?) (or/c ntspec? false/c))]
    [id->spec (-> identifier? language? (or/c tspec? ntspec? false/c))]
    [term-id->tspec? (-> identifier? (listof ntspec?) (or/c ntspec? false/c))]

    [meta-name->tspec (-> identifier? (listof tspec?) (or/c false/c tspec?))]
    [meta-name->ntspec (-> identifier? (listof ntspec?) (or/c false/c ntspec?))]

    [make-nano-dots (-> syntax? nano-dots?)]
    [nano-dots? (-> any/c boolean?)]
    [nano-dots-x (-> nano-dots? syntax?)]

    [make-nano-quote (-> syntax? nano-quote?)]
    [nano-quote? (-> any/c boolean?)]
    [nano-quote-x (-> nano-quote? syntax?)]

    [make-nano-unquote (-> syntax? nano-unquote?)]
    [nano-unquote? (-> any/c boolean?)]
    [nano-unquote-x (-> nano-unquote? syntax?)]

    [make-nano-meta (-> alt? (listof (or/c nano-dots? nano-quote? nano-unquote? nano-cata? nano-meta?)) nano-meta?)]
    [nano-meta? (-> any/c boolean?)]
    [nano-meta-alt (-> nano-meta? alt?)]
    [nano-meta-fields (-> nano-meta?
                          (listof
                            (or/c nano-dots? nano-quote?
                              nano-unquote? nano-cata?
                              nano-meta?)))]

    [make-nano-cata (-> identifier? syntax? 
                        (or/c false/c syntax?)
                        (or/c false/c (listof syntax?))
                        (listof identifier?) boolean? nano-cata?)]
    [nano-cata? (-> any/c boolean?)]
    [nano-cata-itype (-> nano-cata? identifier?)]
    [nano-cata-syntax (-> nano-cata? syntax?)]
    [nano-cata-procexpr (-> nano-cata? syntax?)]
    [nano-cata-maybe-inid* (-> nano-cata? (or/c false/c (listof syntax?)))]
    [nano-cata-outid* (-> nano-cata? (or/c false/c (listof identifier?)))]
    [nano-cata-maybe? (-> nano-cata? boolean?)]

    [make-language (-> identifier? (or/c false/c identifier?) (listof tspec?) (listof ntspec?) language?)]
    [language? (-> any/c boolean?)]
    [language-name (-> language? identifier?)]
    [language-entry-ntspec (-> language? (or/c false/c ntspec?))]
    [language-tspecs (-> language? (listof tspec?))]
    [language-ntspecs (-> language? (listof ntspec?))]
    [language-tag-mask (-> language? (or/c false/c exact-nonnegative-integer?))]

    [make-tspec (case->
                  (-> identifier? (listof identifier?) tspec?)
                  (-> identifier? (listof identifier?) syntax? tspec?))]
    [tspec? (-> any/c boolean?)]
    [tspec-type (-> tspec? identifier?)]
    [tspec-meta-vars (-> tspec? (listof identifier?))]
    [tspec-handler (-> tspec? (or/c false/c syntax?))]
    [tspec-pred (-> tspec? (or/c false/c identifier?))]
    [tspec-meta-pred (-> tspec? (or/c false/c identifier?))]
    [tspec-tag (-> tspec? (or/c false/c exact-nonnegative-integer?))]
    [tspec-parent? (-> tspec? false/c)]

    [make-ntspec (-> identifier? (listof identifier?) (listof alt?) ntspec?)]
    [ntspec? (-> any/c boolean?)]
    [ntspec-name (-> ntspec? identifier?)]
    [ntspec-meta-vars (-> ntspec? (listof identifier?))]
    [ntspec-alts (-> ntspec? (listof alt?))]
    [ntspec-pred (-> ntspec? (or/c false/c identifier?))]
    [ntspec-all-pred (-> ntspec? (or/c false/c symbol? syntax?))]
    [ntspec-parse-name (-> ntspec? (or/c false/c identifier?))]
    [ntspec-unparse-name (-> ntspec? (or/c false/c identifier?))]
    [ntspec-meta-parse-name (-> ntspec? (or/c false/c identifier?))]
    [ntspec-meta-pred (-> ntspec? (or/c false/c identifier?))]
    [ntspec-tag (-> ntspec? (or/c false/c exact-nonnegative-integer?))]
    [ntspec-all-tag (-> ntspec? (or/c false/c (listof exact-nonnegative-integer?)))]
    [ntspec-all-term-pred (-> ntspec? (or/c false/c syntax?))]

    [alt? (-> any/c alt?)]
    [alt-syn (-> alt? syntax?)]
    [alt-pretty (-> alt? (or/c false/c syntax?))]
    [alt-pretty-procedure? (-> alt? boolean?)]

    [make-pair-alt (-> syntax? (or/c false/c syntax?) boolean? pair-alt?)]
    [pair-alt? (-> any/c boolean?)]
    [pair-alt-pattern (-> pair-alt? (or/c false/c symbol? vector? list?))]
    [pair-alt-field-names (-> pair-alt? (or/c false/c (listof identifier?)))]
    [pair-alt-field-levels (-> pair-alt? (or/c false/c (listof exact-nonnegative-integer?)))]
    [pair-alt-field-maybes (-> pair-alt? (or/c false/c (listof boolean?)))]
    [pair-alt-accessors (-> pair-alt? (or/c false/c (listof identifier?)))]
    [pair-alt-implicit? (-> pair-alt? boolean?)]
    [pair-alt-pred (-> pair-alt? identifier?)]
    [pair-alt-maker (-> pair-alt? identifier?)]
    [pair-alt-tag (-> pair-alt? exact-nonnegative-integer?)]

    [make-terminal-alt (-> syntax? (or/c false/c syntax?) boolean? terminal-alt?)]
    [terminal-alt? (-> any/c boolean?)]
    [terminal-alt-tspec (-> terminal-alt? (or/c false/c tspec?))]

    [make-nonterminal-alt (-> syntax? (or/c false/c syntax?) boolean? nonterminal-alt?)]
    [nonterminal-alt? (-> any/c boolean?)]
    [nonterminal-alt-ntspec (-> nonterminal-alt? (or/c false/c ntspec?))]

    [has-implicit-alt? (-> ntspec? boolean?)]
    [spec-all-pred (-> (or/c tspec? ntspec?) syntax?)]
    [spec-type (-> (or/c tspec? ntspec?) identifier?)]

    [subspec? (-> (or/c tspec? ntspec?) (or/c tspec? ntspec?) boolean?)]

    [annotate-language! (-> language? identifier? any)]
    [language->lang-records (-> language? syntax?)]
    [language->lang-predicates (-> language? identifier? syntax?)]
    [exists-alt? (-> alt? ntspec? boolean?)]))

(require racket/fixnum)
(require racket/syntax)
(require syntax/stx)
(require "helpers.rkt" "syntaxconvert.rkt")
(require (for-template racket))
(require (for-template (only-in "helpers.rkt" define-nanopass-record)))

(define-struct language
  (name entry-ntspec tspecs ntspecs (tag-mask #:mutable) (record #:mutable) (pred #:mutable))
  #:prefab #:constructor-name $make-language)

(define make-language
  (let ()
    (define check-meta!
      (let ()
        (define (spec-meta-vars spec) (if (ntspec? spec) (ntspec-meta-vars spec) (tspec-meta-vars spec)))
        (define (spec-name spec) (if (ntspec? spec) (ntspec-name spec) (tspec-type spec)))
        (lambda (lang-name tspecs ntspecs)
          (let f ([specs (append tspecs ntspecs)])
            (unless (null? specs)
              (let ([test-spec (car specs)])
                (for-each
                  (lambda (mv)
                    (let ([mv-sym (syntax->datum mv)])
                      (for-each
                        (lambda (spec)
                          (when (memq mv-sym (map syntax->datum (spec-meta-vars spec)))
                            (raise-syntax-error 'define-language
                              (format "the forms ~s and ~s in language ~s uses the same meta-variable"
                                (syntax->datum (spec-name test-spec))
                                (syntax->datum (spec-name spec)) (syntax->datum lang-name))
                              mv)))
                        (cdr specs))))
                  (spec-meta-vars test-spec))))))))
    (lambda (name entry-ntspec tspecs ntspecs)
      (check-meta! name tspecs ntspecs)
      ($make-language name entry-ntspec tspecs ntspecs #f #f #f))))

(define-struct tspec 
  (meta-vars type handler (pred #:mutable) (meta-pred #:mutable) (tag #:mutable) (parent? #:mutable))
  #:prefab #:constructor-name $make-tspec)

(define make-tspec
  (case-lambda
    [(type meta-vars) ($make-tspec meta-vars type #f #f #f #f #f)]
    [(type meta-vars handler) ($make-tspec meta-vars type handler #f #f #f #f)]))

(define-struct ntspec
  (name meta-vars alts
    (unparse-name #:mutable) (parse-name #:mutable)
    (meta-parse-name #:mutable)
    (tag #:mutable)
    (pred #:mutable)         ; this record?
    (all-pred #:mutable)     ; this record or valid sub-grammar element
    ; e.g., if Rhs -> Triv, Triv -> Lvalue, and Lvalue -> var,
    ; then all-pred returns true for any Rhs, Triv, Lvalue, or var
    (all-term-pred #:mutable) ; this record's term sub-grammar elements
    (all-tag #:mutable)      ; tag for this record logor all sub grammar elements
    ; following all-pred order
    (meta-pred #:mutable)     ; name of a predicate used in the meta-parser
    (struct-name #:mutable)   ; name of the struct for this nonterminal
    (record-name #:mutable))
  #:prefab #:constructor-name $make-ntspec)

(define make-ntspec
  (lambda (name meta-vars alts)
    ($make-ntspec name meta-vars alts #f #f #f #f #f #f #f #f #f #f #f)))

(define-struct alt
  (syn pretty pretty-procedure?)
  #:prefab)

(define-struct (pair-alt alt)
  ((pattern #:mutable)
   (field-names #:mutable)
   (field-levels #:mutable)
   (field-maybes #:mutable)
   (implicit? #:mutable)
   (tag #:mutable)
   (pred #:mutable)
   (maker #:mutable)
   (accessors #:mutable)
   (name #:mutable))
  #:prefab #:constructor-name $make-pair-alt)

(define make-pair-alt
  (lambda (syn pretty pretty-procedure?)
    ($make-pair-alt syn pretty pretty-procedure?
      #f #f #f #f #f #f #f #f #f #f)))

(define-struct (terminal-alt alt)
  ((tspec #:mutable))
  #:prefab #:constructor-name $make-terminal-alt)

(define make-terminal-alt
  (lambda (syn pretty pretty-procedure?)
    ($make-terminal-alt syn pretty pretty-procedure? #f)))

(define-struct (nonterminal-alt alt)
  ((ntspec #:mutable))
  #:prefab #:constructor-name $make-nonterminal-alt)

(define make-nonterminal-alt
  (lambda (syn pretty pretty-procedure?)
    ($make-nonterminal-alt syn pretty pretty-procedure? #f)))

(define-who spec-all-pred
  (lambda (x)
    (cond
      [(tspec? x) (tspec-pred x)]
      [(ntspec? x) (ntspec-all-pred x)]
      [else (error who "unrecognized type" x)])))

(define-who spec-type
  (lambda (x)
    (cond
      [(tspec? x) (tspec-type x)]
      [(ntspec? x) (ntspec-name x)]
      [else (error who "unrecognized type" x)]))) 

;;; records produced by meta parsers 
(define-struct nano-dots (x) #:prefab)

(define-struct nano-quote (x) #:prefab)
  
(define-struct nano-unquote (x) #:prefab)
  
(define-struct nano-meta (alt fields) #:prefab)
  
(define-struct nano-cata
  (itype syntax procexpr maybe-inid* outid* maybe?)
  #:prefab)
  
;; record helpers 
(define find-spec
  (lambda (m lang)
    (let ([name (meta-var->raw-meta-var (syntax->datum m))])
      (or (findf (lambda (ntspec)
                   (memq name (map syntax->datum (ntspec-meta-vars ntspec))))
            (language-ntspecs lang))
          (findf (lambda (tspec)
                   (memq name (map syntax->datum (tspec-meta-vars tspec))))
            (language-tspecs lang))
          (raise-syntax-error #f "meta not found" (language-name lang) m)))))

(define nonterminal-meta?
  (lambda (m ntspec*)
    (let ([m (meta-var->raw-meta-var (syntax->datum m))])
      (ormap (lambda (x) (memq m (map syntax->datum (ntspec-meta-vars x))))
        ntspec*)))) 
  
(define nonterminal-meta->ntspec
  (lambda (meta ntspecs)
    (let ([meta (meta-var->raw-meta-var (syntax->datum meta))])
      (findf (lambda (x) (memq meta (map syntax->datum (ntspec-meta-vars x))))
        ntspecs))))
  
(define terminal-meta->tspec
  (lambda (meta tspecs)
    (let ([meta (meta-var->raw-meta-var (syntax->datum meta))])
      (findf (lambda (x) (memq meta (map syntax->datum (tspec-meta-vars x))))
        tspecs))))

;;; TODO, figure out if this can ever be called, if not remove the
;;;       reference to it, if so, figure out what should be implemented.
(define nano-alt->ntspec
  (lambda (alt ntspecs)
    (error 'nano-alt->ntspec "Not implemented"))) 
  
(define id->spec
  (lambda (id lang)
    (or (nonterm-id->ntspec? id (language-ntspecs lang))
        (term-id->tspec? id (language-tspecs lang)))))

(define term-id->tspec?
  (lambda (id tspecs)
    (let ([type (syntax->datum id)])
      (findf (lambda (tspec) (eq? (syntax->datum (tspec-type tspec)) type))
        tspecs))))

(define nonterm-id->ntspec?
  (lambda (id ntspecs)
    (let ([ntname (syntax->datum id)])
      (findf (lambda (ntspec) (eq? (syntax->datum (ntspec-name ntspec)) ntname))
        ntspecs))))

(define-syntax nonterm-id->ntspec
  (syntax-rules ()
    [(_ ?who ?id ?ntspecs)
     (let ([id ?id])
       (or (nonterm-id->ntspec? id ?ntspecs)
           (raise-syntax-error ?who "unrecognized non-terminal" id)))]))

(define-who meta-name->tspec
  (lambda (m tspecs)
    (let ([m (meta-var->raw-meta-var (syntax->datum m))])
      (findf (lambda (tspec)
               (memq m (map syntax->datum (tspec-meta-vars tspec)))) 
        tspecs))))
  
(define-who meta-name->ntspec
  (lambda (m ntspecs)
    (let ([m (meta-var->raw-meta-var (syntax->datum m))])
      (findf (lambda (ntspec)
               (memq m (map syntax->datum (ntspec-meta-vars ntspec)))) 
        ntspecs))))

(define subspec?
  (lambda (maybe-subspec spec)
    (let loop ([spec* (list spec)] [seen* '()])
      (and (not (null? spec*))
           (let ([spec (car spec*)])
             (or (eq? maybe-subspec spec)
                 (loop
                   (if (tspec? spec)
                       (cdr spec*)
                       (foldl
                         (lambda (alt spec*)
                           (cond
                             [(terminal-alt? alt)
                              (let ([spec (terminal-alt-tspec alt)])
                                (if (memq spec seen*)
                                    spec*
                                    (cons spec spec*)))]
                             [(nonterminal-alt? alt)
                              (let ([spec (nonterminal-alt-ntspec alt)])
                                (if (memq spec seen*)
                                    spec*
                                    (cons spec spec*)))]
                             [else spec*]))
                         (cdr spec*)
                         (ntspec-alts spec)))
                   (cons spec seen*))))))))

(define type->pred-prefixes
  (lambda (id mrec) 
    (define find-related-ntspecs
      (lambda (ntspec mrec)
        (let ([ntspecs (language-ntspecs mrec)])
          (let f ([alts (ntspec-alts ntspec)] [ls '()])
            (foldl (lambda (alt ls)
                     (if (nonterminal-alt? alt)
                         (let ([ntspec (nonterminal-alt-ntspec alt)])
                           (cons ntspec (f (ntspec-alts ntspec) ls)))
                         ls))
              ls alts)))))
    (define find
      (lambda (specs)
        (cond
          [(null? specs) #f]
          [(eq? (syntax->datum id)
             (syntax->datum 
               (let ([spec (car specs)])
                 (cond
                   [(tspec? spec) (tspec-type spec)]
                   [(ntspec? spec) (ntspec-name spec)]
                   [else (error 'type->pred-prefixes
                           "unable to find matching spec, wrong type"
                           spec)]))))
            (car specs)]
          [else (find (cdr specs))])))
    (let ([found (find (language-tspecs mrec))])
      (if found
          (list found)
          (let ([found (find (language-ntspecs mrec))])
            (if found
                (let ([ntspecs (find-related-ntspecs found  mrec)])
                  (cons found ntspecs))
                (error 'type->pred-prefixes "unrecognized non-terminal"
                  id)))))))
  
(define has-implicit-alt? 
  (lambda (ntspec)
    (ormap
      (lambda (alt)
        (if (pair-alt? alt)
            (pair-alt-implicit? alt)
            (and (nonterminal-alt? alt)
                 (has-implicit-alt? (nonterminal-alt-ntspec alt)))))
      (ntspec-alts ntspec))))

(define gather-meta
  (lambda (lang)
    (let ([tmeta (map tspec-meta-vars (language-tspecs lang))]
           [pmeta (map ntspec-meta-vars (language-ntspecs lang))])
      (apply append (append tmeta pmeta)))))
(define bitwise-length
  (lambda (n)
    (let loop ([n (abs n)] [c 0])
      (if (= n 0)
          c
          (loop (arithmetic-shift n -1) (+ c 1))))))
(define annotate-language!
  (lambda (lang id)
    (let ([lang-name (language-name lang)])
      (let ([lang-rec-id (format-id id "~a-record" lang-name)]
            [tspec* (language-tspecs lang)]
            [ntspec* (language-ntspecs lang)]
            [np-bits #f])
        (set-language-record! lang lang-rec-id)
        (set-language-pred! lang (format-id id "~a?" lang-rec-id))
        ;; Needs to return #t because it ends up encoded in a field this way
        (define meta?
          (lambda (m)
            (let ([m (meta-var->raw-meta-var (syntax->datum m))])
              (and (or (ormap (lambda (tspec)
                                (memq m (map syntax->datum (tspec-meta-vars tspec))))
                         tspec*)
                       (ormap (lambda (ntspec)
                                (memq m (map syntax->datum (ntspec-meta-vars ntspec))))
                         ntspec*))
                   #t))))
        (define annotate-tspec!
          (lambda (tspec tspec-tag-all)
            (let ([t (tspec-type tspec)])
              (set-tspec-pred! tspec (format-id t "~a?" t))
              (set-tspec-meta-pred! tspec (format-id id "meta-~a?" t))
              (let ([tag #f])
                (if tag
                    (begin
                      (set-tspec-tag! tspec tag)
                      (set-tspec-parent?! tspec #f)
                      (bitwise-ior tag tspec-tag-all))
                    tspec-tag-all)))))
        (define annotate-alt*!
          (lambda (bits)
            (lambda (ntspec alt-all-tag)
              (let ([tag (ntspec-tag ntspec)] [ntname (ntspec-name ntspec)])
                (let ([ntname-sym (syntax->datum ntname)])
                  (let f ([alt* (ntspec-alts ntspec)] [next 1] [alt-all-tag alt-all-tag])
                    (if (null? alt*)
                        alt-all-tag
                        (let ([a (car alt*)] [alt* (cdr alt*)])
                          (cond
                            [(pair-alt? a)
                             (let* ([syn (alt-syn a)]
                                    [name (stx-car syn)]
                                    [rec-sym (unique-symbol lang-name ntname name)]
                                    [m? (meta? name)])
                               (let-values ([(p fields levels maybes) (convert-pattern (if m? syn (stx-cdr syn)))])
                                 (unless (all-unique-identifiers? fields)
                                   (raise-syntax-error 'define-language "found one or more duplicate fields in production" syn))
                                 (let ([tag (+ (arithmetic-shift next bits) tag)])
                                   (set-pair-alt-tag! a tag)
                                   (set-pair-alt-pattern! a p)
                                   (set-pair-alt-field-names! a fields)
                                   (set-pair-alt-field-levels! a levels)
                                   (set-pair-alt-field-maybes! a maybes)
                                   (set-pair-alt-implicit?! a m?)
                                   (set-pair-alt-accessors! a
                                     (map (lambda (field)
                                            (format-id id "~a-~a" rec-sym field))
                                       fields))
                                   (set-pair-alt-pred! a (format-id id "~a?" rec-sym))
                                   (set-pair-alt-maker! a (format-id id "make-~a" rec-sym))
                                   (set-pair-alt-name! a (format-id id "~a" rec-sym))
                                   (f alt* (+ next 1) (bitwise-ior alt-all-tag tag)))))]
                            [(nonterminal-alt? a)
                              (let ([a-ntspec (nonterminal-meta->ntspec (alt-syn a) ntspec*)])
                                (unless a-ntspec
                                  (raise-syntax-error 'define-language "no nonterminal for meta-variable"
                                    lang-name (alt-syn a)))
                                (set-nonterminal-alt-ntspec! a a-ntspec)
                                (f alt* next alt-all-tag))]
                            [(terminal-alt? a)
                              (let ([tspec (terminal-meta->tspec (alt-syn a) tspec*)])
                                (unless tspec
                                  (raise-syntax-error 'define-language "no terminal for meta-variable"
                                    lang-name (alt-syn a)))
                                (set-terminal-alt-tspec! a tspec)
                                (f alt* next alt-all-tag))])))))))))
        (define annotate-ntspec*!
          (lambda (ntspec*)
            (let f ([nt-counter 0] [ntspec* ntspec*])
              (if (null? ntspec*)
                  nt-counter
                  (let ([ntspec (car ntspec*)] [ntspec* (cdr ntspec*)])
                    (let ([nterm (ntspec-name ntspec)])
                      (let ([nt-rec-sym (unique-symbol lang-name nterm)])
                        (set-ntspec-record-name! ntspec (format-id id "~a" nt-rec-sym))
                        (set-ntspec-tag! ntspec nt-counter)
                        (set-ntspec-pred! ntspec (format-id id "~a?" nt-rec-sym))
                        (set-ntspec-meta-pred! ntspec (format-id id "meta-~a?" nterm))
                        (set-ntspec-unparse-name! ntspec (format-id id "unparse-~a" nterm))
                        (set-ntspec-parse-name! ntspec (format-id id "parse-~a" nterm))
                        (set-ntspec-meta-parse-name! ntspec (format-id id "meta-parse-~a" nterm))
                        (f (+ nt-counter 1) ntspec*))))))))
        (define-who annotate-all-pred!
          (lambda (ntspec)
            (let ([all-pred (ntspec-all-pred ntspec)])
              (cond
                [(eq? all-pred 'processing) (raise-syntax-error 'define-language "found mutually recursive nonterminals" (ntspec-name ntspec))]
                [all-pred (values all-pred (ntspec-all-term-pred ntspec) (ntspec-all-tag ntspec))]
                [else
                  (set-ntspec-all-pred! ntspec 'processing)
                  (let f ([alt* (ntspec-alts ntspec)] [pred* '()] [term-pred* '()] [tag '()])
                    (if (null? alt*)
                        (let ([all-pred (if (null? pred*)
                                            (ntspec-pred ntspec)
                                            #`(lambda (x)
                                                (or (#,(ntspec-pred ntspec) x)
                                                    #,@(map (lambda (pred) #`(#,pred x)) pred*))))]
                               [all-term-pred (cond
                                                [(null? term-pred*) #f]
                                                [(null? (cdr term-pred*)) (car term-pred*)]
                                                [else #`(lambda (x) (or #,@(map (lambda (pred) #`(#,pred x)) term-pred*)))])]
                               [tag (cons (ntspec-tag ntspec) tag)])
                          (set-ntspec-all-pred! ntspec all-pred)
                          (set-ntspec-all-term-pred! ntspec all-term-pred)
                          (set-ntspec-all-tag! ntspec tag)
                          (values all-pred all-term-pred tag))
                        (let ([alt (car alt*)])
                          (cond
                            [(pair-alt? alt) (f (cdr alt*) pred* term-pred* tag)]
                            [(terminal-alt? alt)
                              (let* ([tspec (terminal-alt-tspec alt)]
                                      [new-tag (tspec-tag tspec)]
                                      [pred (tspec-pred tspec)])
                                (f (cdr alt*) (cons pred pred*) (cons pred term-pred*) tag))]
                            [(nonterminal-alt? alt)
                              (let-values ([(pred term-pred new-tag) (annotate-all-pred! (nonterminal-alt-ntspec alt))])
                                (f (cdr alt*) (cons pred pred*)
                                  (if term-pred (cons term-pred term-pred*) term-pred*)
                                  (append new-tag tag)))]))))]))))
        (let ([tspec-tag-bits (foldl annotate-tspec! 0 tspec*)])
          (let ([nt-counter (annotate-ntspec*! ntspec*)])
            (let ([bits (bitwise-length nt-counter)])
              (unless (= (bitwise-and tspec-tag-bits (- (arithmetic-shift 1 bits) 1)) 0)
                (raise-syntax-error 'define-language "nanopass-record tags interfere with language production tags"
                  lang-name))
              (set-language-tag-mask! lang (- (arithmetic-shift 1 bits) 1))
              (let ([ntalt-tag-bits (foldl (annotate-alt*! bits) 0 ntspec*)])
                ;; currently the support for nanopass-records is disabled, so there is no need to have this here.
                #;(unless (or (not np-bits)
                (= (bitwise-and ntalt-tag-bits
                     (fxreverse-bit-field (- (arithmetic-shift 1 np-bits) 1)
                       0 (- (fixnum-width) 1)))
                   0))
                    (raise-syntax-error 'define-language "language production tags interfere with nanopass-record tags"
                      lang-name))
                (for-each annotate-all-pred! ntspec*)))))))))

(define language->lang-records
  (lambda (lang)
    (let ([lang-rec-id (language-record lang)]
          [ntspecs (language-ntspecs lang)]
          [tspecs (language-tspecs lang)])
      (define alt->lang-record
        (lambda (ntspec alt)
          ; TODO: handle fld and msgs that are lists.
          (define build-field-check
            (lambda (fld msg level maybe?)
              (with-values 
                (cond
                  [(nonterminal-meta->ntspec fld ntspecs) =>
                   (lambda (ntspec) (values (ntspec-all-pred ntspec) (ntspec-name ntspec)))]
                  [(terminal-meta->tspec fld tspecs) =>
                   (lambda (tspec) (values (tspec-pred tspec) (tspec-type tspec)))]
                  [else (raise-syntax-error 'define-language
                          (format "unrecognized meta-variable in language ~s"
                            (syntax->datum (language-name lang)))
                          fld)])
                (lambda (pred? name)
                  (with-syntax ([pred? (if maybe?
                                           #`(lambda (x) (or (eq? x #f) (#,pred? x)))
                                           pred?)])
                    #`(#,(let f ([level level])
                           (if (= level 0)
                               #`(lambda (x)
                                   (unless (pred? x)
                                     (let ([msg #,msg])
                                       (if msg
                                           (error who
                                             "expected ~s but received ~s in field ~s of ~s from ~a"
                                             '#,name x '#,fld '#,(alt-syn alt) msg)
                                           (error who
                                             "expected ~s but received ~s in field ~s of ~s"
                                             '#,name x '#,fld '#,(alt-syn alt))))))
                               #`(lambda (x)
                                   (for-each #,(f (- level 1)) x))))
                        #,fld))))))
          (with-syntax ([(fld ...) (pair-alt-field-names alt)])
            (with-syntax ([(msg ...) (generate-temporaries #'(fld ...))]
                          [$maker (format-id (pair-alt-name alt) "$~a" (pair-alt-maker alt))])
              #`(begin
                  (define-struct (#,(pair-alt-name alt) #,(ntspec-record-name ntspec))
                    (fld ...)
                    #:constructor-name $maker)
                  (define #,(pair-alt-maker alt)
                    (lambda (who fld ... msg ...)
                      #,@(if (= (optimize-level) 3)
                             '()
                             (map build-field-check
                               (syntax->list #'(fld ...))
                               (syntax->list #'(msg ...))
                               (pair-alt-field-levels alt)
                               (pair-alt-field-maybes alt)))
                      ($maker fld ...))))))))
      (define ntspec->lang-record
        (lambda (ntspec)
          #`(define-struct (#,(ntspec-record-name ntspec) #,lang-rec-id) () #:prefab)))
      (define ntspecs->lang-records
        (lambda (ntspec*)
          (let f ([ntspec* ntspec*] [ntrec* '()] [altrec* '()])
            (if (null? ntspec*)
                #`(#,ntrec* #,altrec*)
                (let ([ntspec (car ntspec*)])
                  (let g ([alt* (ntspec-alts ntspec)] [altrec* altrec*])
                    (if (null? alt*)
                        (f (cdr ntspec*)
                          (cons (ntspec->lang-record ntspec) ntrec*)
                          altrec*)
                        (let ([alt (car alt*)])
                          (if (pair-alt? alt)
                              (g (cdr alt*)
                                (cons (alt->lang-record ntspec alt) altrec*))
                              (g (cdr alt*) altrec*))))))))))

      (with-syntax ([((ntrec ...) (altrec ...))
                     (ntspecs->lang-records ntspecs)])
          #`((define-nanopass-record)
             (define-struct (#,lang-rec-id nanopass-record) () #:prefab)
             ntrec ...
             altrec ...)))))

(define language->lang-predicates
  (lambda (desc id)
    (let ([name (language-name desc)])
      (let loop ([ntspecs (language-ntspecs desc)] [nt?* '()] [term?* '()])
        (if (null? ntspecs)
            (with-syntax ([lang? (format-id name "~a?" name)]
                          [lang-pred? (language-pred desc)]
                          [(nt? ...) nt?*]
                          [(term? ...) term?*])
              #'((define lang?
                   (lambda (x)
                     (or (lang-pred? x) (term? x) ...)))
                  nt? ...))
            (let ([ntspec (car ntspecs)])
              (loop (cdr ntspecs)
                (with-syntax ([nt? (format-id name "~a-~a?" name (ntspec-name ntspec))]
                               [lambda-expr (ntspec-all-pred ntspec)])
                  (cons #'(define nt? lambda-expr) nt?*))
                (let loop ([alts (ntspec-alts ntspec)] [term?* term?*])
                  (if (null? alts)
                      term?*
                      (loop (cdr alts)
                        (let ([alt (car alts)])
                          (if (terminal-alt? alt)
                              (cons (tspec-pred (terminal-alt-tspec alt)) term?*)
                              term?*))))))))))))
  
;; utilities moved out of pass.ss
(define-who exists-alt?
  (lambda (ialt ntspec)
    (define scan-alts
      (lambda (pred?)
        (let f ([alt* (ntspec-alts ntspec)])
          (if (null? alt*)
              #f
              (let ([alt (car alt*)])
                (if (nonterminal-alt? alt)
                    (or (f (ntspec-alts (nonterminal-alt-ntspec alt)))
                        (f (cdr alt*)))
                    (if (pred? alt) alt (f (cdr alt*)))))))))
    (let ([syn (alt-syn ialt)])
      (cond
        [(terminal-alt? ialt)
         (let ([type (syntax->datum (tspec-type (terminal-alt-tspec ialt)))])
           (scan-alts
             (lambda (alt)
               (and (terminal-alt? alt)
                    (eq? (syntax->datum (tspec-type (terminal-alt-tspec alt))) type)))))]
        [(pair-alt? ialt)
         (if (pair-alt-implicit? ialt)
             (let ([pattern (pair-alt-pattern ialt)])
               (scan-alts
                 (lambda (alt)
                   (and (pair-alt? alt)
                        (pair-alt-implicit? alt)
                        (let ([apattern (pair-alt-pattern alt)])
                          (equal? apattern pattern))))))
             (let ([pattern (pair-alt-pattern ialt)])
               (scan-alts
                 (lambda (alt)
                   (and (pair-alt? alt)
                        (not (pair-alt-implicit? alt))
                        (let ([asyn (alt-syn alt)])
                          (let ([apattern (pair-alt-pattern alt)])
                            (and (eq? (syntax->datum (car asyn)) (syntax->datum (car syn)))
                                 (equal? apattern pattern)))))))))]
        [else (error who "unexpected alt" ialt)]))))
