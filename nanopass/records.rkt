#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for detatils

(provide find-spec nonterminal-meta? nano-alt->ntspec 
         nonterm-id->ntspec? nonterm-id->ntspec id->spec term-id->tspec?

         meta-name->tspec meta-name->ntspec

         make-nano-dots nano-dots? nano-dots-x 

         make-nano-quote nano-quote? nano-quote-x

         make-nano-unquote nano-unquote? nano-unquote-x

         make-nano-meta nano-meta? nano-meta-alt nano-meta-fields

         make-nano-cata nano-cata? nano-cata-itype nano-cata-syntax
         nano-cata-procexpr nano-cata-maybe-inid* nano-cata-outid*
         nano-cata-maybe?

         make-language language? language-name language-entry-ntspec
         language-tspecs language-ntspecs 
         language-tag-mask language-nongenerative-id

         make-tspec tspec-meta-vars tspec-type tspec-pred tspec-meta-pred
         tspec-handler tspec? tspec-tag tspec-parent?

         ntspec? make-ntspec ntspec-name ntspec-meta-vars
         ntspec-alts ntspec-pred ntspec-all-pred
         ntspec-parse-name ntspec-unparse-name
         ntspec-meta-parse-name ntspec-meta-pred
         ntspec-tag ntspec-all-tag ntspec-all-term-pred

         alt? alt-syn alt-pretty alt-pretty-procedure?
         make-pair-alt pair-alt? pair-alt-pattern
         pair-alt-field-names pair-alt-field-levels pair-alt-field-maybes
         pair-alt-accessors pair-alt-implicit? pair-alt-pred pair-alt-maker
         pair-alt-tag
         make-terminal-alt terminal-alt? terminal-alt-tspec
         make-nonterminal-alt nonterminal-alt? nonterminal-alt-ntspec

         has-implicit-alt? 
         spec-all-pred
         spec-type

         subspec?

         annotate-language!
         language->lang-records
         language->lang-predicates

         define-nanopass-record

         exists-alt?)

(require racket/fixnum)
(require "helpers.rkt" "syntaxconvert.rkt")

(define-nanopass-record)

(define-struct language
  (name entry-ntspec tspecs ntspecs (tag-mask #:mutable) nongenerative-id (pred #:mutable))
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
                          (when (memq mv-sym (syntax->datum (spec-meta-vars spec)))
                            (raise-syntax-error 'define-language
                              (format "the forms ~s and ~s in language ~s uses the same meta-variable"
                                (syntax->datum (spec-name test-spec))
                                (syntax->datum (spec-name spec)) (syntax->datum lang-name))
                              mv)))
                        (cdr specs))))
                  (spec-meta-vars test-spec))))))))
    (lambda (name entry-ntspec tspecs ntspecs nongen-id)
      (check-meta! name tspecs ntspecs)
      ($make-language name entry-ntspec tspecs ntspecs #f #f #f nongen-id #f))))

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
    (meta-pred #:mutable))    ; name of a predicate used in the meta-parser
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
   (accessors #:mutable))
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
                   (memq name (syntax->datum (ntspec-meta-vars ntspec))))
            (language-ntspecs lang))
          (findf (lambda (tspec)
                   (memq name (syntax->datum (tspec-meta-vars tspec))))
            (language-tspecs lang))
          (raise-syntax-error #f "meta not found" (language-name lang) m)))))

(define nonterminal-meta?
  (lambda (m ntspec*)
    (let ([m (meta-var->raw-meta-var (syntax->datum m))])
      (ormap (lambda (x) (memq m (syntax->datum (ntspec-meta-vars x))))
        ntspec*)))) 
  
(define nonterminal-meta->ntspec
  (lambda (meta ntspecs)
    (let ([meta (meta-var->raw-meta-var (syntax->datum meta))])
      (findf (lambda (x) (memq meta (syntax->datum (ntspec-meta-vars x))))
        ntspecs))))
  
(define terminal-meta->tspec
  (lambda (meta tspecs)
    (let ([meta (meta-var->raw-meta-var (syntax->datum meta))])
      (findf (lambda (x) (memq meta (syntax->datum (tspec-meta-vars x))))
        tspecs))))

(define meta->pred
  (lambda (m lang)
    (let ([name (meta-var->raw-meta-var (syntax->datum m))])
      (or (findf (lambda (ntspec)
                   (and (memq name (syntax->datum (ntspec-meta-vars ntspec)))
                        (ntspec-all-pred ntspec)))
            (language-ntspecs lang))
          (findf (lambda (tspec)
                   (and (memq name (syntax->datum (tspec-meta-vars tspec)))
                        (tspec-pred tspec)))
            (language-tspecs lang))
          (raise-syntax-error #f "meta not found" (language-name lang) m)))))
  
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
               (memq m (syntax->datum (tspec-meta-vars tspec)))) 
        tspecs))))
  
(define-who meta-name->ntspec
  (lambda (m ntspecs)
    (let ([m (meta-var->raw-meta-var (syntax->datum m))])
      (findf (lambda (ntspec)
               (memq m (syntax->datum (ntspec-meta-vars ntspec)))) 
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

(define annotate-language!
  (lambda (r lang id)
    (let ([lang-name (language-name lang)] [nongen-id (language-nongenerative-id lang)])
      (let ([lang-rec-id (format-id id "~a-record" lang-name)]
            [tspec* (language-tspecs lang)]
            [ntspec* (language-ntspecs lang)]
            [np-bits #f]
            [nongen-sym (and nongen-id (syntax->datum nongen-id))])
        ;; Needs to return #t because it ends up encoded in a field this way
        (define meta?
          (lambda (m)
            (let ([m (meta-var->raw-meta-var (syntax->datum m))])
              (or (ormap (lambda (tspec) (memq m (syntax->datum (tspec-meta-vars tspec)))) tspec*)
                  (ormap (lambda (ntspec) (memq m (syntax->datum (ntspec-meta-vars ntspec)))) ntspec*)))))
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
                           (fxior tag tspec-tag-all))
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
                                         [name (car syn)]
                                         [rec-sym (unique-symbol lang-name ntname name)]
                                         [m? (meta? name)])
                                    (let-values ([(p fields levels maybes) (convert-pattern (if m? syn (cdr syn)))])
                                      (unless (all-unique-identifiers? fields)
                                        (raise-syntax-error 'define-language "found one or more duplicate fields in production" syn))
                                      (let ([tag (fx+ (fxlshift next bits) tag)])
                                        (set-pair-alt-tag! a tag)
                                        (set-pair-alt-rtd! a
                                          (make-record-type-descriptor rec-sym nt-rtd
                                            (if nongen-sym
                                                (regensym nongen-sym
                                                  (format ":~s:~s" ntname-sym (syntax->datum name))
                                                  (format "-~s" tag))
                                                rec-sym)
                                            #t #f
                                            (let loop ([fields fields] [count 0])
                                              (if (null? fields)
                                                  (make-vector count)
                                                  (let ([v (loop (cdr fields) (fx+ count 1))])
                                                    (vector-set! v count `(immutable ,(syntax->datum (car fields))))
                                                    v)))))
                                        (set-pair-alt-pattern! a p)
                                        (set-pair-alt-field-names! a fields)
                                        (set-pair-alt-field-levels! a levels)
                                        (set-pair-alt-field-maybes! a maybes)
                                        (set-pair-alt-implicit! a m?)
                                        (set-pair-alt-accessors! a
                                          (map (lambda (field)
                                                 (format-id id "~a-~a" rec-sym field))
                                            fields))
                                        (set-pair-alt-pred! a (format-id id "~a?" rec-sym))
                                        (set-pair-alt-maker! a (format-id id "make-~a" rec-sym))
                                        (f alt* (fx+ next 1) (fxior alt-all-tag tag)))))]
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
                             (let ([nt-rtd (make-record-type-descriptor
                                             nt-rec-sym (language-rtd lang)
                                             (if nongen-sym
                                                 (regensym nongen-sym
                                                   (format ":~s"
                                                     (syntax->datum nterm))
                                                   (format "-~d" nt-counter))
                                                 nt-rec-sym)
                                             #f #f (vector))])
                               (set-ntspec-tag! ntspec nt-counter)
                               (set-ntspec-rtd! ntspec nt-rtd)
                               (set-ntspec-rcd! ntspec
                                 (make-record-constructor-descriptor nt-rtd
                                   (language-rcd lang) #f))
                               (set-ntspec-pred! ntspec (format-id id "~a?" nt-rec-sym))
                               (set-ntspec-meta-pred! ntspec (format-id id "meta-~a?" nterm))
                               (set-ntspec-unparse-name! ntspec (format-id id "unparse-~a" nterm))
                               (set-ntspec-parse-name! ntspec (format-id id "parse-~a" nterm))
                               (set-ntspec-meta-parse-name! ntspec (format-id id "meta-parse-~a" nterm))
                               (f (fx+ nt-counter 1) ntspec*)))))))))
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
             (let ([lang-rtd (make-record-type-descriptor (syntax->datum lang-name)
                               (record-type-descriptor nanopass-record)
                               (let ([nongen-id (language-nongenerative-id lang)])
                                 (if nongen-id
                                     (syntax->datum nongen-id)
                                     (unique-symbol lang-name)))
                               #f #f (vector))])
               (set-language-rtd! lang lang-rtd)
               (set-language-rcd! lang
                 (make-record-constructor-descriptor lang-rtd
                   (record-constructor-descriptor nanopass-record) #f))
               (set-language-pred! lang #`(record-predicate '#,lang-rtd)))
             (let ([tspec-tag-bits (foldl annotate-tspec! 0 tspec*)])
               (let ([nt-counter (annotate-ntspec*! ntspec*)])
                 (let ([bits (fxlength nt-counter)])
                   (unless (fxzero? (fxand tspec-tag-bits (fx- (fxlshift 1 bits) 1)))
                     (raise-syntax-error 'define-language "nanopass-record tags interfere with language production tags"
                       lang-name))
                   (language-tag-mask-set! lang (fx- (fxlshift 1 bits) 1))
                   (let ([ntalt-tag-bits (foldl (annotate-alt*! bits) 0 ntspec*)])
                     (unless (or (not np-bits)
                                 (fxzero? (fxand ntalt-tag-bits
                                            (fxreverse-bit-field (fx- (fxlshift 1 np-bits) 1)
                                              0 (fx- (fixnum-width) 1)))))
                       (raise-syntax-error 'define-language "language production tags interfere with nanopass-record tags"
                         lang-name))
                     (for-each annotate-all-pred! ntspec*)))))))))

(define language->lang-records
  (lambda (lang)
    (let ([ntspecs (language-ntspecs lang)] [tspecs (language-tspecs lang)])
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
                           (if (fx=? level 0)
                               #`(lambda (x)
                                   (unless (pred? x)
                                     (let ([msg #,msg])
                                       (if msg
                                           (errorf who
                                             "expected ~s but received ~s in field ~s of ~s from ~a"
                                             '#,name x '#,fld '#,(alt-syn alt) msg)
                                           (errorf who
                                             "expected ~s but received ~s in field ~s of ~s"
                                             '#,name x '#,fld '#,(alt-syn alt))))))
                               #`(lambda (x)
                                   (for-each #,(f (fx- level 1)) x))))
                        #,fld))))))
          (with-syntax ([(fld ...) (pair-alt-field-names alt)])
            (with-syntax ([(msg ...) (generate-temporaries #'(fld ...))]
                          [$maker (format-id (pair-alt-name alt) "$~a" (pair-alt-maker alt))])
              #`(begin
                  (define-struct (#,(pair-alt-name alt) #,(pair-alt-parent alt))
                    (fld ...)
                    #:constructor-name $maker)
                  (define #,(pair-alt-maker alt)
                    (lambda (who fld ... msg ...)
                      #,@(if (fx=? (optimize-level) 3)
                             '()
                             (map build-field-check #'(fld ...) #'(msg ...)
                               (pair-alt-field-levels alt)
                               (pair-alt-field-maybes alt)))
                      ($maker fld ...))))))))
      (define ntspec->lang-record
        (lambda (ntspec)
          #`(define #,(ntspec-pred ntspec) (record-predicate '#,(ntspec-rtd ntspec)))))
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
      (define ntspecs->indirect-id*
        (lambda (ntspec*)
          (let f ([ntspec* ntspec*] [id* '()])
            (if (null? ntspec*)
                id*
                (let ([ntspec (car ntspec*)])
                  (let g ([alt* (ntspec-alts ntspec)] [id* id*])
                    (if (null? alt*)
                        (f (cdr ntspec*) (cons (ntspec-pred ntspec) id*))
                        (g (cdr alt*)
                          (let ([alt (car alt*)])
                            (if (pair-alt? alt)
                                (cons* (pair-alt-pred alt)
                                  (pair-alt-maker alt)
                                  (append (pair-alt-accessors alt) id*))
                                id*))))))))))
      (with-syntax ([((ntrec ...) (altrec ...))
                     (ntspecs->lang-records (language-ntspecs lang))]
                    [lang-id (language-name lang)]
                    [(indirect-id* ...) (ntspecs->indirect-id* (language-ntspecs lang))])
        #`(ntrec ...  altrec ...  (indirect-export lang-id indirect-id* ...))))))

(define language->lang-predicates
  (lambda (desc)
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
