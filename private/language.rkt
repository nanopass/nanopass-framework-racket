#lang racket/base
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

;;; Producs are : record defs, parser, meta parser, lang 
;;; may need to use meta define meta-parser.
;;; 
;;; TODO:
;;;   - add facility to allow for functional transformations while unparsing
;;;     (instead of just the pattern ones available now).  this should be
;;;     clearer than the old definitions form.
;;;   - re-investigate how language extensions work and see if there is a
;;;     cleaner way to do this
;;;   - better comparison of alts then simple symbolic equality
;;;   - checking for language output to make sure constructed languages are
;;;     internally consistent:
;;;     - check to make sure metas are unique
(provide define-language language->s-expression diff-languages prune-language
         define-pruned-language)

(require  "helpers.rkt"
          "unparser.rkt"
          (for-syntax racket/syntax
                      racket/base
                      racket/list
                      syntax/stx
                      syntax/parse
                      unstable/pretty
                      "helpers.rkt"
                      "language-helpers.rkt"
                      "records.rkt"
                      "meta-parser.rkt"))

(define-syntax (define-language x)
  ;; Doing a little extra work here to make sure that we are able to track
  ;; errors.  The basic idea is that we want to go through the list of
  ;; existing tspecs, and when we keep them, make a new copy (so that
  ;; language specific information can be updated in them), and when they
  ;; are being removed, we "mark" that we found the one to remove by
  ;; pulling it out of our removal list.  If any remain in the removal
  ;; list when we're done, we complain about it.
  (define ((freshen-objects o=? maybe-fresh-o unpack what) os os-)
    (let ([os (foldl (lambda (o- os)
                       (let-values ([(o os) (partition (lambda (o) (o=? o- o)) os)])
                         (when (null? o)
                           (raise-syntax-error 'define-language
                             (format "unrecognized ~s in subtract" what)
                             x (unpack o-)))
                         os))
                os os-)])
      (if maybe-fresh-o
          (map maybe-fresh-o os)
          os)))
  
  (define freshen-tspecs (freshen-objects tspec=? #f tspec-type 'terminal))
  (define freshen-alts (freshen-objects alt=? fresh-alt alt-syn 'production))

  (define ((add-objects o=? unpack what) os os+)
    (foldl (lambda (o+ os)
             (cond
               [(memf (lambda (x) (o=? o+ x)) os) =>
                (lambda (os)
                  (raise-syntax-error 'define-language
                    (format "duplicate ~s in add" what)
                    x (unpack o+) (list (unpack (car os)))))]
               [else (cons o+ os)]))
      os os+))
  
  (define add-tspecs (add-objects tspec=? tspec-type 'terminal))
  (define add-alts (add-objects alt=? alt-syn 'production))
  
  (define freshen-ntspecs
    (lambda (ntspecs ntspecs-)
      (cond
        [(and (null? ntspecs) (not (null? ntspecs-)))
         (if (> (length ntspecs-) 1)
             (raise-syntax-error 'define-language
                                 "multiple unrecognized ntspecs, including"
                                 (ntspec-name (car ntspecs-)))
             (raise-syntax-error 'define-language
                                 "unrecognized ntspec" (ntspec-name (car ntspecs-))))]
        [(null? ntspecs) '()]
        [else
         (let g ([ntspecs- ntspecs-] [ntspec (car ntspecs)] [remaining '()])
           (if (null? ntspecs-)
               (cons (fresh-ntspec ntspec) (freshen-ntspecs (cdr ntspecs) remaining))
               (let ([ntspec- (car ntspecs-)])
                 (if (ntspec=? ntspec- ntspec)
                     (let ([alts (freshen-alts (ntspec-alts ntspec) (ntspec-alts ntspec-))])
                       (if (null? alts)
                           (freshen-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs-)))
                           (cons (fresh-ntspec ntspec alts)
                                 (freshen-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs-))))))
                     (g (cdr ntspecs-) ntspec (cons (car ntspecs-) remaining))))))])))
  
  (define add-ntspecs
    (lambda (ntspecs ntspecs+)
      (cond
        [(null? ntspecs) ntspecs+]
        [else
         (let g ([ntspecs+ ntspecs+] [ntspec (car ntspecs)] [remaining '()])
           (if (null? ntspecs+)
               (cons ntspec (add-ntspecs (cdr ntspecs) remaining))
               (let ([ntspec+ (car ntspecs+)])
                 (if (ntspec=? ntspec+ ntspec)
                     (let ([alts (add-alts (ntspec-alts ntspec) (ntspec-alts ntspec+))])
                       (cons (fresh-ntspec ntspec alts)
                             (add-ntspecs (cdr ntspecs) (append remaining (cdr ntspecs+)))))
                     (g (cdr ntspecs+) ntspec (cons (car ntspecs+) remaining))))))])))
  
  (define partition-terms
    (lambda (id terms)
      (let loop ([terms terms] [terms+ '()] [terms- '()])
        (syntax-parse terms
          #:datum-literals (+ -)
          [() (values terms+ terms-)]
          [((+ . ts) . terms)
           (loop #'terms
                 (append terms+ (parse-terms id #'ts))
                 terms-)]
          [((- . ts) . terms)
           (loop #'terms
                 terms+
                 (append terms- (parse-terms id #'ts)))]
          [(x . terms)
           (raise-syntax-error 'define-language
                               "unrecognized terminal extension syntax, expected (+ term ...) or (- term ...)"
                               #'x)]
          [x (raise-syntax-error 'define-language
                                 "unrecognized terminal extension list" #'x)]))))
  
  (define partition-ntspecs
    (lambda (ntspecs terminal-meta*)
      (let f ([ntspecs ntspecs] [ntspecs+ '()] [ntspecs- '()])
        (if (null? ntspecs)
            (values ntspecs+ ntspecs-) ;; lists returned are reversed (okay?)
            (let ([ntspec (car ntspecs)] [ntspecs (cdr ntspecs)])
              (let g ([alts (cddr ntspec)] [alts+ '()] [alts- '()])
                (syntax-case alts ()
                  [() (let ([name (car ntspec)] [metas (cadr ntspec)])
                        (f ntspecs
                           (if (null? alts+)
                               ntspecs+
                               (cons (make-ntspec name metas alts+)
                                     ntspecs+))
                           (if (null? alts-)
                               ntspecs-
                               (cons (make-ntspec name metas alts-)
                                     ntspecs-))))]
                  [((+ a* ...) . alts) (plus? #'+)
                                       (g #'alts (append alts+ (parse-alts #'(a* ...) terminal-meta*))
                                          alts-)]
                  [((- a* ...) . alts) (minus? #'-)
                                       (g #'alts alts+
                                          (append alts- (parse-alts #'(a* ...) terminal-meta*)))]
                  [(x . alts)
                   (raise-syntax-error 'define-language
                                       "unrecognized production extension syntax, expected (+ prod ...) or (- prod ...)"
                                       #'x)]
                  [x (raise-syntax-error 'define-language
                                         "unrecognzied production extension list" #'x)])))))))
  
  (define parse-alts
    (lambda (alt* terminal-meta*)
      (define make-alt
        (lambda (syn pretty pretty-procedure?)
          (syntax-case syn ()
            [(s s* ...) (make-pair-alt #'(s s* ...) pretty pretty-procedure?)]
            [(s s* ... . sr) (make-pair-alt #'(s s* ... . sr) pretty pretty-procedure?)]
            [s
             (identifier? #'s)
             (if (memq (meta-var->raw-meta-var (syntax->datum #'s)) terminal-meta*)
                 (make-terminal-alt #'s pretty pretty-procedure? #f)
                 (make-nonterminal-alt #'s pretty pretty-procedure? #f))]
            [x (raise-syntax-error 'define-language "unrecognized production syntax" #'x)])))
      (let f ([alt* alt*])
        (syntax-parse alt*
          #:datum-literals (=> -> :)
          [() '()]
          [((=> syn pretty) . alt*)
           (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
          [(syn => pretty . alt*)
           (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
          [((-> syn prettyf) . alt*)
           #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
           (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
          [(syn -> prettyf . alt*)
           #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
           (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
          [(syn . alt*) (cons (make-alt #'syn #f #f) (f #'alt*))]
          [x (raise-syntax-error 'define-language "unrecognized production list" #'x)]))))
  
  (define parse-terms
    (lambda (id terms)
      (define (build-tspec t mvs maybe-handler)
        (make-tspec t (stx->list mvs) maybe-handler (syntax-property (format-id id "~a?" t #:source t)
                                                                     'original-for-check-syntax #t)))
      (let loop ([terms terms] [tspecs '()])
        (syntax-parse terms
          #:datum-literals (=>)
          [() tspecs]
          [((=> (t (tmeta* ...)) handler) . terms)
           (loop #'terms
                 (cons (build-tspec #'t #'(tmeta* ...) #'handler) tspecs))]
          [((t (tmeta* ...)) => handler . terms)
           (loop #'terms
                 (cons (build-tspec #'t #'(tmeta* ...) #'handler) tspecs))]
          [((t (tmeta* ...)) . terms) 
           (loop #'terms
                 (cons (build-tspec #'t #'(tmeta* ...) #f) tspecs))]
          [(x . terms) (raise-syntax-error 'define-language "unrecognized terminal syntax" #'x)]
          [x (raise-syntax-error 'define-language "unrecognized terminals list" #'x)]))))
  
  (define parse-language-and-finish
    (lambda (name ldef)
      (define parse-clauses
        (lambda (ldef)
          (let f ([ldef ldef] [base-lang #f] [found-entry #f]
                              [entry-ntspec #f] [first-ntspec #f] [terms '()] [ntspecs '()])
            (syntax-parse ldef
                #:literals (extends entry terminals)
              [() (values base-lang (if base-lang entry-ntspec (or entry-ntspec first-ntspec)) terms (reverse ntspecs))]
              [((extends ?L:id) . rest)
               (when base-lang
                 (raise-syntax-error 'define-language
                                     "only one extends clause allowed in language definition"
                                     #'(extends ?L) name))
               (f #'rest #'?L found-entry entry-ntspec first-ntspec terms ntspecs)]
              [((entry ?P:id) . rest)
               (when found-entry
                 (raise-syntax-error 'define-language
                                     "only one entry clause allowed in language definition"
                                     #'(entry ?P) entry-ntspec))
               (f #'rest base-lang #t #'?P first-ntspec terms ntspecs)]
              [((terminals ?t* ...) . rest)
               (f #'rest base-lang found-entry entry-ntspec first-ntspec 
                  (append terms #'(?t* ...)) ntspecs)]
              [((ntspec:id (meta*:id ...) a a* ...) . rest)
               (f #'rest base-lang found-entry
                  entry-ntspec
                  (if first-ntspec first-ntspec #'ntspec)
                  terms (cons (list* #'ntspec (stx->list #'(meta* ...)) #'a #'(a* ...)) ntspecs))]
              [(x . rest)
               (raise-syntax-error
                'define-language
                "expected nonterminal clause of the form (keyword (meta-var ...) production-clause ...)"
                #'x)]
              [x (raise-syntax-error 'define-language
                                     "unexpected define-language syntax, ending in improper list" #'x)]))))
      (let-values ([(base-lang entry-ntspec terms ntspecs) (parse-clauses ldef)])
        (if base-lang
            (let ([base-pair (lookup-language 'define-language
                                              "unrecognized base language name"
                                              base-lang)])
              (unless (and (pair? base-pair)
                           (language? (car base-pair))
                           (procedure? (cdr base-pair)))
                (raise-syntax-error 'define-language
                                    "unrecognized base language" base-lang x))
              (let ([base (car base-pair)])
                (let ([entry-ntspec (or entry-ntspec (language-entry-ntspec base))])
                  (finish entry-ntspec name name
                          (let-values ([(terms+ terms-) (partition-terms name terms)])
                            (let* ([tspecs (freshen-tspecs (language-tspecs base) terms-)]
                                   [tspecs (add-tspecs tspecs terms+)]
                                   [terminal-meta* (extract-terminal-metas tspecs)])
                              (let-values ([(ntspecs+ ntspecs-) (partition-ntspecs ntspecs terminal-meta*)])
                                (let* ([ntspecs (freshen-ntspecs (language-ntspecs base) ntspecs-)]
                                       [ntspecs (add-ntspecs ntspecs ntspecs+)])
                                  (make-language name entry-ntspec tspecs ntspecs)))))))))
            (let* ([tspecs (parse-terms name terms)]
                   [terminal-meta* (extract-terminal-metas tspecs)])
              (finish entry-ntspec name name
                      (make-language name
                                     entry-ntspec
                                     tspecs
                                     (map (lambda (ntspec)
                                            (make-ntspec (car ntspec) (cadr ntspec)
                                                         (parse-alts (cddr ntspec) terminal-meta*)))
                                          ntspecs))))))))
  
  (define escape-pattern
    (lambda (x)
      (syntax-case x (...)
        [... #'(... (... ...))]
        [(a . d) (with-syntax ([a (escape-pattern #'a)]
                               [d (escape-pattern #'d)])
                   #'(a . d))]
        [() #'()]
        [id  (identifier? #'id) #'id])))
  
  (define (finish ntname lang* id desc) ; constructs the output
    (define lang (syntax-property lang* 'original-for-check-syntax #t))
    (annotate-language! desc id)
    (with-syntax ([(records ...) (language->lang-records desc)]
                  [(predicates ...) (language->lang-predicates desc id)]
                  [unparser-name (syntax-property (format-id id "unparse-~a" lang #:source lang)
                                                  'original-for-check-syntax #t)]
                  [unparser-out-name (language-unparser desc)]
                  [meta-parser (make-meta-parser desc)]
                  [(tspec-preds ...) (map tspec-pred (language-tspecs desc))])
      (define stx
        #`(begin
            (define (unparser-out-name lang port mode)
              (define-unparser unparser-name #,lang)
              (define unparsed (format "#(language:~a ~a)"
                                       '#,lang
                                       (unparser-name lang)))
              (cond [(eq? mode #f) (display unparsed port)]
                    [(eq? mode #t) (display unparsed port)]
                    [else          (display unparsed port)]))
            records ...
            predicates ...
            (define-syntax #,lang
              (cons
               ($make-language
                #'#,(language-name desc)
                #'#,(language-entry-ntspec desc)
                (list
                 #,@(map (lambda (tspec)
                           #`(make-tspec
                              #'#,(tspec-type tspec)
                              (list #,@(map (lambda (mv) #`#'#,mv) (tspec-meta-vars tspec)))
                              #,(let ([t (tspec-handler tspec)])
                                  (and t #`#'#,t))
                              #'#,(tspec-pred tspec)))
                         (language-tspecs desc)))
                (list
                 #,@(map (lambda (ntspec)
                           #`($make-ntspec
                              #'#,(ntspec-name ntspec)
                              (list #,@(map (lambda (mv) #`#'#,mv) (ntspec-meta-vars ntspec)))
                              (list #,@(map (lambda (alt)
                                              (cond
                                                [(pair-alt? alt)
                                                 #`($make-pair-alt
                                                    #'#,(escape-pattern (alt-syn alt))
                                                    #,(let ([t (alt-pretty alt)])
                                                        (and t 
                                                             (if (alt-pretty-procedure? alt)
                                                                 #`#'#,(alt-pretty alt)
                                                                 #`#'#,(escape-pattern
                                                                        (alt-pretty alt)))))
                                                    #,(alt-pretty-procedure? alt)
                                                    '#,(pair-alt-pattern alt)
                                                    #,(let ([t (pair-alt-field-names alt)])
                                                        (and t
                                                             #`(list #,@(map (lambda (id) #`#'#,id)
                                                                             t))))
                                                    '#,(pair-alt-field-levels alt)
                                                    '#,(pair-alt-field-maybes alt)
                                                    #,(pair-alt-implicit? alt)
                                                    #,(pair-alt-tag alt)
                                                    #'#,(pair-alt-pred alt)
                                                    #'#,(pair-alt-maker alt)
                                                    #,(let ([t (pair-alt-accessors alt)])
                                                        (and t
                                                             #`(list #,@(map (lambda (id) #`#'#,id)
                                                                             t))))
                                                    #'#,(pair-alt-name alt))]
                                                [(terminal-alt? alt)
                                                 #`(make-terminal-alt
                                                    #'#,(escape-pattern (alt-syn alt))
                                                    #,(let ([t (alt-pretty alt)])
                                                        (and t 
                                                             (if (alt-pretty-procedure? alt)
                                                                 #`#'#,(alt-pretty alt)
                                                                 #`#'#,(escape-pattern
                                                                        (alt-pretty alt)))))
                                                    #,(alt-pretty-procedure? alt)
                                                    #'#,(terminal-alt-type alt))]
                                                [(nonterminal-alt? alt)
                                                 #`(make-nonterminal-alt
                                                    #'#,(escape-pattern (alt-syn alt))
                                                    #,(let ([t (alt-pretty alt)])
                                                        (and t 
                                                             (if (alt-pretty-procedure? alt)
                                                                 #`#'#,(alt-pretty alt)
                                                                 #`#'#,(escape-pattern
                                                                        (alt-pretty alt)))))
                                                    #,(alt-pretty-procedure? alt)
                                                    #'#,(nonterminal-alt-name alt))]))
                                            (ntspec-alts ntspec)))
                              #,(ntspec-tag ntspec)
                              #'#,(ntspec-pred ntspec)
                              #'#,(ntspec-all-pred ntspec)
                              #,(let ([t (ntspec-all-term-pred ntspec)])
                                  (and t #`#'#,t))
                              '#,(ntspec-all-tag ntspec)
                              #'#,(ntspec-struct-name ntspec)))
                         (language-ntspecs desc)))
                #'#,(language-struct desc)
                #'#,(language-unparser desc)
                #,(language-tag-mask desc))
               meta-parser))
            ;(define-property #,lang meta-parser-property meta-parser)
            (define-unparser unparser-name #,lang)
            (void)))
      (syntax-property stx
                       'mouse-over-tooltips
                       (vector lang
                               (- (syntax-position lang) 1)
                               (+ (syntax-position lang)
                                  (syntax-span lang))
                               (format "Language ~a:~n~a"
                                       (syntax-e lang)
                                       (pretty-format/write
                                        (language->s-expression-internal desc)))))))

  (syntax-case x ()
    [(_ ?L ?rest ...)
     (identifier? #'?L)
     (parse-language-and-finish #'?L #'(?rest ...))]))

(define-syntax (language->s-expression x)
  (define who 'language->s-expression)
  (define (doit lang handler?)
    (define (tspec->s-expression t)
      (if (and handler? (tspec-handler t))
          #`(=> (#,(tspec-type t) #,(tspec-meta-vars t))
                #,(tspec-handler t))
          #`(#,(tspec-type t) #,(tspec-meta-vars t))))
    (define (alt->s-expression a)
      (if (and handler? (alt-pretty a))
          #`(=> #,(alt-syn a) #,(alt-pretty a))
          (alt-syn a)))
    (define (ntspec->s-expression p)
      #`(#,(ntspec-name p) #,(ntspec-meta-vars p)
          #,@(map alt->s-expression (ntspec-alts p))))
    (let ([lang-pair (lookup-language 'language->s-expression
                                      "unrecognized language name" lang)])
      (let ([lang (car lang-pair)])
        #`'(define-language #,(language-name lang)
             (entry #,(language-entry-ntspec lang))
             (terminals #,@(map tspec->s-expression (language-tspecs lang)))
             #,@(map ntspec->s-expression (language-ntspecs lang))))))
  (syntax-case x ()
    [(_ lang) (identifier? #'lang) (doit #'lang #f)]
    [(_ lang handler?) (identifier? #'lang) (doit #'lang (syntax->datum #'handler?))]))

(define-syntax diff-languages
  (lambda (x)
    (define who 'diff-languages)
    (define combine
      (lambda (same removed added)
        (if (null? removed)
            (if (null? added)
                '()
                #`((+ #,@added)))
            (if (null? added)
                #`((- #,@removed))
                #`((- #,@removed) (+ #,@added))))))
    (define tspec->syntax
      (lambda (tspec)
        #`(#,(tspec-type tspec) #,(tspec-meta-vars tspec))))
    (define ntspec->syntax
      (lambda (ntspec)
        #`(#,(ntspec-name ntspec) #,(ntspec-meta-vars ntspec) #,@(map alt-syn (ntspec-alts ntspec)))))
    (define diff-meta-vars
      (lambda (mv0* mv1*)
        mv1*
        #;(let f ([mv0* mv0*] [mv1* mv1*] [same '()] [removed '()] [added '()])
            (cond
              [(and (null? mv0*) (null? mv1*)) (combine same removed added)]
              [(null? mv0*) (f mv0* (cdr mv1*) same removed (cons (car mv1*) added))]
              [else
               (let* ([mv0 (car mv0*)] [mv0-sym (syntax->datum mv0)])
                 (cond
                   [(findf (lambda (mv1) (eq? (syntax->datum mv1) mv0-sym)) mv1*)
                    =>
                    (lambda (mv1) (f (cdr mv0*) (remq mv1 mv1*) (cons mv1 same) removed added))]
                   [else (f (cdr mv0*) mv1* same (cons mv0 removed) added)]))]))))
    (define diff-terminals
      (lambda (t0* t1*)
        (let f ([t0* t0*] [t1* t1*] [same '()] [removed '()] [added '()])
          (cond
            [(and (null? t0*) (null? t1*)) (combine same removed added)]
            [(null? t0*) (f t0* (cdr t1*) same removed (cons (tspec->syntax (car t1*)) added))]
            [else
             (let* ([t0 (car t0*)] [t0-type (tspec-type t0)] [t0-type-sym (syntax->datum t0-type)])
               (cond
                 [(findf (lambda (t1) (eq? (syntax->datum (tspec-type t1)) t0-type-sym)) t1*)
                  =>
                  (lambda (t1)
                    (with-syntax ([(meta-vars ...) (diff-meta-vars (tspec-meta-vars t0) (tspec-meta-vars t1))])
                      (f (cdr t0*) (remq t1 t1*) (cons #`(#,t0-type (meta-vars ...)) same) removed added)))]
                 [else (f (cdr t0*) t1* same (cons (tspec->syntax t0) removed) added)]))]))))
    (define diff-alts
      (lambda (a0* a1*)
        (let f ([a0* a0*] [a1* a1*] [same '()] [removed '()] [added '()])
          (cond
            [(and (null? a0*) (null? a1*)) (combine same removed added)]
            [(null? a0*) (f a0* (cdr a1*) same removed (cons (alt-syn (car a1*)) added))]
            [else
             (let* ([a0 (car a0*)] [a0-syn (alt-syn a0)] [a0-syn-s-expr (syntax->datum a0-syn)])
               (cond
                 [(findf (lambda (a1) (equal? (syntax->datum (alt-syn a1)) a0-syn-s-expr)) a1*)
                  =>
                  (lambda (a1) (f (cdr a0*) (remq a1 a1*) (cons a0-syn same) removed added))]
                 [else (f (cdr a0*) a1* same (cons (alt-syn a0) removed) added)]))]))))
    (define diff-nonterminals
      (lambda (nt0* nt1*)
        (let f ([nt0* nt0*] [nt1* nt1*] [updated '()])
          (cond
            [(and (null? nt0*) (null? nt1*)) updated]
            [(null? nt0*)
             (f nt0* (cdr nt1*)
                (let ([nt1 (car nt1*)])
                  (cons #`(#,(ntspec-name nt1) #,(ntspec-meta-vars nt1) (+ #,@(map alt-syn (ntspec-alts nt1))))
                        updated)))]
            [else
             (let* ([nt0 (car nt0*)] [nt0-name (ntspec-name nt0)] [nt0-name-sym (syntax->datum nt0-name)])
               (cond
                 [(findf (lambda (nt1) (eq? (syntax->datum (ntspec-name nt1)) nt0-name-sym)) nt1*)
                  =>
                  (lambda (nt1)
                    (f (cdr nt0*) (remq nt1 nt1*)
                       (let ([alts (diff-alts (ntspec-alts nt0) (ntspec-alts nt1))])
                         (syntax-case alts ()
                           [() updated]
                           [(alts ...)
                            (with-syntax ([(meta-vars ...) (diff-meta-vars (ntspec-meta-vars nt0) (ntspec-meta-vars nt1))])
                              (cons #`(#,nt0-name (meta-vars ...) alts ...) updated))]))))]
                 [else (f (cdr nt0*)
                          nt1*
                          (cons #`(#,nt0-name #,(ntspec-meta-vars nt0) (- #,@(map alt-syn (ntspec-alts nt0))))
                                updated))]))]))))
    (syntax-case x ()
      [(_ lang0 lang1)
       (let ([l0-pair (lookup-language 'diff-language "unrecognized base language name" #'lang0)]
             [l1-pair (lookup-language 'diff-language "unrecognized target language name" #'lang1)])
         (let ([l0 (car l0-pair)] [l1 (car l1-pair)])
           (with-syntax ([l1-entry (language-entry-ntspec l1)]
                         [(term ...) (diff-terminals (language-tspecs l0) (language-tspecs l1))]
                         [(nonterm ...) (diff-nonterminals (language-ntspecs l0) (language-ntspecs l1))])
             (syntax-case #'(term ...) ()
               [() #''(define-language lang1 (extends lang0)
                        (entry l1-entry)
                        nonterm ...)]
               [(term ...) #''(define-language lang1 (extends lang0)
                                (entry l1-entry)
                                (terminals term ...)
                                nonterm ...)]))))])))

(define-syntax prune-language
  (lambda (x)
    (define who 'prune-language)
    (syntax-case x ()
      [(_ L)
       (let ([l-pair (lookup-language 'prune-language "unrecognized language name" #'L)])
         (let ([l (car l-pair)])
           (with-syntax ([((ts ...) (nts ...)) (prune-language-helper l)]
                         [entry-nt (language-entry-ntspec l)])
             (syntax-case #'(ts ...) ()
               [() #''(define-language L
                        (entry entry-nt)
                        nts ...)]
               [(ts ...) #''(define-language L
                              (entry entry-nt)
                              (terminals ts ...)
                              nts ...)]))))])))

(define-syntax define-pruned-language
  (lambda (x)
    (define who 'define-pruned-language)
    (syntax-case x ()
      [(_ L new-name)
       (let ([l-pair (lookup-language 'define-pruned-language "unrecognized language name" #'L)])
         (unless l-pair (raise-syntax-error who "language not found" #'L))
         (let ([l (car l-pair)])
           (with-syntax ([((ts ...) (nts ...)) (prune-language-helper l)]
                         [entry-nt (language-entry-ntspec l)])
             #'(define-language new-name
                 (entry entry-nt)
                 (terminals ts ...)
                 nts ...))))])))
