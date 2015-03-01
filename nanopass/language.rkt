#lang racket
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
                      syntax/stx
                      syntax/parse
                      "helpers.rkt"
                      "language-helpers.rkt"
                      "records.rkt"
                      "meta-parser.rkt"))

(define-syntax define-language
  (lambda (x) 
    ;; Doing a little extra work here to make sure that we are able to track
    ;; errors.  The basic idea is that we want to go through the list of
    ;; existing tspecs, and when we keep them, make a new copy (so that
    ;; language specific information can be updated in them), and when they
    ;; are being removed, we "mark" that we found the one to remove by
    ;; pulling it out of our removal list.  If any remain in the removal
    ;; list when we're done, we complain about it.
    (define freshen-objects
      (lambda (o=? fresh-o msg unpacker)
        (rec f
          (lambda (os os-)
            (cond
              [(and (null? os) (not (null? os-)))
               (raise-syntax-error 'define-language msg (map unpacker os-))]
              [(null? os) '()]
              [else
               (let g ([os- os-] [o (car os)] [checked-os- '()])
                 (cond
                   [(null? os-) (cons (fresh-o o) (f (cdr os) checked-os-))]
                   [(o=? o (car os-))
                    (f (cdr os) (append checked-os- (cdr os-)))]
                   [else (g (cdr os-) o (cons (car os-) checked-os-))]))])))))

    (define freshen-tspecs
      (freshen-objects tspec=? values "unrecognized tspecs" tspec-type))
    (define freshen-alts
      (freshen-objects alt=? fresh-alt "unrecognized alts" alt-syn))

    (define add-objects
      (lambda (o=? msg)
        (letrec ([f (lambda (os os+)
                      (if (null? os+)
                          os
                          (let ([o+ (car os+)])
                            (when (memf (lambda (x) (o=? o+ x)) os)
                              (raise-syntax-error 'define-language msg o+))
                            (f (cons o+ os) (cdr os+)))))])
          f)))

    (define add-tspecs (add-objects tspec=? "duplicate tspec in add"))
    (define add-alts (add-objects alt=? "duplicate alt in add"))

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
                   (append terms- (parse-terms id #'ts.)))]))))

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
                    [((+ a* ...) alts ...) (plus? #'+)
                     (g #'(alts ...) (append alts+ (parse-alts #'(a* ...) terminal-meta*))
                       alts-)]
                    [((- a* ...) alts ...) (minus? #'-)
                     (g #'(alts ...) alts+
                       (append alts- (parse-alts #'(a* ...) terminal-meta*)))])))))))

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
                   (make-nonterminal-alt #'s pretty pretty-procedure? #f))])))
        (let f ([alt* alt*])
          (syntax-parse alt*
            [() '()]
            [((=> syn pretty) . alt*)
             #:when (double-arrow? #'=>)
             (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
            [(syn => pretty . alt*)
             #:when (double-arrow? #'=>)
             (cons (make-alt #'syn #'pretty #f) (f #'alt*))]
            [((-> syn prettyf) . alt*)
             #:when (arrow? #'->)
             #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
             (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
            [(syn -> prettyf . alt*)
             #:when (arrow? #'->)
             #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
             (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
            [(syn . alt*) (cons (make-alt #'syn #f #f) (f #'alt*))]
            [_ (raise-syntax-error 'define-language "unexpected alt" alt*)]))))

    (define parse-terms
      (lambda (id terms)
        (define (build-tspec t mvs maybe-handler)
          (make-tspec t (stx->list mvs) maybe-handler (format-id id "~a?" t)))
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
                   (cons (build-tspec #'t #'(tmeta* ...) #f) tspecs))]))))

    (define parse-language-and-finish
      (lambda (name ldef)
        (define parse-clauses
          (lambda (ldef)
            (let f ([ldef ldef] [base-lang #f] [found-entry #f]
                    [entry-ntspec #f] [first-ntspec #f] [terms '()] [ntspecs '()])
              (syntax-case ldef (extends entry terminals)
                [() (values base-lang (if base-lang entry-ntspec (or entry-ntspec first-ntspec)) terms (reverse ntspecs))]
                [((extends ?L) . rest)
                 (identifier? #'?L)
                 (begin
                   (when base-lang
                     (raise-syntax-error 'define-language
                       "only one extends clause allowed in language definition"
                       #'(extends ?L) name))
                   (f #'rest #'?L found-entry entry-ntspec first-ntspec terms ntspecs))]
                [((entry ?P) . rest)
                 (identifier? #'?P)
                 (begin
                   (when found-entry
                     (raise-syntax-error 'define-language
                       "only one entry clause allowed in language definition"
                       #'(entry ?P) entry-ntspec))
                   (f #'rest base-lang #t #'?P first-ntspec terms ntspecs))]
                [((terminals ?t* ...) . rest)
                 (f #'rest base-lang found-entry entry-ntspec first-ntspec 
                   (append terms #'(?t* ...)) ntspecs)]
                [((ntspec (meta* ...) a a* ...) . rest)
                 (and (identifier? #'ntspec) (andmap identifier? (stx->list #'(meta* ...))))
                 (f #'rest base-lang found-entry
                   entry-ntspec
                   (if first-ntspec first-ntspec #'ntspec)
                   terms (cons (list* #'ntspec (stx->list #'(meta* ...)) #'a #'(a* ...)) ntspecs))]
                [(x . rest) (raise-syntax-error 'define-language "unrecognized clause" #'x)]
                [x (raise-syntax-error 'define-language
                     "unrecognized rest of language clauses" #'x)]))))
        (let-values ([(base-lang entry-ntspec terms ntspecs) (parse-clauses ldef)])
          (if base-lang
              (let ([base-pair (syntax-local-value base-lang)])
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

    (define finish 
      (lambda (ntname lang id desc) ; constructs the output
        (annotate-language! desc id)
        (with-syntax ([(records ...) (language->lang-records desc)]
                      [(predicates ...) (language->lang-predicates desc id)]
                      [unparser-name (format-id id "unparse-~a" lang)]
                      [meta-parser (make-meta-parser desc)])
          (with-syntax ([(tspec-preds ...) (map tspec-pred (language-tspecs desc))])
            #;(pretty-print (list 'unparser (syntax->datum lang) (syntax->datum #'unparser)))
            #;(pretty-print (list 'meta-parser (syntax->datum lang) (syntax->datum #'meta-parser)))
            (let ([stx #`(begin
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
                                                                        #'#,(pair-alt-name alt))]
                                                                   [(terminal-alt? alt)
                                                                    #`(make-terminal-alt
                                                                        #'#,(escape-pattern (alt-syn alt))
                                                                        #,(let ([t (alt-pretty alt)])
                                                                            (and t 
                                                                                 (if (alt-pretty-procedure? alt)
                                                                                     #`#'#,(alt-pretty alt)
                                                                                     #`#'#,(escape-pattern (alt-pretty alt)))))
                                                                        #,(alt-pretty-procedure? alt)
                                                                        #'#,(terminal-alt-type alt))]
                                                                   [(nonterminal-alt? alt)
                                                                    #`(make-nonterminal-alt
                                                                        #'#,(escape-pattern (alt-syn alt))
                                                                        #,(let ([t (alt-pretty alt)])
                                                                            (and t 
                                                                                 (if (alt-pretty-procedure? alt)
                                                                                     #`#'#,(alt-pretty alt)
                                                                                     #`#'#,(escape-pattern (alt-pretty alt)))))
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
                                 #,(language-tag-mask desc))
                               meta-parser))
                           ;(define-property #,lang meta-parser-property meta-parser)
                           (define-unparser unparser-name #,lang)
                           ;(printf "testing preds:\n")
                           ;(begin (printf "~s:\n" 'tspec-preds) (tspec-preds 'a)) ...
                           (void))])
                 #;(pretty-print (syntax->datum stx))
                 stx)))))

    (syntax-case x ()
      [(_ ?L ?rest ...)
       (identifier? #'?L)
       (parse-language-and-finish #'?L #'(?rest ...))])))

(define-syntax language->s-expression
  (lambda (x)
    (define who 'language->s-expression)
    (define doit
      (lambda (lang handler?)
        (define tspec->s-expression
          (lambda (t)
            (if (and handler? (tspec-handler t))
                #`(=> (#,(tspec-type t) #,(tspec-meta-vars t))
                    #,(tspec-handler t))
                #`(#,(tspec-type t) #,(tspec-meta-vars t)))))
        (define alt->s-expression
          (lambda (a)
            (if (and handler? (alt-pretty a))
                #`(=> #,(alt-syn a) #,(alt-pretty a))
                (alt-syn a))))
        (define ntspec->s-expression
          (lambda (p)
            #`(#,(ntspec-name p) #,(ntspec-meta-vars p)
                #,@(map alt->s-expression (ntspec-alts p)))))
        (let ([lang-pair (syntax-local-value lang)])
          (unless lang-pair (raise-syntax-error who "language not found" lang))
          (let ([lang (car lang-pair)])
            #`'(define-language #,(language-name lang)
                 (entry #,(language-entry-ntspec lang))
                 (terminals #,@(map tspec->s-expression (language-tspecs lang)))
                 #,@(map ntspec->s-expression (language-ntspecs lang)))))))
    (syntax-case x ()
      [(_ lang) (identifier? #'lang) (doit #'lang #f)]
      [(_ lang handler?) (identifier? #'lang) (doit #'lang (syntax->datum #'handler?))])))

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
               [(findf (lambda (mv1) (eq? (syntax->datum mv1) mv0-sym)) mv1*) =>
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
               [(findf (lambda (t1) (eq? (syntax->datum (tspec-type t1)) t0-type-sym)) t1*) =>
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
               [(findf (lambda (a1) (equal? (syntax->datum (alt-syn a1)) a0-syn-s-expr)) a1*) =>
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
               [(findf (lambda (nt1) (eq? (syntax->datum (ntspec-name nt1)) nt0-name-sym)) nt1*) =>
                (lambda (nt1)
                  (f (cdr nt0*) (remq nt1 nt1*)
                    (let ([alts (diff-alts (ntspec-alts nt0) (ntspec-alts nt1))])
                      (syntax-case alts ()
                        [() updated]
                        [(alts ...)
                         (with-syntax ([(meta-vars ...) (diff-meta-vars (ntspec-meta-vars nt0) (ntspec-meta-vars nt1))])
                           (cons #`(#,nt0-name (meta-vars ...) alts ...) updated))]))))]
               [else (f (cdr nt0*) nt1* (cons #`(#,nt0-name #,(ntspec-meta-vars nt0) (- #,@(map alt-syn (ntspec-alts nt0)))) updated))]))]))))
  (syntax-case x ()
    [(_ lang0 lang1)
     (let ([l0-pair (syntax-local-value #'lang0)] [l1-pair (syntax-local-value #'lang1)])
       (unless l0-pair (raise-syntax-error who "language not found" #'lang0))
       (unless l1-pair (raise-syntax-error who "language not found" #'lang1))
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
       (let ([l-pair (syntax-local-value #'L)])
         (unless l-pair (raise-syntax-error who "language not found" #'L))
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
       (let ([l-pair (syntax-local-value #'L)])
         (unless l-pair (raise-syntax-error who "language not found" #'L))
         (let ([l (car l-pair)])
           (with-syntax ([((ts ...) (nts ...)) (prune-language-helper l)]
                          [entry-nt (language-entry-ntspec l)])
             #'(define-language new-name
                 (entry entry-nt)
                 (terminals ts ...)
                 nts ...))))])))

#|

(require syntax/parse)

(define-syntax (new-define-language x)
  (define-who 'new-define-language)
  (struct form (meta-vars name))
  (struct terminal form (pretty-printer pred meta-pred))
  (struct nonterminal form (productions-syntax (productions #:mutable)))

  (define terminal-table (make-hasheq))
  (define meta-var-table (make-hasheq))
  (define nonterminal-table (make-hasheq))

  (define (add-terminal! tid t meta-vars pretty-printer)
    (let ([tsym (syntax->datum t)])
      (when (hash-ref terminal-table tsym #f)
        (raise-syntax-error who "duplicate terminal found" t #f
          (list (form-name (hash-ref terminal-table tsym)))))
      (let ([term (terminal meta-vars t pretty-printer
                            (format-id tid "~a?" t)
                            (format-id tid "meta-~a?" t))])
        (hash-set! terminal-table tsym term)
        (for-each
          (lambda (meta-var) (add-meta-var! meta-var term))
          (syntax->datum meta-vars)))))

  (define (add-nonterminal! tid name meta-vars alts)
    (let ([namesym (syntax->datum t)])
      (when (hash-ref nonterminal-table namesym #f)
        (raise-syntax-error who "duplicate nonterminal found" name #f
          (list (form-name (hash-ref nonterminal-table namesym)))))
      (let ([nt (nonterminal meta-vars name alts #f)])
        (hash-set! nonterminal-table namesym nt)
        (for-each
          (lambda (meta-var) (add-meta-var! meta-var nt))
          (syntax->datum meta-vars)))))

  (define (add-meta-var! mv form)
    (when (hash-ref meta-var-table mv #f)
      (raise-syntax-error who "duplicate meta-variable found"
        (form-name form) #f (list (form-name (hash-ref meta-var-table mv)))))
    (hash-set! meta-var-table mv thing))

  (define parse-alts
    (lambda (alts)
      (let f ([alt* alt*])
        (syntax-parse alt*
          #:datum-literals (=> ->)
          [() '()]
          [((=> syn pretty) . rest)
           #:with rest (f #'rest)
           #:with alt (syn->alt #'syn #'pretty #f)
           #'(alt . rest)]
          [(syn => pretty . rest)
           #:with rest (f #'rest)
           #:with alt (syn->alt #'syn #'pretty #f)
           #'(alt . rest)]
          [((-> syn prettyf) . rest)
           #:with rest (f #'rest)
           #:with alt (syn->alt #'syn #'
           #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
           #'(alt . rest)
           (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
          [(syn -> prettyf . rest)
           #:with rest (f #'rest)
           #:with with-extended-quasiquote (datum->syntax #'-> 'with-extended-quasiquote)
           (cons (make-alt #'syn #'(with-extended-quasiquote prettyf) #t) (f #'alt*))]
          [(syn . rest)
           #:with rest (f #'rest)
           (cons (make-alt #'syn #f #f) (f #'alt*))]
          [_ (raise-syntax-error 'define-language "unexpected alt" alt*)]))))
  (define (parse-terms! tid terms)
    (let loop ([terms terms])
      (syntax-parse terms
        #:datum-literals (=>)
        [() (void)]
        [((=> (t:id (tmetas:id ...)) pretty-printer:expr) . rest)
         (add-terminal! tid #'t #'(tmetas ...) #'pretty-printer)
         (loop #'rest)]
        [((t:id (tmetas:id ...)) => pretty-printer:expr . rest)
         (add-terminal! tid #'t #'(tmetas ...) #'pretty-printer)
         (loop #'rest)]
        [((t:id (tmetas:id ...)) . rest) 
         (add-terminal! tid #'t #'(tmetas ...) #f)
         (loop #'rest)])))
  (define (parse-clauses clauses)
    (let f ([clauses clauses] [base-lang #f] [entry-ntspec #f] [first-ntspec #f] [terms '()] [nts '()])
      (syntax-parse clauses
        #:literals (extends entry terminals)
        #:context x
        [() (values base-lang (if base-lang entry-ntspec (or entry-ntspec first-ntspec)) terms)]
        [((extends ?L:id) . rest)
         (when base-lang
           (raise-syntax-error 'define-language
             "only one extends clause allowed in language definition"
             #'(extends ?L) name))
         (f #'rest #'?L entry-ntspec first-ntspec terms)]
        [((entry ?P:id) . rest)
         (when entry-ntspec
           (raise-syntax-error 'define-language
             "only one entry clause allowed in language definition"
             #'(entry ?P) entry-ntspec))
         (f #'rest base-lang #'?P first-ntspec terms)]
        [((terminals ?t* ...) . rest)
         (f #'rest base-lang entry-ntspec first-ntspec (append terms #'(?t* ...)))]
        [((ntspec:id (metas:id ...) . desc) . rest)
         (f #'rest base-lang entry-ntspec (or first-ntspec #'ntspec) terms
            #`((ntspec (mats ...) . desc) . #,nts))]
        [(x . rest) (raise-syntax-error 'define-language "unexpected clause" #'x)]
        [x (raise-syntax-error 'define-language "unexpected syntax" #'x)])))
  (define-syntax with-values
    (syntax-rules ()
      [(_ e0 e1) (call-with-values (lambda () e0) e1)]))
  (syntax-case x ()
    [(_ ?L:id . ?clauses)
     (with-values (parse-clauses #'?clauses)
       (lambda (base-lang 

|# 
