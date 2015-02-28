#lang racket
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

;;; TODO:
;;; 1. write make-processors (based on make-processor, currently in meta-parsers
;;; 2. add else clause to processors
;;; Make sure the following are obeyed:
;;; 1. allow ir to be named
;;; 2. loosen up form of pass body
;;; 3. don't require () in pass body
;;; 4. add else clause
;;; 5. support Datum output
;;; 6. don't bind quasiquote with Datum output
;;; 7. make cata work with Datum output

(provide define-pass trace-define-pass echo-define-pass with-output-language nanopass-case)

(require (for-syntax racket/list
                     racket/syntax
                     syntax/stx
                     syntax/parse
                     "helpers.rkt"
                     "records.rkt"
                     "syntaxconvert.rkt"
                     "meta-parser.rkt"
                     "pass-helper.rkt")
         (only-in "helpers.rkt"
                  nanopass-record-tag trace-let))

;; NOTE: the following is less general then the with-output-language because it does not
;; support multiple return values.  It also generates nastier code for the expander to deal
;; with, though cp0 should clean it up.  It is possible that in the long run, we'll want to
;; have a separate pass-lambda form, or that we'll loosen up the body further to return
;; multiple values even when they aren't specified. For now, this is moth-balled.
#;(define-syntax with-output-language
(lambda (x)
  (syntax-case x ()
    [(k (lang type) b b* ...)
      (with-syntax ([pass (datum->syntax #'k 'pass)])
        #'(let ()
            (define-pass pass : * () -> (lang type) () (begin b b* ...))
            (pass)))]
    [(k lang b b* ...)
      (with-syntax ([pass (datum->syntax #'k 'pass)])
        #'(let ()
            (define-pass pass : * () -> lang  () (begin b b* ...))
            (pass)))])))

(define-syntax (with-output-language x)
  (syntax-parse x
    [(id (lang type) b b* ...)
     #:with in-context (datum->syntax #'id 'in-context)
     #:with quasiquote (datum->syntax #'id 'quasiquote)
     (let* ([olang-pair (syntax-local-value #'lang)]
            [olang (and olang-pair (car olang-pair))]
            [meta-parser (and olang-pair (cdr olang-pair))])
       (unless (language? olang)
         (raise-syntax-error 'with-output-language "unrecognized language" #'lang))
       (unless (procedure? meta-parser)
         (raise-syntax-error 'with-output-language "missing meta parser for language" #'lang))
       #`(let-syntax ([quasiquote '#,(make-quasiquote-transformer
                                      #'id #'type olang
                                      meta-parser)]
                      [in-context '#,(make-in-context-transformer
                                      #'id olang
                                      meta-parser)])
           b b* ...))]
      [(id lang b b* ...)
       #:with in-context (datum->syntax #'id 'in-context)
       (let* ([olang-pair (syntax-local-value #'lang)]
              [olang (and olang-pair (car olang-pair))]
              [meta-parser (and olang-pair (cdr olang-pair))])
         (unless (language? olang)
           (raise-syntax-error 'with-output-language "unrecognized language" #'lang))
         (unless (procedure? meta-parser)
           (raise-syntax-error 'with-output-language "missing meta parser for language" #'lang))
         #`(let-syntax
               ([in-context '#,(make-in-context-transformer #'id olang
                                                            meta-parser)])
             b b* ...))]))

(define-syntax (nanopass-case x)
  ; (nanopass-case (lang type) id ---) rebinds id so that it always holds the
  ; current ir even through cata recursion
  (syntax-parse x
    #:literals (else)
    [(k (lang type) x:id cl ... [else b0 b1 ...])
     #:with quasiquote (datum->syntax #'k 'quasiquote) ; if we were in a rhs, pick-up the output quasiquote
     #'(let ()
         (define-pass p : (lang type) (x) -> * (val)
           (proc : type (x) -> * (val) cl ... [else b0 b1 ...])
           (proc x))
         (p x))]
     [(k (lang type) e cl ... [else b0 b1 ...])
      #'(let ([ir e]) (k (lang type) ir cl ... [else b0 b1 ...]))]
     [(k (lang type) e cl ...)
      #`(k (lang type) e cl ...
           [else (error 'nanopass-case
                   ; TODO: we were using $strip-wrap here, should be something like
                   ; $make-source-oops, but at least pseudo r6rs portable if possible
                   #,(let ([si (syntax->source-info x)])
                       (if si
                           (format "empty else clause hit ~s ~a"
                             (maybe-syntax->datum x) si)
                           (format "empty else clause hit ~s"
                             (maybe-syntax->datum x)))))])]))

(define-syntax trace-define-pass
  (lambda (x)
    (define unparser
      (lambda (lang)
        (cond
          [(eq? (maybe-syntax->datum lang) '*) #f]
          [(identifier? lang) (format-id lang "unparse-~a" lang)]
          [else (syntax-case lang ()
                  [(lang type) (format-id #'lang "unparse-~a" #'lang)])])))
    (syntax-case x ()
      [(_ name ?colon ilang (id ...) ?arrow olang (xtra ...) . body)
       (and (identifier? #'name) (eq? (datum ?arrow) '->) (eq? (datum ?colon) ':)
            (andmap identifier? (syntax->list #'(id ...))))
       (let ([iunparser (unparser #'ilang)] [ounparser (unparser #'olang)])
         #`(define name
             (lambda (id ...)
               (define-pass name ?colon ilang (id ...) ?arrow olang (xtra ...) . body)
               (let ([tpass name])
                 #,(if iunparser
                       (if ounparser
                           (with-syntax ([(ot xvals ...) (generate-temporaries #'(name xtra ...))]
                                         [(tid xargs ...) (generate-temporaries #'(id ...))]
                                         [(id id* ...) #'(id ...)])
                             #`(let ([result #f])
                                 (trace-let name ([tid (#,iunparser id #t)] [xargs id*] ...)
                                   (let-values ([(ot xvals ...) (tpass id id* ...)])
                                     (set! result (list ot xvals ...))
                                     (values (#,ounparser ot #t) xvals ...)))
                                 (apply values result)))
                           (with-syntax ([(xvals ...) (generate-temporaries #'(xtra ...))]
                                         [(tid xargs ...) (generate-temporaries #'(id ...))]
                                         [(id id* ...) #'(id ...)])
                             #`(trace-let name ([tid (#,iunparser id #t)] [xargs id*] ...)
                                 (tpass id id* ...))))
                       (if ounparser
                           (with-syntax ([(ot xvals ...) (generate-temporaries #'(name xtra ...))])
                             #`(let ([result #f])
                                 (trace-let name ([id id] ...)
                                   (let-values ([(ot xvals ...) (tpass id ...)])
                                     (set! result (list ot xvals ...))
                                     (values (#,ounparser ot #t) xvals ...)))
                                 (apply values result)))
                           #`(trace-let name ([id id] ...)
                               (tpass id ...))))))))])))

(define-syntax define-pass
  (syntax-rules ()
    [(_ . more) (x-define-pass . more)]))

(define-syntax echo-define-pass
  (lambda (x)
    (syntax-case x ()
      [(_ name ?colon ilang (fml ...) ?arrow olang (xval ...) . more)
       (and (identifier? #'name)
            (eq? (datum ?colon) ':)
            (or (identifier? #'ilang)
                (syntax-case #'ilang ()
                  [(ilang itype) (and (identifier? #'ilang) (identifier? #'itype))]
                  [_ #f]))
            (or (identifier? #'olang)
                (syntax-case #'olang ()
                  [(olang otype) (and (identifier? #'olang) (identifier? #'otype))]
                  [_ #f]))
            (andmap identifier? (syntax->list #'(fml ...))))
       #'(x-define-pass name ?colon ilang (fml ...) ?arrow olang (xval ...)
           (expand-modifier echo) . more)])))

(define-syntax x-define-pass
  (lambda (x)
    (define who 'define-pass)

    (define-struct pass-desc
      (name maybe-ilang maybe-olang (pdesc* #:mutable)) #:prefab)

    (define-struct pdesc
      (name maybe-itype fml* dflt* maybe-otype xval* body trace? echo?)
      #:prefab)

    (define-struct pclause
      (lhs guard id rhs-arg* rhs-lambda
        (used? #:mutable)
        (related-alt* #:mutable))
      #:prefab
      #:constructor-name $make-pclause)
    (define make-pclause
      (lambda (lhs guard id rhs-arg* rhs-lambda)
        ($make-pclause lhs guard id rhs-arg* rhs-lambda #f '())))

    (define make-processors
      (lambda (pass-desc maybe-imeta-parser maybe-ometa-parser)
        (let loop ([pdesc* (pass-desc-pdesc* pass-desc)] [processor* '()])
          (if (null? pdesc*)
              (let ([pdesc* (let ([ls (pass-desc-pdesc* pass-desc)])
                              (list-head ls (- (length ls) (length processor*))))])
                (if (null? pdesc*)
                    processor*
                    (loop pdesc* processor*)))
              (loop (cdr pdesc*)
                (cons (make-processor pass-desc maybe-imeta-parser maybe-ometa-parser (car pdesc*))
                  processor*))))))

    (define make-processor
      (lambda (pass-desc maybe-imeta-parser maybe-ometa-parser pdesc)
        (define echo-processor
          (lambda (result)
            (when (pdesc-echo? pdesc)
              (printf "~s in pass ~s expanded into:\n"
                (maybe-syntax->datum (pdesc-name pdesc))
                (maybe-syntax->datum (pass-desc-name pass-desc)))
              (pretty-print (maybe-syntax->datum result)))
            result))
        (with-syntax ([lambda-expr (make-processor-lambda pass-desc maybe-imeta-parser maybe-ometa-parser pdesc)]
                      [name (pdesc-name pdesc)])
          (echo-processor    
            #`(define name
                #,(if (pdesc-trace? pdesc)
                      (let ([maybe-ilang (pass-desc-maybe-ilang pass-desc)]
                            [maybe-olang (pass-desc-maybe-olang pass-desc)])    
                        (let ([iunparser (and maybe-ilang (pdesc-maybe-itype pdesc)
                                              (let ([ilang (language-name maybe-ilang)])
                                                (format-id ilang "unparse-~a" ilang)))]
                              [ounparser (and maybe-olang (pdesc-maybe-otype pdesc)
                                              (let ([olang (language-name maybe-olang)])
                                                (format-id olang "unparse-~a" olang)))])
                          (if iunparser
                              (if ounparser
                                  (with-syntax ([(fml fml* ...) (generate-temporaries (pdesc-fml* pdesc))]
                                                [(ot xrt ...) (generate-temporaries (cons 'ot (pdesc-xval* pdesc)))]
                                                [(tot txrt ...) (generate-temporaries (cons 'tot (pdesc-xval* pdesc)))])
                                    #`(lambda (fml fml* ...)
                                        (let ([tproc lambda-expr])
                                          (let ([ot #f] [xrt #f] ...)
                                            (trace-let name ([t (#,iunparser fml #t)] [fml* fml*] ...)
                                              (let-values ([(tot txrt ...) (tproc fml fml* ...)])
                                                (set! ot tot)
                                                (set! xrt txrt) ...
                                                (values (#,ounparser tot #t) txrt ...)))
                                            (values ot xrt ...)))))
                                  (with-syntax ([(fml fml* ...) (generate-temporaries (pdesc-fml* pdesc))])
                                    #`(lambda (fml fml* ...)
                                        (let ([tproc lambda-expr])
                                          (trace-let name ([t (#,iunparser fml #t)] [fml* fml*] ...)
                                            (tproc fml fml* ...))))))
                              (if ounparser
                                  (with-syntax ([(fml ...) (generate-temporaries (pdesc-fml* pdesc))]
                                                [(ot xrt ...) (generate-temporaries (cons 'ot (pdesc-xval* pdesc)))]
                                                [(tot txrt ...) (generate-temporaries (cons 'tot (pdesc-xval* pdesc)))])
                                    #`(lambda (fml ...)
                                        (let ([tproc lambda-expr])
                                          (let ([ot #f] [xrt #f] ...)
                                            (trace-let name ([fml fml] ...)
                                              (let-values ([(tot txrt ...) (tproc fml ...)])
                                                (set! ot tot)
                                                (set! xrt txrt) ...
                                                (values (#,ounparser tot #t) txrt ...)))
                                            (values ot xrt ...)))))
                                  (with-syntax ([(fml ...) (generate-temporaries (pdesc-fml* pdesc))])
                                    #'(lambda (fml ...)
                                        (let ([tproc lambda-expr])
                                          (trace-let name ([fml fml] ...)
                                            (tproc fml ...)))))))))
                      #'lambda-expr))))))

    (define make-processor-lambda
      (lambda (pass-desc maybe-imeta-parser maybe-ometa-parser pdesc)
        (let ([maybe-olang (pass-desc-maybe-olang pass-desc)]
              [maybe-otype (pdesc-maybe-otype pdesc)] ; HERE
              [tfml (car (generate-temporaries '(x)))]
              [fml* (pdesc-fml* pdesc)])
          #`(lambda #,fml*
              (let ([#,tfml #,(car fml*)])
                #,@((lambda (forms)
                      (if maybe-olang
                          (list
                            (rhs-in-context-quasiquote (pass-desc-name pass-desc)
                              maybe-otype maybe-olang maybe-ometa-parser #`(begin #,@forms)))
                          forms))
                     (if (let ([maybe-itype (pdesc-maybe-itype pdesc)])
                           (and maybe-itype 
                                (nonterm-id->ntspec? maybe-itype
                                  (language-ntspecs
                                    (pass-desc-maybe-ilang pass-desc)))))
                         (let-values  ([(body defn*)
                                        (syntax-case (pdesc-body pdesc) ()
                                          [((definitions defn* ...) . body)
                                           (eq? (datum definitions) 'definitions)
                                           (values #'body #'(defn* ...))]
                                          [body (values #'body '())])])
                           #`(#,@defn*
                               #,(make-processor-clauses pass-desc tfml maybe-imeta-parser maybe-ometa-parser pdesc body)))
                         (pdesc-body pdesc))))))))

    (define make-processor-clauses
      (lambda (pass-desc tfml imeta-parser maybe-ometa-parser pdesc cl*)
        (let* ([itype (pdesc-maybe-itype pdesc)] ; HERE
               [ilang (pass-desc-maybe-ilang pass-desc)]
               [intspec* (language-ntspecs ilang)]
               [maybe-otype (pdesc-maybe-otype pdesc)] ; HERE
               [maybe-olang (pass-desc-maybe-olang pass-desc)]
               [maybe-ontspec* (and maybe-otype (language-ntspecs maybe-olang))]
               [fml* (pdesc-fml* pdesc)]
               [fml tfml]
               [xfml* (cdr fml*)])
          (define parse-clauses
            (lambda (cl*)
              (define nano-meta->fml*
                (lambda (nm)
                  #;(printf "nanometa: ~s\n" nm)
                  (let f ([nrec* (nano-meta-fields nm)] [fml* '()])
                    (foldr
                      (rec g
                        (lambda (nrec fml*)
                          (cond
                            [(nano-dots? nrec) (g (nano-dots-x nrec) fml*)]
                            [(nano-unquote? nrec) (cons (nano-unquote-x nrec) fml*)]
                            [(nano-cata? nrec)
                             (let ([fml* (append
                                           (let ([outid* (nano-cata-outid* nrec)])
                                             (if (and maybe-olang (not (null? outid*))
                                                      (eq? (maybe-syntax->datum (car outid*)) '*))
                                                 (cdr outid*)
                                                 outid*))
                                           fml*)]
                                   [maybe-inid* (nano-cata-maybe-inid* nrec)])
                               (if (and maybe-inid*
                                        (let ([id (car maybe-inid*)])
                                          (and (identifier? id)
                                               (not (memf (lambda (fml)
                                                            (free-identifier=? fml id))
                                                      fml*)))))
                                   (cons (car maybe-inid*) fml*)
                                   fml*))]
                            [(nano-meta? nrec) (f (nano-meta-fields nrec) fml*)]
                            [(list? nrec) (f nrec fml*)]
                            [(nano-quote? nrec) (raise-syntax-error who "quoted terminals currently unsupported in match patterns" (nano-quote-x nrec))]
                            [else (error who "unrecognized nano-rec" nrec)])))
                      fml* nrec*))))
              (define (helper lhs guard rhs rhs*)
                (let ([nano-meta (imeta-parser (maybe-syntax->datum itype) lhs #t)])
                  (let ([fml* (nano-meta->fml* nano-meta)])
                    (unless (all-unique-identifiers? fml*)
                      (raise-syntax-error who "pattern binds one or more identifiers more then once" lhs))
                    (make-pclause nano-meta guard
                      (datum->syntax #'* (gensym "rhs"))
                      fml* #`(lambda #,fml* #,rhs #,@rhs*)))))
              (let f ([cl* cl*] [pclause* '()])
                (syntax-case cl* (guard else)
                  [() (values (reverse pclause*) #f #f)]
                  [([else rhs0 rhs1 ...] . cl*)
                   (stx-null? #'cl*)
                   (values (reverse pclause*)
                      #'else-th #'(lambda () (begin rhs0 rhs1 ...)))]
                  [([lhs (guard g0 g1 ...) rhs0 rhs1 ...] . cl*)
                    (f #'cl*
                      (cons (helper #'lhs #'(and g0 g1 ...) #'rhs0 #'(rhs1  ...)) pclause*))]
                  [([lhs rhs0 rhs1 ...] . cl*)
                    (f #'cl* (cons (helper #'lhs #t #'rhs0 #'(rhs1  ...)) pclause*))]
                  [_ (raise-syntax-error (maybe-syntax->datum (pass-desc-name pass-desc))
                       "invalid processor clause" (pdesc-name pdesc) (car cl*))]))))
          (define make-system-clause
            (lambda (alt)
              (define genmap
                (lambda (proc level maybe? arg args)
                  (define add-maybe
                    (lambda (e-arg e)
                      (if maybe? #`(let ([t #,e-arg]) (and t #,e)) e)))
                  (cond
                    [(= level 0) (add-maybe arg #`(#,proc #,arg #,@args))]
                    [(= level 1) #`(map (lambda (m) #,(add-maybe #'m #`(#,proc m #,@args))) #,arg)]
                    [else
                      (genmap
                        #`(lambda (x) (map (lambda (m) #,(add-maybe #'m #`(#,proc m #,@args))) x))
                        (- level 1)
                        #f ; once we've applied the maybe turn it off, since we can have a
                        ; list of maybes but not maybe of a list.
                        arg '())])))
              (define-who process-alt
                (lambda (in-altsyn in-altrec out-altrec)
                  (define process-alt-field
                    (lambda (level maybe? fname aname ofname)
                      (if (and (nonterminal-meta? fname intspec*)
                               (nonterminal-meta? ofname maybe-ontspec*))
                          (let ([callee-pdesc
                                  (find-proc "process-alt, callee-pdesc 1" pass-desc (pdesc-name pdesc)
                                    (spec-type (find-spec fname ilang))
                                    (spec-type (find-spec ofname maybe-olang)) #t
                                    (lambda (id* dflt*)
                                      (andmap
                                        (lambda (req)
                                          (memf (lambda (x) (bound-identifier=? req x)) fml*))
                                        (list-head id* (- (length id*) (length dflt*)))))
                                    (lambda (dflt*)
                                      ; punting when there are return values for now
                                      (null? dflt*)))])
                            (genmap (pdesc-name callee-pdesc) level maybe? #`(#,aname #,fml)
                              (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                    [dflt* (pdesc-dflt* callee-pdesc)])
                                (let ([n (- (length id*) (length dflt*))])
                                  #`(#,@(list-head id* n)
                                      #,@(map (lambda (id dflt)
                                                (if (memf (lambda (x) (bound-identifier=? id x))
                                                      (cdr fml*))
                                                    id
                                                    dflt))
                                           (list-tail id* n)
                                           dflt*))))))
                          (let ([callee-pdesc
                                  (find-proc "process-alt, callee-pdesc 2" pass-desc (pdesc-name pdesc)
                                    (spec-type (find-spec fname ilang))
                                    (spec-type (find-spec ofname maybe-olang)) #f
                                    (lambda (id* dflt*)
                                      (andmap
                                        (lambda (req)
                                          (memf (lambda (x) (bound-identifier=? req x)) fml*))
                                        (list-head id* (- (length id*) (length dflt*)))))
                                    (lambda (dflt*)
                                      ; punting when there are return values for now
                                      (null? dflt*)))])
                            (if callee-pdesc
                                (genmap (pdesc-name callee-pdesc) level maybe? #`(#,aname #,fml)
                                  (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                         [dflt* (pdesc-dflt* callee-pdesc)])
                                    (let ([n (- (length id*) (length dflt*))])
                                      #`(#,@(list-head id* n)
                                          #,@(map (lambda (id dflt)
                                                    (if (memf (lambda (x) (bound-identifier=? id x))
                                                          (cdr fml*))
                                                        id
                                                        dflt))
                                               (list-tail id* n)
                                               dflt*)))))
                                (begin
                                  (when (or (nonterminal-meta? fname intspec*)
                                            (nonterminal-meta? ofname maybe-ontspec*))
                                    (raise-syntax-error who
                                      (format "unable to automatically translate ~s in ~s to ~s in ~s"
                                        (maybe-syntax->datum fname) (maybe-syntax->datum (alt-syn in-altrec))
                                        (maybe-syntax->datum ofname) (maybe-syntax->datum (alt-syn out-altrec)))
                                      (pass-desc-name pass-desc) (pdesc-name pdesc)))
                                  #`(#,aname #,fml)))))))
                  (cond
                    [(pair-alt? in-altrec)
                     (let* ([in-field-level* (pair-alt-field-levels in-altrec)]
                            [in-field-maybe* (pair-alt-field-maybes in-altrec)]
                            [in-acc* (pair-alt-accessors in-altrec)]
                            [in-field-name* (pair-alt-field-names in-altrec)]
                            [out-field-name* (pair-alt-field-names out-altrec)]
                            [out-field*
                             (map process-alt-field
                               in-field-level*
                               in-field-maybe*
                               in-field-name*
                               in-acc*
                               out-field-name*)])
                       ; always using the non-checking form here, because we are simply rebuilding;
                       ; TODO: terminals should be checked to be matching from the input language
                       ; to the output language, otherwise a check should be made here or the
                       ; checking version of the maker should be used.
                       ; AWK: this has been changed to use the checking alt, because we cannot
                       ; assume that other transformers will always create a valid element for
                       ; sub-parts of this particular maker.
                       ; TODO: Need to find a way to give a better error message in the checking maker
                       #`(#,(pair-alt-maker out-altrec)
                           '#,(pass-desc-name pass-desc)
                           #,@out-field*
                           #,@(map (lambda (x) (format "~s" x)) (map maybe-syntax->datum in-field-name*))))]
                    [(terminal-alt? in-altrec) (error who "unexpected terminal alt" in-altrec)]
                    [(nonterminal-alt? in-altrec) (error who "unexpected nonterminal alt" in-altrec)])))
              (cond
                [(nonterminal-alt? alt)
                 (build-subtype-call (ntspec-name (nonterminal-alt-ntspec alt)))]
                [(terminal-alt? alt)
                 (let ([proc (find-proc "make system clause, terminal-alt" pass-desc (pdesc-name pdesc)
                               (tspec-type (terminal-alt-tspec alt))
                               maybe-otype #f
                               (lambda (id* dflt*) (< (- (length id*) (length dflt*)) (length fml*)))
                               (lambda (dflt*) (= (length dflt*) (length (pdesc-xval* pdesc)))))]
                       [xval* (pdesc-xval* pdesc)])
                   (let ([alt-code (if proc (build-call fml* proc) fml)])
                     (if (null? xval*)
                         alt-code
                         #`(values #,alt-code #,@xval*))))]
                [else
                 (let ([alt-syntax (alt-syn alt)])
                   (let ([oalt (exists-alt? alt (nonterm-id->ntspec who maybe-otype maybe-ontspec*))])
                     (if oalt
                         (let ([alt-code (process-alt alt-syntax alt oalt)]
                               [xval* (pdesc-xval* pdesc)])
                           (if (null? xval*)
                               alt-code
                               #`(values #,alt-code #,@xval*)))
                         ; TODO: if there were no user provided clauses for this input alt,
                         ; we could raise a compile time error here, otherwise we have to rely
                         ; on the runtime error
                         #`(error '#,(pass-desc-name pass-desc)
                             (format "no matching clause for input ~s in processor ~s"
                               '#,alt-syntax
                               '#,(pdesc-name pdesc))
                             #,fml))))])))

          (define gen-binding (lambda (t v) (if (eq? t v) '() (list #`(#,t #,v)))))
          (define gen-t (lambda (acc) (if (identifier? acc) acc (generate-temporary))))
          (define gen-let1
            (lambda (t v e)
              (cond [(eq? t v) e]
                [(eq? e #t) #t]
                [else #`(let ([#,t #,v]) #,e)])))
          ;; Note: gen-and DOES NOT actually function like and. For instance,
          ;; normally (and exp #t) would return #t, but with gen-and we get exp
          ;; so if exp does not evaluate to #t, the result is different.
          ;; This is used in the generated results.
          (define gen-and
            (lambda (e1 e2)
              (cond [(eq? e1 #t) e2] [(eq? e2 #t) e1] [else #`(and #,e1 #,e2)])))
          (define gen-andmap
            (lambda (t v e)
              (if (eq? e #t) #t #`(andmap (lambda (#,t) #,e) #,v))))

          ; TODO: Right now process-nano-fields and its helpers are generating a predicate
          ; on incoming records, and two bindings for each user specified unquote expression.
          ; I think the infrastructure should be assuming that the input is well structured
          ; (i.e. it should rely on the builder of the structure to do the checking and not
          ; check on input, and hence should not generate the temporary bindings, or the
          ; checks.)
          (define process-nano-fields
            (lambda (elt* acc-id aname* itype*)
              (if (null? elt*)
                  (values #t '() '() '())
                  (let-values
                    ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                       (process-nano-elt (car elt*) #`(#,(car aname*) #,acc-id)
                         (car itype*))]
                      [(rest-ipred rest-tbinding* rest-ibinding* rest-obinding*)
                        (process-nano-fields (cdr elt*) acc-id (cdr aname*)
                          (cdr itype*))])
                    (values
                      (gen-and elt-ipred rest-ipred)
                      (append elt-tbinding* rest-tbinding*)
                      (append elt-ibinding* rest-ibinding*)
                      (append elt-obinding* rest-obinding*))))))

          (define gen-mvmap
            (lambda (who ids proc arg . args)
              (with-syntax ([who who] [proc proc] [arg arg])
                (with-syntax ([(arg* ...) args]
                              [(ls2 ...) (generate-temporaries args)]
                              [(id ...) (generate-temporaries ids)]
                              [(id* ...) (generate-temporaries ids)])
                  (with-syntax ([(ls ...) #'(ls1 ls2 ...)])
                    #'(let ([p proc] [ls1 arg] [ls2 arg*] ...)
                        (unless (list? ls) (error 'who "not a proper list" ls))
                        ...
                        (let ([n (length ls1)])
                          (unless (and (= (length ls2) n) ...)
                            (error 'who "mismatched list lengths" ls1 ls2 ...)))
                        (let f ([ls1 ls1] [ls2 ls2] ...)
                          (if (null? ls1)
                              (let ([id '()] ...) (values id ...))
                              (let-values ([(id ...) (p (car ls1) (car ls2) ...)]
                                            [(id* ...) (f (cdr ls1) (cdr ls2) ...)])
                                (values (cons id id*) ...))))))))))

          (define process-nano-dots
            (lambda (elt acc itype)
              (let ([map-t (generate-temporary "map-t")])
                (let-values ([(ipred tbinding* ibinding* obinding*)
                               (process-nano-elt elt map-t itype)])
                  (let ([ls-t (gen-t acc)])
                    (values
                      (gen-andmap map-t acc ipred)
                      (gen-binding ls-t acc)
                      (map
                        (lambda (ibinding)
                          (syntax-case ibinding ()
                            [(id expr)
                              (if (and (identifier? #'expr) (eq? map-t #'expr))
                                  #`(id #,ls-t)
                                  #`(id (map (lambda (#,map-t)
                                               #,(if (null? tbinding*) 
                                                     #'expr
                                                     #`(let* #,tbinding* expr)))
                                          #,ls-t)))]))
                        ibinding*)
                      (map
                        (lambda (obinding)
                          ;; TODO: rather than tearing apart the code we've constructed
                          ;; in the nano-cata case to support dotted cata, the nano-cata
                          ;; should be constructed to just build the correct code in the first
                          ;; place.
                          (syntax-case obinding ()
                            [(ids (procexpr var args ...)) ;; contains expr itself
                              #`(ids ((let ([p (let ([p procexpr]) (lambda (m) (p m args ...)))])
                                        (lambda (x)
                                          #,(cond
                                              [(stx-null? #'ids) #'(begin (for-each p x) (values))]
                                              [(stx-null? (stx-cdr #'ids)) #'(map p x)]
                                              [else (gen-mvmap (pass-desc-name pass-desc)
                                                      #'ids #'p #'x)])))
                                       var))]))
                        obinding*)))))))

          (define process-nano-list
            (lambda (elt* acc itype)
              (define helper
                (lambda (elt* tail-acc)
                  (if (null? elt*)
                      (values #t '() '() '() 0 #f)
                      (let ([elt (car elt*)])
                        (if (nano-dots? elt)
                            (let ([t (gen-t tail-acc)] [n (length (cdr elt*))])
                              (let-values
                                ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                                   (process-nano-dots (nano-dots-x elt)
                                     (if (= n 0)
                                         t
                                         #`(list-head #,t (- (length #,t) #,n)))
                                     itype)]
                                  [(rest-ipred rest-tbinding* rest-ibinding*
                                     rest-obinding* i dots?)
                                    (helper (cdr elt*)
                                      (if (= n 0)
                                          t
                                          #`(list-tail #,t (- (length #,t) #,n))))])
                                (values
                                  (gen-let1 t tail-acc
                                    (gen-and elt-ipred rest-ipred))
                                  (append (gen-binding t tail-acc)
                                    elt-tbinding* rest-tbinding*)
                                  (append elt-ibinding* rest-ibinding*)
                                  (append elt-obinding* rest-obinding*)
                                  i #t)))
                            (let ([t (gen-t tail-acc)])
                              (let-values
                                ([(elt-ipred elt-tbinding* elt-ibinding* elt-obinding*)
                                   (process-nano-elt elt #`(car #,t) itype)]
                                  [(rest-ipred rest-tbinding* rest-ibinding*
                                     rest-obinding* i dots?)
                                    (helper (cdr elt*) #`(cdr #,t))])
                                (values
                                  (gen-let1 t tail-acc
                                    (gen-and elt-ipred rest-ipred))
                                  (append (gen-binding t tail-acc)
                                    elt-tbinding* rest-tbinding*)
                                  (append elt-ibinding* rest-ibinding*)
                                  (append elt-obinding* rest-obinding*)
                                  (+ i 1) dots?))))))))
              (let ([t (gen-t acc)])
                (let-values ([(ipred tbinding* ibinding* obinding* i dots?)
                               (helper elt* t)])
                  (values
                    (gen-let1 t acc
                      (if dots?
                          (if (= i 0)
                              ipred
                              (gen-and #`(>= (length #,t) #,i) ipred))
                          (gen-and #`(= (length #,t) #,i) ipred)))
                    (append (gen-binding t acc) tbinding*)
                    ibinding* obinding*)))))

          (define build-meta-variable-check
            (lambda (id acc itype)
              (let ([spec (find-spec id ilang)])
                ;; SYMBOLIC
                (cond
                  [(eq? (maybe-syntax->datum (spec-type spec)) (maybe-syntax->datum itype)) #t]
                  [(nonterm-id->ntspec? itype (language-ntspecs ilang)) =>
                   (lambda (ntspec)
                      (if (subspec? spec ntspec)
                          #`(#,(spec-all-pred spec) #,acc)
                          (raise-syntax-error
                            (maybe-syntax->datum (pass-desc-name pass-desc))
                            (format
                              "expected meta-variable for nonterminal ~s, but got"
                              (if (syntax? itype) (maybe-syntax->datum itype) itype))
                            id)))]
                  [(term-id->tspec? itype (language-tspecs ilang)) =>
                   (lambda (tspec)
                     (raise-syntax-error
                       (maybe-syntax->datum (pass-desc-name pass-desc))
                       (format
                         "expected meta-variable for terminal ~s, but got"
                         (if (syntax? itype) (maybe-syntax->datum itype) itype))
                       id))]
                  [else (raise-syntax-error
                          (maybe-syntax->datum (pass-desc-name pass-desc))
                          (format
                            "NANOPASS INTERNAL ERROR: unable to find spec for type ~s"
                            (if (syntax? itype) (maybe-syntax->datum itype) itype))
                          id)]))))

          (define process-nano-elt
            (lambda (elt acc itype)
              (cond
                [(nano-meta? elt)
                 (let ([t (gen-t acc)])
                   (let-values ([(ipred tbinding* ibinding* obinding*)
                                  (process-nano-meta elt t)])
                     (values
                       (gen-let1 t acc
                         (gen-and
                           ;; TODO: if the nt here doesn't have any terminals, then we only
                           ;; need to do the tag comparison.
                           #;#`(eqv? (nanopass-record-tag #,t) #,(pair-alt-tag (nano-meta-alt elt)))
                           #`(#,(pair-alt-pred (nano-meta-alt elt)) #,t)
                           ipred))
                       (append (gen-binding t acc) tbinding*)
                       ibinding* obinding*)))]
                [(nano-quote? elt) 
                  (raise-syntax-error (maybe-syntax->datum (pass-desc-name pass-desc))
                    "quoted items are currently unsupported in patterns"
                    (nano-quote-x elt))]
                [(nano-unquote? elt)
                  ; TODO: will break if two ids are same
                  (let ([id (nano-unquote-x elt)])
                    (values
                      (build-meta-variable-check id acc itype)
                      '()
                      (list #`(#,id #,acc))
                      '()))]
                [(nano-cata? elt)
                  ; TODO: will break if two ids are same
                  ; HERE: if this is a cata for a (maybe x) field, it needs to not bother
                  ; parsing the #f
                  (let* ([maybe-inid* (nano-cata-maybe-inid* elt)]
                          [t (or (and maybe-inid* (car maybe-inid*)) (generate-temporary))]
                          [maybe? (nano-cata-maybe? elt)]
                          [itype (if (syntax? itype) (maybe-syntax->datum itype) itype)])
                    (let-values ([(maybe-otype outid*)
                                   (let ([outid* (nano-cata-outid* elt)])
                                     (if maybe-olang
                                         (if (null? outid*)
                                             (values #f outid*)
                                             (if (eq? (maybe-syntax->datum (car outid*)) '*)
                                                 (values #f (cdr outid*))
                                                 (values
                                                   (maybe-syntax->datum
                                                     (spec-type
                                                       (find-spec (car outid*) maybe-olang)))
                                                   outid*)))
                                         (values #f outid*)))])
                      (define build-cata-call-1
                        (lambda (itype maybe-otype inid* outid*)
                          (build-call inid*
                            (find-proc "build-cata call 1" pass-desc (nano-cata-syntax elt) itype maybe-otype #t
                              (lambda (id* dflt*)
                                (< (- (length id*) (length dflt*)) (length inid*)))
                              (lambda (dflt*)
                                (= (length dflt*)
                                  (length (if maybe-otype (cdr outid*) outid*)))))
                            maybe?)))
                      ; TODO: check pdesc-maybe-itype >= itype and pdesc-otype <= otype
                      (define pdesc-ok?
                        (lambda (pdesc outid*)
                          (and (andmap
                                 (lambda (req) (memf (lambda (x) (bound-identifier=? req x)) fml*))
                                 (list-head xfml* (- (length xfml*) (length (pdesc-dflt* pdesc)))))
                               (= (length (pdesc-xval* pdesc))
                                 ; TODO: when we don't have an otype for a processor, we may not have an otype here
                                 ; we should check this out to be sure.
                                 (length (if itype (cdr outid*) outid*))))))
                      (define build-cata-call-2
                        (lambda (callee-pdesc t)
                          (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                 [dflt* (pdesc-dflt* callee-pdesc)])
                            (with-syntax ([(earg* ...)
                                            (let* ([n (- (length id*) (length dflt*))])
                                              #`(#,@(list-head id* n)
                                                  #,@(map (lambda (id dflt)
                                                            (if (memf (lambda (x) (bound-identifier=? id x))
                                                                  fml*)
                                                                id
                                                                dflt))
                                                       (list-tail id* n)
                                                       dflt*)))])
                              (if maybe?
                                  (with-syntax ([(t* ...) (generate-temporaries #'(earg* ...))])
                                    #`((lambda (#,t t* ...)
                                         (and #,t (#,(pdesc-name callee-pdesc) #,t t* ...)))
                                        #,t earg* ...))

                                  #`(#,(pdesc-name callee-pdesc) #,t earg* ...))))))
                      (define build-cata-call-3
                        (lambda (itype maybe-otype t outid*)
                          (let ([callee-pdesc
                                  (find-proc "build-cata call 3" pass-desc (nano-cata-syntax elt) itype maybe-otype #t
                                    (lambda (id* dflt*)
                                      (andmap
                                        (lambda (req)
                                          (memf (lambda (x) (bound-identifier=? req x)) fml*))
                                        (list-head id* (- (length id*) (length dflt*)))))
                                    (lambda (dflt*)
                                      (= (length dflt*)
                                        (let ([len (length outid*)])
                                          (if maybe-otype (- len 1) len)))))])
                            (let ([id* (cdr (pdesc-fml* callee-pdesc))]
                                   [dflt* (pdesc-dflt* callee-pdesc)])
                              (with-syntax ([(earg* ...)
                                              (let ([n (- (length id*) (length dflt*))])
                                                #`(#,@(list-head id* n)
                                                    #,@(map (lambda (id dflt)
                                                              (if (memf (lambda (x) (bound-identifier=? id x))
                                                                    fml*)
                                                                  id dflt))
                                                         (list-tail id* n)
                                                         dflt*)))])
                                (if maybe?
                                    (with-syntax ([(t* ...) (generate-temporaries #'(earg* ...))])
                                      #`((lambda (#,t t* ...)
                                           (and #,t (#,(pdesc-name callee-pdesc) #,t t* ...)))
                                          #,t earg* ...))
                                    #`(#,(pdesc-name callee-pdesc) #,t earg* ...)))))))
                      ; check number of arguments when we have a maybe
                      (when (and maybe? (not (= (length outid*) 1)))
                        (raise-syntax-error who
                          "cannot use cata-morphism that returns multiple values with a maybe field"
                          (nano-cata-syntax elt)))
                      (let ([procexpr (nano-cata-procexpr elt)])
                        (define build-procexpr-call
                          (lambda ()
                            (let ([inid* (or maybe-inid* (list t))])
                              (if maybe?
                                  (with-syntax ([(t t* ...) (generate-temporaries inid*)])
                                    #`((lambda (t t* ...) (and t (#,procexpr t t* ...))) #,@inid*))
                                  #`(#,procexpr #,@inid*)))))
                        #;(unless procexpr
                        (unless (nonterm-id->ntspec? itype (language-ntspecs ilang))
                          (raise-syntax-error who
                            "cannot use cata-morphism without specifying a procedure to call for an input terminal field"
                            (nano-cata-syntax elt))))
                      #;(when maybe-otype
                      (unless (or procexpr (nonterm-id->ntspec? maybe-otype (language-ntspecs maybe-olang)))
                        (raise-syntax-error who
                          "cannot use cata-morphism without specifying a procedure to call for an output terminal field"
                          (nano-cata-syntax elt))))
                        ; when we are not given a processor, make sure our itype is valid
                        (values
                          ; input predicate check
                          (if maybe-inid*
                              (build-meta-variable-check (car maybe-inid*)
                                acc (nano-cata-itype elt))
                              #t)
                          ; binding of temporaries
                          '()
                          ; binding of input variable from language record
                          (list #`(#,t #,acc))
                          ; binding of output variable(s)
                          (if maybe-inid*
                              (if procexpr
                                  (list #`[#,outid* #,(build-procexpr-call)])
                                  (list #`[#,outid* #,(build-cata-call-1 itype maybe-otype maybe-inid* outid*)]))
                              (cond
                                [(and (identifier? procexpr)
                                      (findf (lambda (pdesc)
                                               (bound-identifier=? procexpr (pdesc-name pdesc)))
                                        (pass-desc-pdesc* pass-desc))) =>
                                  (lambda (callee-pdesc)
                                    (if (pdesc-ok? callee-pdesc outid*)
                                        (list #`[#,outid* #,(build-cata-call-2 callee-pdesc t)])
                                        (raise-syntax-error (maybe-syntax->datum (pass-desc-name pass-desc))
                                          (format "incorrect arguments for ~s in cata" (maybe-syntax->datum procexpr))
                                          (nano-cata-syntax elt))))]
                                [procexpr (list #`[#,outid* #,(build-procexpr-call)])]
                                [else (list #`[#,outid* #,(build-cata-call-3 itype maybe-otype t outid*)])]))))))]
                [(list? elt) (process-nano-list elt acc itype)]
                [else (values #`(equal? #,acc #,elt) '() '() '())])))

          (define-who process-nano-meta
            (lambda (x acc-id)
              (let ([prec-alt (nano-meta-alt x)])
                (if (pair-alt? prec-alt)
                    (process-nano-fields (nano-meta-fields x) acc-id
                      (pair-alt-accessors prec-alt)
                      (map (lambda (x) (spec-type (find-spec x ilang)))
                        (pair-alt-field-names prec-alt)))
                    (let ([elt (car (nano-meta-fields x))])
                      ; TODO: we'd like to more generally support cata for terminal and nonterminal-alt and
                      ; this code will have to change to support that.
                      #;(assert (nano-unquote? elt))
                      (let ([id (nano-unquote-x elt)])
                        (values #t '() (list #`(#,id #,acc-id)) '())))))))

          (define find-eq-constraints
            (lambda (ibinding*)
              (let f ([ibinding* ibinding*] [id* '()])
                (if (null? ibinding*)
                    (values '() #t)
                    (let* ([ibinding (car ibinding*)] [id (stx-car ibinding)])
                      (if (bound-id-member? id id*)
                          (raise-syntax-error who "eq constraints are not supported" id)
                          #;(let-values ([(ibinding* ieqpred)
                          (f (cdr ibinding*) id*)])
                              (let ([t (generate-temporary)])
                                (values
                                  #`((#,t #,(cadr ibinding)) #,@ibinding*)
                                  (gen-and #`(nano-equal? #,t #,id) ieqpred))))
                          (let-values ([(ibinding* ieqpred)
                                         (f (cdr ibinding*) (cons id id*))])
                            (values #`(#,ibinding #,@ibinding*) ieqpred))))))))

          (define make-user-clause
            (lambda (pclause k)
              (let ([lhs-rec (pclause-lhs pclause)]
                     [guard-code (pclause-guard pclause)]
                     [rhs-id (pclause-id pclause)]
                     [rhs-arg* (pclause-rhs-arg* pclause)])
                (let-values ([(ipred tbinding* ibinding* obinding*)
                               (process-nano-meta lhs-rec fml)])
                  (let-values ([(ibinding* ieqpred)
                                 (find-eq-constraints ibinding*)])
                    (let ([guard-code (gen-and guard-code ieqpred)]
                           [body-code #`(let-values #,obinding* (#,rhs-id #,@rhs-arg*))])
                      (if (eq? ipred #t)
                          #`(let* (#,@tbinding* #,@ibinding*)
                              #,(if (eq? guard-code #t)
                                    body-code
                                    #`(if #,guard-code #,body-code #,(k))))
                          (if (eq? guard-code #t)
                              #`(if #,ipred
                                    (let* (#,@tbinding* #,@ibinding*)
                                      #,body-code)
                                    #,(k))
                              #`(let ([next-th (lambda () #,(k))])
                                  (if #,ipred
                                      (let* (#,@tbinding* #,@ibinding*)
                                        (if #,guard-code #,body-code (next-th)))
                                      (next-th)))))))))))

          (define generate-system-clauses
            (lambda (alt*)
              ; NB: don't use variants here to see how that impacts performance for testing purposes.
              #;(let f ([alt* alt*] [rcond-cl* '()])
              (if (null? alt*)
                  (reverse rcond-cl*)
                  (let* ([alt (car alt*)] [alt (if (pair? alt) (car alt) alt)])
                    (f (cdr alt*)
                      (cons 
                        #`[((let ()
                              (define-values (x)
                                #,(cond
                                    [(pair-alt? alt) (pair-alt-pred alt)]
                                    [(terminal-alt? alt) (tspec-pred (terminal-alt-tspec alt))]
                                    [else (ntspec-all-pred (nonterminal-alt-ntspec alt))]))
                              x)
                             #,fml)
                            #,(make-clause alt '() #f)]
                        rcond-cl*)))))
            (let f ([alt* alt*] [rcond-rec-cl* '()] [rcond-case-cl* '()])
              #;(printf "length alt*: ~s\n" (length alt*))
              (if (null? alt*)
                  (values (reverse rcond-rec-cl*) (reverse rcond-case-cl*))
                  (let* ([alt (car alt*)] [alt (if (pair? alt) (car alt) alt)])
                    #;(printf "alt: ~s\n" alt)
                    (with-syntax ([body (make-clause alt '() #f)])
                      (cond
                        [(pair-alt? alt)
                          (f (cdr alt*) rcond-rec-cl*
                            (cons #`[(eqv? tag #,(pair-alt-tag alt)) body] rcond-case-cl*))]
                        [(terminal-alt? alt)
                          (let ([tspec (terminal-alt-tspec alt)])
                            (f (cdr alt*)
                               (cons
                                 #`[(#,(tspec-pred (terminal-alt-tspec alt)) #,fml)
                                    body]
                                 rcond-rec-cl*)
                               rcond-case-cl*))]
                        [else
                          (let ([ntspec (nonterminal-alt-ntspec alt)])
                            (let ([maybe-term-pred? (ntspec-all-term-pred ntspec)])
                              (f (cdr alt*)
                                (if maybe-term-pred?
                                    (cons #`[(#,maybe-term-pred? #,fml) body] rcond-rec-cl*)
                                    rcond-rec-cl*)
                                (with-syntax ([(all-tag ...) (ntspec-all-tag ntspec)])
                                  (cons #`[(let ([t (bitwise-and tag #,(language-tag-mask ilang))]) (or (= t all-tag) ...)) body] rcond-case-cl*)))))])))))))

          (define build-subtype-call
            (lambda (itype)
              (build-call fml*
                (find-proc "build subtype call" pass-desc (pdesc-name pdesc) itype maybe-otype #t
                  (lambda (id* dflt*) (< (- (length id*) (length dflt*)) (length fml*)))
                  (lambda (dflt*) (= (length dflt*) (length (pdesc-xval* pdesc))))))))

          (define make-clause
            (lambda (alt pclause* else-id)
              (let f ([pclause* pclause*])
                (if (null? pclause*)
                    (cond
                      [else-id #`(#,else-id)]
                      ; TODO: Consider dropping the (not maybe-olang) and
                      ; building the subtype call even if there is no otype
                      ; for this.  (Need to make sure build-subtype-call
                      ; can handle this appropriately (possibly also need
                      ; to decide if a user-supplied sub-type call with an
                      ; output type is okay to call).)
                      [(and (or (and maybe-olang maybe-otype) (not maybe-olang)) (nonterminal-alt? alt))
                        (build-subtype-call (maybe-syntax->datum (ntspec-name (nonterminal-alt-ntspec alt))))]
                      [(and maybe-olang maybe-otype)
                        (make-system-clause alt)]
                      [else
                        (raise-syntax-error (maybe-syntax->datum (pass-desc-name pass-desc))
                          (format "missing ~s clause cannot be generated with no output type"
                            (maybe-syntax->datum (alt-syn alt)))
                          (pdesc-name pdesc))])
                    (let ([pclause (car pclause*)] [pclause* (cdr pclause*)])
                      (set-pclause-used?! pclause #t)
                      (make-user-clause pclause (lambda () (f pclause*))))))))

          (define maybe-add-lambdas
            (lambda (pclause* else-id else-body body)
              (with-syntax ([((id* rhs-body*) ...)
                              (foldl (lambda (pclause ls)
                                       (if (pclause-used? pclause)
                                           (cons (list (pclause-id pclause)
                                                   (pclause-rhs-lambda pclause))
                                             ls)
                                           ls))
                                (if else-id
                                    (list (list else-id else-body))
                                    '())
                                pclause*)])
                #`(let ([id* rhs-body*] ...) #,body))))
          ; note: assumes grammar nonterminal clauses form a DAG
          ; TODO: reject grammars that have nonterminal clauses that don't form DAG
          ; TODO: should we build this structure up front? also is there a better DS for us
          ; to figure out how the various pclauses are interrelated while we process them
          (define-struct nt-alt-info
            (alt (up* #:mutable) (down* #:mutable))
            #:prefab
            #:constructor-name $make-nt-alt-info)
          (define make-nt-alt-info
            (lambda (alt)
              ($make-nt-alt-info alt '() '())))

          (define build-ntspec-ht
            (lambda (ntspec)
              (let ([ht (make-hasheq)])
                (define set-cons (lambda (item ls) (if (memq item ls) ls (cons item ls))))
                (define set-append
                  (lambda (ls1 ls2)
                    (cond
                      [(null? ls1) ls2]
                      [(null? ls2) ls1]
                      [else (foldl (lambda (ls item) (set-cons item ls)) ls2 ls1)])))
                (define discover-nt-alt-info!
                  (lambda (alt up*)
                    (let ([nt-alt-info (or (hash-ref ht alt #f)
                                           (let ([nt-alt-info (make-nt-alt-info alt)])
                                             (hash-set! ht alt nt-alt-info)
                                             nt-alt-info))])
                      (set-nt-alt-info-up*! nt-alt-info
                        (set-append up* (nt-alt-info-up* nt-alt-info)))
                      (let ([up* (cons alt up*)])
                        (let ([down* (foldl
                                       (lambda (down* alt)
                                         (set-append (discover-nt-alt-info! alt up*) down*))
                                       (nt-alt-info-down* nt-alt-info)
                                       (filter nonterminal-alt? (ntspec-alts (nonterminal-alt-ntspec alt))))])
                          (set-nt-alt-info-down*! nt-alt-info down*)
                          (cons alt down*))))))
                (for-each (lambda (alt) (discover-nt-alt-info! alt '()))
                  (filter nonterminal-alt? (ntspec-alts ntspec)))
                ht)))
          (define build-alt-tree
            (lambda (ntspec)
              (let f ([alt* (ntspec-alts ntspec)] [ralt* '()])
                (if (null? alt*)
                    (reverse ralt*)
                    (f (cdr alt*)
                      (cons
                        (let ([alt (car alt*)])
                          (if (nonterminal-alt? alt)
                              (cons alt (f (ntspec-alts (nonterminal-alt-ntspec alt)) '()))
                              alt))
                        ralt*))))))
          (define alt-tree->s-expr
            (lambda (tree)
              (let f ([alt* tree])
                (if (null? alt*)
                    '()
                    (let ([alt (car alt*)])
                      (if (pair? alt)
                          (cons (f alt) (f (cdr alt*)))
                          (cons (maybe-syntax->datum (alt-syn alt)) (f (cdr alt*)))))))))
          (define remove-alt
            (lambda (covered-alt alt*)
              (let f ([alt* alt*])
                (if (null? alt*)
                    '()
                    (let ([alt (car alt*)] [alt* (cdr alt*)])
                      (if (pair? alt)
                          (if (eq? (car alt) covered-alt)
                              alt*
                              (let ([calt* (f (cdr alt))])
                                (if (null? calt*)
                                    alt*
                                    (cons (cons (car alt) calt*) (f alt*)))))
                          (if (eq? alt covered-alt)
                              alt*
                              (cons alt (f alt*)))))))))
          (define handle-pclause*
            (lambda (pclause* else-id alt-tree ht)
              (define partition-pclause*
                (lambda (alt pclause pclause*)
                  (if (nonterminal-alt? alt)
                      (let* ([nt-alt-info (hash-ref ht alt #f)]
                              [this-and-down* (cons alt (nt-alt-info-down* nt-alt-info))]
                              [up* (nt-alt-info-up* nt-alt-info)])
                        (let-values ([(matching-pclause* other-pclause*)
                                       (partition (lambda (pclause)
                                                    (memq (nano-meta-alt (pclause-lhs pclause)) this-and-down*))
                                         pclause*)])
                          (let ([related-pclause* (filter (lambda (pclause)
                                                            (memq (nano-meta-alt (pclause-lhs pclause)) up*))
                                                    other-pclause*)])
                            (values (cons pclause (append matching-pclause* related-pclause*)) other-pclause*))))
                      (let-values ([(matching-pclause* other-pclause*)
                                     (partition (lambda (pclause) (eq? (nano-meta-alt (pclause-lhs pclause)) alt))
                                       pclause*)])
                        (let ([related-pclause* (filter
                                                  (let ([nt-alt* (pclause-related-alt* pclause)])
                                                    (lambda (pclause)
                                                      (memq (nano-meta-alt (pclause-lhs pclause)) nt-alt*)))
                                                  pclause*)])
                          (values (cons pclause (append matching-pclause* related-pclause*)) other-pclause*))))))
              #;(let f ([pclause* pclause*] [alt-tree alt-tree] [rcond-cl* '()])
              (if (null? pclause*)
                  (values (reverse rcond-cl*) alt-tree)
                  (let* ([pclause (car pclause*)] [alt (nano-meta-alt (pclause-lhs pclause))])
                    (let-values ([(related-pclause* other-pclause*)
                                   (partition-pclause* alt pclause (cdr pclause*))])
                      (f other-pclause*
                        (remove-alt alt alt-tree)
                        (cons
                          #`[((let ()
                                (define-values (x)
                                  #,(cond
                                      [(pair-alt? alt) (pair-alt-pred alt)]
                                      [(terminal-alt? alt) (tspec-pred (terminal-alt-tspec alt))]
                                      [else (ntspec-all-pred (nonterminal-alt-ntspec alt))]))
                                x)
                               #,fml)
                              #,(make-clause alt related-pclause* else-id)]
                          rcond-cl*))))))
            (let f ([pclause* pclause*] [alt-tree alt-tree] [rcond-rec-cl* '()] [rcond-case-cl* '()])
              #;(printf "length pclause*: ~s\n" (length pclause*))
              (if (null? pclause*)
                  (values (reverse rcond-rec-cl*) (reverse rcond-case-cl*) alt-tree)
                  (let* ([pclause (car pclause*)] [alt (nano-meta-alt (pclause-lhs pclause))])
                    (let-values ([(related-pclause* other-pclause*)
                                   (partition-pclause* alt pclause (cdr pclause*))])
                      #;(printf "length related-pclause*: ~s, other-pclause*: ~s\n"
                        (length related-pclause*) (length other-pclause*))
                      (with-syntax ([body (make-clause alt related-pclause* else-id)])
                        (cond
                          [(pair-alt? alt)
                            (f other-pclause* (remove-alt alt alt-tree) rcond-rec-cl*
                              (cons #`[(eqv? tag #,(pair-alt-tag alt)) body] rcond-case-cl*))]
                          [(terminal-alt? alt)
                           (f other-pclause* (remove-alt alt alt-tree)
                              (cons #`[(#,(tspec-pred (terminal-alt-tspec alt)) #,fml)
                                       body]
                                    rcond-rec-cl*)
                              rcond-case-cl*)]
                          [else
                            (let ([ntspec (nonterminal-alt-ntspec alt)])
                              (let ([maybe-term-pred? (ntspec-all-term-pred ntspec)])
                                (f other-pclause* (remove-alt alt alt-tree)
                                  (if maybe-term-pred?
                                      (cons #`[(#,maybe-term-pred? #,fml) body] rcond-rec-cl*)
                                      rcond-rec-cl*)
                                  (with-syntax ([(all-tag ...) (ntspec-all-tag ntspec)])
                                    (cons #`[(let ([t (bitwise-and tag #,(language-tag-mask ilang))]) (or (= t all-tag) ...)) body] rcond-case-cl*)))))]))))))))
          (define annotate-pclause*!
            (lambda (pclause* ntspec ht)
              (let f ([pclause* pclause*]
                       [alt* (filter nonterminal-alt? (ntspec-alts ntspec))]
                       [curr-alt #f])
                (if (or (null? alt*) (null? pclause*))
                    pclause*
                    (let ([alt (car alt*)])
                      (if (nonterminal-alt? alt)
                          (f (f pclause* (ntspec-alts (nonterminal-alt-ntspec alt)) alt) (cdr alt*) curr-alt)
                          (let-values ([(matching-pclause* other-pclause*)
                                         (partition (lambda (pclause)
                                                      (eq? (nano-meta-alt (pclause-lhs pclause)) alt))
                                           pclause*)])
                            (for-each
                              (lambda (pclause)
                                (set-pclause-related-alt*! pclause
                                  (cons curr-alt (nt-alt-info-up* (hash-ref ht curr-alt #f)))))
                              matching-pclause*)
                            (f other-pclause* (cdr alt*) curr-alt))))))))
          (let-values ([(pclause* else-id else-body) (parse-clauses cl*)])
            (let ([ntspec (nonterm-id->ntspec who itype intspec*)])
              (maybe-add-lambdas pclause* else-id else-body
                (let ([ht (build-ntspec-ht ntspec)])
                  (annotate-pclause*! pclause* ntspec ht)
                  #;(let-values ([(user-clause* alt*)
                  (handle-pclause* pclause* else-id
                    (if else-id '() (build-alt-tree ntspec))
                    ht)])
                      (let ([system-clause* (if else-id '() (generate-system-clauses alt*))])
                        #`(cond
                            #,@user-clause*
                            #,@system-clause*
                            [else #,(if else-id
                                        #`(#,else-id) 
                                        #`(error '#,(pass-desc-name pass-desc)
                                            #,(format "unexpected ~s" (maybe-syntax->datum itype))
                                            #,fml))])))
                  (let-values ([(user-rec-clause* user-case-clause* alt*)
                                 (handle-pclause* pclause* else-id
                                   (if else-id '() (build-alt-tree ntspec))
                                   ht)])
                    (let-values ([(system-rec-clause* system-case-clause*)
                                   (if else-id
                                       (values
                                         (if (ntspec-all-term-pred ntspec)
                                             #`([(not (nanopass-record? #,fml)) (#,else-id)])
                                             '())
                                         '())
                                       (generate-system-clauses alt*))])
                      #`(cond
                          #,@user-rec-clause*
                          #,@system-rec-clause*
                          [else 
                            (let ([tag (nanopass-record-tag #,fml)])
                              (cond
                                #,@user-case-clause*
                                #,@system-case-clause*
                                [else #,(if else-id
                                            #`(#,else-id) 
                                            #`(error '#,(pass-desc-name pass-desc)
                                                #,(format "unexpected ~s" (maybe-syntax->datum itype))
                                                #,fml))]))]))))))))))

    ; build-call and find-proc need to work in concert, so they are located near eachother
    ; to increase the chance that we actually remember to alter both of them when the
    ; interface is effected by changing one.
    (define build-call
      (case-lambda
        [(caller-fml* callee-pdesc) (build-call caller-fml* callee-pdesc #f)]
        [(caller-fml* callee-pdesc maybe?)
          (define build-args
            (lambda (callee-fml* callee-init* caller-fml*)
              (let f ([required-cnt (- (length callee-fml*) (length callee-init*))]
                       [callee-fml* callee-fml*]
                       [callee-init* callee-init*]
                       [caller-fml* caller-fml*])
                (cond
                  [(null? callee-fml*) '()]
                  [(and (= required-cnt 0) (null? caller-fml*))
                    (cons (car callee-init*)
                      (f required-cnt (cdr callee-fml*) (cdr callee-init*) caller-fml*))]
                  [(= required-cnt 0)
                    (cons (car caller-fml*)
                      (f required-cnt (cdr callee-fml*) (cdr callee-init*) (cdr caller-fml*)))]
                  [else (cons (car caller-fml*)
                          (f (- required-cnt 1) (cdr callee-fml*) callee-init*  (cdr caller-fml*)))]))))
          (with-syntax ([pname (pdesc-name callee-pdesc)]
                         [(arg* ...) (build-args (pdesc-fml* callee-pdesc) (pdesc-dflt* callee-pdesc) caller-fml*)])
            (if maybe?
                (with-syntax ([(t t* ...) (generate-temporaries #'(arg* ...))])
                  #'((lambda (t t* ...) (and t (pname t t* ...))) arg* ...))
                #'(pname arg* ...)))]))

    (define find-proc
      ; will never be asked to find a proc without an itype, so itype is never #f
      (lambda (msg pass-desc src-stx itype maybe-otype try-to-generate? xfmls-ok? xvals-ok?)
        (define (try-to-generate)
          (unless (and (xfmls-ok? '() '()) (xvals-ok? '()))
            (raise-syntax-error who
              (format "cannot find a processor that accepts input type ~s and output type ~s, \
                and cannot generate one with extra formals or return values"
                itype maybe-otype)
              (pass-desc-name pass-desc) src-stx))
          (unless (and (nonterm-id->ntspec? itype (language-ntspecs (pass-desc-maybe-ilang pass-desc)))
                       (nonterm-id->ntspec? maybe-otype (language-ntspecs (pass-desc-maybe-olang pass-desc))))
            (raise-syntax-error who
              (format "cannot find a processor that accepts input type ~s and output type ~s, \
                and cannot generate one when either the input or output type is a terminal"
                itype maybe-otype)
              (pass-desc-name pass-desc) src-stx))
          #;(printf "Making Pdesc: ~a\n" (format "~s->~s" (maybe-syntax->datum itype) (maybe-syntax->datum maybe-otype)))
          (let ([pdesc (make-pdesc (datum->syntax #'* (gensym (format "~s->~s" (maybe-syntax->datum itype) (maybe-syntax->datum maybe-otype))))
                         itype (list #'ir) '() maybe-otype '() '() #f #f)])
            (set-pass-desc-pdesc*! pass-desc
              (cons pdesc (pass-desc-pdesc* pass-desc)))
            pdesc))
        (define find-subspecs
          (lambda (ospec sub-ospec*)
            (if (ntspec? ospec)
                (let f ([alt* (ntspec-alts ospec)] [sub-ospec* sub-ospec*])
                  (if (null? alt*)
                      sub-ospec*
                      (let ([alt (car alt*)])
                        (cond
                          [(nonterminal-alt? alt)
                            (f (cdr alt*) (cons (nonterminal-alt-ntspec alt) sub-ospec*))]
                          [(terminal-alt? alt)
                            (f (cdr alt*) (cons (terminal-alt-tspec alt) sub-ospec*))]
                          [else (f (cdr alt*) sub-ospec*)]))))
                sub-ospec*)))
        (define find-candidate
          (lambda (maybe-otype)
            (let loop ([pdesc* (pass-desc-pdesc* pass-desc)] [candidate #f])
              (if (null? pdesc*)
                  candidate
                  (loop (cdr pdesc*)
                    (let ([pdesc (car pdesc*)])
                      #;(printf "finding-candidate:\n  itype: ~s - ~s\n  maybe-otype: ~s - ~s\n  xfmls bits: ~s - ~s\n  xvals bits: ~s\n  and: ~s\n"
                        (pdesc-maybe-itype pdesc) itype
                        (pdesc-maybe-otype pdesc) maybe-otype
                        (cdr (pdesc-fml* pdesc)) (pdesc-dflt* pdesc)
                        (pdesc-xval* pdesc)
                        (list (eq? (maybe-syntax->datum (pdesc-maybe-itype pdesc)) (maybe-syntax->datum itype)) ; HERE
                              (eq? (maybe-syntax->datum (pdesc-maybe-otype pdesc)) (maybe-syntax->datum maybe-otype)) ; HERE
                              (xfmls-ok? (cdr (pdesc-fml* pdesc)) (pdesc-dflt* pdesc))
                              (xvals-ok? (pdesc-xval* pdesc))))
                      (if (and (eq? (maybe-syntax->datum (pdesc-maybe-itype pdesc)) (maybe-syntax->datum itype)) ; HERE
                               (eq? (maybe-syntax->datum (pdesc-maybe-otype pdesc)) (maybe-syntax->datum maybe-otype)) ; HERE
                               (xfmls-ok? (cdr (pdesc-fml* pdesc)) (pdesc-dflt* pdesc))
                               (xvals-ok? (pdesc-xval* pdesc)))
                          (if candidate
                              (raise-syntax-error who
                                (format "ambiguous target for implicit processor call from ~s to ~s"
                                  itype maybe-otype)
                                (pass-desc-name pass-desc) src-stx)
                              pdesc)
                          candidate)))))))
        ; doing a breadth-first search of maybe-otype and its subtypes
        ; could go up to parent itype(s) on itype as well
        #;(printf "starting search (~a)...\n" msg)
        (if maybe-otype
            (let ospec-loop ([ospec* (list (id->spec maybe-otype (pass-desc-maybe-olang pass-desc)))]
                             [sub-ospec* '()])
              #;(printf "length opsec*: ~s, sub-ospec*: ~s\n" (length ospec*) (length sub-ospec*))
              (if (null? ospec*)
                  (if (null? sub-ospec*)
                      (and try-to-generate? (try-to-generate))
                      (ospec-loop sub-ospec* '()))
                  (or (find-candidate (spec-type (car ospec*)))
                      (ospec-loop (cdr ospec*) (find-subspecs (car ospec*) sub-ospec*)))))
            (or (find-candidate #f)
                (raise-syntax-error who
                  (format "cannot find a processor that accepts input type ~s and no output type" itype)
                  (pass-desc-name pass-desc) src-stx)))))

      (define parse-proc
        (lambda (pass-name ilang olang)
          (lambda (x)
            (let loop ([x x] [trace? #f] [echo? #f])
              (syntax-case x ()
                [(?echo ?not-colon . rest)
                  (and (eq? (datum ?echo) 'echo) (not (eq? (datum ?not-colon) ':)))
                  (loop #'(?not-colon . rest) trace? #t)]                  
                [(?trace ?not-colon . rest)
                  (and (eq? (datum ?trace) 'trace) (not (eq? (datum ?not-colon) ':)))
                  (loop #'(?not-colon . rest) #t echo?)]
                [(proc-name ?colon itype (arg ...) ?arrow otype (rv ...) body ...)
                  (let ([squawk (lambda (msg what) (raise-syntax-error (maybe-syntax->datum pass-name) msg what))])
                    (unless (identifier? #'proc-name) (squawk "invalid processor name" #'proc-name))
                    (unless (eq? (datum ?colon) ':) (squawk "expected colon" #'?colon))
                    (let ([maybe-itype
                            (syntax-case #'itype ()
                              [* (eq? (datum *) '*) #f]
                              [id
                                (identifier? #'id)
                                (if ilang
                                    (if (or (nonterm-id->ntspec? #'id (language-ntspecs ilang))
                                            (term-id->tspec? #'id (language-tspecs ilang)))
                                        #'id
                                        (squawk "unrecognized input non-terminal" #'id))
                                    (squawk "specified input non-terminal without input language" #'id))]
                              [_ (squawk "invalid input type specifier" #'itype)])])
                      (let ([arg* (stx->list #'(arg ...))])
                        (when maybe-itype
                          (when (null? arg*) (squawk "expected non-empty argument list" arg*))
                          (unless (identifier? (car arg*)) (squawk "invalid first argument" (car arg*))))
                        (let-values ([(fml* init*)
                                       (let f ([arg* arg*] [dflt? #f])
                                         (if (null? arg*)
                                             (values '() '())
                                             (syntax-case (car arg*) ()
                                               [id
                                                 (identifier? #'id)
                                                 (if dflt?
                                                     (squawk "missing default value" #'id)
                                                     (let-values ([(fml* init*) (f (cdr arg*) #f)])
                                                       (values (cons #'id fml*) init*)))]
                                               [[id expr]
                                                 (identifier? #'id)
                                                 (let-values ([(fml* init*) (f (cdr arg*) #t)])
                                                   (values (cons #'id fml*) (cons #'expr init*)))]
                                               [arg (squawk "invalid argument specifier" #'arg)])))])
                          (unless (eq? (datum ?arrow) '->) (squawk "expected arrow" #'?arrow))
                          (let ([maybe-otype (syntax-case #'otype ()
                                               [* (eq? (datum *) '*) #f]
                                               [id
                                                 (identifier? #'id)
                                                 (if olang
                                                     (if (or (nonterm-id->ntspec? #'id (language-ntspecs olang))
                                                             (term-id->tspec? #'id (language-tspecs ilang)))
                                                         #'id
                                                         (squawk "unrecognized output non-terminal" #'id))
                                                     (squawk "specified output non-terminal without output language" #'id))]
                                               [_ (squawk "invalid output-type specifier" #'otype)])])
                            (make-pdesc #'proc-name maybe-itype fml* init*
                              maybe-otype (syntax->list #'(rv ...)) #'(body ...) trace? echo?))))))])))))

      (define lookup-lang
        (lambda (pass-name maybe-name)
          (if maybe-name
              (let* ([olang-pair (syntax-local-value maybe-name)]
                     [lang (and olang-pair (car olang-pair))]
                     [meta-parser (and olang-pair (cdr olang-pair))])
                (unless (language? lang)
                  (raise-syntax-error (maybe-syntax->datum pass-name) "unrecognized language" maybe-name))
                (unless (procedure? meta-parser)
                  (raise-syntax-error (maybe-syntax->datum pass-name) "missing meta parser for language" maybe-name))
                (values lang meta-parser))
              (values #f #f))))

      (define build-checked-body
        (lambda (pass-desc maybe-fml xval* maybe-itype maybe-otype maybe-ometa-parser maybe-body)
          (define generate-output-check
            (lambda (type x ntspec*)
              ((lambda (ls) (syntax-case ls ()
                              [(thing) #'thing]
                              [(stuff ...) #'(or stuff ...)]))
               (let f ([ntspec (nonterm-id->ntspec who type ntspec*)] [test* '()])
                 #`((#,(ntspec-all-pred ntspec) #,x)
                    #,@(foldl
                         (lambda (alt test*)
                           (if (nonterminal-alt? alt)
                               (f (nonterminal-alt-ntspec alt) test*)
                               test*))
                         test* (ntspec-alts ntspec)))))))
          (define generate-body
            (lambda (maybe-olang maybe-otype)
              (cond
                [(and maybe-body maybe-otype)
                 (rhs-in-context-quasiquote (pass-desc-name pass-desc) maybe-otype
                   maybe-olang maybe-ometa-parser maybe-body)]
                [maybe-body]
                [else
                 (unless (stx-null? xval*)
                   (raise-syntax-error who "cannot auto-generate body for pass with extra return values"
                     (pass-desc-name pass-desc)))
                 (let ([ilang (pass-desc-maybe-ilang pass-desc)])
                   (unless ilang
                     (raise-syntax-error who "cannot auto-generate body without input language"
                       (pass-desc-name pass-desc)))
                   (let ([itype (or maybe-itype (language-entry-ntspec ilang))])
                     (let ([pdesc (find-proc "generating body" pass-desc (pass-desc-name pass-desc) itype maybe-otype #t
                                    (lambda (id* dflt*) (= (length dflt*) (length id*)))
                                    (lambda (dflt*) (= (length dflt*) 0)))])
                       (let ([init* (pdesc-dflt* pdesc)] [rv* (pdesc-xval* pdesc)])
                         (if (null? rv*)
                             #`(#,(pdesc-name pdesc) #,maybe-fml #,@init*)
                             #`(let-values ([(result #,@(map (lambda (x) (gensym "rv")) rv*))
                                             (#,(pdesc-name pdesc) #,maybe-fml #,@init*)])
                                 result))))))])))
          (let ([olang (pass-desc-maybe-olang pass-desc)])
            (if olang
                (begin
                  #;(printf "olang: ~s\n" olang)
                (let ([otype (or maybe-otype (language-entry-ntspec olang))])
                  (with-syntax ([checked-body
                                  #`(unless #,(generate-output-check otype #'x (language-ntspecs olang))
                                      (error '#,(pass-desc-name pass-desc)
                                        (format "expected ~s but got ~s" '#,(datum->syntax #'* otype) x)))])
                    (if (null? xval*)
                        #`(let ([x #,(generate-body olang otype)])
                            checked-body
                            x)
                        (with-syntax ([(res* ...) (generate-temporaries xval*)])
                          #`(let-values ([(x res* ...) #,(generate-body olang otype)])
                              checked-body
                              (values x res* ...)))))))
                (generate-body #f #f)))))

      (define do-define-pass
        (lambda (pass-name echo? maybe-iname maybe-itype fml* maybe-oname maybe-otype xval* defn* p* maybe-body)
          (define echo-pass
            (lambda (x)
              (when echo?
                (printf "pass ~s expanded into:\n" (maybe-syntax->datum pass-name))
                (pretty-print (maybe-syntax->datum x))
                (newline))
              x))
          #;(unless (and maybe-iname (not (null? fml*)))
          (raise-syntax-error who "can't yet handle \"*\" iname" pass-name))
        (let-values ([(maybe-ilang maybe-imeta-parser) (lookup-lang pass-name maybe-iname)]
                     [(maybe-olang maybe-ometa-parser) (lookup-lang pass-name maybe-oname)])
          (when (and maybe-itype (not (nonterm-id->ntspec? maybe-itype (language-ntspecs maybe-ilang))))
            (raise-syntax-error who "unrecognized pass input non-terminal" pass-name maybe-itype))
          (when (and maybe-otype (not (nonterm-id->ntspec? maybe-otype (language-ntspecs maybe-olang))))
            (raise-syntax-error who "unrecognized pass output non-terminal" pass-name maybe-otype))
          (let* ([pdesc* (map (parse-proc pass-name maybe-ilang maybe-olang) p*)]
                 [pass-desc (make-pass-desc pass-name maybe-ilang maybe-olang pdesc*)]
                 [body (build-checked-body pass-desc (and (pair? fml*) (car fml*))
                         xval* maybe-itype maybe-otype maybe-ometa-parser maybe-body)])
            (echo-pass
              (with-syntax ([who (datum->syntax pass-name 'who)])
                #`(define #,pass-name
                    (lambda #,fml*
                      (define who '#,pass-name)
                      #,@defn*
                      #,@(make-processors pass-desc maybe-imeta-parser maybe-ometa-parser)
                      #,body))))))))

    (syntax-case x ()
      [(_ pass-name ?colon iname (fml ...) ?arrow oname (xval ...) stuff ...)
       (let ([squawk (lambda (msg what) (raise-syntax-error who msg x what))])
         (unless (identifier? #'pass-name) (squawk "invalid pass name" #'pass-name))
         (unless (eq? (datum ?colon) ':) (squawk "expected colon" #'?colon))
         (let-values ([(maybe-iname maybe-itype)
                       (syntax-case #'iname ()
                         [* (eq? (datum *) '*) (values #f #f)]
                         [iname (identifier? #'iname) (values #'iname #f)]
                         [(iname itype)
                           (and (identifier? #'iname) (identifier? #'itype))
                           (values #'iname #'itype)]
                         [_ (squawk "invalid input language specifier" #'iname)])])
           (let ([fml* (stx->list #'(fml ...))])
             (unless (andmap identifier? fml*) (squawk "expected list of identifiers" fml*))
             (when (and maybe-iname (null? fml*)) (squawk "expected non-empty list of formals" fml*))
             (unless (eq? (datum ?arrow) '->) (squawk "expected arrow" #'?arrow))
             (let-values ([(maybe-oname maybe-otype)
                           (syntax-case #'oname ()
                             [* (eq? (datum *) '*) (values #f #f)]
                             [id (identifier? #'id) (values #'id #f)]
                             [(oname otype)
                              (and (identifier? #'oname) (identifier? #'otype))
                              (values #'oname #'otype)]
                             [_ (squawk "invalid output-language specifier" #'oname)])])
               (define (s1 stuff* defn* processor* echo?)
                 (if (stx-null? stuff*)
                     (s2 defn* processor* #f echo?)
                     (let ([stuff (stx-car stuff*)])
                       (if (let processor? ([stuff stuff] [mcount 0])                        
                             (syntax-case stuff ()
                               [(pname ?colon itype (fml ...) ?arrow otype (xval ...) . more)
                                (and  (eq? (datum ?colon) ':)
                                      (eq? (datum ?arrow) '->)
                                      (identifier? #'itype)
                                      (identifier? #'otype)
                                      (andmap (lambda (fml)
                                                (or (identifier? fml)
                                                    (syntax-case fml ()
                                                      [[fml exp-val] (identifier? #'fml)])))
                                        (stx->list #'(fml ...)))
                                      #t)]
                               [(?modifier ?not-colon . more)
                                (and (memq (datum ?modifier) '(trace echo))
                                     (not (eq? (datum ?not-colon) ':))
                                     (< mcount 2))
                                (processor? #'(?not-colon . more) (+ mcount 1))]
                               [_ #f]))
                           (s1 (stx-cdr stuff*) defn* (cons stuff processor*) echo?)
                           (s2 defn* processor* #`(begin #,@stuff*) echo?)))))
               (define (s2 defn* processor* maybe-body echo?)
                 #;(printf "do-define-pass: ~s, ~s, ~s, ~s, ~s, ~s, ~s, ~s, ~s, ~s, ~s\n"
                   #'pass-name echo? maybe-iname maybe-itype fml*
                   maybe-oname maybe-otype #'(xval ...) defn* (reverse processor*) maybe-body)
                 (do-define-pass #'pass-name echo? maybe-iname maybe-itype fml*
                   maybe-oname maybe-otype #'(xval ...) defn* (reverse processor*) maybe-body))
               (let s0 ([stuff* #'(stuff ...)] [defn* '()] [echo? #f])
                 (syntax-case stuff* ()
                   [((definitions defn ...) . stuff*)
                     (eq? (datum definitions) 'definitions)
                     (s0 #'stuff* #'(defn ...) echo?)]
                   [((?expand-modifier ?echo) . stuff*)
                     (and (eq? (datum ?expand-modifier) 'expand-modifier)
                          (eq? (datum ?echo) 'echo))
                     (s0 #'stuff* defn* #t)]
                   [_ (s1 stuff* defn* '() echo?)]))))))]
      [(_ . rest) (raise-syntax-error who "invalid syntax" #'(define-pass . rest))])))
