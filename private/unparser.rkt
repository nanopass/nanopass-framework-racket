#lang racket/base
;;; Copyright (c) 2000-2013 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell,
;;; Leif Andersen
;;; See the accompanying file Copyright for details

(provide define-unparser)

(require
  (only-in "helpers.rkt"
           with-extended-quasiquote
           with-auto-unquote
           define-who)
  (for-syntax racket/syntax
              syntax/stx
              syntax/parse
              racket/base
              "helpers.rkt"
              "records.rkt"
              "syntaxconvert.rkt"))

(define-syntax (define-unparser x)
  (define ntspec-unparsers (make-hasheq))
  
  (define make-unparse-term-clause-body
    (lambda (tspec)
      (let ([h (tspec-handler tspec)])
        (if h
            #`(if raw? ir (#,h ir))
            #'ir))))
  
  (define (make-unparse-proc name tspecs ntspecs ntspec)
    ;; handles alts of the form: LambdaExpr where LambdaExpr is another
    ;; non-terminal specifier with no surrounding markers.
    (define make-nonterm-clause
      (lambda (alt)
        (let ([ntspec (nonterminal-alt-ntspec alt ntspecs)])
          (with-syntax ([nonterm-proc (hash-ref ntspec-unparsers ntspec #f)]
                        [nonterm-pred? (ntspec-all-pred ntspec)])
            (list #`((nonterm-pred? ir) (nonterm-proc ir)))))))
    
    ;; handles alts of the form: x, c where x and c are meta-variables
    ;; that refer to terminals, and have no surrounding marker.
    (define-who make-term-clause ;; only atom alt cases
      (lambda (alt)
        (let ([tspec (terminal-alt-tspec alt tspecs)])
          (with-syntax ([pred? (tspec-pred tspec)]
                        [tspec-body (make-unparse-term-clause-body tspec)])
            #'((pred? ir) tspec-body)))))
    
    (define (strip-maybe tmpl)
      (syntax-case tmpl (maybe)
        [(maybe x) (and (identifier? #'x) (eq? (datum maybe) 'maybe)) (syntax/loc tmpl x)]
        [(a . d) (with-syntax ([a (strip-maybe #'a)] [d (strip-maybe #'d)]) (syntax/loc tmpl (a . d)))]
        [() tmpl]
        [oth tmpl]))
    
    (define build-accessor-expr
      (lambda (acc level maybe?)
        (let loop ([level level] [f #`(lambda (t) 
                                        #,(if maybe?
                                              #`(and t (#,name t raw?))
                                              #`(#,name t raw?)))])
          (if (= level 0)
              #`(#,f (#,acc ir))
              (loop (- level 1) #`(lambda (t) (map #,f t)))))))
    
    (define (build-template-wrapper tmpl alt)
      (with-syntax ([(e ...) (map build-accessor-expr
                                  (pair-alt-accessors alt)
                                  (pair-alt-field-levels alt)
                                  (pair-alt-field-maybes alt))]
                    [(fld ...) (pair-alt-field-names alt)]
                    [tmpl* tmpl])
        (quasisyntax/loc tmpl
          (let ([fld e] ...)
            (with-extended-quasiquote
                (with-auto-unquote (fld ...) #,(syntax/loc tmpl `tmpl*)))))))
    
    (define (make-pair-clause alt)
      (with-syntax ([pred? (pair-alt-pred alt)]
                    [raw-body (build-template-wrapper (strip-maybe (alt-syn alt)) alt)])
        #`((pred? ir)
           #,(let ([pretty (alt-pretty alt)])
               (if pretty
                   #`(if raw?
                         raw-body
                         #,(if (alt-pretty-procedure? alt)
                               (with-syntax ([(acc ...) (pair-alt-accessors alt)])
                                 #`(#,pretty f (acc ir) ...))
                               (build-template-wrapper pretty alt)))
                   #'raw-body)))))
    
    ;; When one nonterminalA alternative is another nonterminalB, we
    ;; expand all the alternatives of nonterminalB with the alternatives
    ;; of nonterminalA However, nonterminalA and nonterminalB cannot
    ;; (both) have an implicit case, by design.
    (partition-syn
     (ntspec-alts ntspec)
     ([term-alt* terminal-alt?] [nonterm-alt* nonterminal-alt?] [pair-alt* otherwise])
     (partition-syn nonterm-alt*
                    ([nonterm-imp-alt* (lambda (alt)
                                         (has-implicit-alt?
                                          (nonterminal-alt-ntspec alt ntspecs)
                                          ntspecs))]
                     [nonterm-nonimp-alt* otherwise])
                    #`(lambda (ir)
                        (cond
                          #,@(map make-term-clause term-alt*)
                          #,@(map make-pair-clause pair-alt*)
                          ;; note: the following two can potentially be combined
                          #,@(apply append (map make-nonterm-clause nonterm-nonimp-alt*))
                          #,@(apply append (map make-nonterm-clause nonterm-imp-alt*))
                          [else (error who "unexpected language form ~s in ~s" ir '#,name)])))))
  
  (define (make-unparser name lang)
    (define l-pair (lookup-language 'define-unparser "unrecognized language name" lang))
    (define desc (car l-pair))
    (define ntspecs (language-ntspecs desc))
    (define tspecs (language-tspecs desc))
    (with-syntax ([(proc-name ...)
                   (map (lambda (ntspec)
                          (let ([n (format-id name "unparse-~a" (ntspec-name ntspec))])
                            (hash-set! ntspec-unparsers ntspec n)
                            n))
                        ntspecs)]
                  [(ntspec? ...) (map ntspec-pred ntspecs)]
                  [(tspec? ...) (map tspec-pred tspecs)]
                  [(tspec-body ...) (map make-unparse-term-clause-body tspecs)])
      (with-syntax ([(proc ...)
                     (map (lambda (ntspec)
                            (make-unparse-proc name tspecs ntspecs ntspec))
                          ntspecs)])
        #`(define #,name
            (case-lambda
              [(ir) (#,name ir #f)]
              [(ir raw?)
               (define-who proc-name proc) ...
               (cond
                 [(ntspec? ir) (proc-name ir)] ...
                 ; TODO: should be calling the prettify function on these potentially
                 [(tspec? ir) tspec-body] ...
                 [else (error '#,name
                              "unrecognized ~s language form ~s"
                              '#,lang
                              ir)])])))))
  (syntax-parse x
    [(_ name:id lang:id)
     (make-unparser #'name #'lang)]))
