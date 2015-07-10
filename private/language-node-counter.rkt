#lang racket/base
;;; Copyright (c) 2000-2013 Andrew W. Keep
;;; See the accompanying file Copyright for details

(provide define-language-node-counter)

(require (for-syntax racket/syntax
                     racket/base
                     "records.rkt"))

(define-syntax define-language-node-counter
  (lambda (x)
    (define ntspec-counters (make-hasheq))
    (define build-counter-proc
      (lambda (proc-name l)
        (lambda (ntspec)
          (let loop ([alt* (ntspec-alts ntspec)] [term* '()] [nonterm* '()] [pair* '()])
            (if (null? alt*)
                #`(lambda (x)
                    (cond
                      #,@term*
                      #,@pair*
                      #,@nonterm*
                      [else (error '#,proc-name "unrecognized term ~s" x)]))
                (let ([alt (car alt*)] [alt* (cdr alt*)])
                  (cond
                    [(terminal-alt? alt)
                     (loop alt*
                       (cons #`[(#,(tspec-pred (terminal-alt-tspec alt (language-tspecs l))) x) 1] term*)
                       nonterm* pair*)]
                    [(nonterminal-alt? alt)
                     (let ([ntspec (nonterminal-alt-ntspec alt (language-ntspecs l))])
                       (loop alt* term* 
                         (cons #`[(#,(ntspec-all-pred ntspec) x)
                                   (#,(hash-ref ntspec-counters ntspec #f) x)]
                           nonterm*)
                         pair*))]
                    [(pair-alt? alt)
                     (let inner-loop ([fld* (pair-alt-field-names alt)]
                                      [lvl* (pair-alt-field-levels alt)]
                                      [maybe?* (pair-alt-field-maybes alt)]
                                      [acc* (pair-alt-accessors alt)]
                                      [rec* '()])
                       (if (null? fld*)
                           (loop alt* term* nonterm*
                             (cons #`[(#,(pair-alt-pred alt) x) (+ 1 #,@rec*)] pair*))
                           (inner-loop (cdr fld*) (cdr lvl*) (cdr maybe?*) (cdr acc*)
                             (cons 
                               (let ([fld (car fld*)] [maybe? (car maybe?*)] [acc (car acc*)])
                                 (let ([spec (find-spec fld l)])
                                   (if (ntspec? spec)
                                       #`(let ([x (#,acc x)])
                                           #,(let loop ([lvl (car lvl*)] [outer-most? #t])
                                               (if (= lvl 0)
                                                   (if maybe?
                                                       (if outer-most?
                                                           #`(if x (#,(hash-ref ntspec-counters spec #f) x) 0)
                                                           #`(+ a (if x (#,(hash-ref ntspec-counters spec #f) x) 0)))
                                                       (if outer-most?
                                                           #`(#,(hash-ref ntspec-counters spec #f) x)
                                                           #`(+ a (#,(hash-ref ntspec-counters spec #f) x))))
                                                   (if outer-most?
                                                       #`(fold-left
                                                           (lambda (a x) #,(loop (- lvl 1) #f))
                                                           0 x)
                                                       #`(fold-left
                                                           (lambda (a x) #,(loop (- lvl 1) #f))
                                                           a x)))))
                                       0)))
                               rec*))))]
                    [else (raise-syntax-error 'define-language-node-counter
                            "unrecognized alt ~s" alt)])))))))
    (syntax-case x ()
      [(_ name lang)
       (and (identifier? #'name) (identifier? #'lang))
       (lambda (r)
         (let ([l-pair (r #'lang)])
           (unless l-pair (raise-syntax-error 'define-language-node-counter "Unknown language" x #'lang))
           (let ([l (car l-pair)])
             (let ([ntspecs (language-ntspecs l)] [tspecs (language-tspecs l)])
               (with-syntax ([(proc-name ...) (map (lambda (ntspec)
                                                     (let ([n (format-id #'name "count-~a" (ntspec-name ntspec))])
                                                       (hash-set! ntspec-counters ntspec n)
                                                       n))
                                                   ntspecs)])
                 (with-syntax ([(ntspec? ...) (map ntspec-pred ntspecs)]
                               [(tspec? ...) (map tspec-pred tspecs)]
                               [(proc ...) (map (build-counter-proc #'name l) ntspecs)])
                   #'(define name
                       (lambda (x)
                         (define proc-name proc) ...
                         (cond
                           [(ntspec? x) (proc-name x)] ...
                           [(tspec? x) 1] ...
                           [else (error 'name "unrecognized language record ~s" x)])))))))))])))
