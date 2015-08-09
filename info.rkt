#lang info
(define version "0.1")
(define collection "nanopass")

(define deps '("base"
               "rackunit-lib"
               "compatibility-lib"
               "unstable-pretty-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("doc/user-guide.scrbl")))
