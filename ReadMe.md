Nanopass Compiler Library
==========================

This repositiory contains a Racket version of the Nanopass Compiler
Infrastructure described in \[1, 2, 3, 4\], along with the beginnings of a test
compiler for the library and the rough start to a users guide.  The nanopass
framework for Racket has been tested on Racket version 6.0, 6.1, 6.2, and the
current development version.  An R6RS Scheme version also exists which support
Chez Scheme, Vicare Scheme, and Ikarus Scheme.

Files
======



    ReadMe.md               -- this readme file
    Acknowledgements        -- thanks to those who have supported the work
    Copyright               -- copyright information
    TODO                    -- the head of the infinite todo list
    LOG                     -- change log for the nanopass framework
    main.rkt                -- the main interface to the nanopass compiler library
    base.rkt                -- the main interface to the nanopass/base compiler library
    private/                -- contains the parts that nanopass.ss aggregates
    lang/reader.rkt         -- contains the Racket lang definition for the nanopass framework
    tests/                  -- contains a testing compiler along with tests for that
                               compiler and a driver for running the tests
    tests/test-all.rkt      -- is a simple wrapper for importing the compiler and
                               performing a testing run of all of the tests.
    doc/                    -- contains a user guide and developer guide along with a
                               makefile for generating their pdfs with pdflatex

For more information on using the pre-compile binaries, see the README.md file
in the `lib` directory.

References
===========

[1] A. Keep and R. K. Dybvig. A Nanopass Compiler for Commercial Compiler
    Development. In ICFP ’13: Proceedings of the 18th ACM SIGPLAN International
    Conference on Functional Programming, New York, NY, USA, 2013. ACM.

[2] A. Keep. A Nanopass Framework for Commercial Compiler Development.
    Doctoral dissertation, Indiana University,
    Bloomington, Indiana, USA, Feb. 2013.

[3] D. Sarkar. Nanopass Compiler Infrastructure. 
    Doctoral dissertation, Indiana University, 
    Bloomington, Indiana, USA, 2008.

[4] D. Sarkar, O. Waddell, and R. K. Dybvig. A nanopass infrastructure for 
    compiler education. In ICFP ’04: Proceedings of the ninth ACM SIGPLAN 
    International Conference on Functional Programming, pages 201–212, 
    New York, NY, USA, 2004. ACM.
