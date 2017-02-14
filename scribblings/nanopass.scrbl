#lang scribble/manual

@require[@for-label[@except-in[racket + - * => -> ...]]]

@title{Nanopass Framework}
@author["Andrew W. Keep" "Leif Andersen"]

@;@declare-exporting[nanopass/base nanopass]
@declare-exporting[nanopass/base nanopass #:use-sources (nanopass/base)]

@defmodule*/no-declare[(nanopass/base)]{
 The @racketmodname[nanopass/base] library provides all of
 the Nanopass framework.}

@defmodulelang*/no-declare[(nanopass)]{
 The @racketmodname[nanopass] language provides everything
 in @racketmodname[nanopass/base], as well as the 
 @racketmodname[racket], language. Making it suitable for
 use as language.}

@section{Overview}
The nanopass framework provides a tool for writing compilers composed of
several simple passes that operate over well-defined intermediate languages.
The goal of this organization is both to simplify the understanding of each
pass, because it is responsible for a single task, and to simplify the addition
of new passes anywhere in the compiler.

The most complete, public example of the nanopass framework in use is in the
@tt{tests/compiler.rkt} file.
This is the start of a compiler implementation for a simplified subset of
Racket, used in a course on compiler implementation.

@section{Defining Languages}

The nanopass framework operates over a set of compiler-writer defined
languages.
A language definition for a simple variant of the Racket programming language
might look like:

@racketblock[
(define-language L0
  (terminals
    (variable (x))
    (primitive (pr))
    (datum (d))
    (constant (c)))
  (Expr (e body)
    x
    pr
    c
    (quote d)
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (lambda (x* ...) body* ... body)
    (let ([x* e*] ...) body* ... body)
    (letrec ([x* e*] ...) body* ... body)
    (e0 e1 ...)))
]

The @racket[L0] language consists of a set of terminals (listed in in the
@racket[terminals] form) and a signle non-terminal @racket[Expr].
The terminals of this language are @racket[variable], @racket[primitive],
@racket[datum], and @racket[constant].
Listed with each terminal is one or more meta-variable that can be used to
indicate the terminal in a non-terminal expression.
In this case @racket[variable] can be represented by @racket[x],
@racket[primitive] can be represented by @racket[pr], @racket[datum] can be
represented by @racket[d], and @racket[constant] can be represented by
@racket[c].
The nanopass framework expects the compiler writer to supply a predicate that
corresponds to each terminal.
The name of the predicate is derived from the name of the terminal, by adding a
@racket[?] character.
For the @racket[L0] language we will need to provide @racket[variable?],
@racket[primitive?], @racket[datum?], and @racket[constant?] predicates.
We might decided to represent variables as symbols, select a small list of
primitives, represent datum as any Racket value, and limit constants to those
things that have a syntax that does not require a quote:

@racketblock[
(define variable?
  (lambda (x)
    (symbol? x)))

(define primitive?
  (lambda (x)
    (memq x '(+ - * / cons car cdr pair? vector make-vector vector-length
              vector-ref vector-set! vector? string make-string
              string-length string-ref string-set! string? void))))

(define datum?
  (lambda (x)
    #t))

(define constant?
  (lambda (x)
    (or (number? x)
        (char? x)
        (string? x))))
]

The @racket[L0] language also defines one non-terminal @racket[Expr].
Non-terminals start with a name (@racket[Expr]) followed by a list of
meta-variables (@racket[(e body)]) and a set of productions.
Each production follows one of three forms.
It is either a stand alone meta-variable (in this case @racket[x], @racket[pr],
or @racket[c]), an S-expression that starts with a literal (in this case
@racket[(quote d)], @racket[(begin e* ... e)], @racket[(if e0 e1)], etc.), or
an S-expression that does not start with a literal (in this case
@racket[(e0 e1 ...)].
In addition to the numeric or start (@racket[*]) suffix used in the example, a
question mark (@racket[?]) or carot (@racket[^]) can also be used.
The suffixes can also be used in combination.
The suffixes do not contain any semantic value and are used simply to allow the
meta-variable to be used more then once (as in the @racket[(if e0 e1 e2)]
form).

@subsection{Extending Languages}

Now that we have the @racket[L0] language we can imagine specifying a language
with only the two-armed if, i.e., @racket[(if e0 e1 e2)], only quoted
constants, and lambda, let, and letrec bodies that contain only a single
expression.
One option is to simply define a whole new langauge from scratch that contains
all the elements of the old language, with just the changes we want.
This will work fine, but we might want to simply define what changes.
The nanopass framework provides a syntax to allow a language to be defined as
an extension to an already defined language:

@racketblock[
(define-language L1
  (extends L0)
  (terminals
    (- (constant (c))))
  (Expr (e body)
    (- c
       (if e0 e1)
       (lambda (x* ...) body* ... body)
       (let ([x* e*] ...) body* ... body)
       (letrec ([x* e*] ...) body* ... body))
    (+ (lambda (x* ...) body)
       (let ([x* e*] ...) body)
       (letrec ([x* e*] ...) body))))
]

The newly defined language uses the @racket[extends] form to indicate that
@racket[L1] is an extension of the existing language @racket[L0].
The @racket[extends] form changes how the terminal and non-terminal forms are
processed.
These forms are now expected to contain a set of @racket[-] forms indicating
productions or terminals that should be removed or @racket[+] forms indicating
productions or terminals that should be added.
It is also possible to add new non-terminals, by simply including a new
non-terminal form with a single @racket[+] form that adds all of the
productions for the new non-terminal.

@subsection{The define-language form}

The full syntax for @racket[define-language] is as follows:

@defform[#:literals (extends entry terminals => + -)
         (define-language language-name clause ...)
         #:grammar [(clause (extends language-name)
                            (entry non-terminal-name)
                            (terminals terminal-clause ...)
                            (terminals extended-terminal-clause ...)
                            (non-terminal-name (meta-var ...)
                              production-clause ...)
                            (non-terminal-name (meta-var ...)
                              extended-production-clause ...))

                    (terminal-clause (terminal-name (meta-var ...))
                                     (=> (terminal-name (meta-var ...))
                                         prettifier)
                                     (code:line (terminal-name (meta-var ...)) => prettifier))
                    (extended-terminal-caluse (+ terminal-clause ...)
                                              (- terminal-clause ...))

                    (production-clause terminal-meta-var
                                       non-terminal-meta-var
                                       production-pattern
                                       (keyword . production-pattern))

                    (extended-production-clause (+ production-clause ...)
                                                (- production-clause ...))
                    ]]{
Where @racket[clause] is an extension clause, a entry clause, a terminals clause, or a non-terminal clause.
}

@defidform[extends]{
The @racket[extends] clause indicates that the language is an extension of an
existing language.
Only one extension clause can be specified in a language definition.
A @racket[extends] clause is an error elsewhere.

@racket[language-name] is the name of an already defined language.
}

@defidform[entry]{
The @racket[entry] clause specifies which non-terminal is the starting point for
this language.
This information is used when generating passes to determine which non-terminal
should be expected first by the pass.
This default can be overridden in a pass definition, as described in
@secref{pass-syntax}.
Only one entry clause can be specified in a language definition.
A @racket[entry] clause is an error elsewhere.

@racket[non-terminal-name] corresponds to one of the non-terminals specified
in this language (or the language it extends from).
}

@defidform[terminals]{
The @racket[terminals] clause specifies one or more terminals used by the language.
For instance in the @racket[L0] example language, the terminals clause
specifies four terminal types variable, primitive, datum, and constant.

@racket[terminal-name] is the name of the terminal and a corresponding
@racket[terminal-name?] predicate function exists to determine if a Racket
object is of this type when checking the output of a pass.

@racket[meta-var] is the name of a meta-variable used for referring to this
terminal type in language and pass definitions.

@racket[prettifier] is an expression that evaluates to a function of one
argument used when the language unparser is called in ``pretty'' mode to
produce pretty, S-expression representation.

The final form is syntactic sugar for the form above it.
When the @racket[prettifier] is omitted, no processing will be done on the
terminal when the unparser runs.

When a language derives from a base language, use @racket[extended-terminal-clause].
The @racket[+] form indicates terminals that should be added to the new language.
The @racket[-] form indicates terminals that should be removed from the list in
the old language when producing the new language.
Terminals not mentioned in a terminals clause will be copied into the new
language, unchanged.

@emph{Note} that adding and removing @racket[meta-var]s from a terminal
currently requires removing the terminal type and re-adding it.
This can be done in the same step with a terminal clause like the following:

@racketblock[
(terminals
  (- (variable (x)))
  (+ (variable (x y))))
]

A @racket[terminals] clause is an error elsewhere.
}

@subsubsection{Non-terminals clause.}
The non-terminals clause specifies the valid productions in a language.
Each non-terminal has a name, a set of meta-variables, and a set of
productions.

@racket[non-terminal-name] is an identifier that names the non-terminal,
@racket[meta-var] is the name of a meta-variable used when referring to this
non-terminal in a language and pass definitions, and @racket[production-clause]
has one of the following forms:

@racket[terminal-meta-var] is a terminal meta-variable that is a stand-alone
production for this non-terminal.

@racket[non-terminal-meta-var] is a non-terminal meta-variable that
indicates any form allowed by the specified non-terminal is also allowed by
this non-terminal.

@racket[keyword] is an identifier that must be matched exactly when parsing
an S-expression representation, language input pattern, or language output
template.

@racket[production-pattern] is an S-expression that represents a
pattern for language, and has the following form.

@racketgrammar*[(production-pattern meta-variable
                                    (maybe meta-variable)
                                    (production-pattern-sequence ...)
                                    (production-pattern-sequence ...
                                      . production-pattern))
                (production-pattern-sequence production-pattern
                                             (code:line ... (code:comment
                                                             "literal ...")))]

@racket[meta-variable] is any terminal or non-terminal meta-variable,
extended with an arbitrary number of digits, followed by an arbitrary
combination of @racket[*], @racket[?], or @racket[^] characters, for example,
if the meta-variable is @racket[e] then @racket[e1], @racket[e*], @racket[e?],
@racket[e4*?] are all valid meta-variable expressions.

@racket[(maybe meta-variable)] indicates that an element in the
production is either of the type of the meta-variable or bottom (represented by
@racket[#f]).

Thus, Racket language forms such as @racket[let] can be represented as a
language production as:

@racketblock[
(let ([x* e*] ...) body* ... body)
]

where @racket[let] is the @racket[keyword], @racket[x*] is a meta-variable that
indicates a list of variables, @racket[e*] & @racket[body*] are meta-variables
that indicate a list of expressions, and @racket[body] is a meta-variable that
indicates a single expression.
Something similar to the named-let form could also be represented as:

@racketblock[
(let (maybe x) ([x* e*] ...) body* ... body)
]

though this would be slightly different from the normal named let form, in that
the non-named form would then need an explicit @racket[#f] to indicate no name
was specified.

When a language extends from a base language then use
@racket[extended-production-clause].
The @racket[+] form indicates non-terminal productions that should be added to
the non-terminal in the new language.
The @racket[-] form indicates non-terminal productions that should be removed
from list of productions for this non-terminal the old language when producing
the new language.
Productions not mentioned in a non-terminals clause will be copied into the
non-terminal in the new language, unchanged.
If a non-terminal has all of its productions removed in a new language, the
non-terminal will be dropped in the new language.
Conversely, new non-terminals can be added by naming the new non-terminal and
using the @racket[+] form to specify the productions of the new non-terminal.

@subsection{Products of define-language}
The @racket[define-language] form produces three user-visible objects:
@itemize[
@item{a parser (named @racket[parse-language-name]) that can be used to
parse an S-expression into a record-based representation that is used
internally by the nanopass framework;}
@item{an unparser (named @racket[unparse-language-name]) that can be used
to unparse a record-based representation back into an S-expression; and}
@item{a language definition, bound to the specified @racket[language-name].}
]

The language definition is used when the @racket[language-name] is specified as
the base of a new language definition and in the definition of a pass.

@defform[(language->s-expression language-name)]{
Produces an S-expression based on the @racket[language-name].

For extended languages, this will produce an S-expression for the full
language.

In the case of @racket[L1]:

@racketblock[
(language->s-expression L1)
]

Will return:

@racketblock[
(define-language L1
  (terminals
    (variable (x))
    (primitive (pr))
    (datum (d)))
  (Expr (e body)
    (letrec ([x* e*] ...) body)
    (let ([x* e*] ...) body)
    (lambda (x* ...) body)
    x
    pr
    (quote d)
    (begin e* ... e)
    (if e0 e1 e2)
    (e0 e1 ...)))
]
}

@section[#:tag "pass-syntax"]{Defining Passes}

Passes are used to specify transformations over languages defined using
@racket[define-language].
Before getting into the details of defining passes, lets take a look at a
simple pass to convert from @racket[L0] to @racket[L1].
This pass will need to:
@itemize[
@item{Remove one armed @racket[if]s,}
@item{Quote constants in the language, and}
@item{Make @racket[begin] an explicit form in the body of the @racket[lambda],
@racket[let], and @racket[letrec] forms.}
]

We can define a pass called @racket[make-explicit] to make all of these forms
explicit.

@racketblock[
(define-pass make-explicit : L0 (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
    [,x x]
    [,pr pr]
    [,c `(quote ,c)]
    [(quote ,d) `(quote ,d)]
    [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
    [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]
    [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
    [(lambda (,x* ...) ,[body*] ... ,[body])
     `(lambda (,x* ...) (begin ,body* ... ,body))]
    [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,e*] ...) (begin ,body* ... ,body))]
    [(letrec ([,x* ,[e*]] ...) ,[body*] ... ,[body])
     `(letrec ([,x* ,e*] ...) (begin ,body* ... ,body))]
    [(,[e0] ,[e1] ...) `(,e0 ,e1 ...)])
  (Expr ir))
]

The pass definition starts with a name (in this case @racket[make-explicit])
and a signature.
The signature starts with an input language specifier (in this case
@racket[L0]) along with a list of formals.
In this case, we have just one formal @racket[ir] the input-language term.
The second part of the signature has an output language specifier (in this case
@racket[L1]) along with a list of extra return values (in this case empty).

Following the name and signature, the pass can define a set of extra
@racket[definitions] (in this case empty), a set of processors (in this case
@racket[(Expr : Expr (ir) -> Expr () ---)]), and a body expression (in this
case @racket[(Expr ir)]).
Similar to the pass, a processor starts with a name (in this case
@racket[Expr]) and a signature.
The first part of a signature is a non-terminal specifier (in this case
@racket[Expr]) along with the list of formals (in this case just the
@racket[ir]).
Here the @racket[Expr] corresponds to the one defined in the input langauge,
@racket[L0].
The output includes a non-terminal output specifier (in this case @racket[Expr]
as well) and a list of extra return expressions.

After the signature, there is a set of clauses.
Each clause consists of an input pattern, an optional clause, and one or more
expressions specify one or more return values based on the signature.
The input pattern is derived from the S-expressions specified in the input
language.
Each variable in the pattern is denoted by unquote (@tt{,}).
When the unquote is followed by an S-expression (as is the case in of
@racket[,[e*]] and @racket[,[e]] in @racket[(begin ,[e*] ...  ,[e])]) it
indicates a @seclink["cata-morphism"]{cata-morphism}.
The cata-morphism performs automatic recursion on the subform mentioned.

Looking again at the example pass, we might notice that the
@racket[(definitions)] form is empty.
When it is empty there is no need to include it.
The body expression @racket[(Expr ir)] simply calls the processor corresponding
to the entry point of the input language.
This can be automatically generated by @racket[define-pass], so we can omit
this from the definition as well.
Finally, we might notice that several clauses simply match the input pattern
and generate an exactly matching output pattern (modulo the cata-morphisms for
nested @racket[Expr] forms).
Since the input and output languages are defined, the @racket[define-pass]
macro can also automatically generate these clauses.
So let's try again:

@racketblock[
(define-pass make-explicit : L0 (ir) -> L1 ()
  (Expr : Expr (ir) -> Expr ()
    [,c `(quote ,c)]
    [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]
    [(lambda (,x* ...) ,[body*] ... ,[body])
     `(lambda (,x* ...) (begin ,body* ... ,body))]
    [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,e*] ...) (begin ,body* ... ,body))]
    [(letrec ([,x* ,[e*]] ...) ,[body*] ... ,[body])
     `(letrec ([,x* ,e*] ...) (begin ,body* ... ,body))]))
]

This is a much simpler way to write the pass, though both are valid ways of
writing the pass.

@subsection{The define-pass form}

@defform[#:literals (* definitions -> : guard else)
         (define-pass name : lang-specifier (formal ...)
                           -> lang-specifier (extra-return-val ...)
           definitions-clause
           processor ...
           body-expr)
         #:grammar [(language-specifier language-name
                                        (language-name non-terminal-name)
                                        *)

                    (definitions-clause (code:line)
                      (definitions definition-clause ...))

                    (processor (processor-name
                                : non-terminal-spec (formal ...)
                                -> non-terminal-spec (extra-return-val ...)
                                definition-clause
                                processor-clause ...
                                ))

                    (non-terminal-spec non-terminal-name
                                       *)

                    (processor-clause [pattern body-expr ...+]
                                      [pattern (guard expr ...+) body-expr ..+]
                                      [else body-expr ...+]
                                      expr)]]{
@racket[name] is the name of the pass.

@racket[formal] is an identifier representing a formal argument.

@racket[language-name] is an identifier corresponding to a defined language.

@racket[non-terminal-name] is a name in that language.

@racket[extra-return-val] is a Racket expression.

@racket[expr] is a Racket expression that can contain a
@racket[quasiquote-expr].

@racket[definition] is a Racket definition.

@racket[body-expr] is a Racket expression.

@racket[quasiquote-expr] is a Racket quasiquote expression that corresponds to
a form of the output non-terminal specified in a processor.

@racket[non-terminal-spec] determines the behavior of a processor.
If the processor's input @racket[non-terminal-spec] is a @racket[*],
then the body of the processor are racket expressions rather then nanopass
expressions.
If the processor's output @racket[non-terminal-spec] is a @racket[*],
then @racket[quasiquote] will be Racket's @racket[quasiquote],
rather then generating the output language.
Additionally, the first value in @racket[extra-return-val] will be the default
first value returned from the processor.

Otherwise, the @racket[non-terminal-spec] describes the non terminal for the
input and output languages.

The @racket[define-pass] macro re-binds the Racket quasiquote to use as a way
to construct records in the output language.
Similar to the patterns in a processor clause, the quasiquote expression can be
arbitrarily nested to create language records nested within other language
records.
When a literal expression, such as a symbol, number, or boolean expression is
one of the parts of a record, it need not be unquoted.
Any literal that is not unquoted is simply put into the record as is.
An unquoted expression can contain any arbitrary expression as long as the
output of the expression corresponds to the type expected in the record.

When creating a quasiquoted expression, it is important to realize that while
the input and output both look like arbitrary S-expressions, they are not, in
fact, arbitrary S-expressions.
For instance, if we look at the S-expression for a @racket[let] in @racket[L1]
we see:

@racketblock[
(let ([x* e*] ...) body)
]

This will create a record that expects three parts:
@itemize[
@item{a list of variables (@racket[x*]),}
@item{a list of expressions (@racket[e*]), and}
@item{an expression (@racket[body]).}
]

If this were simply an S-expression, we might imagine that we could construct
an output record as:

@racketblock[
(code:comment "binding*: is ([x0 e0] ... [xn en])")
(let ([binding* (f ---)])
  `(let (,binding* ...) ,body))
]

However, this will not work because the infrastructure does not know how to
destructure the @racket[binding*] into its parts.
Instead it is necessary to destructure the @racket[binding*] list before
including it in the quasiquoted expression:

@racketblock[
(code:comment "binding*: is ([x0 e0] ... [xn en])")
(let ([binding* (f ---)])
  (let ([x* (map car binding*)]
        [e* (map cadr binding*)])
    `(let ([,x* ,e*] ...) ,body)))
]
}
@subsection[#:tag "cata-morphism"]{Cata-morphims}

Cata-morphisms are defined in patterns as an unquoted S-expression.  A
cata-morphism has the following syntax:

@racketgrammar*[(cata-morphism ,[identifier ...]
                               ,[identifier expr ... -> identifer ...]
                               ,[func-expr : identifier expr ... -> identifier ...]
                               ,[func-expr : -> identifier ...])]

Where @racket[expr] is a Racket expression, @racket[func-expr] is an expression that
results in a function, and @racket[identifier] is an identifier that will be bound
to the input subexpression when it is on the left of the @racket[-] and the return
value(s) of the func when it is on the right side.

When the @racket[func-expr] is not specified, @racket[define-pass] attempts to find or
auto-generate an appropriate processor.  When the @racket[func-expr] is the name of
a processor, the input non-terminal, output non-terminal, and number of input
expressions and return values will be checked to ensure they match.

If the input expressions are omitted the first input will be the sub-expression
and additional values will be taken from the formals for the containing
processor by name.

@subsection{Auto-generated clauses}

Within a processor a clause can be autogenerated when a matching form exists in
the input-language non-terminal and the output-language non-terminal in a
processor.
For instance, we could eliminate the @racket[x], @racket[pr], and
@racket[(quote d)] clauses in our example pass because these forms exist in
both the @racket[Expr] non-terminal of the input lanugage and the @racket[Expr]
non-terminal of the output language.
The auto-generated clauses can also process sub-forms recursively.
This is why the @racket[(begin ,e* ... ,e)], @racket[(if ,e0 ,e1 ,e2)], and
@racket[(,e0 ,e1 ...)] clauses can be eliminated.

The auto-generated clauses can also handle grammars with more then one
non-terminal.
For each sub-term in an auto-generated clause, a processor is selected.
Processors are selected based on the input non-terminal and output non-terminal
for the given sub-terminal.
A processor with a matching input and output non-terminals is then selected.
In addition to the input and output non-terminal any extra arguments required
by the processor must be available, and any additional return values required
by the auto-generated clause must be returned by the processor.
If an appropriate processor cannot be located, a new processor will be
auto-generated if possible (this is described in the next section) or an
exception will be raised at compile time.

When an @racket[else] clause is included in a processor, no clauses will be
auto-generated, since it is expected that the else clause will handle all other
cases.

@subsection{Auto-generated processors}

When a cata-morphism, auto-generated clause, or auto-generated pass body
expression needs to process a sub-term, but no processor exists that can handle
it, the nanopass framework might create a new processor.
Processors can only be created when only a single argument in the input
language and a single return value in the output language is expected, and the
output-language non-terminal contains productions that match all of the
input-language non-terminal.
This means it is possible for the output-language non-terminal to contain extra
forms, but cannot leave out any of the input terms.

This feature is currently implemented in the framework, but it is possible to
get into trouble with this feature.
In general, there are two cases where undesired clauses can be auto-generated.
@itemize[
@item{A nested pattern in a clause matches all posible clauses, but because the
pattern is nested the framework cannot determine this.
In this case, the
auto-generated clauses are a drag on the system, but the auto-generate
processor(s) will never be called, and the code generated is effectively
unreachable.}
@item{A cata-morphism or auto-generated clause provides a single input expression
and needs a single output expression, but no processor exists that can satisfy
this match.  This can lead to a shadow set of processors being created and
potentially undesired results being generated by a pass.}
]

The first item is a solvable problem, though we have not yet invested the
development time to fix it.  The second problem is more difficult to solve, and
the reason why this feature is still experimental.

