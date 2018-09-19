# MiniML
A minimal subset of the OCaml language itself implemented within OCaml.

## Contributors
Ethan Craigo: most of expr.ml, expr.mli, evaluation.ml; minor edits to miniml_lex.mll and miniml_parse.mly.

Stuart M. Shieber: miniml.ml, miniml_lex.mll, miniml_parse.mly; stub versions of expr.ml, expr.mli, evaluation.ml.

## Functionality

Addition, multiplication, negation ("~"), and subtraction are supported for arithmetic; equality and less-than testing are supported for binary comparisons. Supported data types are integers, Boolean values, references to values, and functions. Only bound variables are permitted in evaluations. Function definitions (explicitly, with the keyword "fun") and applications are permitted; regular and recursive substitution via "let (rec)" keywords are likewise also permitted. Assignments to references are permitted with the ":=" operator; de-referencing is supported with the "!" operator.

## Usage

These instructions are for Unix machines. OCaml and ocamlbuild must be installed to build this in the recommended manner. Once it is installed, clone the repository, open a terminal in the folder into which the repository was cloned, and first run ```ocamlbuild miniml.byte``` and then ```./miniml.byte```. A prompt will appear, into which one may type expressions. Pressing Enter evaluates the expression and prints the result, if the expression was well-formed. If the expression was not well-formed, an error will be reported. Press Ctrl-C to quit the prompt.

Syntax is similar to regular OCaml, with the biggest differences being that variable values do not persist over multiple lines and also that one must define functions in the form ```let f = fun a -> a * a``` rather than the OCaml syntactic sugar ```let f a = a * a```.

By default this also uses dynamic scoping. Dynamic scoping has its limitations; it does not support nested functions, for example. There is, however, another version of evaluation supported using pure substitution semantics that one can access by changing the line in evaluation.ml that says:

```
let evaluate = eval_d_ref ;;
```

to say:

```
let evaluate = eval_s ;;
```

These substitution semantics support nested functions, but do not support references and de-references.
