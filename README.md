
# MiniML

A metacircular interpreter for a subset of OCaml expressions. Supports the substitution model, a dynamically scoped environment model, and the environment model manifesting lexical scoping.

Final project for CS51, submitted May 2018.

## Getting Started

### Starting the interpreter

First, compile  `ocamlbuild -use-ocamlfind miniml.byte`, then run `./miniml.byte` to start the interpreter.

### Changing the evaluation model

In the last line of [evaluation](evaluation.ml), set `evaluate` to `eval_s` for substitution model (the default), `eval_d` for the dynamically scoped environment model, or `eval_l` for the lexically scoped environment model.

## Running the Tests

To run the tests, compile the tests `ocamlbuild -use-ocamlfind tests.byte`, then run `./tests.byte` to execute the unit tests. Will print tested for exceptions.
