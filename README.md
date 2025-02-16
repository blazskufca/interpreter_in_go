# Interpreter in Go ![CI Workflow](https://github.com/blazskufca/interpreter_in_go/actions/workflows/ci.yaml/badge.svg)

This repository follows/implements in-place tree interpreter from book [Writing An Interpreter In Go By Thorsten Ball](https://interpreterbook.com/)
and bytecode interpreter from [Writing A Compiler In Go by Thorsten Ball](https://compilerbook.com/) and slightly expands on them.

For a bit of a language specification checkout [Monkey](https://monkeylang.org/) (or/and checkout the tests in packages).

![Monkey language](https://github.com/user-attachments/assets/95034ecc-7427-46d7-ab7a-a9028c93debf)

![Performance benchmark: in-place evaluator vs. bytecode](https://github.com/user-attachments/assets/a35fb9d3-961a-4958-902a-68d811f5ceef)

## Run locally

There are two modes of operation tree walking interpreter and a bytecode interpreter.
You can select between them with the `-m` flag:

- ***Tree walking interpreter:*** `./monkey-REPL -m inplace-evaluator`. _This is the default option_


- ***Bytecode interpreter:*** `./monkey-REPL -m bytecode`. _Note that for now the bytecode interpreter does not
support macros. Besides this the feature set is identical_
