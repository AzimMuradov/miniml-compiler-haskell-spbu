# An implementation of Go mini-language

**Author**: Azim Muradov, azim.muradov.dev@gmail.com

**License**: APACHE LICENSE, VERSION 2.0


## Project parts:

- application
  - **CLI app** built with [optparse-applicative](https://github.com/pcapriotti/optparse-applicative)
- library
  - **parser** and **lexer** implemented using [megaparsec](https://github.com/mrkkrp/megaparsec)
  - **analyzer**
    - check for name collision or missing names
    - type checker
    - const expressions converters (w/o `const` keyword)
  - **interpreter**


## Features:

- supported types: `int`, `bool`, `string`, multidimensional arrays, functions
- `void` support
- function literals (anonymous functions), closures (including mutable closures) support
- operators (arithmetic, logic, comparison) support
- `if`, `else`, `for` support
- recursion, mutual recursion support
- function definitions, globals support
- variable declarations, variable assignments, short variable declarations (`:=`) support
- increment, decrement support
- supported stdlib functions: `len`, `print`, `println`, `panic`


## TODO:

- add pretty printer
- add missing docs
- add unit tests
