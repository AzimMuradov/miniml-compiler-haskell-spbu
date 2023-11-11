# MiniML Compiler

MiniML is a minimal dialect of ML (Meta Language).

**work in progress (see [supported features](#supported-features) and [implemented modules](#implemented-modules))**

## Supported Features

- Basic data types (`unit`, `bool`, `int`, `'a -> 'b`)
- Basic literals (`true`, `false`, decimal integers)
- Basic boolean operators (`&&`, `||`) and arithmetic operators (`+`, `-`, `*`, `/`)
- Comparison operators (`=`, `<>`, `<`, `<=`, `>`, `>=`)
- Unary operators (`-`)
- Standard library
  - `not : bool -> bool`
  - `print_bool : bool -> unit`
  - `print_int : int -> unit`
- Anonymous functions (`fun a b -> a + b`)
- Functions and recursive functions declarations, let-bindings (`let f x = x * x`, `let rec f x = x * f x`, `let x = 42`)
- Let expressions (`let f x = x * x in ...`, `let rec f x = x * f x in ...`, `let x = 42 in ...`)
- Currying
- Conditionals (`if x > 15 then x * 16 else x / 23`)

## Implemented Modules

- Parser - [source](lib/Parser/)
- Type Inference ([Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)) - [source](lib/TypeInference/)
- Abstract Syntax Tree (AST) to A-Normal Form (ANF) Transformations ([The Essence of Compiling with Continuations](https://www.cs.tufts.edu/~nr/cs257/archive/cormac-flanagan/anormal.pdf), [A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)) - [source](lib/Transformations/)
  - AST simplification - [source](lib/Transformations/AstToTypelessAst.hs)
  - Identifier relabeling to avoid naming errors - [source](lib/Transformations/RelabelVars.hs)
  - Conversion to ANF - [source](lib/Transformations/TypelessAstToAnf.hs)

## Developer Documentation

- Development workflow - [flow](docs/dev/flow.md)
