# MiniML Compiler

MiniML is a minimal dialect of ML (Meta Language).

**work in progress (see [supported features](#supported-features) and [implemented modules](#implemented-modules))**

## Supported Features

- Basic data types (`int`, `bool`, `'a -> 'b`)
- Basic literals (decimal integers, `true`, `false`)
- Basic arithmetic operators (`+`, `-`, `*`, `/`) and boolean operators (`&&`, `||`)
- Comparison operators (`=`, `<>`, `<`, `<=`, `>`, `>=`)
- Unary operators (`-`)
- Standard library (`not : bool -> bool`)
- Lambdas (`fun a b -> a + b`)
- Functions and recursive functions declarations, let-bindings (`let f x = x * x`, `let rec f x = x * f x`, `let x = 42`)
- Nested let-bindings, functions (`let f x = x * x in ...`, `let rec f x = x * f x in ...`, `let x = 42 in ...`)
- Currying
- Conditionals (`if ... then ... else ...`)

## Implemented Modules

- Parser - [source](lib/Parser/)
- Type Inference ([Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)) - [source](lib/TypeInference/)

## Developer Documentation

- Development workflow - [flow](docs/dev/flow.md)
