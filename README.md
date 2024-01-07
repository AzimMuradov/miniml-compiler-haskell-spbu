# MiniML Compiler

MiniML is a minimal dialect of ML (Meta Language).

> [!NOTE]
> Work In Progress
>
> See [supported features](#supported-features) and [CLI app docs](#command-line-interface-application).

## Command-Line Interface Application

For help: `miniml --help`

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

## Developer Documentation

- Implementation details - [impl](docs/dev/impl.md)
- Development workflow - [flow](docs/dev/flow.md)
