# MiniML Compiler

MiniML is a minimal dialect of ML (Meta Language).

> [!NOTE]
> Work In Progress
>
> See [supported features](#supported-features) and [CLI app docs](#command-line-interface-application).

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
- Top-level expressions
- Type annotations (`let int_eq (a : int) (b : int) = a = b`)
- Let expressions (`let f x = x * x in ...`, `let rec f x = x * f x in ...`, `let x = 42 in ...`)
- Currying
- Conditionals (`if x > 15 then x * 16 else x / 23`)

## Command-Line Interface Application

### Examples of Usage

```ocaml
(* fibonacci.ml *)

let rec fib n =
  if n < 2
    then n
    else fib (n - 1) + fib (n - 2);;

print_int (fib 10)
```

```bash
$ miniml run fibonacci.ml
55
$ cat fibonacci.ml | miniml run
55
$ miniml compile fibonacci.ml
$ ./fibonacci.out
55
$ miniml compile fibonacci.ml -o "fib"
$ ./fib
55
```

### Full Documentation

```
-- MiniML Runner & Compiler --

Usage: miniml COMMAND [-d|--debug]

  MiniML is a minimal dialect of ML (Meta Language)

Available options:
  -d,--debug               Execute in debug mode
  -h,--help                Show this help text

Available commands:
  run                      Run MiniML program
  compile                  Compile MiniML program to the provided target
```

```
Run MiniML program

Usage: miniml run [FILE]

  Run MiniML program

Available options:
  FILE                     Program file path (optional)
  -h,--help                Show this help text

Global options:
  -d,--debug               Execute in debug mode
```

```
Compile MiniML program

Usage: miniml compile [FILE] [-t|--target TARGET] [-o|--output OUTPUT]

  Compile MiniML program to the provided target

Available options:
  FILE                     Program file path (optional)
  -t,--target TARGET       Compilation target (binary|llvm-ir) (default: binary)
  -o,--output OUTPUT       Output file path (default: filename taken from the
                           MiniML program)
  -h,--help                Show this help text

Global options:
  -d,--debug               Execute in debug mode
```

## Developer Documentation

- [Implementation details](docs/dev/impl.md)
- [Development workflow](docs/dev/flow.md)

### Build Requirements

**GHC**: 9.4.8
**Cabal**: 3.8
**LLVM**: 17
