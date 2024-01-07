# MiniML Compiler Implementation

## Compilation Phases

Initial - Text of the Program

| Phase                             | Resulting Representation |           Properties            |
| :-------------------------------- | :----------------------: | :-----------------------------: |
| Parsing                           |           AST            |    **SYNTACTICALLY CORRECT**    |
| Type Checking                     |           AST            |    **SEMANTICALLY CORRECT**     |
| Simplification                    |      Simplified AST      |          **NO TYPES**           |
| Identifier Relabeling             |      Simplified AST      | **ALL IDENTIFIERS ARE UNIQUE**  |
| Closure Conversion (CC)           |      Simplified AST      |        **CLOSURE FREE**         |
| Lambda Lifting (LL)               |           LFR            | **ALL FUNCTIONS ARE TOP-LEVEL** |
| Conversion to A-Normal Form (ANF) |           ANF            |                                 |
| LLVM IR Code Generation           |         LLVM IR          |                                 |

### Parsing

Source: [lib/Parser/](../../lib/Parser/).

### Type Checking

Source: [lib/TypeChecker/](../../lib/TypeChecker/).

[Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)

### Transformations

Source: [lib/Transformations/](../../lib/Transformations/).

#### AST Simplification

Source: [lib/Transformations/Simplification/](../../lib/Transformations/Simplifier/).

#### Identifier Relabeling

Identifier relabeling to avoid naming errors.

Source: [lib/Transformations/Relabeler/](../../lib/Transformations/Relabeler/).

#### Closure Conversion

Source: [lib/Transformations/Cc/](../../lib/Transformations/Cc/).

#### Lambda Lifting

Source: [lib/Transformations/Ll/](../../lib/Transformations/Ll/).

#### Conversion to ANF

Source: [lib/Transformations/Anf/](../../lib/Transformations/Anf/).

[The Essence of Compiling with Continuations](https://www.cs.tufts.edu/~nr/cs257/archive/cormac-flanagan/anormal.pdf)

[A-Normalization: Why and How](https://matt.might.net/articles/a-normalization/)

### LLVM IR Code Generation

Source: [lib/CodeGen/Ll/](../../lib/CodeGen/).
