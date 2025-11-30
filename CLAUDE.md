# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell implementation of a MicroC language interpreter/analyzer. MicroC is a simplified C-like language with support for:
- Functions with arguments and return values
- Variable declarations
- Control flow (if/else, while loops)
- Expressions with arithmetic, comparison, and pointer operations
- Records (struct-like data structures)
- Heap memory management with pointers and allocation
- Input/output operations
- Bilingual keywords (English and Czech)

## Build System

This project uses **Stack** (Haskell build tool):

```bash
# Build the project
stack build

# Run the executable (with CLI options)
stack run microc -- run <program.mc> [args...]
stack run microc -- type <program.mc>

# Run tests
stack test

# Run a specific test suite
stack test apr-semestral:apr-semestral-test

# Run a single test (use -m pattern matching)
stack test --test-arguments "-m \"specific test name\""

# Enter GHCi with project modules loaded
stack ghci

# Clean build artifacts
stack clean
```

## CLI Usage

The `microc` executable provides two main commands:

**Run a program:**
```bash
stack run microc -- run <program.mc> [args...]
```
- Parses the source file
- Runs semantic analysis (undefined/duplicate names, lvalue checks, etc.)
- Executes the `main()` function with provided integer arguments
- Returns exit code from `main()`

**Type check a program:**
```bash
stack run microc -- type <program.mc>
```
- Parses the source file
- Performs constraint-based type inference
- Prints inferred types for functions and identifiers

## Language Features

MicroC programs consist of functions only (no global variables). Each function:
- Has N parameters
- Declares local variables at the beginning (`var x, y, z;`)
- Contains statements (assignment, if/else, while, output)
- Returns a single expression at the end

**Example:**
```c
main() {
    var rec, ptr;
    rec = { foo: 3, bar: main };
    ptr = &(rec.bar);
    *ptr = 5;
    output rec.bar;
    return 0;
}
```

**Types:**
- `Int`: Integer values
- `↑T`: Pointer to type T
- `{ field: T, ... }`: Record types (struct-like)
- Functions are first-class (can be stored in records/pointers)

**Key Operations:**
- `alloc <expr>`: Allocates heap memory (like malloc)
- `&<expr>`: Takes address (reference)
- `*<expr>`: Dereferences pointer
- `input`: Reads integer from stdin
- `output <expr>`: Prints value to stdout

## Code Architecture

### Multi-Stage Pipeline

The codebase follows a classic interpreter/compiler architecture:

1. **Lexing** (Lex/): Tokenizes source code into tokens
2. **Parsing** (Parse/): Builds an Abstract Syntax Tree (AST)
3. **Analysis** (Analysis/): Semantic analysis and type checking
4. **Interpretation** (Interpreter/): Evaluates the AST
5. **Compilation** (IR/): Three-Address Code (TAC) generation (in progress)
6. **Control Flow** (Analysis/Cfg/): Control Flow Graph construction (in progress)

### Module Structure

**Lex/** - Lexical Analysis
- `Tokens.hs`: Token definitions including bilingual keywords (English/Czech)
- `Lexer.hs`: Parsec-based lexer functions for tokenizing

**Parse/** - Syntax Analysis
- `AST.hs`: Complete AST data types (Program, FunDecl, FunBlock, Stmt, Expr, BiOp, UnOp)
- `ExprParser.hs`: Expression parser with operator precedence (uses Parsec.Expr)
  - Handles postfix operations (field access, function calls)
  - Arithmetic with proper precedence
  - Unary operators (*, &, alloc)
- `DeclParser.hs`: Top-level parser for function declarations and variable statements
- `StmtParser.hs`: Statement parsers (output, while, if/else, blocks, assignments)

**Interpreter/** - Execution (Work in Progress)
- `Data.hs`: Runtime value types (VNumber, Pointer, Function, Record)
- `State.hs`: Interpreter state management
  - Stack-based variable scoping (stack of Maps)
  - Heap memory model (Map Address Value)
  - Functions: `getsVar`, `putsVar`, `getsAddr`, `putsAddr`, `newFrame`, `dropFrame`
- `Interpret.hs`: Expression and statement evaluation

**Analysis/** - Static Analysis
- `Analysis.hs`: Entry point for running semantic analysis (combines Semantics and Typecheck)
- `Semantics.hs`: Semantic checks including:
  - Undefined/duplicate identifiers
  - Taking address of functions
  - Invalid assignments (must be lvalues)
  - Record field validation
  - Recursive record detection
- `Typecheck/`: Constraint-based type inference system
  - `Type.hs`: Type representation (Int, Ptr, Fun, Record, type variables)
  - `Constraints.hs`: Constraint types (Typeable wrapper for AST nodes)
  - `ConstraintGenerator.hs`: Generates type constraints from AST
  - `ConstraintSolver.hs`: Solves constraint system via unification
  - `Typecheck.hs`: Main entry point for type checking
- `Cfg/`: Control Flow Graph construction (Work in Progress)
  - `Cfg.hs`: CFG data structures (CFGNode, CFGMap) with FunEntry/FunExit/Node
  - `Builder.hs`: State monad-based CFG builder for statements and functions

**IR/** - Intermediate Representation (Work in Progress)
- `Tac.hs`: Three-Address Code (TAC) instruction set and Emitter monad
  - Instructions: arithmetic ops, jumps, calls, memory operations
  - Uses Writer monad pattern for code generation
- `TacCompiler.hs`: Compiles AST to TAC format
- `CompilerState.hs`: State management for TAC generation (registers, labels)
- `Emit.hs`: Helper functions for emitting TAC instructions

### Key Design Decisions

**Everything Lives on the Heap**: All values (including variables) are stored on the heap and referenced by addresses. This simplifies pointer semantics but differs from typical stack/heap separation.

**Stack Contains Addresses**: Each stack frame is a `Map Identifier Address` that maps variable names to heap addresses. This enables easy pointer manipulation.

**Bilingual Support**: Keywords support both English and Czech (e.g., "var"/"pro", "return"/"vrať", "output"/"vypiš"). The lexer handles this via the `strToKw` mapping in `Tokens.hs`.

**Parsec-Based Parsing**: Uses Text.Parsec for both lexing and parsing. Expression parsing uses `Text.Parsec.Expr.buildExpressionParser` for operator precedence.

**Constraint-Based Type System**: Type checking uses a constraint generation and solving approach:
- Each AST node generates type constraints
- Constraints are unified to infer types
- Supports recursive types via type variable bindings (μ notation)

### AST Structure Notes

- `FunBlock` contains three parts: variable declarations (`idDecl`), statements (`body`), and a return expression
- `Stmt` includes `AssignmentStmt Expr Expr` (left-hand side can be any lvalue expression)
- `Expr` supports postfix operations: `Call` for function calls, `FieldAccess` for record fields
- Records use `newtype Record = Fields [(Identifier, Expr)]`

## Testing

Tests are written using HSpec and located in `test/`:

**Parser Tests:**
- `ExprParserSpec.hs`: Comprehensive expression parser tests
- `StmtParserSpec.hs`: Statement parser tests
- `DeclParserSpec.hs`: Declaration parser tests

**Interpreter Tests:**
- `StateSpec.hs`: Interpreter state management tests
- `InterpretExprSpec.hs`: Expression evaluation tests
- `InterpretStmtSpec.hs`: Statement execution tests

**Analysis Tests:**
- `TypecheckSpec.hs`: Type inference system tests
- `ConstraintSolverSpec.hs`: Constraint unification tests
- `CfgSpec.hs`: Control Flow Graph tests

**Test Utilities:**
- `TestUtils.hs`: Shared helper functions for tests

Test files follow the pattern: parse/execute input and compare against expected results.

## Common Development Workflows

When adding new language features:
1. Add tokens to `Lex/Tokens.hs` if needed (including bilingual support)
2. Add lexer functions to `Lex/Lexer.hs`
3. Update AST types in `Parse/AST.hs` (ensure Data and Typeable instances)
4. Implement parser in appropriate `Parse/*Parser.hs` file
5. Add test cases in corresponding `test/*Spec.hs` file
6. Update `Analysis/Typecheck/ConstraintGenerator.hs` if needed
7. Implement interpreter logic in `Interpreter/Interpret.hs`

When working on the interpreter:
- Follow the state monad pattern using `StateT State IO`
- Use helper functions from `Interpreter/State.hs` for state manipulation
- Remember that all values go through the heap (use `putsVar`, not direct map insertion)
- Functions are stored in global scope during initialization (see `Main.hs:initializeState`)

When working on type checking:
- Constraint generation happens in `ConstraintGenerator.hs` via `genConstraintsFun`
- Constraints are solved via unification in `ConstraintSolver.hs`
- Type variables use `Unknown Int` for fresh variables and `BoundTypeVar Int` for recursive types
- The `Typeable` wrapper tags AST nodes with their location for better error messages

When working on TAC compilation:
- The `Emitter` monad combines `WriterT TAC (State CState)` for code generation
- Use `emit_` to emit instructions, `emitL` to emit labeled instructions
- `emitExpr` and `emitStmt` recursively compile AST nodes to TAC
- Registers are allocated via the state monad in `CompilerState.hs`
- Use `RecursiveDo` extension (`mdo`) for forward references in control flow

When working on CFG construction:
- CFG nodes have unique IDs and track predecessors/successors
- Use the State monad pattern with `CFGMap` to build graphs incrementally
- `buildStmt` returns (first node, list of last nodes) for proper edge connections
- Functions have explicit FunEntry and FunExit nodes for graph boundaries
