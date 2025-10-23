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

# Run the executable
stack run

# Run tests
stack test

# Run a specific test suite
stack test apr-semestral:apr-semestral-test

# Enter GHCi with project modules loaded
stack ghci

# Clean build artifacts
stack clean
```

## Code Architecture

### Three-Stage Pipeline

The codebase follows a classic interpreter architecture:

1. **Lexing** (Lex/): Tokenizes source code into tokens
2. **Parsing** (Parse/): Builds an Abstract Syntax Tree (AST)
3. **Interpretation** (Interpreter/): Evaluates the AST (in progress)

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
- `Analysis.hs`: Entry point for running semantic analysis
- `Semantics.hs`: Semantic checks including:
  - Undefined/duplicate identifiers
  - Taking address of functions
  - Invalid assignments (must be lvalues)
  - Record field validation
  - Recursive record detection

### Key Design Decisions

**Everything Lives on the Heap**: All values (including variables) are stored on the heap and referenced by addresses. This simplifies pointer semantics but differs from typical stack/heap separation.

**Stack Contains Addresses**: Each stack frame is a `Map Identifier Address` that maps variable names to heap addresses. This enables easy pointer manipulation.

**Bilingual Support**: Keywords support both English and Czech (e.g., "var"/"pro", "return"/"vrať", "output"/"vypiš"). The lexer handles this via the `strToKw` mapping in `Tokens.hs`.

**Parsec-Based Parsing**: Uses Text.Parsec for both lexing and parsing. Expression parsing uses `Text.Parsec.Expr.buildExpressionParser` for operator precedence.

### AST Structure Notes

- `FunBlock` contains three parts: variable declarations (`idDecl`), statements (`body`), and a return expression
- `Stmt` includes `AssignmentStmt Expr Expr` (left-hand side can be any lvalue expression)
- `Expr` supports postfix operations: `Call` for function calls, `FieldAccess` for record fields
- Records use `newtype Record = Fields [(Identifier, Expr)]`

## Testing

Tests are written using HSpec and located in `test/`:
- `ExprParserSpec.hs`: Comprehensive expression parser tests
- `StmtParserSpec.hs`: Statement parser tests
- `DeclParserSpec.hs`: Declaration parser tests

Test files follow the pattern: parse input strings and compare against expected AST values.

## Common Development Workflows

When adding new language features:
1. Add tokens to `Lex/Tokens.hs` if needed
2. Add lexer functions to `Lex/Lexer.hs`
3. Update AST types in `Parse/AST.hs`
4. Implement parser in appropriate `Parse/*Parser.hs` file
5. Add test cases in corresponding `test/*Spec.hs` file
6. Implement interpreter logic in `Interpreter/` (when ready)

When working on the interpreter:
- Follow the state monad pattern using `StateT State IO`
- Use helper functions from `Interpreter/State.hs` for state manipulation
- Remember that all values go through the heap (use `putsVar`, not direct map insertion)
