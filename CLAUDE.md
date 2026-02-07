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
stack run microc -- <command> <program.mc> [args...]

# Run tests
stack test

# Run a single test (use -m pattern matching)
stack test --test-arguments "-m \"specific test name\""

# Enter GHCi with project modules loaded
stack ghci

# Clean build artifacts
stack clean
```

## CLI Commands

```bash
# Run a program (executes main() with args, returns exit code)
stack run microc -- run <program.mc> [args...]

# Type check (constraint-based inference)
stack run microc -- type <program.mc>

# Generate Control Flow Graph (DOT format)
stack run microc -- cfg <program.mc>

# Dataflow analyses
stack run microc -- const <program.mc>   # Constant propagation
stack run microc -- sign <program.mc>    # Sign analysis
stack run microc -- vbusy <program.mc>   # Very busy expressions
stack run microc -- reach <program.mc>   # Reaching definitions

# Compilation to .t86 assembly
stack run microc -- compile <program.mc> [output]  # Default: <input>.s
stack run microc -- asm <program.mc>               # Output to stdout
```

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

```
Source -> Lex -> Parse -> Analysis -> Interpret/Compile
                    |
                    +-> CFG -> Dataflow Analysis
```

1. **Lexing** (`Lex/`): Tokenizes source code
2. **Parsing** (`Parse/`): Builds Abstract Syntax Tree
3. **Analysis** (`Analysis/`): Semantic analysis, type inference, CFG, dataflow
4. **Interpretation** (`Interpreter/`): Direct AST evaluation
5. **Compilation** (`IR/`, `Compile/`): TAC generation and .t86 assembly output

### Module Structure

**Lex/** - Lexical Analysis
- `Tokens.hs`: Token definitions with bilingual keywords (English/Czech)
- `Lexer.hs`: Parsec-based lexer

**Parse/** - Syntax Analysis
- `AST.hs`: AST data types (Program, FunDecl, FunBlock, Stmt, Expr)
- `ExprParser.hs`: Expression parser with operator precedence (uses `buildExpressionParser`)
- `DeclParser.hs`: Function declarations and variable statements
- `StmtParser.hs`: Statement parsers

**Interpreter/** - Execution
- `Data.hs`: Runtime value types (VNumber, Pointer, Function, Record)
- `State.hs`: Interpreter state (stack of variable frames, heap)
- `Interpret.hs`: Expression and statement evaluation
- `InterpretRun.hs`: Top-level interpretation entry point

**Analysis/** - Static Analysis
- `Semantics.hs`: Semantic checks (undefined names, lvalue validation, etc.)
- `Typecheck/`: Constraint-based type inference
  - `ConstraintGenerator.hs`: Generates constraints from AST
  - `ConstraintSolver.hs`: Unification-based constraint solving
- `Cfg/`: Control Flow Graph construction
  - `Cfg.hs`: CFG data structures (CFGNode with FunEntry/FunExit/Node)
  - `Builder.hs`: State monad-based CFG builder
- `Dataflow/`: Dataflow analysis framework
  - `Analysis.hs`: Generic worklist algorithm for lattice-based analysis
  - `Const.hs`, `Sign.hs`: Forward analyses (constant/sign propagation)
  - `VeryBusy.hs`, `ReachingDef.hs`: Backward/forward analyses

**IR/** - Intermediate Representation
- `Tac.hs`: Three-Address Code instruction set
- `TacCompiler.hs`: AST to TAC compilation
- `Desugar.hs`: Desugars high-level TAC to machine instructions
- `TacPrint.hs`: Assembly output formatting
- `Stdlib.hs`: Built-in runtime functions

**Compile/** - Assembly Generation
- `Compile.hs`: Orchestrates IR generation, desugaring, and emission

**app/** - CLI
- `Main.hs`: CLI with optparse-applicative
- `Workflow.hs`: Orchestrates parsing, typing, and analysis passes

### Key Design Decisions

**Everything Lives on the Heap**: Variables are stored on the heap and referenced by addresses. Each stack frame is a `Map Identifier Address`. This simplifies pointer semantics.

**Bilingual Support**: Keywords support both English and Czech (e.g., "var"/"pro", "return"/"vrať"). See `strToKw` mapping in `Tokens.hs`.

**Constraint-Based Type System**: Type checking generates constraints from AST nodes, then unifies them. Supports recursive types via μ notation.

**Lattice-Based Dataflow**: `Lattice.hs` provides the typeclass for dataflow abstractions. Analyses define `top`, `bottom`, and lattice operators `<&>` (meet) / `<|>` (join). The generic worklist algorithm in `Analysis/Dataflow/Analysis.hs` handles fixed-point iteration.

### AST Structure Notes

- `FunBlock` contains: variable declarations (`idDecl`), statements (`body`), return expression
- `Stmt` includes `AssignmentStmt Expr Expr` (LHS can be any lvalue)
- `Expr` supports postfix operations: `Call`, `FieldAccess`
- Records: `newtype Record = Fields [(Identifier, Expr)]`

## Testing

Tests use HSpec in `test/`. Run a single test with:
```bash
stack test --test-arguments "-m \"test name pattern\""
```

Test files follow the pattern: parse/execute input and compare against expected results.

## Common Development Workflows

**Adding new language features:**
1. Add tokens to `Lex/Tokens.hs` (include bilingual support)
2. Update AST types in `Parse/AST.hs` (ensure Data and Typeable instances)
3. Implement parser in appropriate `Parse/*Parser.hs` file
4. Add test cases in corresponding `test/*Spec.hs` file
5. Update `Analysis/Typecheck/ConstraintGenerator.hs`
6. Implement interpreter logic in `Interpreter/Interpret.hs`
7. If compiling: update `IR/TacCompiler.hs`

**Interpreter notes:**
- Uses `StateT State IO` monad
- All values go through the heap (use `putsVar`, not direct map insertion)

**Type checking notes:**
- Constraint generation: `ConstraintGenerator.hs` via `genConstraintsFun`
- Type variables: `Unknown Int` for fresh, `BoundTypeVar Int` for recursive types
- `Typeable` wrapper tags AST nodes with source location

**TAC compilation notes:**
- `Emitter` monad: `WriterT TAC (State CState)`
- `emit_` for unlabeled instructions, `emitL` for labeled
- Use `RecursiveDo` (`mdo`) for forward references in control flow

**Adding dataflow analyses:**
1. Define a lattice type implementing the `Lattice` typeclass
2. Implement `evalStmt` to transfer function state through statements
3. Use `runAnalysisOnVars` or `runAnalysis` from `Analysis/Dataflow/Analysis.hs`
4. See `Const.hs` or `Sign.hs` for forward analysis examples
