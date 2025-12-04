# MicroC

Intepreter, Static Analyzer, and hopefully soon a compiler of MicroC.

## Project features

- Interpretr
- Semantic analysis: undefined/dup names, creating pointer to fun is forbidden, recursive records, accessing undefined fields of a record, assignments save to l-value.
- Dataflow analysis: const & sign analysis, very busy and reaching definitions analysis


# Language overview

The language is sourced from the [NI-APR](https://bilakniha.cvut.cz/en/predmet6082106.html#gsc.tab=0) course.

A program is a collection of functions. (No global variables or so.)

Regarding types, we have integers, records and pointers.

A function has N parameters. It begins with definition of variables used in the body
of the function, and has only a single return at the end.

Grammar:
```hs
Fun ⟶ Id ([Id, ..., Id]) {
  [ var Id, ..., Id; ]
  [ Stmt ]
  return Expr;
}
```

Regarding statements, we support assignment, `if` condition, `while` loop and `output` statement (that prints out its argument).
Assignments 

Regarding expressions, we have basic arithmetics, `alloc` (something like `malloc`), reference, dereference, user `input`, and basic literals.

Example program:
```
main() {
    var rec, ptr;

    rec = { foo: 3, bar: main };
    ptr = &(rec.bar);
    *ptr = 5;
    output rec.bar;
    
    return 0;
}
```