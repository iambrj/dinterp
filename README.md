# Definitional Interpreters for Higher-Order Programming Languages

- Interesting definition of higher order : "a language is higher order if
    procedures or labels can occur as data"
- Criteria
    1. Is defining language higher order?
    2. Do defined language semantics depend on defining language's order of
       application?
- Assuming termination, do call by name and call by value always produce the
    same value?
- Objections to metacircularity
    1. Does not shed light on nature of higher order functions - defined
       language should be higher order, but defining language should be first
       order. Solution - use records to implement higher order functions using
       first order functions.
    2. Order of application in defining language carries over to order of
       application in defined language. Solution - use records to implement
       functions, gives finer control over application order.
    3. Not possible to implement control flow operations in defined language
       without introducing them into the defining language. Solution -
       continuations.

## To read

- GEDANKEN—a simple typeless language based on the principle of completeness and the reference concept
- Outline of mathematical theory of computation
- Lattice theory, data types and semantics
- Fixpoint induction and proofs of program properties
- Models for various type-free calculi
- Continuous lattices, Dana Scott
