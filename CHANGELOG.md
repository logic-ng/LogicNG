# Changelog

LogicNG uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.2] - 2020-09-19
### Fixed
- Fixed another bug for a special case in the DRUP proof generation


## [2.0.1] - 2020-09-18
### Fixed
- Fixed a bug for a special case in the DRUP proof generation


## [2.0.0] - 2020-07-30
### Added
- DNNF data structure and compilation
- DNNF-based model counting
- BDD Reordering
- Computation of shortest MUSes
- Computation of prime implicant and implicates
- New algorithms for simplifying Boolean formulas including the possibility to define an own rating function for the formula complexity
- A new method for generation constraint graphs of formulas
- A SAT encoding of the SET COVER problem
- New explicit data structure for cardinality constraints
- New formula functions for 
  - Computing the depth of a formula
  - Computing a minimum prime implicant of a formula
- New formula predicates for
  - Pseudo Boolean Constraint containment
  - Fast evaluation to a constant
- New formula transformations for
  - Literal substitution
  - Expansion of pseudo-Boolean constraints
- New solver function for optimizing the current formula on the solver (wrt. the number of positive/negative literals)
- New formula randomizer and corner case generator, especially useful for testing
- Configuration object for formula factory which can be used to allow trivial contradictions and tautologies in formulas and to specify
  a default merge strategy for formulas from different factories 
- New helper classes for collections
- Stream operators on formulas
 
### Changed
- Changed Java Version to JDK 8
- switched to ANTLR 4.8
- switched to JUnit 5
- PBC and CC methods in the formula factory return `Formula` objects now (not `PBConstraint` objects) and can simplify the constraints
- Moved BDD package to `knowledgecompilation`
- Reorganized `explanations` package
- Reorganized code location in the BDD package and simplified the `BDDFactory`
- Reorganized code location in the SAT Solver package, introduced solver functions which allow better separation of code for functions 
  of the solver
- Propositions now hold a simple formula, no `ImmutableFormulaList` anymore
- fixed a spelling problem: propositions now have a correct `backpack`
- More classes are `protected` now and can be extended from outside
- Moved parser grammars from `resources` to `antlr`

### Removed
- CleaneLing solver
- `ImmutableFormulaList` class


## [1.6.2] - 2020-01-18
### Added
- New BDD handlers

### Changed
- Some improvements to handlers for computations


## [1.6.1] - 2019-09-16
### Added
- A new method for solving a formula with a given literal ordering.

### Changed
- Minor refactoring of the Formatter super class (no effects on callers).

### Fixed
- Fixed the behaviour of model enumeration with additional variables.


## [1.6.0] - 2019-09-04
### Added
- A new method for generating CNFs directly on the solver instead of using the formula factory.
  This often leads to a faster generation and reduced Heap consumption but with the loss of 
  caching
- The current formula on a MiniSat-based solver can be extracted
  
### Changed
- The standard MiniSat-based solvers can now directly efficiently compute a backone.  No extra solver 
  is required anymore
- BDD factory can now be extended externally


## [1.5.2] - 2019-07-15
### Changed
- Clarified behaviour of the `Backbone` object

### Fixed
- Fixed caching behaviour when using a `sat()` call without assumptions after a call with assumptions


## [1.5.1] - 2019-05-08
### Added
- Introduced a new `FormulaHelper` class for small utility methods on formulas
- Added a new NNF predicate

### Fixed
- Fixed an unspecified behaviour in `SATPredicate`
- Fixed a small performance issue in the new backbone solver
- Fixed a bug in a special case of the CNF transformation of a pseudo-Boolean constraint


## [1.5.0] - 2019-03-17
### Added
- Algorithm & data structures for efficiently computing backbones of formulas
- Data structures for UBTrees in order to efficiently identify sub- and supersets
- CNF and DNF subsumption as formula transformations
- Backbone simplifier (compute and propagate the backbone of a formula)
- A new sorted formula formatter which respects a given variable ordering when printing formulas

### Changed 
- Minor code refactorings and improvements

### Deprecated
- Deprecation of CleaneLing - this solver will be removed in future versions.


## [1.4.1] - 2018-12-07
### Changed
- Some refactorings for unit tests on Windows regarding encodings
- The Quine-McCluskey implementation does not yield CNF auxiliary variables anymore

### Fixed
- Fixed a minor bug in the generation of incremental cardinality constraints


## [1.4.0] - 2018-06-03
### Added
- BDD package (based on Buddy) for creating, manipulating, and writing BDDs
  - Creation of BDDs from LogicNG formulas
  - CNF, DNF transformation of BDDs
  - Restriction, existential & universal quantifier elimination
  - Model counting & enumeration
  - Different static variable ordering heuristics (FORCE, DFS, BFS, MinMax)
  - Writing BDDs in the GraphViz .dot format
- Quine-McCluskey Implementation for minimizing canonical DNFs
- New formula transformation for anonymizing formulas

### Changed
- Internal parser and IO improvements.  Variables can now start with a digit.


## [1.3.1] - 2018-01-28
### Added
- New formula transformation which imports formulas in another formula factory

### Changed
- Huge performance boost in the model enumeration of MiniSat

### Fixed
- Small bugfix for a trivial case in DRUP


## [1.3.0] - 2017-10-25
### Added
- MiniSat and Glucose have a new option for proof tracing.  A DRUP implementation stores all the necessary information for generating a proof for unsatisfiable formulas after solving them.  The new method can be found in the SAT solver class: `unsatCore()`
- A new simplifier which applies the distributive law was added: `DistributiveSimplifier`

### Changed
- Unsat Cores are now parametrized with the proposition type

### Fixed
- Some minor bug-fixes in handling corner cases of cardinality and pseudo-Boolean constraints 


## [1.2.0] - 2017-07-14
### Added
- Introduced an extended formula factory which is able to return to a previously saved state and delete old formulas (and get them garbage collected)
- A simple data structure for generic graphs including algorithms for connected components and maximal cliques
- Improved IO (Writers for formulas, Dimacs CNFs, and graphs)

### Changed
- SAT solvers can now track the currently known variables
- Updated to ANTLR 4.7

### Fixed
- Various smaller bugfixes


## [1.1.0] - 2016-08-29
### Added
- Implemented cardinality constraint encodings and pseudo-Boolean constraints encodings of PBLib
- Incremental cardinality constraints, including the possibility to add cardinaliy constraints to the solver without introducing new formula factory variables
- Different MUS algorithms

 
## [1.0.0] - 2016-01-25
### Added
- Initial Release of LogicNG

