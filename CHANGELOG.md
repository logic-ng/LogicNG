# LogicNG Java 11 Changelog

LogicNG uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.4.3] - 2024-01-29

### Changed

- Updated ANTLR to version 4.13.1


## [2.4.2] - 2023-03-22

### Changed

- Changed artifact id to `logicng-j11` (for Java 11 only)
- Changed Java Version to JDK 11
- switched to ANTLR 4.11.1

# LogicNG Java 8 Changelog


## [2.4.1] - 2022-12-01

### Changed

- Allowing symbol `#` in variable names for the `PropositionalParser` and the `PseudoBooleanParser`.
- Set the Java Jigsaw automatic module name to `logicng` in the manifest.

## [2.4.0] - 2022-11-24

### Added

- Completely rewritten graphical outputs of formulas, BDDs, and graphs in the package `org.logicng.io.graphical`. It is now possible to configure the
  generated graphs by dynamically styling nodes, edges, and computing node labels. Also, there are now two possible output formats: GraphViz DOT and Mermaid.js.
- Convenience methods `isSatisfiable`, `isTautology`, `isContradiction`, `implies`, `isImpliedBy` and `isEquivalentTo` in the `Formula` class.
- New OLL algorithm for OpenWBO for more efficient weighted MaxSAT solving.
- Two overloaded factory methods `mk` in `MiniSat` to construct a solver by formula factory, solver style and optional configuration.
- Methods to directly apply Boolean functions on BDDs
- Added `toFormula` method on BDDs to generate a formula via Shannon expansion
- Convenience methods `variables(Collection<String> names)` and `variables(String... names)` for creating a list of variables in the `FormulaFactory` class.

### Changed

- Methods `generateFromCnf(Formula formula)` and `generateFromCnf(Collection<Formula> formulas)` in `ConstraintGraphGenerator` are now deprecated, since the
  constraint graph generation is not CNF specific. Both methods will be removed with LogicNG 3.0. Instead, use the general
  method `generateFromFormulas(Collection<Formula> formulas)`.

## [2.3.2] - 2022-08-02

### Changed

- The cached PB and CC encodings are no longer held in the constraint itselt but analogously to the other caches in the formula factory.

### Fixed

- A small bug which could occur when using the extended formula factory in combination with cached CC and PB encodings.

## [2.3.1] - 2022-07-27

### Changed

- Removed `negativeVariables` from the internal representation of `Assignment` it is now computed each time the method is called. This leeds to a minimal
  performance disadvantage but to a proportional better memory footprint. The public API is not changed.
- Updated ANTLR to 4.9.3 (there were no relevant updates to the Java target, therefore no changes are expected for LogicNG)

### Fixed

- A small bug when comparing two backbones with the same set of negative/positive/optional variables but different satisfiability.

## [2.3.0] - 2022-07-18

### Added

- Overloaded method `createAssignment` in `MiniSat` by flag whether the created assignment should be a fast evaluable assignment.
- Extended `ModelEnumerationFunction.Builder` by flag `fastEvaulable` which indicates whether the created assignments should be a fast evaluable assignment.
- Convenience methods `isNNF()`, `isDNF()` and `isCNF()` in class `Formula`
- Two new constructors for `Substitution`s and a new method `getMapping()` to get the internal mapping
- Method `getSubstitution` on `Anonymizer` to get the mapping from original variable to anonymized one
- A DNF from BDD function `BDDDNFFunction`, a subclass of the newly added class `BDDNormalFormFunction`
- A DNF from BDD formula transformation `BDDDNFTransformation`, a subclass of the newly added class `BDDNormalFormTransforamtion`
- A canonical CNF enumeration `CanonicalCNFEnumeration`, a subclass of the newly added class `CanonicalEnumeration`.

### Changed

- Improved methods `intersection` and `union` in `CollectionHelper` by using bounded wildcards.
- Improved performance of `hashCode` and `equals` in `Assignment` by avoiding redundant hash set creation.
- Method `BDD#dnf()` uses the newly introduced `BDDDNFFunction` to obtain a smaller DNF instead of a canonical DNF
- Class `BDDCNFFunction` uses the singleton pattern
- All functions/transformations/predicates with only a default constructor introduce a static `get()` method with the singleton pattern. The public
  constructors are now deprecated and will be removed with LogicNG 3.0
- Always use the default configuration of algorithms from the formula factory and do not construct them in the respective classes separately.

### Fixed

- Minor edge case issue in `NegationSimplifier` which yielded a larger result formula than input formula.
- The `TermPredicate` logic was inverted. In detail, the minterm predicate  `TermPredicate#getMintermPredicate()` tested for a maxterm and the
  `TermPredicate#getMaxtermPredicate()` tested for a minterm. To prevent silent errors for callers of these predicates, the factory method names were changed
  to `minterm()` and `maxterm()`, respectively. Thus, an intentional breaking change on compile time level has been introduced to force callers to adjust their
  logic.
- Minor edge case issue in `MiniSat` when performing assumption solving with proof tracing.
- Fixed two bugs in the backbone computation on the MiniCard solver.

## [2.2.1] - 2022-06-11

### Added

- Basic support for OSGi via `maven-bundle-plugin`

## [2.2.0] - 2021-11-09

### Added

- Improved `FormulaFactory` by avoiding creating unnecessary negations (cache pollution) during the check for complementary operands.
- Improved the NNF computation by avoiding creating unnecessary negations (cache pollution) during the recursive calls.
- Extracted the NNF computation in its own transformation class `NNFTransformation`.
- Moved all formula caches from the `Formula` class to the `FormulaFactory` to save memory by avoiding creating empty cache maps.
- New `TermPredicate` class to check whether a formula is a minterm (clause) or maxterm (DNF term).
- Extended helper classes `CollectionHelper` and `FormulaHelper` by additional convenient methods.

### Fixed

- Fixed a bug in the `addSoftFormula` method of the `MaxSATSolver` class. A soft formula is now weighted properly if the soft formula is not a clause.
- Fixed a bug in the `addWithRelaxation` method of the `SATSolver` class. The CNF of the formula is now computed properly regarding the configuration of the
  solver.

### Deprecated

- Deprecation of method `addWithoutUnknown` in class `SATSolver` - this method will be removed in future versions.
- Deprecation of method `addWithRelaxation` for propositions in class `SATSolver` - this method will be removed in future versions.

## [2.1.0] - 2021-07-18

### Added

- Reworked handlers
    - New handlers for backbones, MUS, SMUS, prime compilation, and advanced simplifier
    - three different types for timeout handlers:
        - `SINGLE_TIMEOUT`: The timeout is started when the handler's `started()` method is called
        - `RESTARTING_TIMEOUT`: The timeout is restarted everytime the handler's `started()` method is called
        - `FIXED_END`: The timeout is interpreted as a fixed point in time (in milliseconds) at which the computation should be aborted
- Improved version detection for compiled and packaged versions of LogicNG
- Introduced Mockito for unit tests

### Fixed

- Fixed a bug in the DIMACS formula writer when there was only a single clause with multiple literals

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
- Configuration object for formula factory which can be used to allow trivial contradictions and tautologies in formulas and to specify a default merge strategy
  for formulas from
  different factories
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
- Reorganized code location in the SAT Solver package, introduced solver functions which allow better separation of code for functions of the solver
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

- A new method for generating CNFs directly on the solver instead of using the formula factory. This often leads to a faster generation and reduced Heap
  consumption but with the
  loss of caching
- The current formula on a MiniSat-based solver can be extracted

### Changed

- The standard MiniSat-based solvers can now directly efficiently compute a backone. No extra solver is required anymore
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

- Internal parser and IO improvements. Variables can now start with a digit.

## [1.3.1] - 2018-01-28

### Added

- New formula transformation which imports formulas in another formula factory

### Changed

- Huge performance boost in the model enumeration of MiniSat

### Fixed

- Small bugfix for a trivial case in DRUP

## [1.3.0] - 2017-10-25

### Added

- MiniSat and Glucose have a new option for proof tracing. A DRUP implementation stores all the necessary information for generating a proof for unsatisfiable
  formulas after
  solving them. The new method can be found in the SAT solver class: `unsatCore()`
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
