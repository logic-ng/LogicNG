![build](https://github.com/logic-ng/LogicNG/workflows/build/badge.svg?branch=development) [![codecov](https://codecov.io/gh/logic-ng/LogicNG/branch/development/graph/badge.svg)](https://codecov.io/gh/logic-ng/LogicNG) ![License](https://img.shields.io/badge/license-Apache%202-blue.svg) ![Version](https://img.shields.io/badge/version-2.0.2-ff69b4.svg)

<img src="https://github.com/logic-ng/LogicNG/blob/master/doc/logo/logo_big.png" alt="logo" width="300">

## Introduction
LogicNG is a Java Library for creating, manipulating and solving Boolean and Pseudo-Boolean formulas. It includes 100% Java implementations of popular tools like [MiniSAT](http://minisat.se), [Glucose](http://www.labri.fr/perso/lsimon/glucose/), [PBLib](http://tools.computational-logic.org/content/pblib.php), or [OpenWBO](http://sat.inesc-id.pt/open-wbo/).

Its main focus lies on memory-efficient data-structures for Boolean formulas and efficient algorithms for manipulating and solving them.
The library is designed to be used in industrial systems which have to manipulate and solve millions of formulas per day.

## Philosophy
The most important philosophy of the library is to avoid unnecessary object creation.  Therefore formulas can only be generated via formula factories.  A formula factory assures that a formula is only created once in memory.  If another instance of the same formula is created by the user, the already existing one is returned by the factory. This leads to a small memory footprint and fast execution of algorithms.  Formulas can cache the results of algorithms executed on them and since every formula is hold only once in memory it is assured that the same algorithm on the same formula is also executed only once.

Compared to other implementation of logic libraries on the JVM this is a huge memory and performance improvement.

## Usage
LogicNG is released in the Maven Central Repository.  To include it just add
```xml
<dependency>
  <groupId>org.logicng</groupId>
  <artifactId>logicng</artifactId>
  <version>2.0.2</version>
</dependency>
```
to your Maven POM.

## Development Model
The `master` branch contains the latest release of LogicNG.  If you want a *stable* and *well tested* version you should choose this branch.  The `development` branch reflects the *current state* of the next version.  This branch will always compile, but code might not be as well tested and APIs may still change before the next release.  If you want to try *cutting edge* features, you can checkout this branch at your own risk.  It is *not recommended* to use the development version for *production* systems.  Larger features will be developed in their own branches and will be merged to the development branch when ready.

## Getting Started
### Compilation
To compile LogicNG simply run `mvn compile` to build the parsers and compile the source code.  You can build a jar of the library with `mvn package` or install it in your local maven repository via `mvn install`.

### First Steps
The following code creates the Boolean Formula *A and not (B or not C)* programatically:
```java
final FormulaFactory f = new FormulaFactory();
final Variable a = f.variable("A");
final Variable b = f.variable("B");
final Literal notC = f.literal("C", false);
final Formula formula = f.and(a, f.not(f.or(b, notC)));
```
Alternatively you can just parse the formula from a string:
```java
final FormulaFactory f = new FormulaFactory();
final PropositionalParser p = new PropositionalParser(f);
final Formula formula = p.parse("A & ~(B | ~C)");
```
Once you created the formula you can for example convert it to NNF or CNF or solve it with an instance of MiniSAT:
```java
final Formula nnf = formula.nnf();
final Formula cnf = formula.cnf();
final SATSolver miniSat = MiniSat.miniSat(f);
miniSat.add(formula);
final Tristate result = miniSat.sat();
```

### Frequently Asked Questions
We recently started a Wiki section for a [FAQ](https://github.com/logic-ng/LogicNG/wiki/FAQ). 

## License & Commercial Support
The library is released under the Apache License and therefore is free to use in any private, educational, or commercial projects.  Commercial support is available through the German company [BooleWorks GmbH](http://www.booleworks.com) - the company behind LogicNG.  Please contact Christoph Zengler at christoph@logicng.org for further details.
