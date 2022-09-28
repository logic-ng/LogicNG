![build](https://github.com/logic-ng/LogicNG/workflows/build/badge.svg) [![codecov](https://codecov.io/gh/logic-ng/LogicNG/branch/development/graph/badge.svg)](https://codecov.io/gh/logic-ng/LogicNG) ![License](https://img.shields.io/badge/license-Apache%202-ff69b4.svg) [![Maven Central](https://img.shields.io/maven-central/v/org.logicng/logicng.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22org.logicng%22%20AND%20a:%22logicng%22)


<a href="https://www.logicng.org"><img src="https://github.com/logic-ng/LogicNG/blob/master/doc/logo/logo_big.png" alt="logo" width="300"></a>

## Introduction

<a href="https://www.logicng.org">LogicNG</a> is a Java Library for creating, manipulating and solving Boolean and Pseudo-Boolean formulas. It includes 100% Java implementations of popular tools
like [MiniSAT](http://minisat.se), [Glucose](http://www.labri.fr/perso/lsimon/glucose/), [PBLib](http://tools.computational-logic.org/content/pblib.php),
or [OpenWBO](http://sat.inesc-id.pt/open-wbo/).

Its main focus lies on memory-efficient data-structures for Boolean formulas and efficient algorithms for manipulating and solving them. The library is designed
to be used in industrial systems which have to manipulate and solve millions of formulas per day.

## White Paper

An extensive white paper with a lot of interesting use cases of LogicNG for configuration problems can be found [here](https://logicng.org/assets/documents/logicng-whitepaper.pdf).

## Philosophy

The most important philosophy of the library is to avoid unnecessary object creation. Therefore formulas can only be generated via formula factories. A formula
factory assures that a formula is only created once in memory. If another instance of the same formula is created by the user, the already existing one is
returned by the factory. This leads to a small memory footprint and fast execution of algorithms. Formulas can cache the results of algorithms executed on them
and since every formula is hold only once in memory it is assured that the same algorithm on the same formula is also executed only once.

Compared to other implementation of logic libraries on the JVM this is a huge memory and performance improvement.

## Usage

LogicNG is released in the Maven Central Repository. To include it just add

```xml

<dependency>
  <groupId>org.logicng</groupId>
  <artifactId>logicng</artifactId>
  <version>2.3.2</version>
</dependency>
```

to your Maven POM.

## Development Model

The `master` branch contains the latest release of LogicNG. If you want a *stable* and *well tested* version you should choose this branch. The `development`
branch reflects the *current state* of the next version. This branch will always compile, but code might not be as well tested and APIs may still change before
the next release. If you want to try *cutting edge* features, you can checkout this branch at your own risk. It is *not recommended* to use the development
version for *production* systems. Larger features will be developed in their own branches and will be merged to the development branch when ready.

## Getting Started

### Compilation

To compile LogicNG simply run `mvn compile` to build the parsers and compile the source code. You can build a jar of the library with `mvn package` or install
it in your local maven repository via `mvn install`.

### First Steps

The following code creates the Boolean Formula *A and not (B or not C)* programmatically:

```java
FormulaFactory f = new FormulaFactory();
Variable a = f.variable("A");
Variable b = f.variable("B");
Literal notC = f.literal("C", false);
Formula formula = f.and(a, f.not(f.or(b, notC)));
```

Alternatively you can just parse the formula from a string:

```java
FormulaFactory f = new FormulaFactory();
PropositionalParser p = new PropositionalParser(f);
Formula formula = p.parse("A & ~(B | ~C)");
```

Once you created the formula you can for example convert it to NNF or CNF or solve it with an instance of MiniSAT:

```java
Formula nnf = formula.nnf();
Formula cnf = formula.cnf();
SATSolver miniSat = MiniSat.miniSat(f);
miniSat.add(formula);
Tristate result = miniSat.sat();
```

### Documentation and Tutorial

For a tutorial and an extensive documentation see the [LogicNG webpage](https://www.logicng.org).

## License & Commercial Support

The library is released under the Apache License and therefore is free to use in any private, educational, or commercial projects. Commercial support is
available through the German company [BooleWorks GmbH](http://www.booleworks.com) - the company behind LogicNG. Please contact Christoph Zengler at
christoph@logicng.org for further details.
