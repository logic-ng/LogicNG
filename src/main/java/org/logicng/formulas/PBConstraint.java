///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-2016 Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.formulas;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.util.Pair;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import static org.logicng.formulas.cache.TransformationCacheEntry.NNF;

/**
 * A pseudo-Boolean constraint of the form {@code c_1 * l_1 + ... + c_n * l_n R k} where {@code R} is one of
 * {@code =, >, >=, <, <=}.
 * @version 1.1
 * @since 1.0
 */
public final class PBConstraint extends Formula {

  private static final Iterator<Formula> ITERATOR = new Iterator<Formula>() {
    @Override
    public boolean hasNext() {
      return false;
    }

    @Override
    public Formula next() {
      throw new NoSuchElementException();
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  };

  private final Literal[] literals;
  private final int[] coefficients;
  private final CType comparator;
  private final int rhs;
  private final boolean isCC;
  private ImmutableFormulaList encoding;
  private int hashCode;
  private int maxWeight;

  /**
   * Constructs a new pseudo-Boolean constraint.
   * @param literals     the literals
   * @param coefficients the coefficients
   * @param comparator   the comparator
   * @param rhs          the right hand side
   * @param f            the formula factory
   * @throws IllegalArgumentException if the number of literals and coefficients do not correspond
   */
  PBConstraint(final Literal[] literals, final int[] coefficients, final CType comparator, int rhs, final FormulaFactory f) {
    super(FType.PBC, f);
    if (literals.length != coefficients.length)
      throw new IllegalArgumentException("Cannot generate a pseudo-Boolean constraint with literals.length != coefficients.length");
    this.literals = literals;
    this.coefficients = coefficients;
    boolean cc = true;
    maxWeight = Integer.MIN_VALUE;
    for (final int c : coefficients) {
      if (c > maxWeight)
        maxWeight = c;
      if (c != 1) {
        cc = false;
        break;
      }
    }
    for (final Literal lit : literals)
      if (!lit.phase()) {
        cc = false;
        break;
      }
    this.isCC = cc;
    this.comparator = comparator;
    this.rhs = rhs;
    this.encoding = null;
    this.hashCode = 0;
  }

  /**
   * Returns the literals of this constraint.
   * @return the literals of this constraint
   */
  public Literal[] operands() {
    return Arrays.copyOf(this.literals, this.literals.length);
  }

  /**
   * Returns the coefficients of this constraint.
   * @return the coefficients of this constraint
   */
  public int[] coefficients() {
    return Arrays.copyOf(this.coefficients, this.coefficients.length);
  }

  /**
   * Returns the comparator of this constraint.
   * @return the comparator of this constraint
   */
  public CType comparator() {
    return this.comparator;
  }

  /**
   * Returns the right-hand side of this constraint.
   * @return the right-hand side of this constraint
   */
  public int rhs() {
    return this.rhs;
  }

  /**
   * Returns {@code true} if this constraint is a cardinality constraint, {@code false} otherwise.
   * @return {@code true} if this constraint is a cardinality constraint
   */
  public boolean isCC() {
    return this.isCC;
  }

  /**
   * Returns the maximal coefficient of this constraint.
   * @return the maximal coefficient of this constraint
   */
  public int maxWeight() {
    return this.maxWeight;
  }

  /**
   * Normalizes this constraint s.t. it can be converted to CNF.
   * @return the normalized constraint
   */
  public Formula normalize() {
    LNGVector<Literal> normPs = new LNGVector<>(this.literals.length);
    LNGIntVector normCs = new LNGIntVector(this.literals.length);
    int normRhs;
    switch (this.comparator) {
      case EQ:
        for (int i = 0; i < this.literals.length; i++) {
          normPs.push(this.literals[i]);
          normCs.push(this.coefficients[i]);
        }
        normRhs = this.rhs;
        Formula f1 = this.normalize(normPs, normCs, normRhs);
        normPs.clear();
        normCs.clear();
        for (int i = 0; i < this.literals.length; i++) {
          normPs.push(this.literals[i]);
          normCs.push(-this.coefficients[i]);
        }
        normRhs = -this.rhs;
        Formula f2 = this.normalize(normPs, normCs, normRhs);
        return f.and(f1, f2);
      case LT:
      case LE:
        for (int i = 0; i < this.literals.length; i++) {
          normPs.push(this.literals[i]);
          normCs.push(this.coefficients[i]);
        }
        normRhs = this.comparator == CType.LE ? this.rhs : this.rhs - 1;
        return this.normalize(normPs, normCs, normRhs);
      case GT:
      case GE:
        for (int i = 0; i < this.literals.length; i++) {
          normPs.push(this.literals[i]);
          normCs.push(-this.coefficients[i]);
        }
        normRhs = this.comparator == CType.GE ? -this.rhs : -this.rhs - 1;
        return this.normalize(normPs, normCs, normRhs);
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean comparator: " + this.comparator);
    }
  }

  /**
   * Internal helper for normalization of a <= constraint.
   * @param ps  the literals
   * @param cs  the coefficients
   * @param rhs the right-hand side
   * @return the normalized constraint
   */
  private Formula normalize(final LNGVector<Literal> ps, final LNGIntVector cs, int rhs) {
    int c = rhs;
    int newSize = 0;
    for (int i = 0; i < ps.size(); i++) {
      if (cs.get(i) != 0) {
        ps.set(newSize, ps.get(i));
        cs.set(newSize, cs.get(i));
        newSize++;
      }
    }
    ps.removeElements(ps.size() - newSize);
    cs.removeElements(cs.size() - newSize);
    final SortedMap<Literal, Pair<Integer, Integer>> var2consts = new TreeMap<>();
    for (int i = 0; i < ps.size(); i++) {
      final Variable x = ps.get(i).variable();
      Pair<Integer, Integer> consts = var2consts.get(x);
      if (consts == null)
        consts = new Pair<>(0, 0);
      if (!ps.get(i).phase())
        var2consts.put(x, new Pair<>(consts.first() + cs.get(i), consts.second()));
      else
        var2consts.put(x, new Pair<>(consts.first(), consts.second() + cs.get(i)));
    }
    final LNGVector<Pair<Integer, Literal>> csps = new LNGVector<>(var2consts.size());
    for (Map.Entry<Literal, Pair<Integer, Integer>> all : var2consts.entrySet()) {
      if (all.getValue().first() < all.getValue().second()) {
        c -= all.getValue().first();
        csps.push(new Pair<>(all.getValue().second() - all.getValue().first(), all.getKey()));
      } else {
        c -= all.getValue().second();
        csps.push(new Pair<>(all.getValue().first() - all.getValue().second(), all.getKey().negate()));
      }
    }
    int sum = 0;
    int zeros = 0;
    cs.clear();
    ps.clear();
    for (final Pair<Integer, Literal> pair : csps) {
      if (pair.first() != 0) {
        cs.push(pair.first());
        ps.push(pair.second());
        sum += cs.back();
      } else
        zeros++;
    }
    ps.removeElements(ps.size() - csps.size() - zeros);
    cs.removeElements(cs.size() - csps.size() - zeros);
    boolean changed;
    do {
      changed = false;
      if (c < 0)
        return f.falsum();
      if (sum <= c) {
        return f.verum();
      }
      assert cs.size() > 0;
      int div = c;
      for (int i = 0; i < cs.size(); i++)
        div = gcd(div, cs.get(i));
      if (div != 0 && div != 1) {
        for (int i = 0; i < cs.size(); i++)
          cs.set(i, cs.get(i) / div);
        c = c / div;
      }
      if (div != 1 && div != 0)
        changed = true;
    } while (changed);
    final Literal[] lits = new Literal[ps.size()];
    for (int i = 0; i < lits.length; i++)
      lits[i] = ps.get(i);
    final int[] coeffs = new int[cs.size()];
    for (int i = 0; i < coeffs.length; i++)
      coeffs[i] = cs.get(i);
    return f.pbc(CType.LE, c, lits, coeffs);
  }

  /**
   * Returns the GCD of two given values.
   * @param small the smaller value
   * @param big   the larger value
   * @return the GCD of the two values
   */
  private static int gcd(int small, int big) {
    return small == 0 ? big : gcd(big % small, small);
  }

  @Override
  public long numberOfAtoms() {
    return 1L;
  }

  @Override
  public long numberOfNodes() {
    if (this.numberOfNodes != -1)
      return this.numberOfNodes;
    this.numberOfNodes = 1L + this.literals.length;
    return this.numberOfNodes;
  }

  @Override
  public boolean isAtomicFormula() {
    return true;
  }

  @Override
  public int numberOfOperands() {
    return 0;
  }

  @Override
  public SortedSet<Variable> variables() {
    if (this.variables == null) {
      this.variables = new TreeSet<>();
      for (final Literal lit : this.literals)
        this.variables.add(lit.variable());
    }
    return this.variables;
  }

  @Override
  public SortedSet<Literal> literals() {
    final SortedSet<Literal> lits = new TreeSet<>();
    Collections.addAll(lits, this.literals);
    return lits;
  }

  @Override
  public boolean containsVariable(final Variable variable) {
    for (final Literal lit : this.literals)
      if (lit.containsVariable(variable))
        return true;
    return false;
  }

  @Override
  public boolean evaluate(final Assignment assignment) {
    int lhs = this.evaluateLHS(assignment);
    return this.evaluateComparator(lhs);
  }

  /**
   * Returns the evaluation of the left-hand side of this constraint.
   * @param assignment the assignment
   * @return the evaluation of the left-hand side of this constraint
   */
  private int evaluateLHS(final Assignment assignment) {
    int lhs = 0;
    for (int i = 0; i < this.literals.length; i++)
      if (this.literals[i].evaluate(assignment))
        lhs += this.coefficients[i];
    return lhs;
  }

  /**
   * Computes the result of evaluating the comparator with a given left-hand side.
   * @param lhs the left-hand side
   * @return {@code true} if the comparator evaluates to true, {@code false} otherwise
   */
  private boolean evaluateComparator(int lhs) {
    switch (this.comparator) {
      case EQ:
        return lhs == this.rhs;
      case LE:
        return lhs <= this.rhs;
      case LT:
        return lhs < this.rhs;
      case GE:
        return lhs >= this.rhs;
      case GT:
        return lhs > this.rhs;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean comparator");
    }
  }

  @Override
  public Formula restrict(final Assignment assignment) {
    final List<Literal> newLits = new LinkedList<>();
    final List<Integer> newCoeffs = new LinkedList<>();
    int lhsFixed = 0;
    for (int i = 0; i < this.literals.length; i++) {
      final Formula restriction = assignment.restrictLit(this.literals[i]);
      if (restriction == null) {
        newLits.add(this.literals[i]);
        newCoeffs.add(this.coefficients[i]);
      } else if (restriction.type == FType.TRUE)
        lhsFixed += this.coefficients[i];
    }
    return newLits.isEmpty()
            ? this.evaluateComparator(lhsFixed) ? f.verum() : f.falsum()
            : f.pbc(this.comparator, this.rhs - lhsFixed, newLits, newCoeffs);
  }

  @Override
  public boolean containsNode(final Formula formula) {
    if (this == formula || this.equals(formula))
      return true;
    if (formula.type == FType.LITERAL) {
      for (final Literal lit : this.literals)
        if (lit.equals(formula) || lit.variable().equals(formula))
          return true;
      return false;
    }
    return false;
  }

  @Override
  public Formula substitute(final Substitution substitution) {
    final List<Literal> newLits = new LinkedList<>();
    final List<Integer> newCoeffs = new LinkedList<>();
    int lhsFixed = 0;
    for (int i = 0; i < this.literals.length; i++) {
      Formula subst = substitution.getSubstitution(this.literals[i].variable());
      if (subst == null) {
        newLits.add(this.literals[i]);
        newCoeffs.add(this.coefficients[i]);
      } else {
        switch (subst.type) {
          case TRUE:
            if (this.literals[i].phase())
              lhsFixed += this.coefficients[i];
            break;
          case FALSE:
            if (!this.literals[i].phase())
              lhsFixed += this.coefficients[i];
            break;
          case LITERAL:
            newLits.add(this.literals[i].phase() ? (Literal) subst : ((Literal) subst).negate());
            newCoeffs.add(this.coefficients[i]);
            break;
          default:
            throw new IllegalArgumentException("Cannnot substitute a formula for a literal in a pseudo-Boolean constraint");
        }
      }
    }
    return newLits.isEmpty()
            ? this.evaluateComparator(lhsFixed) ? f.verum() : f.falsum()
            : f.pbc(this.comparator, this.rhs - lhsFixed, newLits, newCoeffs);
  }

  @Override
  public Formula negate() {
    switch (this.comparator) {
      case EQ:
        return f.or(f.pbc(CType.LT, this.rhs, this.literals, this.coefficients), f.pbc(CType.GT, this.rhs, this.literals, this.coefficients));
      case LE:
        return f.pbc(CType.GT, this.rhs, this.literals, this.coefficients);
      case LT:
        return f.pbc(CType.GE, this.rhs, this.literals, this.coefficients);
      case GE:
        return f.pbc(CType.LT, this.rhs, this.literals, this.coefficients);
      case GT:
        return f.pbc(CType.LE, this.rhs, this.literals, this.coefficients);
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean comparator");
    }
  }

  @Override
  public Formula nnf() {
    Formula nnf = this.transformationCache.get(NNF);
    if (nnf == null) {
      if (this.encoding == null)
        this.encode();
      nnf = f.and(this.encoding.formula(this.f));
      this.setTransformationCacheEntry(NNF, nnf);
    }
    return nnf;
  }

  /**
   * Encodes this constraint as CNF and stores the result.
   */
  private void encode() {
    this.encoding = this.f.pbEncoder().encode(this);
  }

  @Override
  public int hashCode() {
    if (this.hashCode == 0) {
      int temp = this.comparator.hashCode() + this.rhs;
      for (int i = 0; i < literals.length; i++) {
        temp += 11 * literals[i].hashCode();
        temp += 13 * coefficients[i];
      }
      this.hashCode = temp;
    }
    return this.hashCode;
  }

  @Override
  public boolean equals(final Object other) {
    if (this == other)
      return true;
    if (other instanceof Formula && this.f == ((Formula) other).f)
      return false;
    if (other instanceof PBConstraint) {
      final PBConstraint o = (PBConstraint) other;
      return this.rhs == o.rhs && this.comparator == o.comparator
              && Arrays.equals(this.coefficients, o.coefficients)
              && Arrays.equals(this.literals, o.literals);
    }
    return false;
  }

  @Override
  public Iterator<Formula> iterator() {
    return ITERATOR;
  }
}
