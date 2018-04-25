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
//  Copyright 2015-2018 Christoph Zengler                                //
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

package org.logicng.collections;

import org.logicng.datastructures.Substitution;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.functions.LiteralProfileFunction;
import org.logicng.functions.VariableProfileFunction;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * A list of formulas.  This can represent the operands of an n-ary operator, a CNF, a DNF, a constraint, etc.
 * @version 1.0
 * @since 1.0
 */
public final class ImmutableFormulaList implements Iterable<Formula> {

  private final FType operator;
  private final Formula[] formulas;
  private SortedSet<Variable> variables;
  private Formula formula;

  /**
   * Constructs a formula list for a variable number of formulas.
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final Formula... formulas) {
    this.operator = FType.NONE;
    this.formulas = Arrays.copyOf(formulas, formulas.length);
  }

  /**
   * Constructs a formula list for a variable number of formulas.
   * @param operator the operator
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final FType operator, final Formula... formulas) {
    this.operator = operator;
    this.formulas = Arrays.copyOf(formulas, formulas.length);
  }

  /**
   * Constructs a formula list for a collection of formulas.
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final Collection<? extends Formula> formulas) {
    this.operator = FType.NONE;
    this.formulas = formulas.toArray(new Formula[0]);
  }

  /**
   * Constructs a formula list for a collection of formulas.
   * @param operator the operator
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final FType operator, final Collection<? extends Formula> formulas) {
    this.operator = operator;
    this.formulas = formulas.toArray(new Formula[0]);
  }

  /**
   * Constructs a formula list for a vector of formulas.
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final LNGVector<? extends Formula> formulas) {
    this.operator = FType.NONE;
    this.formulas = formulas.toArray();
  }

  /**
   * Constructs a formula list for a vector of formulas.
   * @param operator the operator
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final FType operator, final LNGVector<? extends Formula> formulas) {
    this.operator = operator;
    this.formulas = formulas.toArray();
  }

  /**
   * Constructs a formula list for a vector of formulas.
   * @param operator the operator
   * @param formulas the formulas
   */
  public ImmutableFormulaList(final FType operator, final ImmutableFormulaList formulas) {
    this.operator = operator;
    this.formulas = formulas.formulas;
  }

  /**
   * Constructs a formula list for a variable number of formulas.
   * @param operator the operator
   * @param copy     {@code true} if the input array should be copied, {@code false} otherwise
   * @param formulas the formulas
   */
  private ImmutableFormulaList(final FType operator, final boolean copy, final Formula... formulas) {
    this.operator = operator;
    this.formulas = copy ? Arrays.copyOf(formulas, formulas.length) : formulas;
  }

  /**
   * Returns the operator of this formula list.
   * @return the operator of this formula list
   */
  public FType operator() {
    return this.operator;
  }

  /**
   * Returns this formula list as a formula if there is an operator.
   * @param f the formula factory
   * @return this formula list as a formula
   * @throws IllegalStateException if the operator is not n-ary
   */
  public Formula formula(final FormulaFactory f) {
    if (this.operator != FType.AND && this.operator != FType.OR)
      throw new IllegalStateException("Illegal operator for formula list formula construction: " + this.operator);
    if (this.formula == null) {
      if (this.operator == FType.AND)
        this.formula = f.and(this.formulas);
      else
        this.formula = f.or(this.formulas);
    }
    return this.formula;
  }

  /**
   * Returns the size of this formula list.
   * @return the size of this formula list
   */
  public int size() {
    return this.formulas.length;
  }

  /**
   * Returns {@code true} if this formula list is empty, {@code false} otherwise.
   * @return {@code true} if this formula list is empty
   */
  public boolean empty() {
    return this.formulas.length == 0;
  }

  /**
   * Returns the i-th formula of this formula list.
   * @param i the index
   * @return the i-th formula of this formula list
   */
  public Formula get(final int i) {
    if (i < 0 || i >= this.formulas.length)
      throw new IllegalArgumentException("Illegal formula index: " + i);
    return this.formulas[i];
  }

  /**
   * Returns {@code true} if this formula list contains a given formula, {@code false} otherwise.  Formulas are only
   * checked on the top-level, e.g. a sub-formula check is not performed.
   * @param formula the formula
   * @return {@code true} if this formula list contains a given formula
   */
  public boolean containsFormula(final Formula formula) {
    for (final Formula f : this.formulas)
      if (formula.equals(f))
        return true;
    return false;
  }

  /**
   * Returns all variables occurring in this formula list.
   * @return all variables occurring in this formula list
   */
  public SortedSet<Variable> variables() {
    if (this.variables == null) {
      this.variables = new TreeSet<>();
      for (final Formula f : this.formulas)
        this.variables.addAll(f.variables());
    }
    return this.variables;
  }

  /**
   * Returns all literals occurring in this formula list.
   * @return all literals occurring in this formula list
   */
  public SortedSet<Literal> literals() {
    final SortedSet<Literal> literals = new TreeSet<>();
    for (final Formula f : this.formulas)
      literals.addAll(f.literals());
    return literals;
  }

  /**
   * Returns the variable profile of this formula list.  For each variable the number of occurrences is counted.
   * @return the variable profile of this formula list
   */
  public SortedMap<Variable, Integer> varProfile() {
    final VariableProfileFunction variableProfileFunction = new VariableProfileFunction();
    final SortedMap<Variable, Integer> profile = new TreeMap<>();
    for (final Formula f : this.formulas)
      for (final Map.Entry<Variable, Integer> entry : f.apply(variableProfileFunction).entrySet()) {
        final Integer currentCount = profile.get(entry.getKey());
        if (currentCount == null)
          profile.put(entry.getKey(), entry.getValue());
        else
          profile.put(entry.getKey(), entry.getValue() + currentCount);
      }
    return profile;
  }

  /**
   * Returns the literal profile of this formula list.  For each literal the number of occurrences is counted.
   * @return the literal profile of this formula list
   */
  public SortedMap<Literal, Integer> litProfile() {
    final LiteralProfileFunction literalProfileFunction = new LiteralProfileFunction();
    final SortedMap<Literal, Integer> profile = new TreeMap<>();
    for (final Formula f : this.formulas)
      for (final Map.Entry<Literal, Integer> entry : f.apply(literalProfileFunction).entrySet()) {
        final Integer currentCount = profile.get(entry.getKey());
        if (currentCount == null)
          profile.put(entry.getKey(), entry.getValue());
        else
          profile.put(entry.getKey(), entry.getValue() + currentCount);
      }
    return profile;
  }

  /**
   * Performs a given substitution on this formula list.
   * @param substitution the substitution
   * @return a new substituted formula
   */
  public ImmutableFormulaList substitute(final Substitution substitution) {
    final Formula[] subst = new Formula[this.formulas.length];
    for (int i = 0; i < this.formulas.length; i++)
      subst[i] = this.formulas[i].substitute(substitution);
    return new ImmutableFormulaList(this.operator, false, subst);
  }

  /**
   * Returns a copy of this formula list as array.
   * @return a copy of this formula list as array
   */
  public Formula[] toArray() {
    return Arrays.copyOf(this.formulas, this.formulas.length);
  }

  /**
   * Returns a copy of this formula list as list.
   * @return a copy of this formula list as list
   */
  public List<Formula> toList() {
    return Arrays.asList(this.formulas);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.operator, Arrays.hashCode(this.formulas));
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || (other instanceof ImmutableFormulaList) && this.operator == ((ImmutableFormulaList) other).operator
            && Arrays.equals(this.formulas, ((ImmutableFormulaList) other).formulas);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append(this.operator).append("[");
    for (int i = 0; i < this.formulas.length; i++) {
      sb.append(this.formulas[i]);
      if (i != this.formulas.length - 1)
        sb.append(", ");
    }
    sb.append("]");
    return sb.toString();
  }

  @Override
  public Iterator<Formula> iterator() {
    return new Iterator<Formula>() {
      private int i;

      @Override
      public boolean hasNext() {
        return this.i < ImmutableFormulaList.this.formulas.length;
      }

      @Override
      public Formula next() {
        if (this.i == ImmutableFormulaList.this.formulas.length)
          throw new NoSuchElementException();
        return ImmutableFormulaList.this.formulas[this.i++];
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }
}
