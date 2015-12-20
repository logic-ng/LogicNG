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
//  Copyright 2015 Christoph Zengler                                     //
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

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.CacheEntry;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * Super class for formulas.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public abstract class Formula implements Iterable<Formula> {

  protected final FType type;
  protected final FormulaFactory f;
  protected final SortedMap<CacheEntry, Formula> transformationCache;
  protected final SortedMap<CacheEntry, Tristate> predicateCache;
  protected final SortedMap<CacheEntry, Object> functionCache;
  protected SortedSet<Literal> variables;

  /**
   * Constructor.
   * @param type the type of the formula
   * @param f    the factory which created this formula
   */
  protected Formula(final FType type, final FormulaFactory f) {
    this.type = type;
    this.f = f;
    this.transformationCache = new TreeMap<>();
    this.predicateCache = new TreeMap<>();
    this.functionCache = new TreeMap<>();
    this.variables = null;
  }

  /**
   * Returns the type of this formula.
   * @return the type of this formula
   */
  public FType type() {
    return this.type;
  }

  /**
   * Returns the factory of this formula.
   * @return the factory of this formula
   */
  public FormulaFactory factory() {
    return this.f;
  }

  /**
   * Returns the number of atomic formulas of this formula.  An atomic formula is a predicate (constants and literals)
   * or a pseudo-Boolean constraint.
   * @return the number of atomic formulas of this formula.
   */
  public abstract int numberOfAtoms();

  /**
   * Returns the number of nodes of this formula.
   * @return the number of nodes of this formula.
   */
  public abstract int numberOfNodes();

  /**
   * Returns the number of operands of this formula.
   * @return the number of operands of this formula
   */
  public abstract int numberOfOperands();

  /**
   * Returns the number of internal nodes of this formula.
   * @return the number of internal nodes of this formula.
   */
  public int numberOfInternalNodes() {
    return subformulas().size();
  }

  /**
   * Returns the variable profile of this formula.  For each variable the number of occurrences is counted.
   * @return the variable profile of this formula
   */
  public SortedMap<Literal, Integer> varProfile() {
    final SortedMap<Literal, Integer> varprofile = new TreeMap<>();
    varProfileRec(varprofile);
    return varprofile;
  }

  /**
   * Recursive helper for the variable profile.
   * @param map the variable profile
   */
  protected abstract void varProfileRec(final SortedMap<Literal, Integer> map);

  /**
   * Returns the literal profile of this formula.  For each literal the number of occurrences is counted.
   * @return the literal profile of this formula
   */
  public SortedMap<Literal, Integer> litProfile() {
    final SortedMap<Literal, Integer> litprofile = new TreeMap<>();
    litProfileRec(litprofile);
    return litprofile;
  }

  /**
   * Recursive helper for the literal profile.
   * @param map the literal profile
   */
  protected abstract void litProfileRec(final SortedMap<Literal, Integer> map);

  /**
   * Returns the sub-formulas of this formula.  The order of the sub-formulas is bottom-up, i.e. a sub-formula only
   * appears in the list when all of its sub-formulas are already listed.
   * @return the sub-formulas of this formula
   */
  public abstract LinkedHashSet<Formula> subformulas();

  /**
   * Returns all variables occurring in this formula as positive literals.
   * @return all variables occurring in this formula as positive literals
   */
  public abstract SortedSet<Literal> variables();

  /**
   * Returns all literals occurring in this formula.
   * @return all literals occurring in this formula
   */
  public abstract SortedSet<Literal> literals();

  /**
   * Returns {@code true} if a given literal name is found in this formula, {@code false} otherwise.
   * @param literal the literal to search for
   * @return {@code true} if a given literal is found in this formula
   */
  public boolean contains(final String literal) {
    return this.contains(this.f.literal(literal));
  }

  /**
   * Returns {@code true} if a given positive literal is found in this formula, {@code false} otherwise.
   * @param literal the literal to search for
   * @return {@code true} if a given literal is found in this formula
   */
  public abstract boolean contains(final Literal literal);

  /**
   * Evaluates this formula with a given assignment.  A literal not covered by the assignment evaluates
   * to {@code false} if it is positive, otherwise it evaluates to {@code true}.
   * @param assignment the given assignment
   * @return the result of the evaluation, {@code true} or {@code false}
   */
  public abstract boolean evaluate(final Assignment assignment);

  /**
   * Restricts this formula with a given assignment.
   * @param assignment the given assignment
   * @return a new restricted formula
   */
  public abstract Formula restrict(final Assignment assignment);

  /**
   * Returns {@code true} if this formula contains a given sub-formula, {@code false} otherwise.
   * @param formula the sub-formula
   * @return {@code true} if this formula contains a given sub-formula
   */
  public abstract boolean containsSubformula(final Formula formula);

  /**
   * Performs a substitution on this formula given a single mapping from variable to formula.
   * @param literal the literal
   * @param formula the formula
   * @return a new substituted formula
   */
  public Formula substitute(final Literal literal, final Formula formula) {
    Substitution subst = new Substitution();
    subst.addMapping(literal, formula);
    return this.substitute(subst);
  }

  /**
   * Performs a given substitution on this formula.
   * @param substitution the substitution
   * @return a new substituted formula
   */
  public abstract Formula substitute(final Substitution substitution);

  /**
   * Returns a negated copy of this formula.
   * @return a negated copy of this formula
   */
  public abstract Formula negate();

  /**
   * Returns a copy of this formula which is in NNF.
   * @return a copy of this formula which is in NNF
   */
  public abstract Formula nnf();

  /**
   * Returns a copy of this formula which is in CNF.  The algorithm which is used for the default CNF transformation
   * can be configured in the {@link FormulaFactory}.
   * @return a copy of this formula which is in CNF
   */
  public Formula cnf() {
    return this.transform(f.cnfTransformation());
  }

  /**
   * Adds a dot file string representation of this formula to the given dot file string builder.
   * @param sb  the dot file string builder
   * @param ids the mapping from formulas to node ids
   */
  public abstract void generateDotString(final StringBuilder sb, final Map<Formula, Integer> ids);

  /**
   * Transforms this formula with a given formula transformator and caches the result.
   * @param transformation the formula transformator
   * @return the transformed formula
   */
  public Formula transform(final FormulaTransformation transformation) {
    return transformation.apply(this, true);
  }

  /**
   * Transforms this formula with a given formula transformator.
   * @param transformation the formula transformator
   * @param cache          indicates whether the result (and associated predicates) should be cached in this formula's cache.
   * @return the transformed formula
   */
  public Formula transform(final FormulaTransformation transformation, boolean cache) {
    return transformation.apply(this, cache);
  }

  /**
   * Evaluates a given predicate on this formula, caches the result, and returns {@code true} if the predicate holds,
   * {@code false} otherwise.
   * @param predicate the predicate
   * @return {@code true} if the predicate holds, {@code false} otherwise
   */
  public boolean holds(final FormulaPredicate predicate) {
    return predicate.test(this, true);
  }

  /**
   * Evaluates a given predicate on this formula and returns {@code true} if the predicate holds, {@code false} otherwise.
   * @param predicate the predicate
   * @param cache     indicates whether the result should be cached in this formula's cache
   * @return {@code true} if the predicate holds, {@code false} otherwise
   */
  public boolean holds(final FormulaPredicate predicate, boolean cache) {
    return predicate.test(this, cache);
  }

  /**
   * Applies a given function on this formula and returns the result.
   * @param function the function
   * @param <T>      the result type of the function
   * @return the result of the function application
   */
  public <T> T apply(final FormulaFunction<T> function) {
    return function.apply(this);
  }

  /**
   * Clears the cache of this formula.
   */
  public void clearCache() {
    this.transformationCache.clear();
  }

  /**
   * Returns an entry of the transformation cache of this formula.
   * @param key the cache key
   * @return the cache value or {@code null} if the key is not found
   */
  public Formula getTransformationCacheEntry(final CacheEntry key) {
    return this.transformationCache.get(key);
  }

  /**
   * Sets an entry in the transformation cache of this formula
   * @param key   the cache key
   * @param value the cache value
   */
  public void setTransformationCacheEntry(final CacheEntry key, final Formula value) {
    this.transformationCache.put(key, value);
  }

  /**
   * Returns an entry of the predicate cache of this formula.
   * @param key the cache key
   * @return the cache value (which is {@code UNDEF} if nothing is present)
   */
  public Tristate getPredicateCacheEntry(final CacheEntry key) {
    final Tristate tristate = this.predicateCache.get(key);
    if (tristate == null)
      return Tristate.UNDEF;
    return tristate;
  }

  /**
   * Sets an entry in the predicate cache of this formula
   * @param key   the cache key
   * @param value the cache value
   */
  public void setPredicateCacheEntry(final CacheEntry key, boolean value) {
    this.predicateCache.put(key, Tristate.fromBool(value));
  }

  /**
   * Sets an entry in the predicate cache of this formula
   * @param key   the cache key
   * @param value the cache value
   */
  public void setPredicateCacheEntry(final CacheEntry key, final Tristate value) {
    this.predicateCache.put(key, value);
  }

  /**
   * Returns an entry of the function cache of this formula.
   * @param key the cache key
   * @return the cache value or {@code null} if the key is not found
   */
  public Object getFunctionCacheEntry(final CacheEntry key) {
    return this.functionCache.get(key);
  }

  /**
   * Sets an entry in the function cache of this formula
   * @param key   the cache key
   * @param value the cache value
   */
  public void setFunctionCacheEntry(final CacheEntry key, final Object value) {
    this.functionCache.put(key, value);
  }

  @Override
  public String toString() {
    return f.string(this);
  }
}
