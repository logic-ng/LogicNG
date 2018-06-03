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

package org.logicng.bdds.datastructures;

import org.logicng.bdds.BDDFactory;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.SortedMap;
import java.util.SortedSet;

/**
 * The internal representation of a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDD {

  private final int index;
  private final BDDFactory factory;

  /**
   * Constructs a new BDD with a given index.
   * @param index   the index
   * @param factory the factory of this BDD
   */
  public BDD(final int index, final BDDFactory factory) {
    this.index = index;
    this.factory = factory;
  }

  /**
   * Returns the index of this BDD.
   * @return the index of this BDD
   */
  public int index() {
    return this.index;
  }

  /**
   * Returns the factory of this BDD.
   * @return the factory of this BDD
   */
  public BDDFactory factory() {
    return this.factory;
  }

  /**
   * Returns {@code true} if this BDD is a tautology, {@code false} otherwise.
   * @return {@code true} if this BDD is a tautology, {@code false} otherwise
   */
  public boolean isTautology() {
    return this.factory.isTautology(this);
  }

  /**
   * Returns {@code true} if this BDD is a contradiction, {@code false} otherwise.
   * @return {@code true} if this BDD is a contradiction, {@code false} otherwise
   */
  public boolean isContradiction() {
    return this.factory.isContradiction(this);
  }

  /**
   * Returns the model count of this BDD.
   * @return the model count
   */
  public BigDecimal modelCount() {
    return this.factory.modelCount(this);
  }

  /**
   * Returns the model count of this BDD with a given number of unimportant variables.
   * @param unimportantVars the number of unimportant variables
   * @return the model count
   */
  public BigDecimal modelCount(final int unimportantVars) {
    return this.factory.modelCount(this, unimportantVars);
  }

  /**
   * Enumerates all models of this BDD.
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels() {
    return enumerateAllModels((Collection<Variable>) null);
  }

  /**
   * Enumerates all models of this BDD wrt. a given set of variables.
   * @param variables the variables
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final Variable... variables) {
    return this.enumerateAllModels(Arrays.asList(variables));
  }

  /**
   * Enumerates all models of this BDD wrt. a given set of variables.
   * @param variables the variables
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final Collection<Variable> variables) {
    return this.factory.enumerateAllModels(this, variables);
  }

  /**
   * Returns a CNF formula for this BDD.
   * @return the CNF for the formula represented by this BDD
   */
  public Formula cnf() {
    return this.factory.cnf(this);
  }

  /**
   * Returns the number of clauses for the CNF formula of the BDD.
   * @return the number of clauses for the CNF formula of the BDD
   */
  public BigDecimal numberOfClausesCNF() {
    return this.factory.numberOfClausesCNF(this);
  }

  /**
   * Returns a DNF formula for a this BDD.
   * @return the DNF for the formula represented by this BDD
   */
  public Formula dnf() {
    return this.factory.dnf(this);
  }

  /**
   * Restricts the BDD.
   * @param restriction the restriction
   * @return the restricted BDD
   */
  public BDD restrict(final Collection<Literal> restriction) {
    final BDD resBDD = this.factory.build(this.factory.getF().and(restriction));
    return new BDD(this.factory.underlyingKernel().restrict(index(), resBDD.index()), this.factory);
  }

  /**
   * Restricts the BDD.
   * @param restriction the restriction
   * @return the restricted BDD
   */
  public BDD restrict(final Literal... restriction) {
    return restrict(Arrays.asList(restriction));
  }

  /**
   * Existential quantifier elimination for a given set of variables.
   * @param variables the variables to eliminate
   * @return the BDD with the eliminated variables
   */
  public BDD exists(final Collection<Variable> variables) {
    final BDD resBDD = this.factory.build(this.factory.getF().and(variables));
    return new BDD(this.factory.underlyingKernel().exists(index(), resBDD.index()), this.factory);
  }

  /**
   * Existential quantifier elimination for a given set of variables.
   * @param variables the variables to eliminate
   * @return the BDD with the eliminated variables
   */
  public BDD exists(final Variable... variables) {
    return exists(Arrays.asList(variables));
  }

  /**
   * Universal quantifier elimination for a given set of variables.
   * @param variables the variables to eliminate
   * @return the BDD with the eliminated variables
   */
  public BDD forall(final Collection<Variable> variables) {
    final BDD resBDD = this.factory.build(this.factory.getF().and(variables));
    return new BDD(this.factory.underlyingKernel().forAll(index(), resBDD.index()), this.factory);
  }

  /**
   * Universal quantifier elimination for a given set of variables.
   * @param variables the variables to eliminate
   * @return the BDD with the eliminated variables
   */
  public BDD forall(final Variable... variables) {
    return forall(Arrays.asList(variables));
  }

  /**
   * Returns an arbitrary model of this BDD or {@code null} if there is none.
   * @return an arbitrary model of this BDD
   */
  public Assignment model() {
    return this.factory.model(this);
  }

  /**
   * Returns an arbitrary model of this BDD which contains at least the given variables or {@code null} if there is
   * none.  If a variable is a don't care variable, it will be assigned with the given default value.
   * @param defaultValue the default value for don't care variables
   * @param variables    the set of variable which has to be contained in the model
   * @return an arbitrary model of this BDD
   */
  public Assignment model(final boolean defaultValue, final Collection<Variable> variables) {
    return this.factory.model(this, variables, defaultValue);
  }

  /**
   * Returns an arbitrary model of this BDD which contains at least the given variables or {@code null} if there is
   * none.  If a variable is a don't care variable, it will be assigned with the given default value.
   * @param defaultValue the default value for don't care variables
   * @param variables    the set of variable which has to be contained in the model
   * @return an arbitrary model of this BDD
   */
  public Assignment model(final boolean defaultValue, final Variable... variables) {
    return this.factory.model(this, Arrays.asList(variables), defaultValue);
  }

  /**
   * Returns a full model of this BDD or {@code null} if there is none.n the model
   * @return an full model of this BDD
   */
  public Assignment fullModel() {
    return this.factory.fullModel(this);
  }

  /**
   * Returns the number of paths leading to the terminal 'one' node.
   * @return the number of paths leading to the terminal 'one' node
   */
  public BigDecimal pathCountOne() {
    return this.factory.underlyingKernel().pathCountOne(this.index);
  }

  /**
   * Returns the number of paths leading to the terminal 'zero' node.
   * @return the number of paths leading to the terminal 'zero' node
   */
  public BigDecimal pathCountZero() {
    return this.factory.underlyingKernel().pathCountZero(this.index);
  }

  /**
   * Returns all the variables this BDD depends on.
   * @return all the variables that this BDD depends on
   */
  public SortedSet<Variable> support() {
    return this.factory.support(this);
  }

  /**
   * Returns the number of distinct nodes for this BDD.
   * @return the number of distinct nodes
   */
  public int nodeCount() {
    return this.factory.underlyingKernel().nodeCount(this.index);
  }

  /**
   * Returns how often each variable occurs in this BDD.
   * @return how often each variable occurs in the BDD
   */
  public SortedMap<Variable, Integer> variableProfile() {
    return this.factory.variableProfile(this);
  }

  /**
   * Returns a LogicNG internal BDD data structure of this BDD.
   * @return the BDD as LogicNG data structure
   */
  public BDDNode toLngBdd() {
    return this.factory.toLngBdd(this.index);
  }

  /**
   * Returns the internal nodes of this BDD (e.g. for writing to a DOT file).
   * @return the internal nodes of this BDD
   */
  public List<BDDFactory.InternalBDDNode> internalNodes() {
    return this.factory.getInternalNodes(this.index);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.index, this.factory);
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || other instanceof BDD
            && this.index == ((BDD) other).index
            && Objects.equals(this.factory, ((BDD) other).factory);
  }

  @Override
  public String toString() {
    return "BDD{" + this.index + "}";
  }
}
