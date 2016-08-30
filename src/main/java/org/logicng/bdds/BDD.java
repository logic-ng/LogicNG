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

package org.logicng.bdds;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * The internal representation of a BDD.
 * @version 1.2
 * @since 1.2
 */
public final class BDD {

  private final int index;
  private final BDDFactory factory;

  /**
   * Constructs a new BDD with a given index.
   * @param index   the index
   * @param factory the factory of this BDD
   */
  public BDD(int index, final BDDFactory factory) {
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
  public BigDecimal modelCount(int unimportantVars) {
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
  public List<Assignment> enumerateAllModels(final Variable[] variables) {
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
   * Returns a DNF formula for a this BDD.
   * @return the DNF for the formula represented by this BDD
   */
  public Formula dnf() {
    return this.factory.dnf(this);
  }

  @Override
  public int hashCode() {
    return Objects.hash(index, factory);
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || other instanceof BDD
            && this.index == ((BDD) other).index
            && Objects.equals(this.factory, ((BDD) other).factory);
  }

  @Override
  public String toString() {
    return "BDD{" + index + "}";
  }
}
