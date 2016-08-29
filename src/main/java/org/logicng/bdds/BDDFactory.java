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

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Super class for simple BDD factories.
 * @version 1.2
 * @since 1.2
 */
public abstract class BDDFactory {

  protected final FormulaFactory f;

  /**
   * Constructs a new BDD factory.
   * @param f the formula factory
   */
  public BDDFactory(final FormulaFactory f) {
    this.f = f;
  }

  /**
   * Builds a BDD for a given formula.
   * @param formula the formula
   * @return the top node of the BDD
   */
  public abstract BDD build(final Formula formula);

  /**
   * Is BDD m a tautology.
   * @param m the BDD to test for the tautology test (index of root node)
   * @return 'true' if m is a tautology, 'false' else
   */
  public abstract boolean isTautology(final BDD m);

  /**
   * Is BDD m a contradiction.
   * @param m BDD to test for the contradiction test (index of root node)
   * @return 'true' if m is a contradiction, 'false' else
   */
  public abstract boolean isContradiction(final BDD m);

  /**
   * Sets the variable order for this BDD.  This method must be called BEFORE the BDD is generated.
   * @param varOrder the variable order
   */
  public void setVariableOrder(final List<Variable> varOrder) {
    this.setVariableOrder(varOrder.toArray(new Variable[varOrder.size()]));
  }

  /**
   * Sets the variable order for this BDD.  This method must be called BEFORE the BDD is generated.
   * @param varOrder the variable order
   */
  public void setVariableOrder(final LNGVector<Variable> varOrder) {
    this.setVariableOrder(varOrder.toArray());
  }

  /**
   * Sets the variable order for this BDD.  This method must be called BEFORE the BDD is generated.
   * @param varOrder the variable order
   */
  public abstract void setVariableOrder(final Variable... varOrder);

  /**
   * Returns the model count of a given BDD.
   * @param bdd the BDD
   * @return the model count
   */
  public abstract BigDecimal modelCount(final BDD bdd);

  /**
   * Returns the model count of a given BDD with a given number of unimportant variables.
   * @param bdd             the BDD
   * @param unimportantVars the number of unimportant variables
   * @return the model count
   */
  public abstract BigDecimal modelCount(final BDD bdd, int unimportantVars);

  /**
   * Enumerates all models of a given BDD.
   * @param bdd the BDD
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final BDD bdd) {
    return enumerateAllModels(bdd, (Collection<Variable>) null);
  }

  /**
   * Enumerates all models of a given BDD wrt. a given set of variables.
   * @param bdd       the BDD
   * @param variables the variables
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final BDD bdd, final Variable[] variables) {
    return this.enumerateAllModels(bdd, Arrays.asList(variables));
  }

  /**
   * Enumerates all models of a given BDD wrt. a given set of variables.
   * @param bdd       the BDD
   * @param variables the variables
   * @return the list of all models
   */
  public abstract List<Assignment> enumerateAllModels(final BDD bdd, final Collection<Variable> variables);

  /**
   * Returns a CNF formula for a given BDD.
   * @param bdd the node
   * @return the CNF for the formula represented by the BDD
   */
  public abstract Formula cnf(final BDD bdd);

  /**
   * Returns a DNF formula for a given BDD.
   * @param bdd the BDD
   * @return the DNF for the formula represented by the BDD
   */
  public Formula dnf(final BDD bdd) {
    final List<Formula> ops = new LinkedList<>();
    for (final Assignment ass : this.enumerateAllModels(bdd))
      ops.add(ass.formula(f));
    return f.and(ops);
  }
}
