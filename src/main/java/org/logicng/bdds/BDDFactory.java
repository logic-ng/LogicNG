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

package org.logicng.bdds;

import org.logicng.bdds.datastructures.LNGBDD;
import org.logicng.bdds.datastructures.LNGBDDNode;
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
 * @version 1.4
 * @since 1.4
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
   * Builds a BDD for a given formula.  BDDs support all Boolean formula types but not pseudo-Boolean constraints.
   * The reason is that before converting a formula to a BDD one must specify the number of variables.  In case of
   * pseudo-Boolean constraints this number depends on the translation of the constraint.  Therefore the caller first
   * has to transform any pseudo-Boolean constraints in their respective CNF representation before converting them
   * to a BDD.
   * @param formula the formula
   * @return the top node of the BDD
   */
  public abstract LNGBDD build(final Formula formula);

  /**
   * Is BDD m a tautology.
   * @param m the BDD to test for the tautology test (index of root node)
   * @return 'true' if m is a tautology, 'false' else
   */
  public abstract boolean isTautology(final LNGBDD m);

  /**
   * Is BDD m a contradiction.
   * @param m BDD to test for the contradiction test (index of root node)
   * @return 'true' if m is a contradiction, 'false' else
   */
  public abstract boolean isContradiction(final LNGBDD m);

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
  public abstract BigDecimal modelCount(final LNGBDD bdd);

  /**
   * Returns the model count of a given BDD with a given number of unimportant variables.
   * @param bdd             the BDD
   * @param unimportantVars the number of unimportant variables
   * @return the model count
   */
  public BigDecimal modelCount(final LNGBDD bdd, int unimportantVars) {
    return modelCount(bdd).divide(BigDecimal.valueOf((int) Math.pow(2, unimportantVars)));
  }

  /**
   * Enumerates all models of a given BDD.
   * @param bdd the BDD
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final LNGBDD bdd) {
    return enumerateAllModels(bdd, (Collection<Variable>) null);
  }

  /**
   * Enumerates all models of a given BDD wrt. a given set of variables.
   * @param bdd       the BDD
   * @param variables the variables
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final LNGBDD bdd, final Variable[] variables) {
    return this.enumerateAllModels(bdd, Arrays.asList(variables));
  }

  /**
   * Enumerates all models of a given BDD wrt. a given set of variables.
   * @param bdd       the BDD
   * @param variables the variables
   * @return the list of all models
   */
  public abstract List<Assignment> enumerateAllModels(final LNGBDD bdd, final Collection<Variable> variables);

  /**
   * Returns a CNF formula for a given BDD.
   * @param bdd the node
   * @return the CNF for the formula represented by the BDD
   */
  public abstract Formula cnf(final LNGBDD bdd);

  /**
   * Returns the number of clauses for the CNF formula of the given BDD.
   * @param bdd the node
   * @return the number of clauses for the CNF formula of the given BDD
   */
  public abstract BigDecimal numberOfClausesCNF(final LNGBDD bdd);

  /**
   * Returns a DNF formula for a given BDD.
   * @param bdd the BDD
   * @return the DNF for the formula represented by the BDD
   */
  public Formula dnf(final LNGBDD bdd) {
    final List<Formula> ops = new LinkedList<>();
    for (final Assignment ass : this.enumerateAllModels(bdd))
      ops.add(ass.formula(f));
    return ops.isEmpty() ? f.falsum() : f.or(ops);
  }

  /**
   * Returns a LogicNG internal BDD data structure of a given BDD.
   * @param bdd the BDD
   * @return the BDD as LogicNG data structure
   */
  public abstract LNGBDDNode toLngBdd(int bdd);

  /**
   * Returns the formula factory for this BDD factory.
   * @return the formula factory
   */
  public FormulaFactory getF() {
    return this.f;
  }
}
