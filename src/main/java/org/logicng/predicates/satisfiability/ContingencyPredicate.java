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

package org.logicng.predicates.satisfiability;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.solvers.SATSolver;

/**
 * Contingency predicate.  Indicates whether a formula is contingent
 * (neither a tautology nor a contradiction) or not.
 * @version 1.0
 * @since 1.0
 */
public final class ContingencyPredicate implements FormulaPredicate {

  private final SATPredicate satPredicate;
  private final TautologyPredicate tautologyPredicate;

  /**
   * Constructs a new contingency predicate with a given formula factory.
   * @param f the formula factory
   */
  public ContingencyPredicate(final FormulaFactory f) {
    this.satPredicate = new SATPredicate(f);
    this.tautologyPredicate = new TautologyPredicate(f);
  }

  /**
   * Constructs a new contingency predicate with a given SAT solver.
   * @param solver the SAT solver
   */
  public ContingencyPredicate(final SATSolver solver) {
    this.satPredicate = new SATPredicate(solver);
    this.tautologyPredicate = new TautologyPredicate(solver);
  }

  @Override
  public boolean test(final Formula formula, boolean cache) {
    return formula.holds(this.satPredicate, cache) && !formula.holds(this.tautologyPredicate, cache);
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
