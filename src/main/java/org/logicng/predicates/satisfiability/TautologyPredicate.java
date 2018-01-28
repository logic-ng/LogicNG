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

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.predicates.CNFPredicate;
import org.logicng.solvers.SATSolver;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_TAUTOLOGY;

/**
 * Tautology predicate.  Indicates whether a formula is a tautology or not.
 * @version 1.0
 * @since 1.0
 */
public final class TautologyPredicate implements FormulaPredicate {

  private final CNFPredicate cnfPredicate = new CNFPredicate();
  private final SATPredicate satPredicate;

  /**
   * Constructs a new tautology predicate with a given formula factory.
   * @param f the formula factory
   */
  public TautologyPredicate(final FormulaFactory f) {
    this.satPredicate = new SATPredicate(f);
  }

  /**
   * Constructs a new tautology predicate with a given SAT solver.
   * @param solver the SAT solver
   */
  public TautologyPredicate(final SATSolver solver) {
    this.satPredicate = new SATPredicate(solver);
  }

  @Override
  public boolean test(final Formula formula, boolean cache) {
    final Tristate cached = formula.predicateCacheEntry(IS_TAUTOLOGY);
    if (cached != Tristate.UNDEF)
      return cached == Tristate.TRUE;
    final FormulaFactory factory = formula.factory();
    boolean result;
    if (formula.holds(cnfPredicate))
      result = formula == factory.verum();
    else {
      final Formula negation = formula.negate();
      result = !negation.holds(this.satPredicate);
    }
    if (cache)
      formula.setPredicateCacheEntry(IS_TAUTOLOGY, result);
    return result;
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
