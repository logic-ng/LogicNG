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
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.predicates.DNFPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_SAT;

/**
 * A SAT solver based SAT predicate.  Indicates whether a formula is satisfiable or not.
 * @version 1.1
 * @since 1.0
 */
public final class SATPredicate implements FormulaPredicate {

  private final DNFPredicate dnfPredicate = new DNFPredicate();
  private final SATSolver solver;

  /**
   * Constructs a new SAT predicate with a given formula factory.
   * @param f the formula factory
   */
  public SATPredicate(final FormulaFactory f) {
    this.solver = MiniSat.miniSat(f);
  }

  /**
   * Constructs a new SAT predicate with a given SAT solver.
   * @param solver the SAT solver
   */
  public SATPredicate(final SATSolver solver) {
    this.solver = solver;
  }

  @Override
  public boolean test(final Formula formula, boolean cache) {
    final Tristate cached = formula.predicateCacheEntry(IS_SAT);
    if (cached != Tristate.UNDEF)
      return cached == Tristate.TRUE;
    boolean result;
    if (formula.type() == FType.FALSE)
      result = false;
    else if (formula.type() == FType.TRUE || formula.type() == FType.LITERAL || formula.holds(dnfPredicate))
      result = true;
    else {
      this.solver.add(formula);
      result = solver.sat() == Tristate.TRUE;
      solver.reset();
    }
    if (cache)
      formula.setPredicateCacheEntry(IS_SAT, result);
    return result;
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
