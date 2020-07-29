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
//  Copyright 2015-20xx Christoph Zengler                                //
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

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_SAT;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

/**
 * A SAT solver based SAT predicate.  Indicates whether a formula is satisfiable or not.
 * @version 1.5.1
 * @since 1.0
 */
public final class SATPredicate implements FormulaPredicate {

    private final SATSolver solver;

    /**
     * Constructs a new SAT predicate with a given formula factory.
     * @param f the formula factory
     */
    public SATPredicate(final FormulaFactory f) {
        this.solver = MiniSat.miniSat(f);
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_SAT);
        if (cached != Tristate.UNDEF) {
            return cached == Tristate.TRUE;
        }
        final boolean result;
        if (formula.type() == FType.FALSE) {
            result = false;
        } else {
            this.solver.add(formula);
            result = this.solver.sat() == Tristate.TRUE;
            this.solver.reset();
        }
        if (cache) {
            formula.setPredicateCacheEntry(IS_SAT, result);
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
