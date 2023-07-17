// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
 * A SAT solver based SAT predicate. Indicates whether a formula is satisfiable
 * or not.
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
