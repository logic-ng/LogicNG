// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates.satisfiability;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_TAUTOLOGY;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;

/**
 * Tautology predicate.  Indicates whether a formula is a tautology or not.
 * @version 2.0
 * @since 1.0
 */
public final class TautologyPredicate implements FormulaPredicate {

    private final SATPredicate satPredicate;

    /**
     * Constructs a new tautology predicate with a given formula factory.
     * @param f the formula factory
     */
    public TautologyPredicate(final FormulaFactory f) {
        this.satPredicate = new SATPredicate(f);
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_TAUTOLOGY);
        if (cached != Tristate.UNDEF) {
            return cached == Tristate.TRUE;
        }
        final boolean result;
        final Formula negation = formula.negate();
        result = !negation.holds(this.satPredicate);
        if (cache) {
            formula.setPredicateCacheEntry(IS_TAUTOLOGY, result);
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
