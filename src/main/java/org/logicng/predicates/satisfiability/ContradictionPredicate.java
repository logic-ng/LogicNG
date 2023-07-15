// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates.satisfiability;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;

/**
 * Contradiction predicate.  Indicates whether a formula is contradictory or not.
 * @version 1.0
 * @since 1.0
 */
public final class ContradictionPredicate implements FormulaPredicate {

    private final SATPredicate satPredicate;

    /**
     * Constructs a new contradiction predicate with a given formula factory.
     * @param f the formula factory
     */
    public ContradictionPredicate(final FormulaFactory f) {
        this.satPredicate = new SATPredicate(f);
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        return !formula.holds(this.satPredicate, cache);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
