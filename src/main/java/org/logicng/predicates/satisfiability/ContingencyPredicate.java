// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates.satisfiability;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;

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

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        return formula.holds(this.satPredicate, cache) && !formula.holds(this.tautologyPredicate, cache);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
