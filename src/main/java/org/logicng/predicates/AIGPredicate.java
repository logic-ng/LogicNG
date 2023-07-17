// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_AIG;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Not;

/**
 * And-inverter-graph (AIG) predicate. Returns {@code true} if the given formula
 * is an AIG, {@code false} otherwise.
 * @version 1.0
 * @since 1.0
 */
public final class AIGPredicate implements FormulaPredicate {

    private final static AIGPredicate INSTANCE = new AIGPredicate();

    /**
     * Private empty constructor. Singleton class.
     */
    private AIGPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static AIGPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_AIG);
        if (cached != Tristate.UNDEF) {
            return cached == Tristate.TRUE;
        }
        boolean result;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                result = true;
                break;
            case IMPL:
            case EQUIV:
            case OR:
            case PBC:
                result = false;
                break;
            case NOT:
                result = test(((Not) formula).operand(), cache);
                break;
            case AND:
                result = true;
                for (final Formula op : formula) {
                    if (!test(op, cache)) {
                        result = false;
                        break;
                    }
                }
                break;
            default:
                throw new IllegalArgumentException("Cannot compute AIG predicate on " + formula.type());
        }
        if (cache) {
            formula.setPredicateCacheEntry(IS_AIG, result);
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
