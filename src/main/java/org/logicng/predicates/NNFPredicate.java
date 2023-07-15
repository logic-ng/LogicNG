// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_NNF;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;

/**
 * NNF predicate.  Indicates whether a formula is in NNF or not.
 * @version 1.5.1
 * @since 1.5.1
 */
public final class NNFPredicate implements FormulaPredicate {

    private final static NNFPredicate INSTANCE = new NNFPredicate();

    /**
     * Private empty constructor.  Singleton class.
     */
    private NNFPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static NNFPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_NNF);
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
            case AND:
            case OR:
                result = true;
                for (final Formula op : formula) {
                    if (!test(op, cache)) {
                        result = false;
                        break;
                    }
                }
                break;
            case NOT:
            case IMPL:
            case EQUIV:
            case PBC:
                result = false;
                break;
            default:
                throw new IllegalArgumentException("Cannot compute NNF predicate on " + formula.type());
        }
        if (cache) {
            formula.setPredicateCacheEntry(IS_NNF, result);
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
