// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Not;

/**
 * Predicate to test if a formula contains any subformula that is a
 * pseudo-Boolean constraint.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class ContainsPBCPredicate implements FormulaPredicate {

    private final static ContainsPBCPredicate INSTANCE = new ContainsPBCPredicate();

    /**
     * Private empty constructor. Singleton class.
     */
    private ContainsPBCPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static ContainsPBCPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return false;
            case AND:
            case OR:
                for (final Formula op : formula) {
                    if (test(op, cache)) {
                        return true;
                    }
                }
                return false;
            case NOT:
                return test(((Not) formula).operand(), cache);
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                return test(binary.left(), cache) || test(binary.right(), cache);
            case PBC:
                return true;
            default:
                throw new IllegalArgumentException("Unknown formula type " + formula.type());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
