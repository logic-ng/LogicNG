// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;

import java.util.LinkedHashSet;

/**
 * Boolean conjunction.
 * <p>
 * Invariants: - has at least two elements - does not contain duplicates - does
 * not contain complementary literals - does not contain constants
 * @version 2.2.0
 * @since 1.0
 */
public final class And extends NAryOperator {

    /**
     * Constructor.
     * @param operands the stream of operands
     * @param f        the factory which created this instance
     */
    And(final LinkedHashSet<? extends Formula> operands, final FormulaFactory f) {
        super(FType.AND, operands, f);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        for (final Formula op : this.operands) {
            if (!op.evaluate(assignment)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return hashCode(31);
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a ==
                          // object
        }
        if (other instanceof And) { // this is not really efficient... but
                                    // should not be done anyway!
            return compareOperands(((And) other).operands);
        }
        return false;
    }
}
