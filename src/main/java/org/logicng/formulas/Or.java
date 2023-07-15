// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.PredicateCacheEntry;

import java.util.LinkedHashSet;

/**
 * Boolean disjunction.
 * <p>
 * Invariants:
 * - has at least two elements
 * - does not contain duplicates
 * - does not contain complementary literals
 * - does not contain constants
 * @version 2.2.0
 * @since 1.0
 */
public final class Or extends NAryOperator {

    /**
     * Constructor.
     * @param operands the list of operands
     * @param f        the factory which created this instance
     */
    Or(final LinkedHashSet<? extends Formula> operands, final FormulaFactory f) {
        super(FType.OR, operands, f);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        for (final Formula op : this.operands) {
            if (op.evaluate(assignment)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns {@code true} if this formula is a CNF clause, {@code false} otherwise.
     * @return {@code true} if this formula is a CNF clause
     */
    public boolean isCNFClause() {
        return this.f.predicateCacheEntry(this, PredicateCacheEntry.IS_CNF) == Tristate.TRUE;
    }

    @Override
    public int hashCode() {
        return hashCode(17);
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a == object
        }
        if (other instanceof Or) { // this is not really efficient... but should not be done anyway!
            return compareOperands(((Or) other).operands);
        }
        return false;
    }
}
