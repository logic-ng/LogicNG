// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;

/**
 * Boolean implication.
 * @version 2.2.0
 * @since 1.0
 */
public final class Implication extends BinaryOperator {

    /**
     * Constructor.
     * @param left  the left-hand side operand
     * @param right the right-hand side operand
     * @param f     the factory which created this instance
     */
    Implication(final Formula left, final Formula right, final FormulaFactory f) {
        super(FType.IMPL, left, right, f);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return !this.left.evaluate(assignment) || this.right.evaluate(assignment);
    }

    @Override
    public Formula restrict(final Assignment assignment) {
        return this.f.implication(this.left.restrict(assignment), this.right.restrict(assignment));
    }

    @Override
    public int hashCode() {
        if (this.hashCode == 0) {
            this.hashCode = 37 * this.left.hashCode() + 39 * this.right.hashCode();
        }
        return this.hashCode;
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a == object
        }
        if (other instanceof Implication) {
            final Implication otherImp = (Implication) other;
            return this.left.equals(otherImp.left) && this.right.equals(otherImp.right);
        }
        return false;
    }
}
