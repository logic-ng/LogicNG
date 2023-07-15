// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;

/**
 * Boolean constant "True".
 * @version 1.0
 * @since 1.0
 */
public final class CTrue extends Constant {

    /**
     * Constructor.
     * @param factory the factory which created this instance
     */
    CTrue(final FormulaFactory factory) {
        super(FType.TRUE, factory);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return true;
    }

    @Override
    public Constant negate() {
        return this.f.falsum();
    }

    @Override
    public int hashCode() {
        return 42;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof CTrue;
    }
}
