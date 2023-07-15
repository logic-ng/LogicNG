// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;

/**
 * Boolean constant "False".
 * @version 1.0
 * @since 1.0
 */
public final class CFalse extends Constant {

    /**
     * Constructor.
     * @param factory the factory which created this instance
     */
    CFalse(final FormulaFactory factory) {
        super(FType.FALSE, factory);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return false;
    }

    @Override
    public Constant negate() {
        return this.f.verum();
    }

    @Override
    public int hashCode() {
        return -42;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof CFalse;
    }
}
