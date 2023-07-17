// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import java.util.Arrays;

/**
 * A cardinality constraint of the form {@code l_1 + ... + l_n R k} where
 * {@code R} is one of {@code =, >, >=, <, <=} and with the following
 * restrictions:
 * <ul>
 * <li>The right-hand side {@code k} is greater or equal 0 for
 * {@code =, >=, <=}</li>
 * <li>The right-hand side {@code k} is greater or equal -1 for {@code >}</li>
 * <li>The right-hand side {@code k} is greater or equal 1 for {@code <}</li>
 * </ul>
 * @version 2.0.0
 * @since 2.0.0
 */
public final class CardinalityConstraint extends PBConstraint {

    private static int[] mkCoefficients(final int length) {
        final int[] coefficients = new int[length];
        Arrays.fill(coefficients, 1);
        return coefficients;
    }

    /**
     * Constructs a new cardinality constraint.
     * @param literals   the literals
     * @param comparator the comparator
     * @param rhs        the right-hand side, has to follow the restrictions in
     *                   the class description
     * @param f          the formula factory
     * @throws IllegalArgumentException if the number of literals and
     *                                  coefficients do not correspond
     */
    CardinalityConstraint(final Literal[] literals, final CType comparator, final int rhs, final FormulaFactory f) {
        super(literals, mkCoefficients(literals.length), comparator, rhs, f);
    }

    @Override
    public boolean isCC() {
        return true;
    }

    @Override
    public boolean isAmo() {
        return this.comparator == CType.LE && this.rhs == 1 || this.comparator == CType.LT && this.rhs == 2;
    }

    @Override
    public boolean isExo() {
        return this.comparator == CType.EQ && this.rhs == 1;
    }
}
