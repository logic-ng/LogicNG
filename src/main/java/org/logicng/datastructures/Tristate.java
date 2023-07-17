// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.datastructures;

/**
 * A tristate constant. This constant can have three different values:
 * {@code TRUE}, {@code FALSE}, and {@code UNDEF}.
 * @version 1.0
 * @since 1.0
 */
public enum Tristate {
    TRUE,
    FALSE,
    UNDEF;

    /**
     * Returns the negation of a tristate constant.
     * @param l the tristate constant
     * @return the negation of the tristate constant
     */
    public static Tristate negate(final Tristate l) {
        return l == FALSE ? TRUE : (l == TRUE ? FALSE : UNDEF);
    }

    /**
     * Constructs a tristate constant from an ordinary Boolean value.
     * @param b the Boolean value
     * @return the tristate constant
     */
    public static Tristate fromBool(final boolean b) {
        return b ? TRUE : FALSE;
    }
}
