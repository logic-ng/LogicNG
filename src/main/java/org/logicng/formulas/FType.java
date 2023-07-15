// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

/**
 * Formula types for LogicNG.
 * @version 1.3
 * @since 1.0
 */
public enum FType {
    PBC((byte) 0x00),
    EQUIV((byte) 0x01),
    IMPL((byte) 0x02),
    OR((byte) 0x03),
    AND((byte) 0x04),
    NOT((byte) 0x05),
    LITERAL((byte) 0x06),
    TRUE((byte) 0x07),
    FALSE((byte) 0x08);

    private final byte precedence;

    /**
     * Constructs a new formula type with a given precedence and syntax string
     * @param precedence the precedence
     */
    FType(final byte precedence) {
        this.precedence = precedence;
    }

    /**
     * Returns the precedence of this formula type.
     * @return the precedence of this formula type
     */
    public byte precedence() {
        return this.precedence;
    }

    /**
     * Returns the dual operator of the given operator, i.e. AND for OR and vice versa.
     * @param type the type (AND or OR)
     * @return the dual operator
     */
    public static FType dual(final FType type) {
        if (type != AND && type != OR) {
            throw new IllegalArgumentException("Expected type AND or OR.");
        }
        return type == AND ? OR : AND;
    }
}
