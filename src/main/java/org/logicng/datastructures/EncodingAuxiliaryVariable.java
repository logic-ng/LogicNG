// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.datastructures;

import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * An auxiliary variable for encoding results.
 * <p>
 * This variable is used, if the result is added directly to a solver. In this
 * case no variable on the factory has to be created.
 * @version 1.3
 * @since 1.1
 */
final class EncodingAuxiliaryVariable extends Variable {

    final boolean negated;

    /**
     * Constructs a new auxiliary variable
     * @param name    the literal name
     * @param negated {@code true} if the variables is negated, {@code false}
     *                otherwise
     */
    EncodingAuxiliaryVariable(final String name, final boolean negated) {
        super(name, null);
        this.negated = negated;
    }

    @Override
    public Literal negate() {
        return new EncodingAuxiliaryVariable(this.name(), !this.negated);
    }

    @Override
    public String toString() {
        return name();
    }
}
