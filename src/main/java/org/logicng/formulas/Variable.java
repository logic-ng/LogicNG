// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

/**
 * Boolean variables.
 * <p>
 * A variable is a shortcut for a positive literal.
 * @version 1.0
 * @since 1.0
 */
public class Variable extends Literal {

    /**
     * Constructor.
     * @param name the literal name
     * @param f    the factory which created this literal
     */
    protected Variable(final String name, FormulaFactory f) {
        super(name, true, f);
    }
}
