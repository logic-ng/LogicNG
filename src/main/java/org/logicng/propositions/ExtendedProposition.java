// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.propositions;

import org.logicng.formulas.Formula;

import java.util.Objects;

/**
 * An extended proposition in LogicNG. An extended proposition is a formula with
 * additional information like a user-provided {@link PropositionBackpack}
 * object.
 * @param <T> the type of the bagback
 * @version 2.0.0
 * @since 1.0
 */
public final class ExtendedProposition<T extends PropositionBackpack> extends Proposition {

    private final Formula formula;
    private final T backpack;

    /**
     * Constructs a new extended proposition for a single formula.
     * @param backpack the backpack
     * @param formula  the formula
     */
    public ExtendedProposition(final T backpack, final Formula formula) {
        this.formula = formula;
        this.backpack = backpack;
    }

    @Override
    public Formula formula() {
        return this.formula;
    }

    /**
     * Returns the backpack of this proposition.
     * @return the backpack of this proposition
     */
    public T backpack() {
        return this.backpack;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.formula, this.backpack);
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other instanceof ExtendedProposition) {
            final ExtendedProposition<?> o = (ExtendedProposition<?>) other;
            return Objects.equals(this.formula, o.formula) && Objects.equals(this.backpack, o.backpack);
        }
        return false;
    }

    @Override
    public String toString() {
        return String.format("ExtendedProposition{formula=%s, backpack=%s}", this.formula, this.backpack);
    }
}
