// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.propositions;

import org.logicng.formulas.Formula;

import java.util.Objects;

/**
 * A proposition in LogicNG. A proposition is a formula with an additional
 * textual description.
 * @version 2.0.0
 * @since 1.0
 */
public final class StandardProposition extends Proposition {

    private final Formula formula;
    private final String description;

    /**
     * Constructs a new proposition for a single formulas.
     * @param formula the formulas
     */
    public StandardProposition(final Formula formula) {
        this.formula = formula;
        this.description = "";
    }

    /**
     * Constructs a new proposition for a single formulas.
     * @param description the description
     * @param formula     the formulas
     */
    public StandardProposition(final String description, final Formula formula) {
        this.formula = formula;
        this.description = description == null ? "" : description;
    }

    @Override
    public Formula formula() {
        return this.formula;
    }

    /**
     * Returns the backpack of this proposition.
     * @return the backpack of this proposition
     */
    public String description() {
        return this.description;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.formula, this.description);
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other instanceof StandardProposition) {
            final StandardProposition o = (StandardProposition) other;
            return Objects.equals(this.formula, o.formula) && Objects.equals(this.description, o.description);
        }
        return false;
    }

    @Override
    public String toString() {
        return String.format("StandardProposition{formula=%s, description=%s}", this.formula, this.description);
    }
}
