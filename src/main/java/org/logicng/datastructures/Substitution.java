// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * A Boolean substitution.
 * @version 2.0.0
 * @since 1.0
 */
public final class Substitution {

    private final Map<Variable, Formula> subst;

    /**
     * Constructs a new empty substitution.
     */
    public Substitution() {
        this.subst = new HashMap<>();
    }

    /**
     * Constructs a new substitution by copying the given one.
     * @param other the substitution to copy
     */
    public Substitution(final Substitution other) {
        this.subst = new HashMap<>(other.subst);
    }

    /**
     * Constructs a new substitution and initializes it with the given mapping.
     * @param mapping the initial mapping
     */
    public Substitution(final Map<Variable, Formula> mapping) {
        this.subst = new HashMap<>(mapping);
    }

    /**
     * Returns the number of mappings in this substitution.
     * @return the number of mappings in this substitution
     */
    public int size() {
        return this.subst.size();
    }

    /**
     * Adds a mapping from variable to formula to this substitution.
     * <p>
     * If there is already a mapping for this variable, it will be overwritten.
     * @param variable the variable
     * @param formula  the formula
     */
    public void addMapping(final Variable variable, final Formula formula) {
        this.subst.put(variable, formula);
    }

    /**
     * Returns a formula for a given variable. If there is no mapping for this
     * variable, {@code null} is returned.
     * @param variable the variable
     * @return a formula or {@code null}
     */
    public Formula getSubstitution(final Variable variable) {
        return this.subst.get(variable);
    }

    /**
     * Returns an unmodifiable reference to the internal mapping of variables to
     * formulas.
     * @return the internal mapping of variables to formulas
     */
    public Map<Variable, Formula> getMapping() {
        return Collections.unmodifiableMap(this.subst);
    }

    @Override
    public int hashCode() {
        return this.subst.hashCode();
    }

    @Override
    public boolean equals(final Object other) {
        return other != null && (this == other ||
                (this.getClass() == other.getClass() && this.subst.equals(((Substitution) other).subst)));
    }

    @Override
    public String toString() {
        return "Substitution" + this.subst;
    }
}
