// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import static org.logicng.formulas.cache.TransformationCacheEntry.ANONYMIZATION;

import org.logicng.datastructures.Substitution;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Variable;

/**
 * An anonymizer replaces all variables in a formula with new variables generated from a given prefix and a counter.
 * <p>
 * An instance can be used to anonymize multiple formulas. In this case, variables with the same name will be replaced
 * with the same anonymized variable.
 * <p>
 * After anonymizing one or more formulas, the mapping from original variable to anonymized variable can be accessed
 * via {@link #getSubstitution()}.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class Anonymizer implements FormulaTransformation {

    private final Substitution substitution;
    private final String prefix;
    private int counter;

    /**
     * Constructs a new anonymizer with a given prefix for the newly introduced variables.
     * @param prefix       the prefix for the new variables
     * @param startCounter where should the counter start
     */
    public Anonymizer(final String prefix, final int startCounter) {
        this.prefix = prefix;
        this.substitution = new Substitution();
        this.counter = startCounter;
    }

    /**
     * Constructs a new anonymizer with a given prefix for the newly introduced variables.
     * @param prefix the prefix for the new variables
     */
    public Anonymizer(final String prefix) {
        this(prefix, 0);
    }

    /**
     * Constructs a new anonymizer with the standard variable prefix 'v'.
     */
    public Anonymizer() {
        this("v");
    }

    /**
     * Returns the substitution which was used to anonymize the formula(s).
     * <p>
     * Although a substitution maps from variables to formulas, it is guaranteed that
     * the substitution always maps to variables. So the following cast will always be
     * safe:
     * <p>
     * {@code (Variable) getSubstitution().getSubstitution(x)}
     * @return the substitution which was used to anonymize the formula(s)
     */
    public Substitution getSubstitution() {
        return new Substitution(this.substitution);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (formula.variables().isEmpty()) {
            return formula;
        }
        final Formula cached = formula.transformationCacheEntry(ANONYMIZATION);
        if (cache && cached != null) {
            return cached;
        }
        for (final Variable variable : formula.variables()) {
            if (this.substitution.getSubstitution(variable) == null) {
                this.substitution.addMapping(variable, formula.factory().variable(this.prefix + this.counter++));
            }
        }
        final Formula transformed = formula.substitute(this.substitution);
        if (cache) {
            formula.setTransformationCacheEntry(ANONYMIZATION, transformed);
        }
        return transformed;
    }
}
