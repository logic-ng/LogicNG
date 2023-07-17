// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.explanations.mus;

import org.logicng.configurations.ConfigurationType;
import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.FormulaFactory;
import org.logicng.propositions.Proposition;

import java.util.List;

/**
 * Computes a minimal unsatisfiable subset (MUS) of a given formula with
 * different algorithms.
 * @version 2.0.0
 * @since 1.1
 */
public final class MUSGeneration {

    private final DeletionBasedMUS deletion;
    private final PlainInsertionBasedMUS insertion;

    /**
     * Constructs a new MUS generator.
     */
    public MUSGeneration() {
        this.deletion = new DeletionBasedMUS();
        this.insertion = new PlainInsertionBasedMUS();
    }

    /**
     * Computes a MUS for the given propositions with the default algorithm and
     * the MUS configuration from the formula factory.
     * @param propositions the propositions
     * @param f            the formula factory
     * @param <T>          the type of the MUSes propositions
     * @return the MUS
     */
    public <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f) {
        return this.computeMUS(propositions, f, (MUSConfig) f.configurationFor(ConfigurationType.MUS));
    }

    /**
     * Computes a MUS for the given propositions and the given configuration of
     * the MUS generation.
     * @param propositions the propositions
     * @param f            the formula factory
     * @param config       the MUS configuration
     * @param <T>          the type of the MUSes propositions
     * @return the MUS
     */
    public <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f,
                                                           final MUSConfig config) {
        if (propositions.isEmpty()) {
            throw new IllegalArgumentException("Cannot generate a MUS for an empty list of propositions");
        }
        switch (config.algorithm) {
            case PLAIN_INSERTION:
                return this.insertion.computeMUS(propositions, f, config);
            case DELETION:
                return this.deletion.computeMUS(propositions, f, config);
            default:
                throw new IllegalStateException("Unknown MUS algorithm: " + config.algorithm);
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
