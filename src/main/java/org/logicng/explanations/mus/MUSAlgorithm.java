// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.explanations.mus;

import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.FormulaFactory;
import org.logicng.propositions.Proposition;

import java.util.List;

/**
 * Abstract super class for MUS computation algorithms.
 * @version 2.1.0
 * @since 1.1
 */
abstract class MUSAlgorithm {

    /**
     * Computes a MUS for the given propositions.
     * @param propositions the propositions
     * @param f            the formula factory
     * @param config       the MUS configuration
     * @param <T>          the type of the MUSes propositions
     * @return the MUS or null if the MUS computation was configured with a
     *         handler and this handler aborted the computation
     */
    public abstract <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f,
                                                                    final MUSConfig config);
}
