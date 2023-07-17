// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * The interface for at-most-k (AMK) cardinality constraints.
 * @version 2.0.0
 * @since 1.0
 */
public interface CCAtMostK {

    /**
     * Builds a cardinality constraint of the form
     * {@code var_1 + var_2 + ... + var_n <= k}.
     * @param result the result for the encoding
     * @param vars   the variables {@code var_1 ... var_n}
     * @param rhs    the right-hand side {@code k} of the constraint
     * @throws IllegalArgumentException if the right-hand side of the
     *                                  cardinality constraint is negative
     */
    void build(final EncodingResult result, final Variable[] vars, int rhs);

    /**
     * Returns the incremental data for the current encoded constraint.
     * @return the incremental data for the current encoded constraint
     */
    CCIncrementalData incrementalData();
}
