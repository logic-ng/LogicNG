// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * The interface for at-most-one (AMO) cardinality constraints.
 * @version 2.0.0
 * @since 1.0
 */
public interface CCAtMostOne {

    /**
     * Builds a cardinality constraint of the form
     * {@code var_1 + var_2 + ... + var_n <= 1}.
     * @param result the result for the encoding
     * @param vars   the variables {@code var_1 ... var_n}
     */
    void build(final EncodingResult result, final Variable... vars);
}
