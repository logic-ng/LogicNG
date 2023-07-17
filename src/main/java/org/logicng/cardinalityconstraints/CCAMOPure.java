// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most one variable is assigned value true. Uses the 'naive'
 * encoding with no introduction of new variables but quadratic size.
 * @version 2.0.0
 * @since 1.0
 */
public final class CCAMOPure implements CCAtMostOne {

    /**
     * Constructs the naive AMO encoder.
     */
    public CCAMOPure() {
        // intentionally left empty
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        for (int i = 0; i < vars.length; i++) {
            for (int j = i + 1; j < vars.length; j++) {
                result.addClause(vars[i].negate(), vars[j].negate());
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
