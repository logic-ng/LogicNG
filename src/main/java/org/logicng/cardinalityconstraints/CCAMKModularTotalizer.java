// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most 'rhs' variables can be assigned value true. Uses the
 * modular totalizer encoding for translating the cardinality constraint into
 * CNF.
 * @version 2.0.0
 * @since 1.0
 */
public final class CCAMKModularTotalizer implements CCAtMostK {

    private final CCModularTotalizer totalizer;

    /**
     * Constructs a new modular totalizer.
     * @param f the formula factory
     */
    CCAMKModularTotalizer(final FormulaFactory f) {
        this.totalizer = new CCModularTotalizer(f);
    }

    @Override
    public void build(final EncodingResult result, final Variable[] vars, final int rhs) {
        this.totalizer.buildAMK(result, vars, rhs);
    }

    @Override
    public CCIncrementalData incrementalData() {
        return this.totalizer.incrementalData();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
