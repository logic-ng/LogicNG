// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.cnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.transformations.CanonicalEnumeration;

/**
 * Canonical CNF generation via enumeration of falsifying assignments by a SAT
 * solver.
 * @version 2.3.0
 * @since 2.3.0
 */
public final class CanonicalCNFEnumeration extends CanonicalEnumeration implements FormulaTransformation {

    private final static CanonicalCNFEnumeration INSTANCE = new CanonicalCNFEnumeration();

    /**
     * Private empty constructor. Singleton class.
     */
    private CanonicalCNFEnumeration() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the transformation.
     * @return the transformation instance
     */
    public static CanonicalCNFEnumeration get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, true);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
