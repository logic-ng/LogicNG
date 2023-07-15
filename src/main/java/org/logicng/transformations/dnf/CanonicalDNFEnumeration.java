// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.dnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.transformations.CanonicalEnumeration;

/**
 * Canonical DNF generation via enumeration of models by a SAT solver.
 * @version 2.3.0
 * @since 1.0
 */
public final class CanonicalDNFEnumeration extends CanonicalEnumeration implements FormulaTransformation {

    private final static CanonicalDNFEnumeration INSTANCE = new CanonicalDNFEnumeration();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public CanonicalDNFEnumeration() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static CanonicalDNFEnumeration get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, false);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
