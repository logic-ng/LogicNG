// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that exactly 'rhs' variables are assigned value true. Uses the
 * cardinality network encoding due to Asín, Nieuwenhuis, Oliveras, and
 * Rodríguez-Carbonell .
 * @version 1.1
 * @since 1.1
 */
public final class CCEXKCardinalityNetwork implements CCAtMostK {

    private final CCCardinalityNetworks cardinalityNetwork;

    /**
     * Constructs a new cardinality encoder.
     */
    CCEXKCardinalityNetwork() {
        this.cardinalityNetwork = new CCCardinalityNetworks();
    }

    @Override
    public void build(final EncodingResult result, final Variable[] vars, final int rhs) {
        this.cardinalityNetwork.buildEXK(result, vars, rhs);
    }

    @Override
    public CCIncrementalData incrementalData() {
        return this.cardinalityNetwork.incrementalData();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
