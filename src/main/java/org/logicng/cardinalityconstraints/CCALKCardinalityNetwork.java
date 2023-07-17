// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that at least 'rhs' variables are assigned value true. Uses the
 * cardinality network encoding due to Asín, Nieuwenhuis, Oliveras, and
 * Rodríguez-Carbonell .
 * @version 2.0.0
 * @since 1.1
 */
public final class CCALKCardinalityNetwork implements CCAtLeastK {

    private final CCCardinalityNetworks cardinalityNetwork;

    /**
     * Constructs a new cardinality encoder.
     */
    CCALKCardinalityNetwork() {
        this.cardinalityNetwork = new CCCardinalityNetworks();
    }

    @Override
    public void build(final EncodingResult result, final Variable[] vars, final int rhs) {
        this.cardinalityNetwork.buildALK(result, vars, rhs);
    }

    @Override
    public CCIncrementalData incrementalData() {
        return this.cardinalityNetwork.incrementalData();
    }

    /**
     * Builds the constraint for incremental usage.
     * @param result the result
     * @param vars   the variables
     * @param rhs    the right-hand side
     */
    void buildForIncremental(final EncodingResult result, final Variable[] vars, final int rhs) {
        this.cardinalityNetwork.buildALKForIncremental(result, vars, rhs);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
