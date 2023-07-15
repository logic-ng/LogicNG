// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most one variable is assigned value true.  Uses the 2-product method due to Chen.
 * @version 2.0.0
 * @since 1.0
 */
public final class CCAMOProduct implements CCAtMostOne {
    private final int recursiveBound;
    private EncodingResult result;

    /**
     * Constructs the naive AMO encoder.
     */
    CCAMOProduct(final int recursiveBound) {
        this.recursiveBound = recursiveBound;
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        this.result = result;
        this.productRec(vars);
    }

    private void productRec(final Variable... vars) {
        final int n = vars.length;
        final int p = (int) Math.ceil(Math.sqrt(n));
        final int q = (int) Math.ceil((double) n / (double) p);
        final Variable[] us = new Variable[p];
        for (int i = 0; i < us.length; i++) {
            us[i] = this.result.newVariable();
        }
        final Variable[] vs = new Variable[q];
        for (int i = 0; i < vs.length; i++) {
            vs[i] = this.result.newVariable();
        }
        if (us.length <= this.recursiveBound) {
            buildPure(us);
        } else {
            this.productRec(us);
        }
        if (vs.length <= this.recursiveBound) {
            buildPure(vs);
        } else {
            this.productRec(vs);
        }
        for (int i = 0; i < p; i++) {
            for (int j = 0; j < q; j++) {
                final int k = i * q + j;
                if (k >= 0 && k < n) {
                    this.result.addClause(vars[k].negate(), us[i]);
                    this.result.addClause(vars[k].negate(), vs[j]);
                }
            }
        }
    }

    private void buildPure(final Variable... vars) {
        for (int i = 0; i < vars.length; i++) {
            for (int j = i + 1; j < vars.length; j++) {
                this.result.addClause(vars[i].negate(), vars[j].negate());
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
