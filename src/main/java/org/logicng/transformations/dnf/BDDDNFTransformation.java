// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.dnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.transformations.BDDNormalFormTransformation;

/**
 * Transformation of a formula in DNF by converting it to a BDD.
 * @version 2.3.0
 * @since 2.3.0
 */
public final class BDDDNFTransformation extends BDDNormalFormTransformation {

    /**
     * Constructs a new BDD-based DNF transformation with an optional BDD kernel.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * the number of variables in the kernel.
     * @param kernel the optional BDD kernel
     */
    public BDDDNFTransformation(final BDDKernel kernel) {
        super(kernel);
    }

    /**
     * Constructs a new BDD-based DNF transformation for a given number of variables.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * {@code numVars}.
     * <p>
     * To improve performance you might want to use {@link #BDDDNFTransformation(BDDKernel)},
     * where you have full control over the node and cache size in the used BDD kernel.
     * @param f       the formula factory to use
     * @param numVars the number of variables
     */
    public BDDDNFTransformation(final FormulaFactory f, final int numVars) {
        super(f, numVars);
    }

    /**
     * Constructs a new BDD-based DNF transformation and constructs a new BDD kernel
     * for every formula application.
     */
    public BDDDNFTransformation() {
        this(null);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, false, cache);
    }
}
