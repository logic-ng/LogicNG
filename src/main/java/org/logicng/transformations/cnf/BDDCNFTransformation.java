// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.cnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.transformations.BDDNormalFormTransformation;

/**
 * Transformation of a formula in CNF by converting it to a BDD.
 * @version 2.3.0
 * @since 1.4.0
 */
public final class BDDCNFTransformation extends BDDNormalFormTransformation {

    /**
     * Constructs a new BDD-based CNF transformation with an optional BDD
     * kernel.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations,
     * <b>but</b> the number of different variables in all applied formulas
     * <b>must not exceed</b> the number of variables in the kernel.
     * @param kernel the optional BDD kernel
     */
    public BDDCNFTransformation(final BDDKernel kernel) {
        super(kernel);
    }

    /**
     * Constructs a new BDD-based CNF transformation for a given number of
     * variables.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations,
     * <b>but</b> the number of different variables in all applied formulas
     * <b>must not exceed</b> {@code numVars}.
     * <p>
     * To improve performance you might want to use
     * {@link #BDDCNFTransformation(BDDKernel)}, where you have full control
     * over the node and cache size in the used BDD kernel.
     * @param f       the formula factory to use
     * @param numVars the number of variables
     */
    public BDDCNFTransformation(final FormulaFactory f, final int numVars) {
        super(f, numVars);
    }

    /**
     * Constructs a new BDD-based CNF transformation and constructs a new BDD
     * kernel for every formula application.
     */
    public BDDCNFTransformation() {
        this(null);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, true, cache);
    }
}
