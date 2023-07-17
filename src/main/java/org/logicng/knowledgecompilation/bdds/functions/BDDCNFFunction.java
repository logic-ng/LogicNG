// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.formulas.Formula;
import org.logicng.knowledgecompilation.bdds.BDD;

/**
 * Creates a CNF from a BDD.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class BDDCNFFunction extends BDDNormalFormFunction implements BDDFunction<Formula> {

    private final static BDDCNFFunction INSTANCE = new BDDCNFFunction();

    /**
     * @deprecated In the next version, the standard constructor will be
     *             replaced by a private constructor. In order to instantiate an
     *             object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public BDDCNFFunction() {
        // intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static BDDCNFFunction get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final BDD bdd) {
        return compute(bdd, true);
    }
}
