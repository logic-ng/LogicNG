// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.formulas.Formula;
import org.logicng.knowledgecompilation.bdds.BDD;

/**
 * Creates a DNF from a BDD.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class BDDDNFFunction extends BDDNormalFormFunction implements BDDFunction<Formula> {

    private final static BDDDNFFunction INSTANCE = new BDDDNFFunction();

    /**
     * Private empty constructor. Singleton class.
     */
    private BDDDNFFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the transformation.
     * @return the transformation instance
     */
    public static BDDDNFFunction get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final BDD bdd) {
        return compute(bdd, false);
    }
}
