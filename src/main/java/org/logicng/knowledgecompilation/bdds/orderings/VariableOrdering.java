// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.orderings;

import java.util.function.Supplier;

/**
 * An enumeration for the different BDD variable orderings.  A variable ordering is associated
 * with its own provider which can generate orderings for this ordering.
 * @version 1.4.0
 * @since 1.4.0
 */
public enum VariableOrdering {

    DFS(DFSOrdering::new),
    BFS(BFSOrdering::new),
    MIN2MAX(MinToMaxOrdering::new),
    MAX2MIN(MaxToMinOrdering::new),
    FORCE(ForceOrdering::new);

    private final Supplier<? extends VariableOrderingProvider> provider;

    /**
     * Constructs a new variable ordering with a given provider.
     * @param provider the provider
     */
    VariableOrdering(final Supplier<? extends VariableOrderingProvider> provider) {
        this.provider = provider;
    }

    /**
     * Returns the provider for this variable ordering.
     * @return the provider for this variable ordering
     */
    public VariableOrderingProvider provider() {
        return this.provider.get();
    }
}
