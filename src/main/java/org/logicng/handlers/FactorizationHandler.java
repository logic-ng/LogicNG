// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import org.logicng.formulas.Formula;

/**
 * A handler for factorization methods (CNF, DNF).
 * @version 2.1.0
 * @since 1.0
 */
public interface FactorizationHandler extends Handler {

    /**
     * This method is called every time a distribution is performed.
     * @return {@code true} if the factorization should be continued, otherwise {@code false}
     */
    default boolean performedDistribution() {
        return true;
    }

    /**
     * This method is called every time a new clause is created.
     * @param clause the clause
     * @return {@code true} if the factorization should be continued, otherwise {@code false}
     */
    default boolean createdClause(final Formula clause) {
        return true;
    }
}
