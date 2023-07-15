// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.knowledgecompilation.bdds.BDD;

/**
 * A function on a BDD.
 * @param <T> the result type of the function
 * @version 2.0.0
 * @since 2.0.0
 */
@FunctionalInterface
public interface BDDFunction<T> {

    /**
     * Applies this function on a given BDD.
     * @param bdd the BDD
     * @return the result of the application
     */
    T apply(BDD bdd);
}
