// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical.generators;

import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDConstruction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

/**
 * An abstract super class for mappers for a graphical representation of a BDD.
 * <p>
 * Since the {@link BddGraphicalGenerator} uses only the indices of BDD nodes, this class provides
 * some helper methods which simplify accessing the BDD by extracting the content of the BDD nodes
 * as constants and variables.
 * @version 2.4.0
 * @since 2.4.0
 */
public class BddMapper {

    protected final BDDKernel kernel;
    protected final BDDConstruction bddConstruction;

    protected BddMapper(final BDDKernel kernel) {
        this.kernel = kernel;
        this.bddConstruction = new BDDConstruction(kernel);
    }

    /**
     * Returns the variable for the node with the given index
     * @param index the index
     * @return the variable
     */
    protected Variable variable(final int index) {
        return this.kernel.getVariableForIndex(this.bddConstruction.bddVar(index));
    }

    /**
     * Returns true if the index is the terminal node for FALSE.
     * @param index the index
     * @return whether the index is the terminal FALSE node
     */
    protected boolean isFalse(final int index) {
        return index == BDDKernel.BDD_FALSE;
    }

    /**
     * Returns true if the index is the terminal node for TRUE.
     * @param index the index
     * @return whether the index is the terminal TRUE node
     */
    protected boolean isTrue(final int index) {
        return index == BDDKernel.BDD_TRUE;
    }
}
