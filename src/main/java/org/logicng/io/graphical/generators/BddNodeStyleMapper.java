///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.io.graphical.generators;

import org.logicng.formulas.Variable;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDConstruction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

/**
 * An abstract super class for a style mapper for a graphical representation of a BDD.
 * <p>
 * Since the {@link BddGraphicalGenerator} uses only the indices of BDD nodes, this class provides
 * some helper methods which simplify styling the BDD by extracting the content of the BDD nodes
 * as constants and variables.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class BddNodeStyleMapper implements NodeStyleMapper<Integer> {

    private final BDDKernel kernel;
    private final BDDConstruction bddConstruction;

    /**
     * Constructs a new BDD style mapper for a given BDD kernel.  The BDDs which are styled
     * must be constructed with this kernel.
     * @param kernel a BDD kernel
     */
    public BddNodeStyleMapper(final BDDKernel kernel) {
        this.kernel = kernel;
        this.bddConstruction = new BDDConstruction(kernel);
    }

    @Override
    public abstract GraphicalNodeStyle computeStyle(final Integer index);

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
