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

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.DnnfSatSolver;

import java.util.BitSet;
import java.util.List;
import java.util.SortedSet;

/**
 * Super class for a Decomposition Tree (DTree) for the DNNF Compiler
 * This is either a {@link DTreeNode} or a {@link DTreeLeaf}.
 * @version 2.0.0
 * @since 2.0.0
 */
public abstract class DTree {

    protected int[] staticVariables;
    protected BitSet staticVarSet;
    protected int[] staticSeparator;

    /**
     * Initializes the DTree.
     * @param solver a specializes DNNF SAT solver
     */
    public abstract void initialize(final DnnfSatSolver solver);

    /**
     * Returns the size of the DTree.
     * @return the size of the DTree
     */
    public abstract int size();

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    int[] staticVarSetArray() {
        return this.staticVariables;
    }

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    public BitSet staticVarSet() {
        return this.staticVarSet;
    }

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    abstract SortedSet<Variable> staticVariableSet();

    /**
     * The dynamic separator of this DTree.  "Dynamic" means that subsumed clauses are ignored during the separator computation.
     * @return The dynamic separator of this DTree
     */
    public abstract BitSet dynamicSeparator();

    /**
     * The ids clauses in this DTree.
     * @return The clause ids
     */
    abstract int[] staticClauseIds();

    /**
     * Counts the number of unsubsumed occurrences for each variable in occurrences.
     * <p>
     * The parameter occurrences should be modified by the method accordingly.
     * @param occurrences The current number of occurrences for each variable which should be modified accordingly
     */
    public abstract void countUnsubsumedOccurrences(final int[] occurrences);

    /**
     * Returns the depth of this tree.
     * @return the depth of this tree
     */
    public abstract int depth();

    /**
     * Returns the widest separator of this tree.
     * @return the widest separator
     */
    public abstract int widestSeparator();

    /**
     * Returns all leafs of this tree.
     * @return all leafs of this tree
     */
    abstract List<DTreeLeaf> leafs();
}
