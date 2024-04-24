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

package org.logicng.knowledgecompilation.bdds.datastructures;

import org.logicng.formulas.Formula;

import java.util.Set;

/**
 * A node in a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public interface BDDNode {

    /**
     * Returns the label of the node.  This can either be a variable or a constant.
     * @return the label of the node
     */
    Formula label();

    /**
     * Returns {@code true} if this node is an inner node, {@code false} if it is a terminal node.
     * @return {@code true} if this node is an inner node, {@code false} if it is a terminal node
     */
    boolean isInnerNode();

    /**
     * Returns the node of the low edge or {@code null} for a terminal node.
     * @return the node of the low edge
     */
    BDDNode low();

    /**
     * Returns the node of the high edge or {@code null} for a terminal node.
     * @return the node of the high edge
     */
    BDDNode high();

    /**
     * Returns all nodes of the sub-BDD starting at this node.
     * @return all nodes of the sub-BDD starting at this node
     */
    Set<BDDNode> nodes();
}
