// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
