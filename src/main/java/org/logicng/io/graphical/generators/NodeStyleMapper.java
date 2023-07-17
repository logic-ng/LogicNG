// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical.generators;

import org.logicng.io.graphical.GraphicalNodeStyle;

/**
 * A style mapper for generating graphical representations of formulas, BDDs and
 * graphs. This mapper can be used to compute a node style for the given node
 * content.
 * <p>
 * This can be used to style nodes of a graphical representation dynamically
 * depending on the content of the node.
 * @param <T> the type of the node content
 * @version 2.4.0
 * @since 2.4.0
 */
@FunctionalInterface
public interface NodeStyleMapper<T> {

    /**
     * Computes a style for the given node content.
     * @param content the content of the node
     * @return the style for the node with this content
     */
    GraphicalNodeStyle computeStyle(T content);
}
