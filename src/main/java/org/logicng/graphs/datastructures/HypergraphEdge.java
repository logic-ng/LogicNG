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

package org.logicng.graphs.datastructures;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * An edge in a hypergraph.
 * @param <T> the content type of the graph's nodes
 * @version 2.0.0
 * @since 1.4.0
 */
public final class HypergraphEdge<T> {

    private final LinkedHashSet<HypergraphNode<T>> nodes;

    /**
     * Constructs a new edge for a given set of nodes.
     * @param nodes the nodes connected by this edge
     */
    public HypergraphEdge(final Collection<HypergraphNode<T>> nodes) {
        this.nodes = new LinkedHashSet<>(nodes);
        for (final HypergraphNode<T> node : nodes) {
            node.addEdge(this);
        }
    }

    /**
     * Constructs a new edge for a given set of nodes.
     * @param nodes the nodes connected by this edge
     */
    @SafeVarargs
    public HypergraphEdge(final HypergraphNode<T>... nodes) {
        this(Arrays.asList(nodes));
    }

    /**
     * Returns the nodes connected by this edge.
     * @return the nodes connected by this edge
     */
    public Set<HypergraphNode<T>> nodes() {
        return this.nodes;
    }

    /**
     * Computes the center of gravity for this edge (see Aloul, Markov, and Sakallah).
     * @param nodeOrdering the node ordering for which the COG is computed
     * @return the center of gravity for this edge
     */
    public double centerOfGravity(final Map<HypergraphNode<T>, Integer> nodeOrdering) {
        int cog = 0;
        for (final HypergraphNode<T> node : this.nodes) {
            final Integer level = nodeOrdering.get(node);
            if (level == null) {
                throw new IllegalStateException("Could not find node " + node + " in the node ordering.");
            }
            cog += level;
        }
        return (double) cog / this.nodes.size();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final HypergraphEdge<?> that = (HypergraphEdge<?>) o;
        return Objects.equals(this.nodes, that.nodes);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.nodes);
    }

    @Override
    public String toString() {
        return "HypergraphEdge{" +
                "nodes=" + this.nodes +
                '}';
    }
}
