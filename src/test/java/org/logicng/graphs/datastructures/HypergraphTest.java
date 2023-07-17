// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.graphs.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

/**
 * Unit tests for {@link Hypergraph}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class HypergraphTest {

    @Test
    public void testGraphConstruction() {
        final Hypergraph<String> hypergraph = new Hypergraph<>();
        final HypergraphNode<String> node1 = new HypergraphNode<>(hypergraph, "A");
        final HypergraphNode<String> node2 = new HypergraphNode<>(hypergraph, "B");
        final HypergraphEdge<String> edge1 = new HypergraphEdge<>(Arrays.asList(node1, node2));

        hypergraph.addEdge(edge1);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(edge1);
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(node1, node2);

        final HypergraphNode<String> node3 = new HypergraphNode<>(hypergraph, "C");
        final HypergraphEdge<String> edge2 = new HypergraphEdge<>(Arrays.asList(node2, node3));
        final HypergraphEdge<String> edge3 = new HypergraphEdge<>(Arrays.asList(node1, node2, node3));

        hypergraph.addEdges(Arrays.asList(edge2, edge3));
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(edge1, edge2, edge3);
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(node1, node2, node3);

        final HypergraphEdge<String> edge4 = new HypergraphEdge<>(Arrays.asList(node1, node3));

        hypergraph.addEdge(node1, node3);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(edge1, edge2, edge3, edge4);
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(node1, node2, node3);
    }

    @Test
    public void testToString() {
        final Hypergraph<String> hypergraph = new Hypergraph<>();
        final HypergraphNode<String> node1 = new HypergraphNode<>(hypergraph, "A");
        final HypergraphNode<String> node2 = new HypergraphNode<>(hypergraph, "B");
        final HypergraphEdge<String> edge1 = new HypergraphEdge<>(Arrays.asList(node1, node2));
        hypergraph.addEdge(edge1);
        assertThat(hypergraph.toString()).isEqualTo(
                "Hypergraph{nodes=[HypergraphNode{content=A}, HypergraphNode{content=B}], edges=[HypergraphEdge{nodes=[HypergraphNode{content=A}, HypergraphNode{content=B}]}]}");
    }
}
