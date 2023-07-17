// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.graphs.datastructures;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * A simple data structure for a hypergraph.
 * @param <T> the content type of the graph's nodes
 * @version 2.0.0
 * @since 1.4.0
 */
public final class Hypergraph<T> {

    private final LinkedHashSet<HypergraphNode<T>> nodes;
    private final LinkedHashSet<HypergraphEdge<T>> edges;

    /**
     * Constructs a new hypergraph.
     */
    public Hypergraph() {
        this.nodes = new LinkedHashSet<>();
        this.edges = new LinkedHashSet<>();
    }

    /**
     * Returns the set of nodes of the hypergraph.
     * @return the set of nodes of the hypergraph
     */
    public Set<HypergraphNode<T>> nodes() {
        return this.nodes;
    }

    /**
     * Returns the set of edges of the hypergraph.
     * @return the set of edges of the hypergraph
     */
    public Set<HypergraphEdge<T>> edges() {
        return this.edges;
    }

    /**
     * Adds a node to the hypergraph.
     * @param node the node
     */
    void addNode(final HypergraphNode<T> node) {
        this.nodes.add(node);
    }

    /**
     * Adds an edge to the hypergraph.
     * @param edge the edge
     */
    public void addEdge(final HypergraphEdge<T> edge) {
        this.edges.add(edge);
    }

    /**
     * Adds an edges to the hypergraph. The edge is represented by its connected
     * nodes.
     * @param nodes the nodes of the edge
     */
    public void addEdge(final Collection<HypergraphNode<T>> nodes) {
        final HypergraphEdge<T> edge = new HypergraphEdge<>(nodes);
        this.nodes.addAll(nodes);
        this.edges.add(edge);
    }

    /**
     * Adds an edges to the hypergraph. The edge is represented by its connected
     * nodes.
     * @param nodes the nodes of the edge
     */
    @SafeVarargs
    public final void addEdge(final HypergraphNode<T>... nodes) {
        addEdge(Arrays.asList(nodes));
    }

    /**
     * Adds a set of edges to the hypergraph.
     * @param edges the edges
     */
    public void addEdges(final Collection<HypergraphEdge<T>> edges) {
        this.edges.addAll(edges);
    }

    @Override
    public String toString() {
        return "Hypergraph{" +
                "nodes=" + this.nodes +
                ", edges=" + this.edges +
                '}';
    }
}
