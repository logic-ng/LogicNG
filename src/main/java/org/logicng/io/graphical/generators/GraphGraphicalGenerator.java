// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical.generators;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.io.graphical.GraphicalEdge;
import org.logicng.io.graphical.GraphicalNode;
import org.logicng.io.graphical.GraphicalRepresentation;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The graphical generator for representations of graphs {@link Graph}.
 * @param <T> the type of the graph's node content
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphGraphicalGenerator<T> extends GraphicalGenerator<T> {

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    GraphGraphicalGenerator(final GraphicalGeneratorBuilder<GraphGraphicalGenerator<T>, T> builder) {
        super(builder.backgroundColor, builder.alignTerminals, builder.defaultEdgeStyle, builder.defaultNodeStyle,
                builder.nodeStyleMapper,
                builder.labelMapper, builder.edgeMapper);
    }

    /**
     * Returns the builder for this generator.
     * @param <T> the type of the graph's node content
     * @return the builder
     */
    public static <T> GraphicalGeneratorBuilder<GraphGraphicalGenerator<T>, T> builder() {
        return new GraphicalGeneratorBuilder<>(GraphGraphicalGenerator::new);
    }

    /**
     * Translates a given graph into its graphical representation.
     * @param graph the graph to translate
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final Graph<T> graph) {
        final Map<Node<T>, GraphicalNode> nodes = new HashMap<>();
        final Set<Node<T>> visited = new HashSet<>();
        final GraphicalRepresentation graphicalRepresentation =
                new GraphicalRepresentation(false, false, this.backgroundColor);
        for (final Node<T> node : graph.nodes()) {
            final GraphicalNode graphicalNode = addNode(node, graphicalRepresentation, nodes);
            for (final Node<T> neighbour : node.neighbours()) {
                final GraphicalNode graphicalNeighbourNode = addNode(neighbour, graphicalRepresentation, nodes);
                if (!visited.contains(neighbour)) {
                    graphicalRepresentation.addEdge(new GraphicalEdge(graphicalNode, graphicalNeighbourNode,
                            edgeStyle(node.content(), neighbour.content())));
                }
            }
            visited.add(node);
        }
        return graphicalRepresentation;
    }

    private GraphicalNode addNode(final Node<T> node, final GraphicalRepresentation graphicalRepresentation,
                                  final Map<Node<T>, GraphicalNode> nodes) {
        GraphicalNode graphicalNode = nodes.get(node);
        if (graphicalNode == null) {
            graphicalNode = new GraphicalNode(ID + nodes.size(),
                    labelOrDefault(node.content(), node.content().toString()), nodeStyle(node.content()));
            graphicalRepresentation.addNode(graphicalNode);
            nodes.put(node, graphicalNode);
        }
        return graphicalNode;
    }
}
