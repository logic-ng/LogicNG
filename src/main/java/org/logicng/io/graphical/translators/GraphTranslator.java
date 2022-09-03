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

package org.logicng.io.graphical.translators;

import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdge;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNode;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class GraphTranslator {

    private static final String ID = "id";

    private final GraphicalColor backgroundColor;
    private final GraphicalEdgeStyle edgeStyle;
    private final GraphicalNodeStyle nodeStyle;

    public GraphTranslator(final Builder builder) {
        this.backgroundColor = builder.backgroundColor;
        this.edgeStyle = builder.edgeStyle;
        this.nodeStyle = builder.nodeStyle;
    }

    public <T> GraphicalRepresentation translate(final Graph<T> graph) {
        return translate(graph, (t) -> this.nodeStyle);
    }

    public <T> GraphicalRepresentation translate(final Graph<T> graph, final StyleMapper<T> styleMapper) {
        int counter = 0;
        final Map<Node<T>, GraphicalNode> nodes = new HashMap<>();
        final Set<Node<T>> doneNodes = new HashSet<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(false, false, this.backgroundColor);
        for (final Node<T> node : graph.nodes()) {
            GraphicalNode graphicalNode = nodes.get(node);
            if (graphicalNode == null) {
                graphicalNode = new GraphicalNode(ID + counter++, node.content().toString(), styleMapper.computeStyle(node.content()));
                graphicalRepresentation.addNode(graphicalNode);
                nodes.put(node, graphicalNode);
            }
            for (final Node<T> neighbour : node.neighbours()) {
                if (!doneNodes.contains(neighbour)) {
                    GraphicalNode neighbourNode = nodes.get(neighbour);
                    if (neighbourNode == null) {
                        neighbourNode = new GraphicalNode(ID + counter++, neighbour.content().toString(), styleMapper.computeStyle(node.content()));
                        graphicalRepresentation.addNode(neighbourNode);
                        nodes.put(neighbour, neighbourNode);
                    }
                    graphicalRepresentation.addEdge(new GraphicalEdge(graphicalNode, neighbourNode, this.edgeStyle));
                }
            }
            doneNodes.add(node);
        }

        return graphicalRepresentation;
    }

    /**
     * Returns a new builder for the translator.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * The builder for a graph translator.
     */
    public static class Builder {

        private GraphicalColor backgroundColor = WHITE;
        private GraphicalEdgeStyle edgeStyle = new GraphicalEdgeStyle();
        private GraphicalNodeStyle nodeStyle = new GraphicalNodeStyle();

        public Builder backgroundColor(final GraphicalColor color) {
            this.backgroundColor = color;
            return this;
        }

        public Builder backgroundColor(final String hexColor) {
            this.backgroundColor = GraphicalColor.hex(hexColor);
            return this;
        }

        public Builder backgroundColor(final int red, final int green, final int blue) {
            this.backgroundColor = GraphicalColor.rgb(red, green, blue);
            return this;
        }

        public Builder edgeStyle(final GraphicalEdgeStyle edgeStyle) {
            this.edgeStyle = edgeStyle;
            return this;
        }

        public Builder nodeStyle(final GraphicalNodeStyle nodeStyle) {
            this.nodeStyle = nodeStyle;
            return this;
        }

        public GraphTranslator build() {
            return new GraphTranslator(this);
        }
    }
}
