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

package org.logicng.io.graphical;

import static org.logicng.io.graphical.GraphicalColor.WHITE;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class GraphicalRepresentation {
    private final boolean alignTerminals;
    private final boolean directed;
    private final GraphicalColor background;
    private final List<GraphicalNode> nodes;
    private final List<GraphicalEdge> edges;

    public GraphicalRepresentation(final boolean alignTerminals, final boolean directed) {
        this(alignTerminals, directed, WHITE, new ArrayList<>(), new ArrayList<>());
    }

    public GraphicalRepresentation(final boolean alignTerminals, final boolean directed, final GraphicalColor background) {
        this(alignTerminals, directed, background, new ArrayList<>(), new ArrayList<>());
    }

    public GraphicalRepresentation(final boolean alignTerminals, final boolean directed, final GraphicalColor background, final List<GraphicalNode> nodes,
                                   final List<GraphicalEdge> edges) {
        this.alignTerminals = alignTerminals;
        this.directed = directed;
        this.background = background;
        this.nodes = nodes;
        this.edges = edges;
    }

    public void addNode(final GraphicalNode node) {
        this.nodes.add(node);
    }

    public void addNodes(final Collection<GraphicalNode> newNodes) {
        this.nodes.addAll(newNodes);
    }

    public void addEdge(final GraphicalEdge edge) {
        this.edges.add(edge);
    }

    public void addEdges(final Collection<GraphicalEdge> newEdges) {
        this.edges.addAll(newEdges);
    }

    public void writeDot(final String fileName) throws IOException {
        GraphicalDotWriter.get().write(fileName, this);
    }

    public void writeDot(final File file) throws IOException {
        GraphicalDotWriter.get().write(file, this);
    }

    public void writeMermaid(final String fileName) throws IOException {
        GraphicalMermaidWriter.get().write(fileName, this);
    }

    public void writeMermaid(final File file) throws IOException {
        GraphicalMermaidWriter.get().write(file, this);
    }

    public String getDotString() {
        return GraphicalDotWriter.get().stringValue(this);
    }

    public String getMermaidString() {
        return GraphicalMermaidWriter.get().stringValue(this);
    }

    public boolean isAlignTerminals() {
        return this.alignTerminals;
    }

    public boolean isDirected() {
        return this.directed;
    }

    public GraphicalColor getBackground() {
        return this.background;
    }

    public List<GraphicalNode> getNodes() {
        return this.nodes;
    }

    public List<GraphicalEdge> getEdges() {
        return this.edges;
    }

    @Override
    public String toString() {
        return "GraphicalRepresentation{" +
                "alignTerminals=" + this.alignTerminals +
                ", nodes=" + this.nodes.stream().map(n -> n.id + ":" + n.label).collect(Collectors.joining(", ")) +
                ", edges=" + this.edges.stream().map(e -> e.getSource().label + " -- " + e.getDestination().label).collect(Collectors.joining(", ")) +
                '}';
    }
}
