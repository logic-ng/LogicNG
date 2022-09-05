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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

/**
 * A writer which writes a graphical representation as a DOT file.  This writer is thread-safe.
 * <p>
 * The DOT file specification can be found <a href="https://graphviz.org/doc/info/lang.html">here</a>.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalDotWriter implements GraphicalRepresentationWriter {

    private static final GraphicalDotWriter INSTANCE = new GraphicalDotWriter();

    private GraphicalDotWriter() {
        // Singleton Pattern
    }

    /**
     * Returns the singleton instance of this writer.
     * @return the singleton instance
     */
    public static GraphicalDotWriter get() {
        return INSTANCE;
    }

    @Override
    public void write(final Writer writer, final GraphicalRepresentation representation) throws IOException {
        final BufferedWriter bufferedWriter = new BufferedWriter(writer);
        writePreamble(bufferedWriter, representation);
        writeNodes(bufferedWriter, representation);
        writeEdges(bufferedWriter, representation);
        writeClosing(bufferedWriter);
        bufferedWriter.close();
    }

    private static void writePreamble(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        writer.write(String.format("%s {", representation.isDirected() ? "digraph G" : "strict graph"));
        writer.newLine();
        if (representation.getBackground() != null) {
            writer.write(String.format("  bgcolor=\"%s\"", representation.getBackground().getHexValue()));
            writer.newLine();
        }
        writer.newLine();
    }

    private static void writeNodes(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        if (representation.isAlignTerminals()) {
            writer.write("{ rank = same;");
            writer.newLine();
            for (final GraphicalNode terminalNode : representation.getTerminalNodes()) {
                writer.write(nodeString(terminalNode));
                writer.newLine();
            }
            writer.write("}");
            writer.newLine();
            for (final GraphicalNode nonTerminalNode : representation.getNonTerminalNodes()) {
                writer.write(nodeString(nonTerminalNode));
                writer.newLine();
            }
        } else {
            for (final GraphicalNode node : representation.getNodes()) {
                writer.write(nodeString(node));
                writer.newLine();
            }
        }
        writer.newLine();
    }

    private static void writeEdges(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        for (final GraphicalEdge edge : representation.getEdges()) {
            writer.write(edgeString(edge, representation.isDirected()));
            writer.newLine();
        }
    }

    private static void writeClosing(final BufferedWriter writer) throws IOException {
        writer.write("}");
        writer.newLine();
    }

    private static String nodeString(final GraphicalNode node) {
        final List<String> attributes = new ArrayList<>();
        if (node.style.getShape() != null) {
            attributes.add(String.format("shape=%s", shapeString(node.style.getShape())));
        }
        if (node.style.getStrokeColor() != null) {
            attributes.add(String.format("color=\"%s\"", node.style.getStrokeColor().getHexValue()));
        }
        if (node.style.getTextColor() != null) {
            attributes.add(String.format("fontcolor=\"%s\"", node.style.getTextColor().getHexValue()));
        }
        if (node.style.getBackgroundColor() != null) {
            attributes.add(String.format("style=filled, fillcolor=\"%s\"", node.style.getBackgroundColor().getHexValue()));
        }
        return String.format("  %s [label=\"%s\"%s]", node.id, node.label, attributes.isEmpty() ? "" : ", " + String.join(", ", attributes));
    }

    private static String edgeString(final GraphicalEdge edge, final boolean isDirected) {
        final List<String> attributes = new ArrayList<>();
        if (edge.style.getColor() != null) {
            attributes.add(String.format("color=\"%1$s\", fontcolor=\"%1$s\"", edge.style.getColor().getHexValue()));
        }
        if (edge.style.getType() != null) {
            attributes.add(String.format("style=%s", edgeStyleString(edge.style.getType())));
        }
        if (edge.label != null) {
            attributes.add(String.format("label=\"%s\"", edge.label));
        }
        final String attributeString = attributes.isEmpty() ? "" : " [" + String.join(", ", attributes) + "]";
        final String edgeSymbol = isDirected ? "->" : "--";
        return String.format("  %s %s %s%s", edge.source.id, edgeSymbol, edge.destination.id, attributeString);
    }

    private static String shapeString(final GraphicalNodeStyle.Shape shape) {
        switch (shape) {
            case RECTANGLE:
                return "box";
            case CIRCLE:
                return "circle";
            case ELLIPSE:
            default:
                return "ellipse";
        }
    }

    private static String edgeStyleString(final GraphicalEdgeStyle.EdgeType edgeType) {
        switch (edgeType) {
            case DOTTED:
                return "dotted";
            case BOLD:
                return "bold";
            case SOLID:
            default:
                return "solid";
        }
    }
}
