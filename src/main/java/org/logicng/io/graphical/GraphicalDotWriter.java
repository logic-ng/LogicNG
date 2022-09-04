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

public class GraphicalDotWriter implements GraphicalRepresentationWriter {

    private static final GraphicalDotWriter INSTANCE = new GraphicalDotWriter();

    private GraphicalDotWriter() {
        // Singleton Pattern
    }

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
        writer.write(String.format("  bgcolor=\"%s\"", representation.getBackground().getHexValue()));
        writer.newLine();
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
            final String edgeSymbol = representation.isDirected() ? "->" : "--";
            final String label = edge.getLabel() != null ? String.format(", label=\"%s\"", edge.getLabel()) : "";
            final String edgeStyle = edgeStyleString(edge.getStyle().getLineType());
            final String string = String.format("  %s %s %s [color=\"%s\", fontcolor=\"%s\", style=%s%s]", edge.getSource().id, edgeSymbol,
                    edge.getDestination().id, edge.getStyle().getColor().getHexValue(), edge.getStyle().getColor().getHexValue(), edgeStyle, label);
            writer.write(string);
            writer.newLine();
        }
    }

    private static void writeClosing(final BufferedWriter writer) throws IOException {
        writer.write("}");
        writer.newLine();
    }

    private static String nodeString(final GraphicalNode node) {
        return String.format("  %s [shape=%s, style=filled, color=\"%s\", fontcolor=\"%s\", fillcolor=\"%s\", label=\"%s\"]",
                node.id, shapeString(node.style.getShape()), node.style.getStrokeColor().getHexValue(), node.style.getTextColor().getHexValue(),
                node.style.getBackgroundColor().getHexValue(), node.label);
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

    private static String edgeStyleString(final GraphicalEdgeStyle.LineType lineType) {
        switch (lineType) {
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
