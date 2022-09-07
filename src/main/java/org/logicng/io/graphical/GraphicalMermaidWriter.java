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
 * A writer which writes a graphical representation as a Mermaid.js file.  This writer is thread-safe.
 * <p>
 * More information on Mermaid.js including a live code editor can be found
 * <a href="https://mermaid-js.github.io/mermaid/#/">here</a>.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalMermaidWriter implements GraphicalRepresentationWriter {

    private static final GraphicalMermaidWriter INSTANCE = new GraphicalMermaidWriter();

    private static final GraphicalNodeStyle.Shape DEFAULT_NODE_SHAPE = GraphicalNodeStyle.Shape.ELLIPSE;
    private static final int DEFAULT_LINE_WIDTH = 2;
    private static final int DEFAULT_LINE_WIDTH_BOLD = 4;

    private GraphicalMermaidWriter() {
        // Singleton Pattern
    }

    /**
     * Returns the singleton instance of this writer.
     * @return the singleton instance
     */
    public static GraphicalMermaidWriter get() {
        return INSTANCE;
    }

    @Override
    public void write(final Writer writer, final GraphicalRepresentation representation) throws IOException {
        final BufferedWriter bufferedWriter = new BufferedWriter(writer);
        writePreamble(bufferedWriter);
        writeNodes(bufferedWriter, representation);
        writeEdges(bufferedWriter, representation);
        bufferedWriter.close();
    }

    private static void writePreamble(final BufferedWriter writer) throws IOException {
        writer.write("graph TD");
        writer.newLine();
    }

    private static void writeNodes(final BufferedWriter bufferedWriter, final GraphicalRepresentation representation) throws IOException {
        for (final GraphicalNode node : representation.getNodes()) {
            bufferedWriter.write(String.format("  %s", nodeString(node)));
            bufferedWriter.newLine();
            final String nodeStyleString = nodeStyleString(node.getId(), node.getStyle());
            if (nodeStyleString != null) {
                bufferedWriter.write(nodeStyleString);
                bufferedWriter.newLine();
            }
        }
        bufferedWriter.newLine();
    }

    private static void writeEdges(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        int counter = 0;
        for (final GraphicalEdge edge : representation.getEdges()) {
            final String edgeSymbol = edgeSymbolString(edge, representation.isDirected());
            writer.write(String.format("  %s %s %s", edge.getSource().getId(), edgeSymbol, edge.getDestination().getId()));
            writer.newLine();
            final String edgeStyleString = edgeStyleString(counter++, edge.getStyle());
            if (edgeStyleString != null) {
                writer.write(edgeStyleString);
                writer.newLine();
            }
        }
    }

    private static String nodeString(final GraphicalNode node) {
        final String start;
        final String end;
        switch (node.getStyle().getShape() != null ? node.getStyle().getShape() : DEFAULT_NODE_SHAPE) {
            case RECTANGLE:
                start = "[";
                end = "]";
                break;
            case CIRCLE:
                start = "((";
                end = "))";
                break;
            case ELLIPSE:
            default:
                start = "([";
                end = "])";
                break;
        }
        return String.format("%s%s\"%s\"%s", node.getId(), start, node.getLabel(), end);
    }

    private static String edgeSymbolString(final GraphicalEdge edge, final boolean directed) {
        final String edgeConnector = directed ? "-->" : "---";
        return edge.getLabel() == null ? edgeConnector : String.format("%s|\"%s\"|", edgeConnector, edge.getLabel());
    }

    private static String nodeStyleString(final String id, final GraphicalNodeStyle style) {
        if (!style.hasStyle() || (style.getStrokeColor() == null && style.getTextColor() == null && style.getBackgroundColor() == null)) {
            return null;
        }
        final List<String> attributes = new ArrayList<>();
        if (style.getStrokeColor() != null) {
            attributes.add(String.format("stroke:%s", style.getStrokeColor().getHexValue()));
        }
        if (style.getTextColor() != null) {
            attributes.add(String.format("color:%s", style.getTextColor().getHexValue()));
        }
        if (style.getBackgroundColor() != null) {
            attributes.add(String.format("fill:%s", style.getBackgroundColor().getHexValue()));
        }
        return String.format("    style %s %s", id, String.join(",", attributes));
    }

    private static String edgeStyleString(final int counter, final GraphicalEdgeStyle style) {
        if (!style.hasStyle()) {
            return null;
        }
        final List<String> attributes = new ArrayList<>();
        if (style.getColor() != null) {
            attributes.add(String.format("stroke:%s", style.getColor().getHexValue()));
        }
        if (style.getType() != null) {
            switch (style.getType()) {
                case SOLID:
                    attributes.add(String.format("stroke-width:%d", DEFAULT_LINE_WIDTH));
                    break;
                case BOLD:
                    attributes.add(String.format("stroke-width:%d", DEFAULT_LINE_WIDTH_BOLD));
                    break;
                case DOTTED:
                    attributes.add(String.format("stroke-width:%d,stroke-dasharray:3", DEFAULT_LINE_WIDTH));
                    break;
            }
        }
        return String.format("    linkStyle %d %s", counter, String.join(",", attributes));
    }
}
