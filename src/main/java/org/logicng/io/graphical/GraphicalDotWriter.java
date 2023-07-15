// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
        final GraphicalNodeStyle style = node.getStyle();
        final List<String> attributes = new ArrayList<>();
        if (style.getShape() != null) {
            attributes.add(String.format("shape=%s", shapeString(style.getShape())));
        }
        if (style.getStrokeColor() != null) {
            attributes.add(String.format("color=\"%s\"", style.getStrokeColor().getHexValue()));
        }
        if (style.getTextColor() != null) {
            attributes.add(String.format("fontcolor=\"%s\"", style.getTextColor().getHexValue()));
        }
        if (style.getBackgroundColor() != null) {
            attributes.add(String.format("style=filled, fillcolor=\"%s\"", style.getBackgroundColor().getHexValue()));
        }
        return String.format("  %s [label=\"%s\"%s]", node.getId(), node.getLabel(), attributes.isEmpty() ? "" : ", " + String.join(", ", attributes));
    }

    private static String edgeString(final GraphicalEdge edge, final boolean isDirected) {
        final GraphicalEdgeStyle style = edge.getStyle();
        final List<String> attributes = new ArrayList<>();
        if (style.getColor() != null) {
            attributes.add(String.format("color=\"%1$s\", fontcolor=\"%1$s\"", style.getColor().getHexValue()));
        }
        if (style.getType() != null) {
            attributes.add(String.format("style=%s", edgeStyleString(style.getType())));
        }
        if (edge.getLabel() != null) {
            attributes.add(String.format("label=\"%s\"", edge.getLabel()));
        }
        final String attributeString = attributes.isEmpty() ? "" : " [" + String.join(", ", attributes) + "]";
        final String edgeSymbol = isDirected ? "->" : "--";
        return String.format("  %s %s %s%s", edge.getSource().getId(), edgeSymbol, edge.getDestination().getId(), attributeString);
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
