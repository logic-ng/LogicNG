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

public class GraphicalMermaidWriter implements GraphicalRepresentationWriter {

    private static final GraphicalMermaidWriter INSTANCE = new GraphicalMermaidWriter();

    private GraphicalMermaidWriter() {
        // Singleton Pattern
    }

    public static GraphicalMermaidWriter get() {
        return INSTANCE;
    }

    @Override
    public void write(final Writer writer, final GraphicalRepresentation representation) throws IOException {
        final BufferedWriter bufferedWriter = new BufferedWriter(writer);
        writePreamble(bufferedWriter, representation);
        writeNodes(bufferedWriter, representation);
        writeEdges(bufferedWriter, representation);
        bufferedWriter.close();
    }

    private static void writePreamble(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        writer.write("graph TD");
        writer.newLine();
        writer.write(String.format("background: %s", representation.getBackground().getHexValue()));
        writer.newLine();
    }

    private static void writeNodes(final BufferedWriter bufferedWriter, final GraphicalRepresentation representation) throws IOException {
        for (final GraphicalNode node : representation.getNodes()) {
            bufferedWriter.write(String.format("  %s", nodeString(node)));
            bufferedWriter.newLine();
            bufferedWriter.write(String.format("    style %s stroke:%s,color:%s,fill:%s", node.id, node.style.getStrokeColor().getHexValue(),
                    node.style.getTextColor().getHexValue(), node.style.getBackgroundColor().getHexValue()));
            bufferedWriter.newLine();
        }
        bufferedWriter.newLine();
    }

    private static void writeEdges(final BufferedWriter writer, final GraphicalRepresentation representation) throws IOException {
        int counter = 0;
        for (final GraphicalEdge edge : representation.getEdges()) {
            final String edgeSymbol = edgeSymbolString(edge, representation.isDirected());
            final String string = String.format("  %s %s %s", edge.getSource().id, edgeSymbol, edge.getDestination().id);
            writer.write(string);
            writer.newLine();
            writer.write(String.format("    linkStyle %d stroke:%s", counter++, edge.getStyle().getColor().getHexValue()));
            writer.newLine();
        }
    }

    private static String nodeString(final GraphicalNode node) {
        final String start;
        final String end;
        switch (node.style.getShape()) {
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
        return String.format("%s%s\"%s\"%s", node.id, start, node.label, end);
    }

    private static String edgeSymbolString(final GraphicalEdge edge, final boolean directed) {
        final String edgeConnetor = edgeConnector(edge.getStyle().getLineType(), directed);
        return edge.getLabel() == null ? edgeConnetor : String.format("%s|\"%s\"|", edgeConnetor, edge.getLabel());
    }

    private static String edgeConnector(final GraphicalEdgeStyle.LineType lineType, final boolean directed) {
        switch (lineType) {
            case DOTTED:
                return directed ? "_._>" : "_._";
            case BOLD:
                return directed ? "==>" : "===";
            case SOLID:
            default:
                return directed ? "-->" : "---";
        }
    }
}
