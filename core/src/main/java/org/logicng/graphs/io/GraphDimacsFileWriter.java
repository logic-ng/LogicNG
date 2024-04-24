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

package org.logicng.graphs.io;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.util.Pair;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * A dimacs file writer for graphs.  Writes the internal data structure of a graph to a dimacs file.
 * @version 2.4.0
 * @since 1.2
 */
public final class GraphDimacsFileWriter {

    private static final String COL_EXTENSION = ".col";
    private static final String MAP_EXTENSION = ".map";

    /**
     * Private constructor.
     */
    private GraphDimacsFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given graph's internal data structure as a dimacs file.
     * @param fileName     the file name of the dimacs file to write, will be extended by suffix {@code .col} if not already present
     * @param graph        the graph
     * @param writeMapping indicates whether an additional file for translating the ids to variable names shall be written
     * @param <T>          the type of the graph content
     * @throws IOException if there was a problem writing the file
     */
    public static <T> void write(final String fileName, final Graph<T> graph, final boolean writeMapping) throws IOException {
        final File file = new File(fileName.endsWith(COL_EXTENSION) ? fileName : fileName + COL_EXTENSION);
        final Map<Node<T>, Long> node2id = new LinkedHashMap<>();
        long i = 1;
        for (final Node<T> node : graph.nodes()) {
            node2id.put(node, i++);
        }

        final StringBuilder sb = new StringBuilder("p edge ");
        final Set<Pair<Node<T>, Node<T>>> edges = new LinkedHashSet<>();
        final Set<Node<T>> doneNodes = new LinkedHashSet<>();
        for (final Node<T> d : graph.nodes()) {
            for (final Node<T> n : d.neighbours()) {
                if (!doneNodes.contains(n)) {
                    edges.add(new Pair<>(d, n));
                }
            }
            doneNodes.add(d);
        }
        sb.append(node2id.size()).append(" ").append(edges.size()).append(System.lineSeparator());

        for (final Pair<Node<T>, Node<T>> edge : edges) {
            sb.append("e ").append(node2id.get(edge.first())).append(" ").append(node2id.get(edge.second())).append(System.lineSeparator());
        }

        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(file.toPath()), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
        if (writeMapping) {
            final String mappingFileName = (fileName.endsWith(COL_EXTENSION) ? fileName.substring(0, fileName.length() - 4) : fileName) + MAP_EXTENSION;
            writeMapping(new File(mappingFileName), node2id);
        }
    }

    private static <T> void writeMapping(final File mappingFile, final Map<Node<T>, Long> node2id) throws IOException {
        final StringBuilder sb = new StringBuilder();
        for (final Map.Entry<Node<T>, Long> entry : node2id.entrySet()) {
            sb.append(entry.getKey().content()).append(";").append(entry.getValue()).append(System.lineSeparator());
        }
        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(mappingFile.toPath()), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
    }
}
