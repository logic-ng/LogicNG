// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.graphs.io;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.io.graphical.GraphicalDotWriter;
import org.logicng.io.graphical.generators.GraphGraphicalGenerator;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for a graph. Writes the internal data structure of the
 * graph to a dot file.
 * @version 2.4.0
 * @since 1.2
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0. For a more
 *             configurable and flexible to use graph writer use
 *             {@link GraphGraphicalGenerator} within the new graphical writer
 *             framework.
 */
@Deprecated
public final class GraphDotFileWriter {

    private static final String DOT_EXTENSION = ".dot";

    /**
     * Private constructor.
     */
    private GraphDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given graph's internal data structure as a dot file.
     * @param fileName the file name of the dot file to write, will be extended
     *                 by suffix {@code .dot} if not already present
     * @param graph    the graph
     * @param <T>      the type of the graph content
     * @throws IOException if there was a problem writing the file
     */
    public static <T> void write(final String fileName, final Graph<T> graph) throws IOException {
        write(new File(fileName.endsWith(DOT_EXTENSION) ? fileName : fileName + DOT_EXTENSION), graph);
    }

    /**
     * Writes a given graph's internal data structure as a dot file.
     * @param file  the file of the dot file to write
     * @param graph the graph
     * @param <T>   the type of the graph content
     * @throws IOException if there was a problem writing the file
     */
    public static <T> void write(final File file, final Graph<T> graph) throws IOException {
        GraphGraphicalGenerator.<T>builder().build().translate(graph).write(file, GraphicalDotWriter.get());
    }
}
