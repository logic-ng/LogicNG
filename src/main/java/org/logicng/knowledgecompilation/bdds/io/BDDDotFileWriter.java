// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.io;

import org.logicng.io.graphical.GraphicalDotWriter;
import org.logicng.io.graphical.generators.BddGraphicalGenerator;
import org.logicng.knowledgecompilation.bdds.BDD;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for BDDs. Writes the internal data structure of a BDD to a
 * dot file.
 * @version 2.4.0
 * @since 1.4.0
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0. For a more
 *             configurable and flexible to use graph writer use
 *             {@link BddGraphicalGenerator} within the new graphical writer
 *             framework.
 */
@Deprecated
public final class BDDDotFileWriter {

    private static final String DOT_EXTENSION = ".dot";

    /**
     * Private constructor.
     */
    private BDDDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given BDD as a dot file.
     * @param fileName the file name of the dot file to write
     * @param bdd      the BDD
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final BDD bdd) throws IOException {
        write(new File(fileName.endsWith(DOT_EXTENSION) ? fileName : fileName + DOT_EXTENSION), bdd);
    }

    /**
     * Writes a given formula's internal data structure as a dot file.
     * @param file the file of the dot file to write
     * @param bdd  the BDD
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final BDD bdd) throws IOException {
        BddGraphicalGenerator.builder().build().translate(bdd).write(file, GraphicalDotWriter.get());
    }
}
