// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

/**
 * An interface for writers of graphical representations of formulas, BDDs, and graphs.
 * @version 2.4.0
 * @since 2.4.0
 */
public interface GraphicalRepresentationWriter {

    /**
     * Writes a given graphical representation with the given writer.
     * @param writer         the writer
     * @param representation the representation to write
     * @throws IOException if there is a problem during writing
     */
    void write(final Writer writer, GraphicalRepresentation representation) throws IOException;

    /**
     * Writes a given graphical representation to a file with the given file name.
     * @param fileName       the file name for the output file
     * @param representation the representation to write
     * @throws IOException if there is a problem during writing the file
     */
    default void write(final String fileName, final GraphicalRepresentation representation) throws IOException {
        write(new File(fileName), representation);
    }

    /**
     * Writes a given graphical representation to a given file.
     * @param file           the file for the output
     * @param representation the representation to write
     * @throws IOException if there is a problem during writing the file
     */
    default void write(final File file, final GraphicalRepresentation representation) throws IOException {
        try (final Writer writer = new OutputStreamWriter(Files.newOutputStream(file.toPath()), StandardCharsets.UTF_8)) {
            write(writer, representation);
        }
    }

    /**
     * Returns the string value of this writer's graphical representation.
     * @param representation the representation
     * @return the string value of the representation
     */
    default String stringValue(final GraphicalRepresentation representation) {
        try (final Writer writer = new StringWriter()) {
            write(writer, representation);
            return writer.toString();
        } catch (final IOException e) {
            throw new IllegalStateException("IO Exception", e);
        }
    }
}
