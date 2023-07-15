// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.writers;

import org.logicng.formulas.Formula;
import org.logicng.io.graphical.GraphicalDotWriter;
import org.logicng.io.graphical.generators.FormulaAstGraphicalGenerator;
import org.logicng.io.graphical.generators.FormulaDagGraphicalGenerator;

import java.io.File;
import java.io.IOException;

/**
 * A dot file writer for a formula.  Writes the internal data structure of the formula to a dot file.
 * @version 2.4.0
 * @since 1.0
 * @deprecated This legacy writer will be removed in LogicNG 3.0.0.  For a more configurable and flexible
 * to use graph writer use {@link FormulaDagGraphicalGenerator} or {@link FormulaAstGraphicalGenerator} within the new
 * graphical writer framework.
 */
@Deprecated
public final class FormulaDotFileWriter {

    private static final String DOT_EXTENSION = ".dot";

    /**
     * Private constructor.
     */
    private FormulaDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default style configuration.
     * @param fileName      the file name of the dot file to write, will be extended by suffix {@code .dot} if not already present
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean alignLiterals) throws IOException {
        write(new File(fileName.endsWith(DOT_EXTENSION) ? fileName : fileName + DOT_EXTENSION), formula, alignLiterals);
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default style configuration.
     * @param file          the file of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean alignLiterals) throws IOException {
        FormulaDagGraphicalGenerator.builder().alignTerminals(alignLiterals).build().translate(formula).write(file, GraphicalDotWriter.get());
    }
}
