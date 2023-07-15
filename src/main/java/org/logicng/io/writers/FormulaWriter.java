// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.writers;

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.printer.FormulaStringRepresentation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

/**
 * A writer for formulas.
 * @version 1.2
 * @since 1.2
 */
public final class FormulaWriter {

    /**
     * Private constructor.
     */
    private FormulaWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given formula to a file using the formula formatter of the formula's formula factory.
     * @param fileName          the file name of the file
     * @param formula           the formula to write
     * @param splitAndMultiline indicates whether - if the formula is a conjunction - the single operands should be
     *                          written to different lines without a conjoining operator
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean splitAndMultiline) throws IOException {
        write(new File(fileName), formula, splitAndMultiline, formula.factory().stringRepresentation());
    }

    /**
     * Writes a given formula to a file with a given formula formatter.
     * @param fileName          the file name of the file
     * @param formula           the formula to write
     * @param splitAndMultiline indicates whether - if the formula is a conjunction - the single operands should be
     *                          written to different lines without a conjoining operator
     * @param formatter         the formatter for the formula
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean splitAndMultiline,
                             final FormulaStringRepresentation formatter) throws IOException {
        write(new File(fileName), formula, splitAndMultiline, formatter);
    }

    /**
     * Writes a given formula to a file using the formula formatter of the formula's formula factory.
     * @param file              the file
     * @param formula           the formula to write
     * @param splitAndMultiline indicates whether - if the formula is a conjunction - the single operands should be
     *                          written to different lines without a conjoining operator
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean splitAndMultiline) throws IOException {
        write(file, formula, splitAndMultiline, formula.factory().stringRepresentation());
    }

    /**
     * Writes a given formula to a file with a given formula formatter.
     * @param file              the file
     * @param formula           the formula to write
     * @param splitAndMultiline indicates whether - if the formula is a conjunction - the single operands should be
     *                          written to different lines without a conjoining operator
     * @param formatter         the formatter for the formula
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean splitAndMultiline,
                             final FormulaStringRepresentation formatter) throws IOException {
        final StringBuilder sb = new StringBuilder();
        if (splitAndMultiline && formula.type() == FType.AND) {
            for (final Formula f : formula) {
                sb.append(formatter.toString(f)).append(System.lineSeparator());
            }
        } else {
            sb.append(formatter.toString(formula));
        }
        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(file.toPath()), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
    }
}
