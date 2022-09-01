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

package org.logicng.io.writers;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

/**
 * A dot file writer for a formula.  Writes the internal data structure of the formula to a dot file.
 * @version 2.4.0
 * @since 1.0
 */
public final class FormulaDotFileWriter {

    /**
     * Private constructor.
     */
    private FormulaDotFileWriter() {
        // Intentionally left empty.
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default color configuration.
     * @param fileName      the file name of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean alignLiterals) throws IOException {
        write(fileName, formula, alignLiterals, new ColorConfig());
    }

    /**
     * Writes a given formula's internal data structure as a dot file with the default color configuration.
     * @param file          the file of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean alignLiterals) throws IOException {
        write(file, formula, alignLiterals, new ColorConfig());
    }

    /**
     * Writes a given formula's internal data structure as a dot file.
     * @param fileName      the file name of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @param colorConfig   the color configuration
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, final boolean alignLiterals, final ColorConfig colorConfig) throws IOException {
        write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), formula, alignLiterals, colorConfig);
    }

    /**
     * Writes a given formula's internal data structure as a dot file.
     * @param file          the file of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @param colorConfig   the color configuration
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, final boolean alignLiterals, final ColorConfig colorConfig) throws IOException {
        final StringBuilder sb = new StringBuilder(String.format("digraph G {%n"));
        final Map<Formula, Integer> ids = new HashMap<>();
        if (alignLiterals && !formula.literals().isEmpty()) {
            sb.append(String.format("{ rank = same;%n"));
        }
        int id = 0;
        for (final Literal lit : formula.literals()) {
            ids.put(lit, id);
            sb.append("  id").append(id).append(" [shape=box, ")
                    .append(colorConfig.literalNodes.toDotString())
                    .append(", label=\"").
                    append(lit.phase() ? lit.name() : "¬" + lit.name()).append(String.format("\"];%n"));
            id++;
        }
        if (alignLiterals && !formula.literals().isEmpty()) {
            sb.append(String.format("}%n"));
        }
        generateDotString(formula, sb, ids, colorConfig);
        sb.append(String.format("}%n"));
        try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(file.toPath()), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
    }

    /**
     * Generates the dot string for a formula
     * @param formula     the formula
     * @param sb          the current string builder
     * @param ids         the current ID mapping
     * @param colorConfig
     */
    private static void generateDotString(final Formula formula, final StringBuilder sb, final Map<Formula, Integer> ids, final ColorConfig colorConfig) {
        switch (formula.type()) {
            case FALSE:
                sb.append(String.format("  false;%n"));
                break;
            case TRUE:
                sb.append(String.format("  true;%n"));
                break;
            case LITERAL:
                break;
            case PBC:
                final int id = ids.size();
                ids.put(formula, id);
                sb.append("  id").append(id).append(" ").append(opNodeWithColor(formula.toString(), colorConfig));
                for (final Formula operand : ((PBConstraint) formula).operands()) {
                    sb.append("  id").append(id).append(" -> id").append(ids.get(operand)).append(arrowStyle(null, colorConfig));
                }
                break;
            case NOT:
                generateNotDotString((Not) formula, sb, ids, colorConfig);
                break;
            case IMPL:
                generateBinaryDotString((BinaryOperator) formula, sb, ids, "⇒", true, colorConfig);
                break;
            case EQUIV:
                generateBinaryDotString((BinaryOperator) formula, sb, ids, "⇔", false, colorConfig);
                break;
            case AND:
                generateNaryDotString((NAryOperator) formula, sb, ids, "∧", colorConfig);
                break;
            case OR:
                generateNaryDotString((NAryOperator) formula, sb, ids, "∨", colorConfig);
                break;
            default:
                throw new IllegalArgumentException("Cannot write the formula type " + formula.type());
        }

    }

    private static void generateNotDotString(final Not not, final StringBuilder sb, final Map<Formula, Integer> ids, final ColorConfig colorConfig) {
        final int id;
        if (!ids.containsKey(not.operand())) {
            generateDotString(not.operand(), sb, ids, colorConfig);
        }
        id = ids.size();
        ids.put(not, id);
        sb.append("  id").append(id).append(String.format(" ")).append(opNodeWithColor("¬", colorConfig));
        sb.append("  id").append(id).append(" -> id").append(ids.get(not.operand())).append(arrowStyle(null, colorConfig));
    }

    private static void generateBinaryDotString(final BinaryOperator formula, final StringBuilder sb,
                                                final Map<Formula, Integer> ids, final String op, final boolean directions, final ColorConfig colorConfig) {
        if (!ids.containsKey(formula.left())) {
            generateDotString(formula.left(), sb, ids, colorConfig);
        }
        if (!ids.containsKey(formula.right())) {
            generateDotString(formula.right(), sb, ids, colorConfig);
        }
        final int id = ids.size();
        ids.put(formula, id);
        sb.append("  id").append(id).append(" ").append(opNodeWithColor(op, colorConfig));
        sb.append("  id").append(id).append(" -> id").append(ids.get(formula.left()));
        sb.append(directions ? arrowStyle("l", colorConfig) : arrowStyle(null, colorConfig));
        sb.append("  id").append(id).append(" -> id").append(ids.get(formula.right()));
        sb.append(directions ? arrowStyle("r", colorConfig) : arrowStyle(null, colorConfig));
    }

    private static void generateNaryDotString(final NAryOperator formula, final StringBuilder sb,
                                              final Map<Formula, Integer> ids, final String op, final ColorConfig colorConfig) {
        for (final Formula operand : formula) {
            if (!ids.containsKey(operand)) {
                generateDotString(operand, sb, ids, colorConfig);
            }
        }
        final int id = ids.size();
        ids.put(formula, id);
        sb.append("  id").append(id).append(" ").append(opNodeWithColor(op, colorConfig));
        for (final Formula operand : formula) {
            sb.append("  id").append(id).append(" -> id").append(ids.get(operand)).append(arrowStyle(null, colorConfig));
        }
    }

    private static String opNodeWithColor(final String operand, final ColorConfig colorConfig) {
        return String.format("[label=\"%s\", %s];%n", operand, colorConfig.operatorNodes.toDotString());
    }

    private static String arrowStyle(final String label, final ColorConfig colorConfig) {
        return String.format(" [%scolor=\"%s\", fontcolor=\"%s\"];%n", label == null ? "" : "label=" + label + ", ", colorConfig.edges, colorConfig.edges);
    }

    /**
     * Color configuration for the formula dot file writer.
     */
    public static class ColorConfig {
        private final DotNodeColor literalNodes;
        private final DotNodeColor operatorNodes;
        private final String edges;

        /**
         * Constructs a new default color configuration: black text and strokes, white fill.
         */
        public ColorConfig() {
            this.literalNodes = new DotNodeColor();
            this.operatorNodes = new DotNodeColor();
            this.edges = "black";
        }

        /**
         * Constructs a new color configuration.
         * @param literalNodes  the color of literal (terminal) nodes
         * @param operatorNodes the color of operator (inner) nodes
         * @param edges         the color of the arrows
         */
        public ColorConfig(final DotNodeColor literalNodes, final DotNodeColor operatorNodes, final String edges) {
            this.literalNodes = literalNodes;
            this.operatorNodes = operatorNodes;
            this.edges = edges;
        }
    }
}
