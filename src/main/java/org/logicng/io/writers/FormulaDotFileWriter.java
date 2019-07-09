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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * A dot file writer for a formula.  Writes the internal data structure of the formula to a dot file.
 * @version 1.1
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
     * Writes a given formula's internal data structure as a dot file.
     * @param fileName      the file name of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final String fileName, final Formula formula, boolean alignLiterals) throws IOException {
        write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), formula, alignLiterals);
    }

    /**
     * Writes a given formula's internal data structure as a dot file.
     * @param file          the file of the dot file to write
     * @param formula       the formula
     * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
     * @throws IOException if there was a problem writing the file
     */
    public static void write(final File file, final Formula formula, boolean alignLiterals) throws IOException {
        final StringBuilder sb = new StringBuilder(String.format("digraph G {%n"));
        final Map<Formula, Integer> ids = new HashMap<>();
        if (alignLiterals && !formula.literals().isEmpty()) {
            sb.append(String.format("{ rank = same;%n"));
        }
        int id = 0;
        for (final Literal lit : formula.literals()) {
            ids.put(lit, id);
            sb.append("  id").append(id).append(" [shape=box, label=\"").
                    append(lit.phase() ? lit.name() : "¬" + lit.name()).append(String.format("\"];%n"));
            id++;
        }
        if (alignLiterals && !formula.literals().isEmpty()) {
            sb.append(String.format("}%n"));
        }
        generateDotString(formula, sb, ids);
        sb.append(String.format("}%n"));
        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8))) {
            writer.append(sb);
            writer.flush();
        }
    }

    /**
     * Generates the dot string for a formula
     * @param formula the formula
     * @param sb      the current string builder
     * @param ids     the current ID mapping
     */
    private static void generateDotString(final Formula formula, final StringBuilder sb, final Map<Formula, Integer> ids) {
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
                sb.append("  id").append(id).append(" [label=\"").append(formula.toString()).append(String.format("\"];%n"));
                for (final Formula operand : ((PBConstraint) formula).operands()) {
                    sb.append("  id").append(id).append(" -> id").append(ids.get(operand)).append(String.format(";%n"));
                }
                break;
            case NOT:
                generateNotDotString((Not) formula, sb, ids);
                break;
            case IMPL:
                generateBinaryDotString((BinaryOperator) formula, sb, ids, "⇒", true);
                break;
            case EQUIV:
                generateBinaryDotString((BinaryOperator) formula, sb, ids, "⇔", true);
                break;
            case AND:
                generateNaryDotString((NAryOperator) formula, sb, ids, "∧");
                break;
            case OR:
                generateNaryDotString((NAryOperator) formula, sb, ids, "∨");
                break;
            default:
                throw new IllegalArgumentException("Cannot write the formula type " + formula.type());
        }

    }

    private static void generateNotDotString(final Not not, final StringBuilder sb, final Map<Formula, Integer> ids) {
        int id;
        if (!ids.containsKey(not.operand())) {
            generateDotString(not.operand(), sb, ids);
        }
        id = ids.size();
        ids.put(not, id);
        sb.append("  id").append(id).append(String.format(" [label=\"¬\"];%n"));
        sb.append("  id").append(id).append(" -> id").append(ids.get(not.operand())).append(String.format(";%n"));
    }

    private static void generateBinaryDotString(final BinaryOperator formula, final StringBuilder sb,
                                                final Map<Formula, Integer> ids, String op, boolean directions) {
        if (!ids.containsKey(formula.left())) {
            generateDotString(formula.left(), sb, ids);
        }
        if (!ids.containsKey(formula.right())) {
            generateDotString(formula.right(), sb, ids);
        }
        final int id = ids.size();
        ids.put(formula, id);
        sb.append("  id").append(id).append(" [label=\"").append(op).append(String.format("\"];%n"));
        sb.append("  id").append(id).append(" -> id").append(ids.get(formula.left()));
        sb.append(directions ? String.format(" [label=\"l\"];%n") : String.format(";%n"));
        sb.append("  id").append(id).append(" -> id").append(ids.get(formula.right()));
        sb.append(directions ? String.format(" [label=\"r\"];%n") : String.format(";%n"));
    }

    private static void generateNaryDotString(final NAryOperator formula, final StringBuilder sb,
                                              final Map<Formula, Integer> ids, final String op) {
        for (final Formula operand : formula) {
            if (!ids.containsKey(operand)) {
                generateDotString(operand, sb, ids);
            }
        }
        final int id = ids.size();
        ids.put(formula, id);
        sb.append("  id").append(id).append(" [label=\"").append(op).append(String.format("\"];%n"));
        for (final Formula operand : formula) {
            sb.append("  id").append(id).append(" -> id").append(ids.get(operand)).append(String.format(";%n"));
        }
    }
}
