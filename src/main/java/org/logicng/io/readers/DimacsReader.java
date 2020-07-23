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

package org.logicng.io.readers;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * A reader for DIMACS CNF files.
 * <p>
 * This reader reads all the clauses and variables - independent of the numbers given in the prefix. Also it assumes
 * that every clause is in its own line and ends with '0'.  Comments are only allowed if the lines start with 'c'.  No
 * C style comments are supported (yes, we have actually seen these in DIMACS files).
 * @version 1.3
 * @since 1.2
 */
public final class DimacsReader {

    /**
     * Private constructor.
     */
    private DimacsReader() {
        // Intentionally left empty.
    }

    /**
     * Reads a given DIMACS CNF file and returns the contained clauses as a list of formulas.
     * @param file the file
     * @param f    the formula factory
     * @return the list of formulas (clauses)
     * @throws IOException if there was a problem reading the file
     */
    public static List<Formula> readCNF(final File file, final FormulaFactory f) throws IOException {
        return readCNF(file, f, "v");
    }

    /**
     * Reads a given DIMACS CNF file and returns the contained clauses as a list of formulas.
     * @param file   the file
     * @param f      the formula factory
     * @param prefix the prefix for the variable names
     * @return the list of formulas (clauses)
     * @throws IOException if there was a problem reading the file
     */
    public static List<Formula> readCNF(final File file, final FormulaFactory f, final String prefix) throws IOException {
        final List<Formula> result = new ArrayList<>();
        try (final BufferedReader br = new BufferedReader(new FileReader(file))) {
            while (br.ready()) {
                final String line = br.readLine();
                if (!line.startsWith("c") && !line.startsWith("p") && !line.trim().isEmpty()) {
                    final String[] split = line.split("\\s+");
                    if (!"0".equals(split[split.length - 1].trim())) {
                        throw new IllegalArgumentException("Line '" + line + "' did not end with 0.");
                    }
                    final LinkedHashSet<Literal> vars = new LinkedHashSet<>(split.length - 1);
                    for (int i = 0; i < split.length - 1; i++) {
                        final String lit = split[i].trim();
                        if (!lit.isEmpty()) {
                            if (lit.startsWith("-")) {
                                vars.add(f.literal(prefix + split[i].trim().substring(1), false));
                            } else {
                                vars.add(f.variable(prefix + split[i].trim()));
                            }
                        }
                    }
                    result.add(f.or(vars));
                }
            }
        }
        return result;
    }

    /**
     * Reads a given DIMACS CNF file and returns the contained clauses as a list of formulas.
     * @param fileName the file name
     * @param f        the formula factory
     * @return the list of formulas (clauses)
     * @throws IOException if there was a problem reading the file
     */
    public static List<Formula> readCNF(final String fileName, final FormulaFactory f) throws IOException {
        return readCNF(new File(fileName), f, "v");
    }

    /**
     * Reads a given DIMACS CNF file and returns the contained clauses as a list of formulas.
     * @param fileName the file name
     * @param f        the formula factory
     * @param prefix   the prefix for the variable names
     * @return the list of formulas (clauses)
     * @throws IOException if there was a problem reading the file
     */
    public static List<Formula> readCNF(final String fileName, final FormulaFactory f, final String prefix) throws IOException {
        return readCNF(new File(fileName), f, prefix);
    }

}
