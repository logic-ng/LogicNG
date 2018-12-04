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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.predicates.CNFPredicate;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * A dimacs file writer for a formula.  Writes the internal data structure of the formula to a dimacs file.
 * @version 1.2
 * @since 1.2
 */
public final class FormulaDimacsFileWriter {

  private static final CNFPredicate CNF_PREDICATE = new CNFPredicate();

  /**
   * Private constructor.
   */
  private FormulaDimacsFileWriter() {
    // Intentionally left empty.
  }

  /**
   * Writes a given formula's internal data structure as a dimacs file.  Must only be called with a formula which is in CNF.
   * @param fileName     the file name of the dimacs file to write
   * @param formula      the formula
   * @param writeMapping indicates whether an additional file for translating the ids to variable names shall be written
   * @throws IOException              if there was a problem writing the file
   * @throws IllegalArgumentException if the formula was not in CNF
   */
  public static void write(final String fileName, Formula formula, boolean writeMapping) throws IOException {
    File file = new File(fileName.endsWith(".cnf") ? fileName : fileName + ".cnf");
    SortedMap<Variable, Long> var2id = new TreeMap<>();
    long i = 1;
    for (Variable var : new TreeSet<>(formula.variables())) {
      var2id.put(var, i++);
    }
    if (!formula.holds(CNF_PREDICATE)) {
      throw new IllegalArgumentException("Cannot write a non-CNF formula to dimacs.  Convert to CNF first.");
    }
    List<Formula> parts = new ArrayList<>();
    if (formula.type().equals(FType.LITERAL)) {
      parts.add(formula);
    } else {
      for (Formula part : formula) {
        parts.add(part);
      }
    }
    StringBuilder sb = new StringBuilder("p cnf ");
    int partsSize = formula.type().equals(FType.FALSE) ? 1 : parts.size();
    sb.append(var2id.size()).append(" ").append(partsSize).append(System.lineSeparator());

    for (Formula part : parts) {
      for (Literal lit : part.literals()) {
        sb.append(lit.phase() ? "" : "-").append(var2id.get(lit.variable())).append(" ");
      }
      sb.append(String.format(" 0%n"));
    }
    if (formula.type().equals(FType.FALSE)) {
      sb.append(String.format("0%n"));
    }
    try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8))) {
      writer.append(sb);
      writer.flush();
    }
    if (writeMapping) {
      String mappingFileName = (fileName.endsWith(".cnf") ? fileName.substring(0, fileName.length() - 4) : fileName) + ".map";
      writeMapping(new File(mappingFileName), var2id);
    }
  }

  private static void writeMapping(File mappingFile, SortedMap<Variable, Long> var2id) throws IOException {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<Variable, Long> entry : var2id.entrySet()) {
      sb.append(entry.getKey()).append(";").append(entry.getValue()).append(System.lineSeparator());
    }
    try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(mappingFile), StandardCharsets.UTF_8))) {
      writer.append(sb);
      writer.flush();
    }
  }
}
