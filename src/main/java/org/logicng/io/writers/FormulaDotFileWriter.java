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
//  Copyright 2015 Christoph Zengler                                     //
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

import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * A dot file writer for a formula.  Writes the internal data structure of the formula to a dot file.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class FormulaDotFileWriter {

  /**
   * Private constructor.
   */
  private FormulaDotFileWriter() {
    throw new AssertionError();
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
    final StringBuilder sb = new StringBuilder("digraph G {\n");
    final Map<Formula, Integer> ids = new HashMap<>();
    if (alignLiterals && !formula.literals().isEmpty())
      sb.append("{ rank = same;\n");
    int id = 0;
    for (final Literal lit : formula.literals()) {
      ids.put(lit, id);
      sb.append("  id").append(id).append(" [shape=box, label=\"").
              append(lit.phase() ? lit.name() : "¬" + lit.name()).append("\"];\n");
      id++;
    }
    if (alignLiterals && !formula.literals().isEmpty())
      sb.append("}\n");
    formula.generateDotString(sb, ids);
    sb.append("}\n");
    try (BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
      writer.append(sb);
      writer.flush();
    } catch (IOException exception) {
      throw exception;
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}