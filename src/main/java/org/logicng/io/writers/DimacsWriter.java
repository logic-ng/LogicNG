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
//  Copyright 2015-2016 Christoph Zengler                                //
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
import org.logicng.formulas.Or;
import org.logicng.formulas.Variable;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

/**
 * A writer for formulas in the Dimacs CNF format.
 * @version 1.2
 * @since 1.2
 */
public final class DimacsWriter {

  /**
   * Private constructor.
   */
  private DimacsWriter() {
    // Intentionally left empty.
  }

  /**
   * Writes a given formula to a file.
   * @param file    the file
   * @param formula the formula to write
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final File file, final Formula formula) throws IOException {
    final Map<String, Integer> mapping = new HashMap<>();
    final Formula cnf = formula.cnf();
    int counter = 1;
    for (final Variable var : cnf.variables())
      mapping.put(var.name(), counter++);
    try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), Charset.forName("UTF-8")))) {
      switch (cnf.type()) {
        case TRUE:
          writer.write(String.format("p cnf 0 0%n"));
          break;
        case FALSE:
          writer.write(String.format("p cnf 0 1%n"));
          writer.write("0\n");
          break;
        case LITERAL:
          writer.write(String.format("p cnf 1 1%n"));
          writer.write(literal((Literal) cnf, mapping) + "0\n");
          break;
        case OR:
          writer.write(String.format("p cnf %d 1%n", cnf.variables().size()));
          writer.write(or((Or) cnf, mapping));
          break;
        case AND:
          writer.write(String.format("p cnf %d %d%n", cnf.variables().size(), cnf.numberOfOperands()));
          for (final Formula op : cnf)
            writer.write(or((Or) op, mapping));
          break;
        default:
          throw new IllegalStateException("The formula type " + cnf.type() + " cannot occur in a CNF formula.");
      }
      writer.flush();
    }
  }

  /**
   * Returns the string for a given literal.
   * @param lit     the literal
   * @param mapping the variable mapping
   * @return the literal string
   */
  private static String literal(final Literal lit, final Map<String, Integer> mapping) {
    return (lit.phase() ? "" : "-") + mapping.get(lit.name());
  }

  /**
   * Returns the string for a given disjunction.
   * @param or      the disjunction
   * @param mapping the variable mapping
   * @return the disjunction string
   */
  private static String or(final Or or, final Map<String, Integer> mapping) {
    String result = "";
    for (final Formula op : or)
      result += literal((Literal) op, mapping) + " ";
    result += String.format("0%n");
    return result;
  }
}
