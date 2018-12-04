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
import org.logicng.formulas.printer.FormulaStringRepresentation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

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
   * Writes a given formula to a file.
   * @param fileName          the file name of the file
   * @param formula           the formula to write
   * @param splitAndMultiline indicates whether - if the formula is an conjunction - the single operands should be
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
   * @param splitAndMultiline indicates whether - if the formula is an conjunction - the single operands should be
   *                          written to different lines without a conjoining operator
   * @param formatter         the formatter for the formula
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final String fileName, final Formula formula, final boolean splitAndMultiline,
                           final FormulaStringRepresentation formatter) throws IOException {
    write(new File(fileName), formula, splitAndMultiline, formatter);
  }

  /**
   * Writes a given formula to a file.
   * @param file              the file
   * @param formula           the formula to write
   * @param splitAndMultiline indicates whether - if the formula is an conjunction - the single operands should be
   *                          written to different lines without a conjoining operator
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final File file, final Formula formula, final boolean splitAndMultiline) throws IOException {
    write(file, formula, splitAndMultiline, formula.factory().stringRepresentation());
  }

  /**
   * Writes a given formula to a file  with a given formula formatter.
   * @param file              the file
   * @param formula           the formula to write
   * @param splitAndMultiline indicates whether - if the formula is an conjunction - the single operands should be
   *                          written to different lines without a conjoining operator
   * @param formatter         the formatter for the formula
   * @throws IOException if there was a problem writing the file
   */
  public static void write(final File file, final Formula formula, final boolean splitAndMultiline,
                           final FormulaStringRepresentation formatter) throws IOException {
    final StringBuilder sb = new StringBuilder();
    if (splitAndMultiline && formula.type() == FType.AND)
      for (final Formula f : formula)
        sb.append(formatter.toString(f)).append(System.lineSeparator());
    else
      sb.append(formatter.toString(formula));
    try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8))) {
      writer.append(sb);
      writer.flush();
    }
  }
}
