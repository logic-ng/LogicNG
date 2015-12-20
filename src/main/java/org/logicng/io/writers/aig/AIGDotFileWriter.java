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

package org.logicng.io.writers.aig;

import org.logicng.formulas.Formula;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * A dot file writer for a formula's AIG.  Writes the AIG data structure of a formula to a dot file.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class AIGDotFileWriter {

  private final AIGDotPrinter printer;

  /**
   * Constructor.
   */
  public AIGDotFileWriter() {
    printer = new AIGDotPrinter();
  }

  /**
   * Writes a given formula's AIG data structure as a dot file.
   * @param fileName      the file name of the dot file to write
   * @param formula       the formula
   * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
   * @throws IOException if there was a problem writing the file
   */
  public void write(final String fileName, final Formula formula, boolean alignLiterals) throws IOException {
    write(new File(fileName.endsWith(".dot") ? fileName : fileName + ".dot"), formula, alignLiterals);
  }

  /**
   * Writes a given formula's AIG data structure as a dot file.
   * @param file          the file of the dot file to write
   * @param formula       the formula
   * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
   * @throws IOException if there was a problem writing the file
   */
  public void write(final File file, final Formula formula, boolean alignLiterals) throws IOException {
    final String dotString = printer.createDotString(formula, alignLiterals);
    try (BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
      writer.append(dotString);
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
