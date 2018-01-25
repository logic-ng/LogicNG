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

package org.logicng.io.readers;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.FormulaParser;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedHashSet;

/**
 * A reader for formulas.
 * <p>
 * Reads a formula from an input file.  If the file has more than one line, the lines will be co-joined.
 * @version 1.2
 * @since 1.2
 */
public final class FormulaReader {

  /**
   * Private constructor.
   */
  private FormulaReader() {
    // Intentionally left empty.
  }

  /**
   * Reads a given file and returns the contained propositional formula.
   * @param fileName the file name
   * @param f        the formula factory
   * @return the parsed formula
   * @throws IOException     if there was a problem reading the file
   * @throws ParserException if there was a problem parsing the formula
   */
  public static Formula readPropositionalFormula(final String fileName, final FormulaFactory f) throws IOException, ParserException {
    return read(new File(fileName), new PropositionalParser(f));
  }

  /**
   * Reads a given file and returns the contained propositional formula.
   * @param file the file
   * @param f    the formula factory
   * @return the parsed formula
   * @throws IOException     if there was a problem reading the file
   * @throws ParserException if there was a problem parsing the formula
   */
  public static Formula readPropositionalFormula(final File file, final FormulaFactory f) throws IOException, ParserException {
    return read(file, new PropositionalParser(f));
  }

  /**
   * Reads a given file and returns the contained pseudo-Boolean formula.
   * @param fileName the file name
   * @param f        the formula factory
   * @return the parsed formula
   * @throws IOException     if there was a problem reading the file
   * @throws ParserException if there was a problem parsing the formula
   */
  public static Formula readPseudoBooleanFormula(final String fileName, final FormulaFactory f) throws IOException, ParserException {
    return read(new File(fileName), new PseudoBooleanParser(f));
  }

  /**
   * Reads a given file and returns the contained pseudo-Boolean formula.
   * @param file the file
   * @param f    the formula factory
   * @return the parsed formula
   * @throws IOException     if there was a problem reading the file
   * @throws ParserException if there was a problem parsing the formula
   */
  public static Formula readPseudoBooleanFormula(final File file, final FormulaFactory f) throws IOException, ParserException {
    return read(file, new PseudoBooleanParser(f));
  }

  /**
   * Internal read function.
   * @param file   the file
   * @param parser the parser
   * @return the parsed formula
   * @throws IOException     if there was a problem reading the file
   * @throws ParserException if there was a problem parsing the formula
   */
  private static Formula read(final File file, final FormulaParser parser) throws IOException, ParserException {
    try (final BufferedReader br = new BufferedReader(new FileReader(file))) {
      final LinkedHashSet<Formula> ops = new LinkedHashSet<>();
      while (br.ready())
        ops.add(parser.parse(br.readLine()));
      return parser.factory().and(ops);
    }
  }
}
