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

package org.logicng.io.parsers;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

/**
 * Super class for a formula parser.
 * @version 1.2
 * @since 1.2
 */
public abstract class FormulaParser {

  private final FormulaFactory f;

  /**
   * Constructor.
   * @param f the formula factory
   */
  public FormulaParser(FormulaFactory f) {
    this.f = f;
  }

  /**
   * Parses and returns a given string.
   * @param string the input string
   * @return the {@link Formula} representation of the given string
   * @throws ParserException if there was a problem parsing the string
   */
  public abstract Formula parse(final String string) throws ParserException;

  /**
   * Returns the factory of this parser.
   * @return the factory of this parser
   */
  public FormulaFactory factory() {
    return this.f;
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
