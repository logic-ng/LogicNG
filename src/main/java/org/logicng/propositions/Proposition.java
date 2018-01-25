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

package org.logicng.propositions;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

/**
 * An interface for a proposition in LogicNG.  A proposition is a collection of formulas with a additional information
 * like a textual description or a user-provided object.
 * @version 1.0
 * @since 1.0
 */
public abstract class Proposition {

  /**
   * Returns the formulas of this constraint.
   * @return the formulas of this constraint
   */
  public abstract ImmutableFormulaList formulas();

  /**
   * Returns the formula of this proposition.
   * @param f the formula factory
   * @return the formula of this proposition
   */
  public Formula formula(final FormulaFactory f) {
    return this.formulas().formula(f);
  }

}
