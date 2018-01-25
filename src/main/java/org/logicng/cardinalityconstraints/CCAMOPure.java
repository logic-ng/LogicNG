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

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most one variable is assigned value true.  Uses the 'naive' encoding with no introduction
 * of new variables but quadratic size.
 * @version 1.1
 * @since 1.0
 */
final class CCAMOPure implements CCAtMostOne {

  /**
   * Constructs the naive AMO encoder.
   */
  CCAMOPure() {
    // intentionally left empty
  }

  @Override
  public void build(final EncodingResult result, final Variable... vars) {
    result.reset();
    for (int i = 0; i < vars.length; i++)
      for (int j = i + 1; j < vars.length; j++)
        result.addClause(vars[i].negate(), vars[j].negate());
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
