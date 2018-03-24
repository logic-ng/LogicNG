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

package org.logicng.formulas;

import java.util.Arrays;

/**
 * A wrapper class for the internal formula factory state.
 * @version 1.2
 * @since 1.2
 */
public final class FormulaFactoryState {

  private final int id;

  private final int[] state;

  /**
   * Creates a new formula factory state with a given id and internal formula factory data.
   * @param id    the id
   * @param state the formula factory data
   */
  FormulaFactoryState(final int id, final int[] state) {
    this.id = id;
    this.state = Arrays.copyOf(state, state.length);
  }

  /**
   * Returns the id of this state.
   * @return the id of this state
   */
  int id() {
    return id;
  }

  /**
   * Returns the internal formula factory state.
   * @return the internal formula factory state
   */
  int[] state() {
    return this.state;
  }

  @Override
  public String toString() {
    return String.format("FormulaFactoryState{id=%d, state=%s}", id, Arrays.toString(state));
  }
}
