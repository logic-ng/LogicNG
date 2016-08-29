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

package org.logicng.bdds;

/**
 * The internal representation of a BDD.
 * @version 1.2
 * @since 1.2
 */
public final class BDD {

  private final int index;

  /**
   * Constructs a new BDD with a given index.
   * @param index the index
   */
  public BDD(int index) {
    this.index = index;
  }

  /**
   * Returns the index of this BDD.
   * @return the index of this BDD
   */
  public int index() {
    return this.index;
  }

  @Override
  public int hashCode() {
    return index;
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || other instanceof BDD && this.index == ((BDD) other).index;
  }

  @Override
  public String toString() {
    return "BDD{" + index + "}";
  }
}
