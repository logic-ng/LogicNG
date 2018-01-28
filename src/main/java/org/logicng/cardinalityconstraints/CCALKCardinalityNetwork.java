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
 * Encodes that at least 'rhs' variables are assigned value true.  Uses the cardinality network
 * encoding due to Asín, Nieuwenhuis, Oliveras, and Rodríguez-Carbonell .
 * @version 1.1
 * @since 1.1
 */
final class CCALKCardinalityNetwork implements CCAtLeastK {

  private final CCCardinalityNetworks cardinalityNetwork;

  /**
   * Constructs a new cardinality encoder.
   */
  CCALKCardinalityNetwork() {
    this.cardinalityNetwork = new CCCardinalityNetworks();
  }

  @Override
  public void build(final EncodingResult result, final Variable[] vars, int rhs) {
    cardinalityNetwork.buildALK(result, vars, rhs);
  }

  @Override
  public CCIncrementalData incrementalData() {
    return cardinalityNetwork.incrementalData();
  }

  /**
   * Builds the constraint for incremental usage.
   * @param result the result
   * @param vars   the variables
   * @param rhs    the right-hand side
   */
  void buildForIncremental(final EncodingResult result, final Variable[] vars, int rhs) {
    cardinalityNetwork.buildALKForIncremental(result, vars, rhs);
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
