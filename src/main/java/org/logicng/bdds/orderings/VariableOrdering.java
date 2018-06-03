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

package org.logicng.bdds.orderings;

/**
 * An enumeration for the different BDD variable orderings.  A variable ordering is associated
 * with its own provider which can generate orderings for this ordering.
 * @version 1.4.0
 * @since 1.4.0
 */
public enum VariableOrdering {

  DFS(DFSOrdering.class),
  BFS(BFSOrdering.class),
  MIN2MAX(MinToMaxOrdering.class),
  MAX2MIN(MaxToMinOrdering.class),
  FORCE(ForceOrdering.class);

  private final Class<? extends VariableOrderingProvider> provider;

  /**
   * Constructs a new variable ordering with a given provider.
   * @param provider the provider
   */
  VariableOrdering(final Class<? extends VariableOrderingProvider> provider) {
    this.provider = provider;
  }

  /**
   * Returns the provider for this variable ordering.
   * @return the provider for this variable ordering
   */
  public Class<? extends VariableOrderingProvider> provider() {
    return this.provider;
  }
}
