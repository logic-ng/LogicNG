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

package org.logicng.bdds.simple;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Node cache for the simple BDD implementation.
 * @version 1.2
 * @since 1.2
 */
final class NodeCache {

  /**
   * Mapping from [var, low, high] -> node index
   */
  private final Map<List<Integer>, Integer> cache;

  /**
   * Constructor.
   */
  NodeCache() {
    this.cache = new HashMap<>();
  }

  /**
   * Inserts a new node into the cache.
   * @param var  the variable index
   * @param low  the low index
   * @param high the high index
   * @param node the node index
   */
  void insert(int var, int low, int high, int node) {
    this.cache.put(Arrays.asList(var, low, high), node);
  }

  /**
   * Looks up a given variable/low/high combination and returns its node index or -1 if not found.
   * @param var  the variable index
   * @param low  the low index
   * @param high the high index
   * @return the node index of the cached node or -1 if the node was not in the cache
   */
  int lookup(int var, int low, int high) {
    final Integer result = this.cache.get(Arrays.asList(var, low, high));
    return result == null ? -1 : result;
  }

}
