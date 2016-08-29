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

import java.util.HashMap;
import java.util.Map;

/**
 * Apply cache for the simple BDD implementation.
 * @version 1.2
 * @since 1.2
 */
final class ApplyCache {
  private final Map<Integer[], Integer> cache;

  /**
   * Constructor.
   */
  ApplyCache() {
    this.cache = new HashMap<>();
  }

  /**
   * Inserts the computation
   * @param node1  the first node index
   * @param node2  the second node index
   * @param result the result node index
   */
  void insert(int node1, int node2, int result) {
    this.cache.put(new Integer[]{node1, node2}, result);
  }

  /**
   * Looks up a given node combination and returns its node index or -1 if not found.
   * @param node1 the first node index
   * @param node2 the second node index
   * @return the node index of the cached node or -1 if the node was not in the cache
   */
  int lookup(int node1, int node2) {
    final Integer result = this.cache.get(new Integer[]{node1, node2});
    return result == null ? -1 : result;
  }
}
