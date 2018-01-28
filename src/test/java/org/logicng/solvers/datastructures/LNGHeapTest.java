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

package org.logicng.solvers.datastructures;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * Unit tests for the class {@link LNGHeap}.
 * @version 1.1
 * @since 1.1
 */
public class LNGHeapTest {

  @Test
  public void test() {
    MiniSatStyleSolver solver = new MiniCard();
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    LNGHeap heap = new LNGHeap(solver);
    Assert.assertTrue(heap.empty());
    heap.insert(1);
    heap.insert(2);
    heap.insert(0);
    Assert.assertEquals(1, heap.get(0));
    Assert.assertEquals("LNGHeap{[1, 2], [2, 0], [0, 1]}", heap.toString());
    Assert.assertEquals(3, heap.size());
    heap.clear();
    Assert.assertTrue(heap.empty());
  }
}
