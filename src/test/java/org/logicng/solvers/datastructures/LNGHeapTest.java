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
//  Copyright 2015-20xx Christoph Zengler                                //
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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * Unit tests for the class {@link LNGHeap}.
 * @version 2.0.0
 * @since 1.1
 */
public class LNGHeapTest {

    @Test
    public void test() {
        final MiniSatStyleSolver solver = new MiniCard();
        solver.newVar(true, true);
        solver.newVar(true, true);
        solver.newVar(true, true);
        final LNGHeap heap = new LNGHeap(solver);
        assertThat(heap.empty()).isTrue();
        heap.insert(1);
        heap.insert(2);
        heap.insert(0);
        assertThat(heap.get(0)).isEqualTo(1);
        assertThat(heap.toString()).isEqualTo("LNGHeap{[1, 2], [2, 0], [0, 1]}");
        assertThat(heap.size()).isEqualTo(3);
        heap.clear();
        assertThat(heap.empty()).isTrue();
    }
}
