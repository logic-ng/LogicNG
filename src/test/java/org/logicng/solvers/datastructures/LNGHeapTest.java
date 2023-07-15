// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
