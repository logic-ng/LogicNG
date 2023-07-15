// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng;

import static org.assertj.core.api.Assertions.assertThat;

import org.logicng.datastructures.Tristate;
import org.logicng.solvers.SATSolver;

public interface LogicNGTest {
    default void assertSolverSat(final SATSolver solver) {
        assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
    }

    default void assertSolverUnsat(final SATSolver solver) {
        assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
    }
}
