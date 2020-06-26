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
