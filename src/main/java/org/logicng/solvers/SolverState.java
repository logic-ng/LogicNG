// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.solvers;

import java.util.Arrays;

/**
 * A wrapper class for the internal solver state.
 * @version 1.1
 * @since 1.0
 */
public final class SolverState {

    private final int id;
    private final int[] state;

    /**
     * Creates a new solver state with a given id and internal solver data.
     * @param id    the id
     * @param state the solver data
     */
    public SolverState(final int id, final int[] state) {
        this.id = id;
        this.state = Arrays.copyOf(state, state.length);
    }

    /**
     * Returns the id of this state.
     * @return the id of this state
     */
    int id() {
        return this.id;
    }

    /**
     * Returns the internal solver state.
     * @return the internal solver state
     */
    int[] state() {
        return this.state;
    }

    @Override
    public String toString() {
        return String.format("SolverState{id=%d, state=%s}", this.id, Arrays.toString(this.state));
    }
}
