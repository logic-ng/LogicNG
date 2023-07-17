// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import java.util.Arrays;

/**
 * A wrapper class for the internal formula factory state.
 * @version 1.2
 * @since 1.2
 */
public final class FormulaFactoryState {

    private final int id;

    private final int[] state;

    /**
     * Creates a new formula factory state with a given id and internal formula
     * factory data.
     * @param id    the id
     * @param state the formula factory data
     */
    FormulaFactoryState(final int id, final int[] state) {
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
     * Returns the internal formula factory state.
     * @return the internal formula factory state
     */
    int[] state() {
        return this.state;
    }

    @Override
    public String toString() {
        return String.format("FormulaFactoryState{id=%d, state=%s}", this.id, Arrays.toString(this.state));
    }
}
