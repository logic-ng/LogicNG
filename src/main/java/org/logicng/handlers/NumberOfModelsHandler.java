// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * A model enumeration handler that terminates the solving process after a given number of models.
 * @version 2.1.0
 * @since 1.0
 */
public class NumberOfModelsHandler extends ComputationHandler implements ModelEnumerationHandler {

    private final int bound;
    private int count;

    /**
     * Constructs a new model handler with an upper bound for the number of models (inclusive).
     * @param bound the upper bound
     * @throws IllegalArgumentException if the number of models to generate is &lt;= 0
     */
    public NumberOfModelsHandler(final int bound) {
        if (bound <= 0) {
            throw new IllegalArgumentException("You must generate at least 1 model.");
        }
        this.bound = bound;
    }

    @Override
    public void started() {
        super.started();
        this.count = 0;
    }

    @Override
    public SATHandler satHandler() {
        return null;
    }

    @Override
    public boolean foundModel(final Assignment assignment) {
        this.aborted = ++this.count >= this.bound;
        return !this.aborted;
    }
}
