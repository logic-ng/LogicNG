// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

/**
 * A BDD handler which cancels the build process after a given number of added nodes.
 * @version 1.6.2
 * @since 1.6.2
 */
public class NumberOfNodesBDDHandler extends ComputationHandler implements BDDHandler {

    private final int bound;
    private int count;

    /**
     * Constructs a new BDD handler with an upper bound for the number of added nodes (inclusive).
     * @param bound the upper bound
     */
    public NumberOfNodesBDDHandler(final int bound) {
        if (bound < 0) {
            throw new IllegalArgumentException("The bound for added nodes must be equal or greater than 0.");
        }
        this.bound = bound;
    }

    @Override
    public void started() {
        super.started();
        this.count = 0;
    }

    @Override
    public boolean newRefAdded() {
        this.aborted = ++this.count >= this.bound;
        return !this.aborted;
    }
}
