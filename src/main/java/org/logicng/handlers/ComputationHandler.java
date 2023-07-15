// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

/**
 * A computation handler.
 * @version 1.6.2
 * @since 1.6.2
 */
public abstract class ComputationHandler implements Handler {

    protected boolean aborted;

    @Override
    public boolean aborted() {
        return this.aborted;
    }

    @Override
    public void started() {
        this.aborted = false;
    }
}
