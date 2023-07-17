// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

/**
 * Interface for a handler for DNNF compilations.
 * @version 2.1.0
 * @since 2.0.0
 */
public interface DnnfCompilationHandler extends Handler {

    /**
     * This method is called when a Shannon expansion was performed.
     * @return {@code true} when the computation should be continued,
     *         {@code false} if it should be aborted with a
     *         {@link java.util.concurrent.TimeoutException}
     */
    default boolean shannonExpansion() {
        return true;
    }
}
