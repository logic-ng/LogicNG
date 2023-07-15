// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

/**
 * Interface for a handler for SAT solvers.
 * @version 2.1.0
 * @since 1.0
 */
public interface SATHandler extends Handler {

    /**
     * This method is called every time a conflict is found.
     * @return {@code true} if the SAT solving should be continued, otherwise {@code false}
     */
    default boolean detectedConflict() {
        return true;
    }

    /**
     * This method is called when the SAT solver finished solving.
     */
    default void finishedSolving() {

    }

    /**
     * Null-safe helper method to call {@link SATHandler#finishedSolving} on a handler.
     * @param handler the handler to finished, may be {@code null}
     */
    static void finishSolving(final SATHandler handler) {
        if (handler != null) {
            handler.finishedSolving();
        }
    }
}
