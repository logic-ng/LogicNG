// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

/**
 * Interface for a handler.  A handler can be used as callback for different time-intensive computations in order
 * to abort these computations.  There are same often used default handlers already implemented and users can
 * implement their own handlers by implementing the respective interfaces.
 * @version 2.1.0
 * @since 1.6.2
 */
public interface Handler {

    /**
     * Returns whether the computation was aborted by the handler.
     * @return {@code true} if the computation was aborted by the handler, otherwise {@code false}
     */
    default boolean aborted() {
        return false;
    }

    /**
     * This method is called when the computation starts.
     */
    default void started() {

    }

    /**
     * Returns {@code true} if the handler is not {@code null} and is already aborted.
     * @param handler the handler to check
     * @return {@code true} if the handler is not {@code null} and is already aborted, otherwise {@code false}
     */
    static boolean aborted(final Handler handler) {
        return handler != null && handler.aborted();
    }

    /**
     * Null-safe helper method to start a handler.
     * @param handler the handler to start, may be {@code null}
     */
    static void start(final Handler handler) {
        if (handler != null) {
            handler.started();
        }
    }
}
