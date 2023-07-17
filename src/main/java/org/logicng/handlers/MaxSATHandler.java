// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * Interface for a handler for MaxSAT solvers.
 * @version 2.1.0
 * @since 1.0
 */
public interface MaxSATHandler extends Handler {

    /**
     * Returns a SAT handler which can be used to cancel internal SAT calls of
     * the MaxSAT solver.
     * @return a SAT handler
     */
    SATHandler satHandler();

    @Override
    default boolean aborted() {
        return satHandler() != null && satHandler().aborted();
    }

    /**
     * This method is called when the MaxSAT solver found a new lower bound for
     * a solution.
     * @param lowerBound the cost of the lower bound
     * @param model      the model of the lower bound, may be null if not
     *                   applicable
     * @return {@code true} if the solving process should be continued,
     *         otherwise {@code false}
     */
    boolean foundLowerBound(final int lowerBound, final Assignment model);

    /**
     * This method is called when the MaxSAT solver found a new upper bound for
     * a solution.
     * @param upperBound the cost of the upper bound
     * @param model      the model of the upper bound, may be null if not
     *                   applicable
     * @return {@code true} if the solving process should be continued,
     *         otherwise {@code false}
     */
    boolean foundUpperBound(final int upperBound, final Assignment model);

    /**
     * This method is called when the MaxSAT solver finished solving.
     */
    default void finishedSolving() {

    }

    /**
     * Returns the last approximation of the result or -1 if there is no
     * approximation for the lower bound. If the handler does not cancel the
     * solving process, it is not guaranteed that this method will return the
     * optimal result. Use the respective method of the MaxSAT solver instead.
     * @return the last approximation of the result or -1 if no approximation is
     *         known
     */
    int lowerBoundApproximation();

    /**
     * Returns the last approximation of the result or -1 if there is no
     * approximation for the lower bound. If the handler does not cancel the
     * solving process, it is not guaranteed that this method will return the
     * optimal result. Use the respective method of the MaxSAT solver instead.
     * @return the last approximation of the result or -1 if no approximation is
     *         known
     */
    int upperBoundApproximation();
}
