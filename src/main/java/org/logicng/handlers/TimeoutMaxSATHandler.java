// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * A MaxSAT handler which cancels the solving process after a given timeout.
 * @version 2.1.0
 * @since 1.0
 */
public class TimeoutMaxSATHandler extends TimeoutHandler implements MaxSATHandler {

    private TimeoutSATHandler satHandler;
    private int currentLb;
    private int currentUb;

    /**
     * Constructs a new timeout handler with a given timeout and a timeout type.
     * The interpretation of the timeout depends on the timeout type:
     * <ul>
     *     <li>{@link TimerType#SINGLE_TIMEOUT}: The timeout is started when {@link Handler#started()} is called.
     *     Further calls to {@link Handler#started()} have no effect on the timeout. Thus, the timeout can only be started once.</li>
     *     <li>{@link TimerType#RESTARTING_TIMEOUT}: The timeout is restarted when {@link Handler#started()} is called.</li>
     *     <li>{@link TimerType#FIXED_END}: Timeout which is interpreted as fixed point in time (in milliseconds)
     *     at which the computation should be aborted. The method {@link Handler#started()} must still be called,
     *     but does not have an effect on the timeout.</li>
     * </ul>
     * Note that it might take a few milliseconds more until the solver is actually canceled, since the handler depends
     * on the solvers call to {@code foundApproximation()} or {@link SATHandler#detectedConflict()}.
     * @param timeout the timeout in milliseconds, its meaning is defined by the timeout type
     * @param type    the type of the timer, must not be {@code null}
     */
    public TimeoutMaxSATHandler(final long timeout, final TimerType type) {
        super(timeout, type);
        this.currentLb = -1;
        this.currentUb = -1;
    }

    /**
     * Constructs a new timeout handler with a given timeout and uses the timeout type {@link TimerType#SINGLE_TIMEOUT}.
     * Thus, the timeout is started when {@link Handler#started()} is called and
     * further calls to {@link Handler#started()} have no effect on the timeout.
     * @param timeout the timeout in milliseconds
     */
    public TimeoutMaxSATHandler(final long timeout) {
        super(timeout);
    }

    @Override
    public void started() {
        super.started();
        if (this.satHandler == null || this.type == TimerType.RESTARTING_TIMEOUT) {
            this.satHandler = new TimeoutSATHandler(this.designatedEnd, TimerType.FIXED_END);
        }
        this.currentLb = -1;
        this.currentUb = -1;
    }

    @Override
    public boolean aborted() {
        return super.aborted() || Handler.aborted(this.satHandler);
    }

    /**
     * Returns a SAT handler which can be used to cancel internal SAT calls of the model enumeration process.
     * Note that this handler will only be available after the first call to {@link #started()}.
     * @return the SAT handler
     */
    @Override
    public SATHandler satHandler() {
        return this.satHandler;
    }

    @Override
    public boolean foundLowerBound(final int lowerBound, final Assignment model) {
        this.currentLb = lowerBound;
        return !timeLimitExceeded();
    }

    @Override
    public boolean foundUpperBound(final int upperBound, final Assignment model) {
        this.currentUb = upperBound;
        return !timeLimitExceeded();
    }

    @Override
    public void finishedSolving() {
        // nothing to do here
    }

    @Override
    public int lowerBoundApproximation() {
        return this.currentLb;
    }

    @Override
    public int upperBoundApproximation() {
        return this.currentUb;
    }
}
