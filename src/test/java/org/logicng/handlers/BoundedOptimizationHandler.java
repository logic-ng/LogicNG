package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

import java.util.function.Supplier;

/**
 * Bounded optimization handler for testing purposes.
 * <p>
 * The handler aborts the optimization if a certain number of starts or a certain number of SAT handler starts is reached.
 * @version 2.1.0
 * @since 2.1.0
 */
public class BoundedOptimizationHandler implements OptimizationHandler {
    private final SATHandler satHandler;
    private final int startsLimit;
    private int numStarts;
    private boolean aborted;

    /**
     * Constructs a new instance with the given starts limits.
     * @param satHandlerStartsLimit the number of starts limit for the SAT handler, if -1 then no limit is set
     * @param startsLimit           the number of starts limit, if -1 then no limit is set
     */
    public BoundedOptimizationHandler(final int satHandlerStartsLimit, final int startsLimit) {
        this.satHandler = new BoundedSatHandler(satHandlerStartsLimit);
        this.startsLimit = startsLimit;
        this.numStarts = 0;
    }

    @Override
    public boolean aborted() {
        return satHandler.aborted() || this.aborted;
    }

    @Override
    public void started() {
        this.aborted = startsLimit != -1 && ++numStarts >= startsLimit;
    }

    @Override
    public SATHandler satHandler() {
        return satHandler;
    }

    @Override
    public boolean foundBetterBound(final Supplier<Assignment> currentResultProvider) {
        return !aborted;
    }
}
