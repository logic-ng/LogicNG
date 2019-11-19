package org.logicng.handlers;

/**
 * A computation handler.
 * @version 1.6.2
 * @since 1.6.2
 */
public abstract class ComputationHandler {

    protected boolean aborted;

    /**
     * Returns whether the computation was aborted by the handler.
     * @return {@code true} if the computation was aborted by the handler, otherwise {@code false}
     */
    public boolean aborted() {
        return this.aborted;
    }

    /**
     * This method is called when the computation starts.
     */
    public void started() {
        this.aborted = false;
    }
}
