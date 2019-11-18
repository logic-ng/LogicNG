package org.logicng.handlers;

/**
 * An abstract timeout handler.
 * @version 1.6.2
 * @since 1.6.2
 */
public abstract class TimeoutHandler {

    protected final long timeout;
    protected long designatedEnd;
    protected boolean aborted;

    /**
     * Constructs a new abstract timeout handler with a given timeout in milliseconds.
     * @param timeout the timeout in milliseconds
     */
    public TimeoutHandler(final long timeout) {
        this.timeout = timeout;
    }

    /**
     * Returns whether the computation was aborted by the timeout handler.
     * @return {@code true} if the computation was aborted by the timeout handler, otherwise {@code false}
     */
    public boolean aborted() {
        return this.aborted;
    }

    /**
     * This method is called when the computation starts.
     */
    public void started() {
        final long start = System.currentTimeMillis();
        this.designatedEnd = start + this.timeout;
        this.aborted = false;
    }

    /**
     * Tests if the current time exceeds the timeout limit.
     * @return {@code true} if the current time exceeds the timeout limit, otherwise {@code false}
     */
    protected boolean testCurrentTime() {
        this.aborted = System.currentTimeMillis() >= this.designatedEnd;
        return !this.aborted;
    }
}
