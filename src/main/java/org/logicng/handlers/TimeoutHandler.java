package org.logicng.handlers;

/**
 * An abstract timeout handler.
 * @version 1.6.2
 * @since 1.6.2
 */
public abstract class TimeoutHandler extends ComputationHandler {

  protected final long timeout;
  protected long designatedEnd;

  /**
   * Constructs a new abstract timeout handler with a given timeout in milliseconds.
   * @param timeout the timeout in milliseconds
   */
  public TimeoutHandler(final long timeout) {
    this.timeout = timeout;
  }

  @Override
  public void started() {
    super.started();
    final long start = System.currentTimeMillis();
    this.designatedEnd = start + this.timeout;
  }

  /**
   * Tests if the current time exceeds the timeout limit.
   * @return {@code true} if the current time exceeds the timeout limit, otherwise {@code false}
   */
  protected boolean timeLimitExceeded() {
    this.aborted = System.currentTimeMillis() >= this.designatedEnd;
    return !this.aborted;
  }
}
