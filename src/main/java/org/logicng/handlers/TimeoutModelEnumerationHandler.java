package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * A model enumeration handler which cancels the computation process after a given timeout.
 * @version 1.6.2
 * @since 1.0
 */
public class TimeoutModelEnumerationHandler extends TimeoutHandler implements ModelEnumerationHandler {

  private TimeoutSATHandler satHandler;

  /**
   * Constructs a new instance with a given timeout in milliseconds.
   * <p>
   * Note that it might take a few milliseconds more until the computation is actually
   * canceled, since the handler depends on the next found model.
   * @param timeout the timeout in milliseconds
   */
  public TimeoutModelEnumerationHandler(final long timeout) {
    super(timeout);
  }

  @Override
  public SATHandler satHandler() {
    this.satHandler = new TimeoutSATHandler(remainingTime());
    return this.satHandler;
  }

  /**
   * Returns the remaining time until the designated end.
   * @return the remaining time in milliseconds
   */
  private long remainingTime() {
    final long remainingTime = this.designatedEnd - System.currentTimeMillis();
    return remainingTime >= 0 ? remainingTime : 0L;
  }

  @Override
  public boolean foundModel(final Assignment assignment) {
    return timeLimitExceeded();
  }

  @Override
  public boolean satSolverFinished() {
    this.aborted = this.satHandler.aborted();
    return !this.aborted;
  }
}
