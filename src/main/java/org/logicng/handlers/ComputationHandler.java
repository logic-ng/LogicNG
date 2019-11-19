package org.logicng.handlers;

/**
 * A computation handler.
 * @version 1.6.2
 * @since 1.6.2
 */
public abstract class ComputationHandler implements Handler {

  protected boolean aborted;

  @Override
  public boolean aborted() {
    return this.aborted;
  }

  @Override
  public void started() {
    this.aborted = false;
  }
}
