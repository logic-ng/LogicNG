package org.logicng.handlers;

/**
 * Interface for a handler.  A handler can be used as callback for different time-intensive computations in order
 * to abort these computations.  There are same often used default handlers already implemented and users can
 * implement their own handlers by implementing the respective interfaces.
 * @version 1.6.2
 * @since 1.6.2
 */
public interface Handler {

  /**
   * Returns whether the computation was aborted by the handler.
   * @return {@code true} if the computation was aborted by the handler, otherwise {@code false}
   */
  public boolean aborted();

  /**
   * This method is called when the computation starts.
   */
  public void started();
}
