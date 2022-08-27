package org.logicng.handlers;

/**
 * Interface for a handler for the advanced enumeration of models.
 * @version 2.1.0
 * @since 1.0
 */
public interface AdvancedModelEnumerationHandler extends Handler {

    /**
     * Returns a SAT handler which can be used to cancel internal SAT calls of the model enumeration process.
     * @return a SAT handler
     */
    SATHandler satHandler();

    @Override
    default boolean aborted() {
        return satHandler() != null && satHandler().aborted();
    }

    boolean foundModel();

    boolean commit();

    boolean rollback();
}
