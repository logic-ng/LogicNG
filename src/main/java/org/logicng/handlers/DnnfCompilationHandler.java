package org.logicng.handlers;

/**
 * Interface for a handler for DNNF compilations.
 * @version 2.0.0
 * @since 2.0.0
 */
public interface DnnfCompilationHandler extends Handler {

    boolean shannonExpansion();

    /**
     * This method is called when the computation ends.
     */
    void end();
}
