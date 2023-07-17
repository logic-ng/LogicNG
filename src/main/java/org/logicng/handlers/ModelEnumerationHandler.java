// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.handlers;

import org.logicng.datastructures.Assignment;

/**
 * Interface for a handler for the enumeration of models.
 * @version 2.1.0
 * @since 1.0
 */
public interface ModelEnumerationHandler extends Handler {

    /**
     * Returns a SAT handler which can be used to cancel internal SAT calls of
     * the model enumeration process.
     * @return a SAT handler
     */
    SATHandler satHandler();

    @Override
    default boolean aborted() {
        return satHandler() != null && satHandler().aborted();
    }

    /**
     * This method is called every time a model is found.
     * @param assignment the respective model
     * @return {@code true} if more models should be searched, otherwise
     *         {@code false}
     */
    boolean foundModel(Assignment assignment);
}
