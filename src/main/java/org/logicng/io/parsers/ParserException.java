// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.parsers;

/**
 * A parser exception for the LogicNG parsers.
 * @version 1.0
 * @since 1.0
 */
public final class ParserException extends Exception {

    /**
     * Constructs a new parser exception with a given message and inner
     * exception.
     * @param message   the message
     * @param exception the inner exception
     */
    public ParserException(final String message, final Exception exception) {
        super(message, exception);
    }
}
