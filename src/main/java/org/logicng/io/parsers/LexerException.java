// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.parsers;

/**
 * A lexer exception for the lexers.
 * @version 1.0
 * @since 1.0
 */
public final class LexerException extends RuntimeException {

    /**
     * Constructs a new lexer exception with a given message.
     * @param message the message
     */
    public LexerException(final String message) {
        super(message);
    }
}
