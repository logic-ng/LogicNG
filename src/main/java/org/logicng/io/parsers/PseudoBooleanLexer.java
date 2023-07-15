// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.parsers;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.LexerNoViableAltException;

/**
 * A lexer for pseudo boolean formulas.
 * @version 1.0
 * @since 1.0
 */
public final class PseudoBooleanLexer extends LogicNGPseudoBooleanLexer {

    /**
     * Constructs a new pseudo boolean lexer.
     * @param inputStream the input stream
     */
    public PseudoBooleanLexer(final CharStream inputStream) {
        super(inputStream);
    }

    @Override
    public void recover(final LexerNoViableAltException exception) {
        throw new LexerException(exception.getMessage());
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
