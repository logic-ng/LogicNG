// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.parsers;

import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

import java.io.IOException;
import java.io.InputStream;

/**
 * Super class for a formula parser.
 * @version 1.2
 * @since 1.2
 */
public abstract class FormulaParser {

    private final FormulaFactory f;
    private Lexer lexer;
    private ParserWithFormula parser;

    /**
     * Constructor.
     * @param f the formula factory
     */
    protected FormulaParser(final FormulaFactory f) {
        this.f = f;
    }

    /**
     * Sets the internal lexer and the parser.
     * @param lexer  the lexer
     * @param parser the parser
     */
    protected void setLexerAndParser(final Lexer lexer, final ParserWithFormula parser) {
        this.lexer = lexer;
        this.parser = parser;
        this.parser.setFormulaFactory(this.f);
        this.lexer.removeErrorListeners();
        this.parser.removeErrorListeners();
        this.parser.setErrorHandler(new BailErrorStrategy());
    }

    /**
     * Parses and returns a given input stream.
     * @param inputStream an input stream
     * @return the {@link Formula} representation of this stream
     * @throws ParserException if there was a problem with the input stream
     */
    public Formula parse(final InputStream inputStream) throws ParserException {
        if (inputStream == null) {
            return this.f.verum();
        }
        try {
            final CharStream input = CharStreams.fromStream(inputStream);
            this.lexer.setInputStream(input);
            final CommonTokenStream tokens = new CommonTokenStream(this.lexer);
            this.parser.setInputStream(tokens);
            return this.parser.getParsedFormula();
        } catch (final IOException e) {
            throw new ParserException("IO exception when parsing the formula", e);
        } catch (final ParseCancellationException e) {
            throw new ParserException("Parse cancellation exception when parsing the formula", e);
        } catch (final LexerException e) {
            throw new ParserException("Lexer exception when parsing the formula.", e);
        }
    }

    /**
     * Parses and returns a given string.
     * @param in a string
     * @return the {@link Formula} representation of this string
     * @throws ParserException if the string was not a valid formula
     */
    public Formula parse(final String in) throws ParserException {
        if (in == null || in.isEmpty()) {
            return this.f.verum();
        }
        try {
            final CharStream input = CharStreams.fromString(in);
            this.lexer.setInputStream(input);
            final CommonTokenStream tokens = new CommonTokenStream(this.lexer);
            this.parser.setInputStream(tokens);
            return this.parser.getParsedFormula();
        } catch (final ParseCancellationException e) {
            throw new ParserException("Parse cancellation exception when parsing the formula", e);
        } catch (final LexerException e) {
            throw new ParserException("Lexer exception when parsing the formula.", e);
        }
    }

    /**
     * Returns the factory of this parser.
     * @return the factory of this parser
     */
    public FormulaFactory factory() {
        return this.f;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
