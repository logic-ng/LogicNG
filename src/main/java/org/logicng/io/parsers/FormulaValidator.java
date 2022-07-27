///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.io.parsers;

import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;

/**
 * Super class for a formula validators.
 * <p>
 * In contrast to a {@link FormulaParser formula parser}, a validator just verifies if a formula <i>can</i> be parsed,
 * but does not actually create formulas and thus does not need an external {@link FormulaFactory formula factory}.
 * <p>
 * Since no formulas are created, validators are a bit faster than their corresponding parsers.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class FormulaValidator {

    private final FormulaFactory f = new NopFormulaFactory();
    private final Lexer lexer;
    private final ParserWithFormula parser;

    /**
     * Constructor.
     * @param parserProvider provider for the underlying parser to be used to validate formulas
     */
    protected FormulaValidator(final Function<CommonTokenStream, ParserWithFormula> parserProvider) {
        this.lexer = new PseudoBooleanLexer(null);
        this.lexer.removeErrorListeners();
        this.parser = parserProvider.apply(new CommonTokenStream(this.lexer));
        this.parser.setFormulaFactory(this.f);
        this.parser.removeErrorListeners();
        this.parser.setErrorHandler(new BailErrorStrategy());
    }

    /**
     * Validates the formula coming from a given input stream.
     * @param inputStream an input stream
     * @return {@code true} if the formula can be parsed, otherwise an exception is thrown
     * @throws ParserException if there was a problem with the input stream or the formula could not be parsed
     */
    public boolean validate(final InputStream inputStream) throws ParserException {
        if (inputStream == null) {
            return true;
        }
        try {
            final CharStream input = CharStreams.fromStream(inputStream);
            this.lexer.setInputStream(input);
            final CommonTokenStream tokens = new CommonTokenStream(this.lexer);
            this.parser.setInputStream(tokens);
            return this.parser.getParsedFormula() != null;
        } catch (final IOException e) {
            throw new ParserException("IO exception when parsing the formula", e);
        } catch (final ParseCancellationException e) {
            throw new ParserException("Parse cancellation exception when parsing the formula", e);
        } catch (final LexerException e) {
            throw new ParserException("Lexer exception when parsing the formula.", e);
        }
    }

    /**
     * Validates the formula in a given string.
     * @param in a string
     * @return {@code true} if the formula can be parsed, otherwise an exception is thrown
     * @throws ParserException if the string was not a valid formula
     */
    public boolean validate(final String in) throws ParserException {
        if (in == null || in.isEmpty()) {
            return true;
        }
        try {
            final CharStream input = CharStreams.fromString(in);
            this.lexer.setInputStream(input);
            final CommonTokenStream tokens = new CommonTokenStream(this.lexer);
            this.parser.setInputStream(tokens);
            return this.parser.getParsedFormula() != null;
        } catch (final ParseCancellationException e) {
            throw new ParserException("Parse cancellation exception when parsing the formula", e);
        } catch (final LexerException e) {
            throw new ParserException("Lexer exception when parsing the formula.", e);
        }
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }

    private static class NopFormulaFactory extends FormulaFactory {
        private final Variable dummyVar = super.variable("X");

        @Override
        public Formula implication(final Formula leftIn, final Formula rightIn) {
            return this.dummyVar;
        }

        @Override
        public Formula equivalence(final Formula leftIn, final Formula rightIn) {
            return this.dummyVar;
        }

        @Override
        public Formula not(final Formula formula) {
            return this.dummyVar;
        }

        @Override
        public Formula and(final Formula... operands) {
            return this.dummyVar;
        }

        @Override
        public Formula and(final Collection<? extends Formula> operands) {
            return this.dummyVar;
        }

        @Override
        public Formula or(final Formula... operands) {
            return this.dummyVar;
        }

        @Override
        public Formula or(final Collection<? extends Formula> operands) {
            return this.dummyVar;
        }

        @Override
        public Literal literal(final String name, final boolean phase) {
            return this.dummyVar;
        }

        @Override
        public Variable variable(final String name) {
            return this.dummyVar;
        }

        @Override
        public Formula pbc(final CType comparator, final int rhs, final List<? extends Literal> literals, final List<Integer> coefficients) {
            return this.dummyVar;
        }

        @Override
        public Formula pbc(final CType comparator, final int rhs, final Literal[] literals, final int[] coefficients) {
            return this.dummyVar;
        }

        @Override
        public Formula cc(final CType comparator, final int rhs, final Collection<Variable> variables) {
            return this.dummyVar;
        }

        @Override
        public Formula cc(final CType comparator, final int rhs, final Variable... variables) {
            return this.dummyVar;
        }
    }
}
