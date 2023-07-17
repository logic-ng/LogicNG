// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.parsers;

import org.antlr.v4.runtime.CommonTokenStream;
import org.logicng.formulas.FormulaFactory;

/**
 * A parser for propositional formulas.
 * <p>
 * The syntax for propositional formulas in LogicNG is:
 * <ul>
 * <li>{@code $true} for the constant TRUE</li>
 * <li>{@code $false} for the constant FALSE</li>
 * <li>{@code ~} for the negation</li>
 * <li>{@code |} for the disjunction (OR)</li>
 * <li>{@code &} for the conjunction (AND)</li>
 * <li>{@code =>} for the implication</li>
 * <li>{@code <=>} for the equivalence</li>
 * </ul>
 * Brackets are {@code (} and {@code )}. For variable names, there are the
 * following rules:
 * <ul>
 * <li>must begin with a alphabetic character, {@code _}, {@code @}, or
 * {@code #}</li>
 * <li>can only contain alphanumerical character, {@code _}, or {@code #}</li>
 * <li>{@code @} is only allowed at the beginning of the variable name and is
 * reserved for special internal variables</li>
 * </ul>
 * @version 2.4.1
 * @since 1.0
 */
public final class PropositionalParser extends FormulaParser {

    /**
     * Constructs a new parser.
     * @param f the formula factory
     */
    public PropositionalParser(final FormulaFactory f) {
        super(f);
        final PropositionalLexer lexer = new PropositionalLexer(null);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final ParserWithFormula parser = new LogicNGPropositionalParser(tokens);
        setLexerAndParser(lexer, parser);
    }
}
