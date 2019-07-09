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
 * Brackets are {@code (} and {@code )}.  For variable names, there are the following rules:
 * <ul>
 * <li>must begin with a alphabetic character, {@code _} or {@code @}</li>
 * <li>can only contain alphanumerical character, or {@code _}</li>
 * <li>{@code @} is only allowed at the beginning of the variable name and is reserved for special internal variables</li>
 * </ul>
 * @version 1.2
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
