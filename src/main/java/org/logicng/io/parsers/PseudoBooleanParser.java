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
 * A parser for pseudo Boolean formulas.
 * <p>
 * The syntax for pseudo Boolean formulas in LogicNG is:
 * <ul>
 * <li>{@code $true} for the constant TRUE</li>
 * <li>{@code $false} for the constant FALSE</li>
 * <li>{@code ~} for the negation</li>
 * <li>{@code |} for the disjunction (OR)</li>
 * <li>{@code &} for the conjunction (AND)</li>
 * <li>{@code =>} for the implication</li>
 * <li>{@code <=>} for the equivalence</li>
 * <li>{@code *} for the multiplication of a literal and its coefficient</li>
 * <li>{@code +} for the summation</li>
 * <li>{@code =} for equals</li>
 * <li>{@code <=} for less-equals</li>
 * <li>{@code <} for less than</li>
 * <li>{@code >=} for greater-equals</li>
 * <li>{@code >} for greater than</li>
 * </ul>
 * Brackets are {@code (} and {@code )}.  For variable names, there are the following rules:
 * <ul>
 * <li>must begin with a alphabetic character, {@code _} or {@code @}</li>
 * <li>can only contain alphanumerical character, or {@code _}</li>
 * <li>{@code @} is only allowed at the beginning of the variable name and is reserved for special internal variables</li>
 * </ul>
 * <p>
 * A valid pseudo Boolean expression is of the form {@code c_1 * l_1 + ... + c_n * l_n R k} where the {@code c_i} are coefficients,
 * {@code l_i} are literals, and {@code R} is one of {@code =, >, >=, <, <=}.
 * @version 1.2
 * @since 1.0
 */
public final class PseudoBooleanParser extends FormulaParser {

    /**
     * Constructs a new parser for pseudo boolean formulas.
     * @param f the formula factory
     */
    public PseudoBooleanParser(final FormulaFactory f) {
        super(f);
        final PseudoBooleanLexer lexer = new PseudoBooleanLexer(null);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final ParserWithFormula parser = new LogicNGPseudoBooleanParser(tokens);
        setLexerAndParser(lexer, parser);
    }
}
