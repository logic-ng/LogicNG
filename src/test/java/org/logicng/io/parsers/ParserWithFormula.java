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

import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

/**
 * Abstract super class for parsers which have a formula factory and return a formula.
 * @version 1.4.0
 * @since 1.4.0
 */
public abstract class ParserWithFormula extends Parser {

    protected FormulaFactory f;

    /**
     * Constructor.
     * @param input the token stream (e.g. a lexer) for the parser
     */
    public ParserWithFormula(final TokenStream input) {
        super(input);
    }

    /**
     * Sets the LogicNG formula factory for this parser
     * @param f the LogicNG formula factory
     */
    public void setFormulaFactory(final FormulaFactory f) {
        this.f = f;
    }

    /**
     * Returns the parsed formula.
     * @return the parsed formula
     */
    public abstract Formula getParsedFormula();
}
