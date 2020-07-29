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

package org.logicng.knowledgecompilation.dnnf.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfFunction;

import java.util.SortedSet;

/**
 * A DNNF - Decomposable Negation Normal Form.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class Dnnf {

    private final SortedSet<Variable> originalVariables;
    private final Formula formula;

    /**
     * Constructs a new DNNF.
     * @param originalVariables the set of original variables
     * @param dnnf              the formula of the DNNF
     */
    public Dnnf(final SortedSet<Variable> originalVariables, final Formula dnnf) {
        this.originalVariables = originalVariables;
        this.formula = dnnf;
    }

    /**
     * Executes a given DNNF function on this DNNF.
     * @param function the function
     * @param <RESULT> the result type
     * @return the result of the function application
     */
    public <RESULT> RESULT execute(final DnnfFunction<RESULT> function) {
        return function.apply(this.originalVariables, this.formula);
    }

    /**
     * Returns the formula of the DNNF.
     * @return the formula
     */
    public Formula formula() {
        return this.formula;
    }

    /**
     * Returns the original variables of the formula
     * @return the original variables
     */
    public SortedSet<Variable> getOriginalVariables() {
        return this.originalVariables;
    }
}
