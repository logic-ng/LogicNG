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

package org.logicng.transformations.qe;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Variable;

import java.util.Arrays;
import java.util.Collection;

/**
 * This transformation eliminates a number of existentially quantified variables by replacing them with the Shannon
 * expansion.  If {@code x} is eliminated from a formula {@code f}, the resulting formula is
 * {@code f[true/x] | f[false/x]}.
 * <p>
 * This transformation cannot be cached since it is dependent on the set of literals to eliminate.
 * @version 1.0
 * @since 1.0
 */
public final class ExistentialQuantifierElimination implements FormulaTransformation {

    private final Variable[] elimination;

    /**
     * Constructs a new existential quantifier elimination for the given variables.
     * @param variables the variables
     */
    public ExistentialQuantifierElimination(final Variable... variables) {
        this.elimination = Arrays.copyOf(variables, variables.length);
    }

    /**
     * Constructs a new existential quantifier elimination for a given collection of variables.
     * @param variables the collection of variables
     */
    public ExistentialQuantifierElimination(final Collection<Variable> variables) {
        this.elimination = variables.toArray(new Variable[0]);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        Formula result = formula;
        final FormulaFactory f = formula.factory();
        for (final Variable var : this.elimination) {
            result = f.or(result.restrict(new Assignment(var)), result.restrict(new Assignment(var.negate())));
        }
        return result;
    }
}
