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

package org.logicng.solvers.functions.modelenumeration.splitvariablesprovider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.SortedSet;

/**
 * A split variable provider which provides split variables which occur particularly often in the formulas on the solver. The variables occurring in the
 * formulas are sorted by their occurrence. This provider returns those variables with the biggest occurrence.
 * @version 2.5.0
 * @since 2.5.0
 */
public class MostCommonVariablesProvider extends SplitVariableProviderWithTakeRate {

    /**
     * Creates a split variable provider returning the most common variables with a take rate of {@code 0.5}.
     */
    public MostCommonVariablesProvider() {
        super(0.5, 18);
    }

    /**
     * Creates a split variable provider returning the most common variables.
     * <p>
     * The take rate specifies the number of variables which should be returned in {@link #getSplitVars}.
     * So the result will contain {@code Math.min(maximumNumberOfVariables, (int) Math.ceil(variables.size() * takeRate))} variables.
     * @param takeRate                 the take rate, must be &gt; 0 and &lt;=1
     * @param maximumNumberOfVariables the maximum number of variables which should be selected
     */
    public MostCommonVariablesProvider(final double takeRate, final int maximumNumberOfVariables) {
        super(takeRate, maximumNumberOfVariables);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
        return chooseVariablesByOccurrences(solver, variables, true);
    }
}
