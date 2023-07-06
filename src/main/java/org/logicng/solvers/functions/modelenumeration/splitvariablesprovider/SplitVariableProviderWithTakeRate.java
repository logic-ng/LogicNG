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
import org.logicng.solvers.functions.VariableOccurrencesOnSolverFunction;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Super class for variable providers which always return a subset of the given variables.
 * The number of selected variables is defined by the {@link #takeRate} which is the ration
 * (between 0 and 1) of selected variables.
 * @version 2.5.0
 * @since 2.5.0
 */
public abstract class SplitVariableProviderWithTakeRate implements SplitVariableProvider {
    protected final double takeRate;
    protected final int maximumNumberOfVariables;

    /**
     * Creates a new split variable provider with the given take rate.
     * @param takeRate                 the take rate, must be &gt; 0 and &lt;=1
     * @param maximumNumberOfVariables the maximum number of variables which should be selected
     */
    protected SplitVariableProviderWithTakeRate(final double takeRate, final int maximumNumberOfVariables) {
        if (takeRate < 0 || takeRate > 1) {
            throw new IllegalArgumentException("Take rate must be a value between 0 and 1");
        }
        this.takeRate = takeRate;
        this.maximumNumberOfVariables = maximumNumberOfVariables;
    }

    /**
     * Returns a subset of the most or least common variables. The number of returned variables is
     * defined by the take rate (see {@link #numberOfVariablesToChoose}).
     * @param solver     the solver used to count the variable occurrences
     * @param variables  the variables to choose from, in case of {@code null} all variables from the solver are considered
     * @param mostCommon {@code true} is the most common variables should be selected, {@code false}
     *                   if the least common variables should be selected
     * @return a subset of the most or least common variables
     */
    protected SortedSet<Variable> chooseVariablesByOccurrences(final SATSolver solver, final Collection<Variable> variables, final boolean mostCommon) {
        final Comparator<Map.Entry<Variable, Integer>> comparator = mostCommon
                ? Map.Entry.comparingByValue(Comparator.reverseOrder())
                : Map.Entry.comparingByValue();
        final Set<Variable> vars = variables == null ? null : new HashSet<>(variables);
        final Map<Variable, Integer> variableOccurrences = solver.execute(new VariableOccurrencesOnSolverFunction(vars));
        return variableOccurrences.entrySet().stream()
                .sorted(comparator)
                .limit(numberOfVariablesToChoose(vars != null ? vars : variableOccurrences.keySet()))
                .map(Map.Entry::getKey)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Returns the number of variables which should be chosen. This depends on the number of variables
     * and the {@link #takeRate}.
     * @param variables the variables
     * @return the number of variables which should be chosen
     */
    protected int numberOfVariablesToChoose(final Collection<Variable> variables) {
        return Math.min(this.maximumNumberOfVariables, (int) Math.ceil(variables.size() * this.takeRate));
    }
}
