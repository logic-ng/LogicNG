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

package org.logicng.np;

import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.util.CollectionHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A simple MaxSAT based implementation of an algorithm finding
 * a minimum set cover for a given collection of sets.  This
 * algorithm is really only meant for small set cover problems
 * with perhaps some tens or hundreds of set and hundreds of
 * variables.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class SetCover {

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private SetCover() {
        // Intentionally left empty
    }

    /**
     * Computes the minimum set cover for the given collection of sets,
     * i.e. a minimum number of sets s.t. each element is covered at
     * least once by a set in the cover.
     * @param sets the sets to cover
     * @param <T>  the type of the elements of the sets.  This type must implement
     *             a meaningful equals/hashCode method since it is internally
     *             used in HashSets
     * @return a minimum cover of the elements in the given sets
     */
    public static <T> List<Set<T>> compute(final Collection<Set<T>> sets) {
        if (sets.isEmpty()) {
            return Collections.emptyList();
        }
        final FormulaFactory f = new FormulaFactory();
        final Map<Variable, Set<T>> setMap = new HashMap<>();
        final Map<T, Set<Variable>> elementOccurrences = new HashMap<>();
        for (final Set<T> set : sets) {
            final Variable setVar = f.variable("@SET_SEL_" + setMap.size());
            setMap.put(setVar, set);
            for (final T element : set) {
                elementOccurrences.computeIfAbsent(element, i -> new LinkedHashSet<>()).add(setVar);
            }
        }
        final MaxSATSolver solver = MaxSATSolver.msu3();
        for (final Set<Variable> occurrences : elementOccurrences.values()) {
            solver.addHardFormula(f.or(occurrences));
        }
        for (final Variable setVar : setMap.keySet()) {
            solver.addSoftFormula(setVar.negate(), 1);
        }
        if (solver.solve() != MaxSAT.MaxSATResult.OPTIMUM) {
            throw new IllegalStateException("Internal optimization problem was not feasible.");
        }
        final ArrayList<Variable> minimumCover = CollectionHelper.intersection(solver.model().positiveVariables(), setMap.keySet(), ArrayList::new);
        final List<Set<T>> result = new ArrayList<>();
        for (final Variable setVar : minimumCover) {
            result.add(setMap.get(setVar));
        }
        return result;
    }
}
