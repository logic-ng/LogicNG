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

package org.logicng.graphs.algorithms;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * This class implements an algorithm to compute the connected components of a graph.
 * @version 2.0.0
 * @since 1.2
 */
public final class ConnectedComponentsComputation {

    /**
     * Private constructor.
     */
    private ConnectedComponentsComputation() {
        // Intentionally left empty.
    }

    /**
     * Computes the set of connected components of a graph, where each component is represented by a set of nodes.
     * @param graph the graph
     * @param <T>   the type of the graph content
     * @return the set of sets of nodes representing the connected components
     */
    public static <T> Set<Set<Node<T>>> compute(final Graph<T> graph) {
        final Set<Set<Node<T>>> connectedComponents = new LinkedHashSet<>();
        final Set<Node<T>> unmarkedNodes = new LinkedHashSet<>(graph.nodes());
        while (!unmarkedNodes.isEmpty()) {
            final Set<Node<T>> connectedComp = new LinkedHashSet<>();
            deepFirstSearch(unmarkedNodes.iterator().next(), connectedComp, unmarkedNodes);
            connectedComponents.add(connectedComp);
        }
        return connectedComponents;
    }

    /**
     * Split a list of formulas in their respective connected components.  The
     * @param formulas   the list of formulas
     * @param components the connected components which should be used for the split
     * @return the list of split formulas
     */
    public static List<List<Formula>> splitFormulasByComponent(final Collection<Formula> formulas, final Set<Set<Node<Variable>>> components) {
        final Map<Set<Node<Variable>>, List<Formula>> map = new LinkedHashMap<>();
        final Map<Variable, Set<Node<Variable>>> varMap = new TreeMap<>();
        for (final Set<Node<Variable>> component : components) {
            for (final Node<Variable> variableNode : component) {
                varMap.put(variableNode.content(), component);
            }
        }
        for (final Formula formula : formulas) {
            final SortedSet<Variable> variables = formula.variables();
            if (variables.isEmpty()) {
                map.computeIfAbsent(Collections.emptySet(), l -> new ArrayList<>()).add(formula);
            } else {
                final Set<Node<Variable>> component = varMap.get(variables.first());
                if (component == null) {
                    throw new IllegalArgumentException("Could not find a component for the variable " + variables.first());
                }
                map.computeIfAbsent(component, l -> new ArrayList<>()).add(formula);
            }
        }
        return Collections.unmodifiableList(new ArrayList<>(map.values()));
    }

    private static <T> void deepFirstSearch(final Node<T> v, final Set<Node<T>> component, final Set<Node<T>> unmarkedNodes) {
        component.add(v);
        unmarkedNodes.remove(v);
        for (final Node<T> neigh : v.neighbours()) {
            if (unmarkedNodes.contains(neigh)) {
                deepFirstSearch(neigh, component, unmarkedNodes);
            }
        }
    }
}
