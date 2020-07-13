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

package org.logicng.knowledgecompilation.bdds.orderings;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Hypergraph;
import org.logicng.graphs.datastructures.HypergraphNode;
import org.logicng.graphs.generators.HypergraphGenerator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Simple implementation of the FORCE BDD variable ordering due to Aloul, Markov, and Sakallah.  This ordering only
 * works for CNF formulas.  A formula has to be converted to CNF before this ordering is called.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class ForceOrdering implements VariableOrderingProvider {

    private static final Comparator<? super Map.Entry<HypergraphNode<Variable>, Double>> COMPARATOR = Map.Entry.comparingByValue();

    private final DFSOrdering dfsOrdering = new DFSOrdering();

    @Override
    public List<Variable> getOrder(final Formula formula) {
        final SortedSet<Variable> originalVariables = new TreeSet<>(formula.variables());
        final Formula nnf = formula.nnf();
        originalVariables.addAll(nnf.variables());
        final Formula cnf = nnf.cnf();
        final Hypergraph<Variable> hypergraph = HypergraphGenerator.fromCNF(cnf);
        final Map<Variable, HypergraphNode<Variable>> nodes = new HashMap<>();
        for (final HypergraphNode<Variable> node : hypergraph.nodes()) {
            nodes.put(node.content(), node);
        }
        final List<Variable> ordering = force(cnf, hypergraph, nodes).stream().filter(originalVariables::contains).collect(Collectors.toList());
        originalVariables.stream().filter(v -> !ordering.contains(v)).forEach(ordering::add);
        return ordering;
    }

    /**
     * Executes the main FORCE algorithm.
     * @param formula    the CNF formula for the ordering
     * @param hypergraph the hypergraph for this formula
     * @param nodes      the variable to hypergraph node mapping
     * @return the variable ordering according to the FORCE algorithm
     */
    private List<Variable> force(final Formula formula, final Hypergraph<Variable> hypergraph,
                                 final Map<Variable, HypergraphNode<Variable>> nodes) {
        final LinkedHashMap<HypergraphNode<Variable>, Integer> initialOrdering = createInitialOrdering(formula, nodes);
        LinkedHashMap<HypergraphNode<Variable>, Integer> lastOrdering;
        LinkedHashMap<HypergraphNode<Variable>, Integer> currentOrdering = initialOrdering;
        do {
            lastOrdering = currentOrdering;
            final LinkedHashMap<HypergraphNode<Variable>, Double> newLocations = new LinkedHashMap<>();
            for (final HypergraphNode<Variable> node : hypergraph.nodes()) {
                newLocations.put(node, node.computeTentativeNewLocation(lastOrdering));
            }
            currentOrdering = orderingFromTentativeNewLocations(newLocations);
        } while (shouldProceed(lastOrdering, currentOrdering));
        final Variable[] ordering = new Variable[currentOrdering.size()];
        for (final Map.Entry<HypergraphNode<Variable>, Integer> entry : currentOrdering.entrySet()) {
            ordering[entry.getValue()] = entry.getKey().content();
        }
        return Arrays.asList(ordering);
    }

    /**
     * Creates an initial ordering for the variables based on a DFS.
     * @param formula the CNF formula
     * @param nodes   the variable to hypergraph node mapping
     * @return the initial variable ordering
     */
    private LinkedHashMap<HypergraphNode<Variable>, Integer> createInitialOrdering(final Formula formula, final Map<Variable, HypergraphNode<Variable>> nodes) {
        final LinkedHashMap<HypergraphNode<Variable>, Integer> initialOrdering = new LinkedHashMap<>();
        final List<Variable> dfsOrder = this.dfsOrdering.getOrder(formula);
        for (int i = 0; i < dfsOrder.size(); i++) {
            initialOrdering.put(nodes.get(dfsOrder.get(i)), i);
        }
        return initialOrdering;
    }

    /**
     * Generates a new integer ordering from tentative new locations of nodes with the double weighting.
     * @param newLocations the tentative new locations
     * @return the new integer ordering
     */
    private LinkedHashMap<HypergraphNode<Variable>, Integer> orderingFromTentativeNewLocations(final LinkedHashMap<HypergraphNode<Variable>, Double> newLocations) {
        final LinkedHashMap<HypergraphNode<Variable>, Integer> ordering = new LinkedHashMap<>();
        final List<Map.Entry<HypergraphNode<Variable>, Double>> list = new ArrayList<>(newLocations.entrySet());
        list.sort(COMPARATOR);
        int count = 0;
        for (final Map.Entry<HypergraphNode<Variable>, Double> entry : list) {
            ordering.put(entry.getKey(), count++);
        }
        return ordering;
    }

    /**
     * The abortion criteria for the FORCE algorithm.
     * @param lastOrdering    the ordering of the last step
     * @param currentOrdering the ordering of the current step
     * @return {@code true} if the algorithm should proceed, {@code false} if it should stop
     */
    private boolean shouldProceed(final Map<HypergraphNode<Variable>, Integer> lastOrdering, final Map<HypergraphNode<Variable>, Integer> currentOrdering) {
        return !lastOrdering.equals(currentOrdering);
    }
}
