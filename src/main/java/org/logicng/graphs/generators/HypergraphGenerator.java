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

package org.logicng.graphs.generators;

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Hypergraph;
import org.logicng.graphs.datastructures.HypergraphNode;
import org.logicng.predicates.CNFPredicate;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

/**
 * A generator for hypergraphs from formulas.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class HypergraphGenerator {

    /**
     * Generates a hypergraph from a CNF given as a list of clauses.  Each variable is represented by a node in the
     * hypergraph, each clause is represented by a hyperedge between all variables of the clause.
     * @param cnf the list of clauses of the CNF for the hypergraph
     * @return the hypergraph for the CNF formula
     */
    public static Hypergraph<Variable> fromCNF(final Formula... cnf) {
        return fromCNF(Arrays.asList(cnf));
    }

    /**
     * Generates a hypergraph from a CNF given as a list of clauses.  Each variable is represented by a node in the
     * hypergraph, each clause is represented by a hyperedge between all variables of the clause.
     * @param cnf the list of clauses of the CNF for the hypergraph
     * @return the hypergraph for the CNF formula
     */
    public static Hypergraph<Variable> fromCNF(final List<Formula> cnf) {
        final Hypergraph<Variable> hypergraph = new Hypergraph<>();
        final Map<Variable, HypergraphNode<Variable>> nodes = new HashMap<>();
        for (final Formula clause : cnf) {
            switch (clause.type()) {
                case PBC:
                case EQUIV:
                case IMPL:
                case NOT:
                case AND:
                    throw new IllegalStateException("Unexpected element in clause: " + clause);
                case LITERAL:
                case OR:
                    addClause(clause, hypergraph, nodes);
                    break;
            }
        }
        return hypergraph;
    }

    /**
     * Generates a hypergraph from a CNF.  Each variable is represented by a node in the hypergraph, each clause
     * is represented by a hyperedge between all variables of the clause.
     * @param cnf the CNF formula for the hypergraph
     * @return the hypergraph for the CNF formula
     */
    public static Hypergraph<Variable> fromCNF(final Formula cnf) {
        if (!cnf.holds(CNFPredicate.get())) {
            throw new IllegalArgumentException("Cannot generate a hypergraph from a non-cnf formula");
        }
        final Hypergraph<Variable> hypergraph = new Hypergraph<>();
        final Map<Variable, HypergraphNode<Variable>> nodes = new HashMap<>();
        switch (cnf.type()) {
            case PBC:
            case EQUIV:
            case IMPL:
            case NOT:
                throw new IllegalStateException("Unexpected element in CNF: " + cnf);
            case LITERAL:
            case OR:
                addClause(cnf, hypergraph, nodes);
                break;
            case AND:
                for (final Formula clause : cnf) {
                    addClause(clause, hypergraph, nodes);
                }
                break;
        }
        return hypergraph;
    }

    /**
     * Adds a single clause to the given hypergraph and updates the variable to node mapping.
     * @param formula    the clause
     * @param hypergraph the current hypergraph
     * @param nodes      the mapping from variables in the CNF to nodes in the hypergraph
     */
    private static void addClause(final Formula formula, final Hypergraph<Variable> hypergraph, final Map<Variable, HypergraphNode<Variable>> nodes) {
        assert formula.type() == FType.LITERAL || formula.type() == FType.OR;
        final SortedSet<Variable> variables = formula.variables();
        final Set<HypergraphNode<Variable>> clause = new LinkedHashSet<>();
        for (final Variable variable : variables) {
            HypergraphNode<Variable> node = nodes.get(variable);
            if (node == null) {
                node = new HypergraphNode<>(hypergraph, variable);
                nodes.put(variable, node);
            }
            clause.add(node);
        }
        hypergraph.addEdge(clause);
    }
}
