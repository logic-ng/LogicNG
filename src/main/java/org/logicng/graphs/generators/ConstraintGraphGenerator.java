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

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Graph;

import java.util.Collection;

/**
 * A graph generator which generates a constraint graph for a
 * given list of formulas.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class ConstraintGraphGenerator {

    /**
     * Private constructor.
     */
    private ConstraintGraphGenerator() {
        // Intentionally left empty.
    }

    /**
     * Constructs the constraint graph.
     * @param formula the formula in extended CNF
     * @return the constraint graph for the given formula
     */
    public static Graph<Variable> generateFromCnf(final Formula formula) {
        final Graph<Variable> constraintGraph = new Graph<>();
        addToGraph(formula, constraintGraph);
        return constraintGraph;
    }

    /**
     * Constructs the constraint graph.
     * @param formulas the formulas in extended CNF as set of CNFs
     * @return the constraint graph for the given formula
     */
    public static Graph<Variable> generateFromCnf(final Collection<Formula> formulas) {
        final Graph<Variable> constraintGraph = new Graph<>();
        for (final Formula clause : formulas) {
            addToGraph(clause, constraintGraph);
        }
        return constraintGraph;
    }

    /**
     * Constructs the constraint graph.
     * @param formulas the formulas
     * @return the constraint graph for the given formula
     */
    public static Graph<Variable> generateFromFormulas(final Collection<Formula> formulas) {
        final Graph<Variable> constraintGraph = new Graph<>();
        for (final Formula formula : formulas) {
            addClause(formula, constraintGraph);
        }
        return constraintGraph;
    }

    private static void addToGraph(final Formula formula, final Graph<Variable> constraintGraph) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                break;
            case LITERAL:
            case OR:
            case PBC:
                addClause(formula, constraintGraph);
                break;
            case AND:
                for (final Formula clause : formula) {
                    addClause(clause, constraintGraph);
                }
                break;
            default:
                throw new IllegalArgumentException("Can only generate a constraint graph from a CNF");
        }
    }

    private static void addClause(final Formula clause, final Graph<Variable> graph) {
        final Variable[] variables = clause.variables().toArray(new Variable[0]);
        if (variables.length == 1) {
            graph.node(variables[0]);
        }
        for (int i = 0; i < variables.length; i++) {
            for (int j = i + 1; j < variables.length; j++) {
                graph.connect(graph.node(variables[i]), graph.node(variables[j]));
            }
        }
    }
}
