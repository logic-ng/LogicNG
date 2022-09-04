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

package org.logicng.io.graphical.generators;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.io.graphical.GraphicalEdge;
import org.logicng.io.graphical.GraphicalNode;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.util.Pair;

import java.util.HashMap;
import java.util.Map;

/**
 * The graphical generator for representations of DAGs (directed acyclic graphs) of a formula.
 * @version 2.4.0
 * @since 2.4.0
 */
public class FormulaDagGraphicalGenerator extends GraphicalGenerator {

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    FormulaDagGraphicalGenerator(final GraphicalGeneratorBuilder<FormulaDagGraphicalGenerator> builder) {
        super(builder.getBackgroundColor(), builder.isAlginTerminal(), builder.getEdgeStyle(), builder.getNodeStyle());
    }

    /**
     * Returns the builder for this generator.
     * @return the builder
     */
    public static GraphicalGeneratorBuilder<FormulaDagGraphicalGenerator> builder() {
        return new GraphicalGeneratorBuilder<>(FormulaDagGraphicalGenerator::new);
    }

    /**
     * Translates a given formula's DAG in its graphical representation.
     * @param formula the formula
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final Formula formula) {
        return translate(formula, (f) -> this.nodeStyle);
    }

    /**
     * Translates a given formula's DAG in its graphical representation.
     * @param formula         the formula
     * @param nodeStyleMapper the node style mapper for dynamically styling nodes
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final Formula formula, final NodeStyleMapper<Formula> nodeStyleMapper) {
        final Map<Formula, GraphicalNode> nodes = new HashMap<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        for (final Literal lit : formula.literals()) {
            final String label = (lit.phase() ? "" : "¬") + lit.name();
            final GraphicalNode literalNode = new GraphicalNode(ID + nodes.size(), label, true, nodeStyleMapper.computeStyle(lit));
            graphicalRepresentation.addNode(literalNode);
            nodes.put(lit, literalNode);
        }
        walkFormula(formula, graphicalRepresentation, nodes, nodeStyleMapper);
        return graphicalRepresentation;
    }

    private GraphicalNode walkFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes,
                                      final NodeStyleMapper<Formula> nodeStyleMapper) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                final Pair<GraphicalNode, Boolean> constPair = addNode(formula, formula.toString(), true, graphicalRepresentation, nodes, nodeStyleMapper);
                return constPair.first();
            case LITERAL:
                return nodes.get(formula); // since this is a literal, it has to be already present
            case PBC:
                final Pair<GraphicalNode, Boolean> pbPair = addNode(formula, formula.toString(), false, graphicalRepresentation, nodes, nodeStyleMapper);
                if (!pbPair.second()) {
                    for (final Formula operand : ((PBConstraint) formula).operands()) {
                        final GraphicalNode literalNode = nodes.get(operand); // since this is a literal, it has to be already present
                        graphicalRepresentation.addEdge(new GraphicalEdge(pbPair.first(), literalNode, this.edgeStyle));
                    }
                }
                return pbPair.first();
            case NOT:
                return walkNotString((Not) formula, graphicalRepresentation, nodes, nodeStyleMapper);
            case IMPL:
            case EQUIV:
                return walkBinaryString((BinaryOperator) formula, graphicalRepresentation, nodes, nodeStyleMapper);
            case AND:
            case OR:
                return walkNaryString((NAryOperator) formula, graphicalRepresentation, nodes, nodeStyleMapper);
            default:
                throw new IllegalArgumentException("Cannot write the formula type " + formula.type());
        }
    }

    private GraphicalNode walkNotString(final Not not, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes,
                                        final NodeStyleMapper<Formula> nodeStyleMapper) {
        final Pair<GraphicalNode, Boolean> pair = addNode(not, "¬", false, graphicalRepresentation, nodes, nodeStyleMapper);
        if (!pair.second()) {
            final GraphicalNode operandNode = walkFormula(not.operand(), graphicalRepresentation, nodes, nodeStyleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, this.edgeStyle));
        }
        return pair.first();
    }

    private GraphicalNode walkBinaryString(final BinaryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                           final Map<Formula, GraphicalNode> nodes, final NodeStyleMapper<Formula> nodeStyleMapper) {
        final boolean isImpl = op.type() == FType.IMPL;
        final String label = isImpl ? "⇒" : "⇔";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes, nodeStyleMapper);
        if (!pair.second()) {
            final GraphicalNode leftNode = walkFormula(op.left(), graphicalRepresentation, nodes, nodeStyleMapper);
            final GraphicalNode rightNode = walkFormula(op.right(), graphicalRepresentation, nodes, nodeStyleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), leftNode, isImpl ? "l" : null, this.edgeStyle));
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), rightNode, isImpl ? "r" : null, this.edgeStyle));
        }
        return pair.first();
    }

    private GraphicalNode walkNaryString(final NAryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                         final Map<Formula, GraphicalNode> nodes, final NodeStyleMapper<Formula> nodeStyleMapper) {
        final String label = op.type() == FType.AND ? "∧" : "∨";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes, nodeStyleMapper);
        if (!pair.second()) {
            for (final Formula operand : op) {
                final GraphicalNode operandNode = walkFormula(operand, graphicalRepresentation, nodes, nodeStyleMapper);
                graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, this.edgeStyle));
            }
        }
        return pair.first();
    }

    private static Pair<GraphicalNode, Boolean> addNode(final Formula formula, final String label, final boolean terminal,
                                                        final GraphicalRepresentation graphicalRepresentation,
                                                        final Map<Formula, GraphicalNode> nodes, final NodeStyleMapper<Formula> nodeStyleMapper) {
        GraphicalNode node = nodes.get(formula);
        if (node == null) {
            node = new GraphicalNode(ID + nodes.size(), label, terminal, nodeStyleMapper.computeStyle(formula));
            graphicalRepresentation.addNode(node);
            nodes.put(formula, node);
            return new Pair<>(node, false);
        } else {
            return new Pair<>(node, true);
        }
    }
}
