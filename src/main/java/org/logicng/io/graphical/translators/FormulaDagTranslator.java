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

package org.logicng.io.graphical.translators;

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

public class FormulaDagTranslator extends GraphicalTranslator {

    FormulaDagTranslator(final GraphicalTranslatorBuilder<FormulaDagTranslator> builder) {
        super(builder.getBackgroundColor(), builder.isAlginTerminal(), builder.getEdgeStyle(), builder.getNodeStyle());
    }

    public static GraphicalTranslatorBuilder<FormulaDagTranslator> builder() {
        return new GraphicalTranslatorBuilder<>(FormulaDagTranslator::new);
    }

    public GraphicalRepresentation translate(final Formula formula) {
        return translate(formula, (f) -> this.nodeStyle);
    }

    public GraphicalRepresentation translate(final Formula formula, final StyleMapper<Formula> styleMapper) {
        final Map<Formula, GraphicalNode> nodes = new HashMap<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        for (final Literal lit : formula.literals()) {
            final String label = (lit.phase() ? "" : "¬") + lit.name();
            final GraphicalNode literalNode = new GraphicalNode(ID + nodes.size(), label, true, styleMapper.computeStyle(lit));
            graphicalRepresentation.addNode(literalNode);
            nodes.put(lit, literalNode);
        }
        walkFormula(formula, graphicalRepresentation, nodes, styleMapper);
        return graphicalRepresentation;
    }

    private GraphicalNode walkFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes,
                                      final StyleMapper<Formula> styleMapper) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                final Pair<GraphicalNode, Boolean> constPair = addNode(formula, formula.toString(), true, graphicalRepresentation, nodes, styleMapper);
                return constPair.first();
            case LITERAL:
                return nodes.get(formula); // since this is a literal, it has to be already present
            case PBC:
                final Pair<GraphicalNode, Boolean> pbPair = addNode(formula, formula.toString(), false, graphicalRepresentation, nodes, styleMapper);
                if (!pbPair.second()) {
                    for (final Formula operand : ((PBConstraint) formula).operands()) {
                        final GraphicalNode literalNode = nodes.get(operand); // since this is a literal, it has to be already present
                        graphicalRepresentation.addEdge(new GraphicalEdge(pbPair.first(), literalNode, this.edgeStyle));
                    }
                }
                return pbPair.first();
            case NOT:
                return walkNotString((Not) formula, graphicalRepresentation, nodes, styleMapper);
            case IMPL:
            case EQUIV:
                return walkBinaryString((BinaryOperator) formula, graphicalRepresentation, nodes, styleMapper);
            case AND:
            case OR:
                return walkNaryString((NAryOperator) formula, graphicalRepresentation, nodes, styleMapper);
            default:
                throw new IllegalArgumentException("Cannot write the formula type " + formula.type());
        }
    }

    private GraphicalNode walkNotString(final Not not, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes,
                                        final StyleMapper<Formula> styleMapper) {
        final Pair<GraphicalNode, Boolean> pair = addNode(not, "¬", false, graphicalRepresentation, nodes, styleMapper);
        if (!pair.second()) {
            final GraphicalNode operandNode = walkFormula(not.operand(), graphicalRepresentation, nodes, styleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, this.edgeStyle));
        }
        return pair.first();
    }

    private GraphicalNode walkBinaryString(final BinaryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                           final Map<Formula, GraphicalNode> nodes, final StyleMapper<Formula> styleMapper) {
        final boolean isImpl = op.type() == FType.IMPL;
        final String label = isImpl ? "⇒" : "⇔";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes, styleMapper);
        if (!pair.second()) {
            final GraphicalNode leftNode = walkFormula(op.left(), graphicalRepresentation, nodes, styleMapper);
            final GraphicalNode rightNode = walkFormula(op.right(), graphicalRepresentation, nodes, styleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), leftNode, isImpl ? "l" : null, this.edgeStyle));
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), rightNode, isImpl ? "r" : null, this.edgeStyle));
        }
        return pair.first();
    }

    private GraphicalNode walkNaryString(final NAryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                         final Map<Formula, GraphicalNode> nodes, final StyleMapper<Formula> styleMapper) {
        final String label = op.type() == FType.AND ? "∧" : "∨";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes, styleMapper);
        if (!pair.second()) {
            for (final Formula operand : op) {
                final GraphicalNode operandNode = walkFormula(operand, graphicalRepresentation, nodes, styleMapper);
                graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, this.edgeStyle));
            }
        }
        return pair.first();
    }

    private static Pair<GraphicalNode, Boolean> addNode(final Formula formula, final String label, final boolean terminal,
                                                        final GraphicalRepresentation graphicalRepresentation,
                                                        final Map<Formula, GraphicalNode> nodes, final StyleMapper<Formula> styleMapper) {
        GraphicalNode node = nodes.get(formula);
        if (node == null) {
            node = new GraphicalNode(ID + nodes.size(), label, terminal, styleMapper.computeStyle(formula));
            graphicalRepresentation.addNode(node);
            nodes.put(formula, node);
            return new Pair<>(node, false);
        } else {
            return new Pair<>(node, true);
        }
    }
}
