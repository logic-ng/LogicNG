// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
public class FormulaDagGraphicalGenerator extends GraphicalGenerator<Formula> {

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    FormulaDagGraphicalGenerator(final GraphicalGeneratorBuilder<FormulaDagGraphicalGenerator, Formula> builder) {
        super(builder.backgroundColor, builder.alignTerminals, builder.defaultEdgeStyle, builder.defaultNodeStyle, builder.nodeStyleMapper,
                builder.labelMapper, builder.edgeMapper);
    }

    /**
     * Returns the builder for this generator.
     * @return the builder
     */
    public static GraphicalGeneratorBuilder<FormulaDagGraphicalGenerator, Formula> builder() {
        return new GraphicalGeneratorBuilder<>(FormulaDagGraphicalGenerator::new);
    }

    /**
     * Translates a given formula's DAG into its graphical representation.
     * @param formula the formula
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final Formula formula) {
        final Map<Formula, GraphicalNode> nodes = new HashMap<>();
        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        for (final Literal lit : formula.literals()) {
            final String label = (lit.phase() ? "" : "¬") + lit.name();
            final GraphicalNode literalNode = new GraphicalNode(ID + nodes.size(), label, true, nodeStyle(lit));
            graphicalRepresentation.addNode(literalNode);
            nodes.put(lit, literalNode);
        }
        walkFormula(formula, graphicalRepresentation, nodes);
        return graphicalRepresentation;
    }

    private GraphicalNode walkFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                final Pair<GraphicalNode, Boolean> constPair = addNode(formula, formula.toString(), true, graphicalRepresentation, nodes);
                return constPair.first();
            case LITERAL:
                return nodes.get(formula); // since this is a literal, it has to be already present
            case PBC:
                return walkPBConstraint((PBConstraint) formula, graphicalRepresentation, nodes);
            case NOT:
                return walkNotFormula((Not) formula, graphicalRepresentation, nodes);
            case IMPL:
            case EQUIV:
                return walkBinaryFormula((BinaryOperator) formula, graphicalRepresentation, nodes);
            case AND:
            case OR:
                return walkNaryFormula((NAryOperator) formula, graphicalRepresentation, nodes);
            default:
                throw new IllegalArgumentException("Encountered unknown formula formula type " + formula.type());
        }
    }

    private GraphicalNode walkPBConstraint(final PBConstraint pbc, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes) {
        final Pair<GraphicalNode, Boolean> pbPair = addNode(pbc, pbc.toString(), false, graphicalRepresentation, nodes);
        if (!pbPair.second()) {
            for (final Formula operand : pbc.operands()) {
                final GraphicalNode literalNode = nodes.get(operand); // since this is a literal, it has to be already present
                graphicalRepresentation.addEdge(new GraphicalEdge(pbPair.first(), literalNode, edgeStyle(pbc, operand)));
            }
        }
        return pbPair.first();
    }

    private GraphicalNode walkNotFormula(final Not not, final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes) {
        final Pair<GraphicalNode, Boolean> pair = addNode(not, "¬", false, graphicalRepresentation, nodes);
        if (!pair.second()) {
            final GraphicalNode operandNode = walkFormula(not.operand(), graphicalRepresentation, nodes);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, edgeStyle(not, not.operand())));
        }
        return pair.first();
    }

    private GraphicalNode walkBinaryFormula(final BinaryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                            final Map<Formula, GraphicalNode> nodes) {
        final boolean isImpl = op.type() == FType.IMPL;
        final String label = isImpl ? "⇒" : "⇔";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes);
        if (!pair.second()) {
            final GraphicalNode leftNode = walkFormula(op.left(), graphicalRepresentation, nodes);
            final GraphicalNode rightNode = walkFormula(op.right(), graphicalRepresentation, nodes);
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), leftNode, isImpl ? "l" : null, edgeStyle(op, op.left())));
            graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), rightNode, isImpl ? "r" : null, edgeStyle(op, op.right())));
        }
        return pair.first();
    }

    private GraphicalNode walkNaryFormula(final NAryOperator op, final GraphicalRepresentation graphicalRepresentation,
                                          final Map<Formula, GraphicalNode> nodes) {
        final String label = op.type() == FType.AND ? "∧" : "∨";
        final Pair<GraphicalNode, Boolean> pair = addNode(op, label, false, graphicalRepresentation, nodes);
        if (!pair.second()) {
            for (final Formula operand : op) {
                final GraphicalNode operandNode = walkFormula(operand, graphicalRepresentation, nodes);
                graphicalRepresentation.addEdge(new GraphicalEdge(pair.first(), operandNode, edgeStyle(op, operand)));
            }
        }
        return pair.first();
    }

    private Pair<GraphicalNode, Boolean> addNode(final Formula formula, final String defaultLabel, final boolean terminal,
                                                 final GraphicalRepresentation graphicalRepresentation, final Map<Formula, GraphicalNode> nodes) {
        GraphicalNode node = nodes.get(formula);
        if (node == null) {
            node = new GraphicalNode(ID + nodes.size(), labelOrDefault(formula, defaultLabel), terminal, nodeStyle(formula));
            graphicalRepresentation.addNode(node);
            nodes.put(formula, node);
            return new Pair<>(node, false);
        } else {
            return new Pair<>(node, true);
        }
    }
}
