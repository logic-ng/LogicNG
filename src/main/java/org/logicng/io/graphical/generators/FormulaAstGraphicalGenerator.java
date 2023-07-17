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

/**
 * The graphical generator for representations of ASTs (abstract syntax trees)
 * of a formula.
 * @version 2.4.0
 * @since 2.4.0
 */
public class FormulaAstGraphicalGenerator extends GraphicalGenerator<Formula> {

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    FormulaAstGraphicalGenerator(final GraphicalGeneratorBuilder<FormulaAstGraphicalGenerator, Formula> builder) {
        super(builder.backgroundColor, builder.alignTerminals, builder.defaultEdgeStyle, builder.defaultNodeStyle,
                builder.nodeStyleMapper,
                builder.labelMapper, builder.edgeMapper);
    }

    /**
     * Returns the builder for this generator.
     * @return the builder
     */
    public static GraphicalGeneratorBuilder<FormulaAstGraphicalGenerator, Formula> builder() {
        return new GraphicalGeneratorBuilder<>(FormulaAstGraphicalGenerator::new);
    }

    /**
     * Translates a given formula's AST into its graphical representation.
     * @param formula the formula
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final Formula formula) {
        final GraphicalRepresentation graphicalRepresentation =
                new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        walkFormula(formula, graphicalRepresentation);
        return graphicalRepresentation;
    }

    private GraphicalNode walkFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return walkAtomicFormula(formula, graphicalRepresentation);
            case PBC:
                return walkPBConstraint((PBConstraint) formula, graphicalRepresentation);
            case NOT:
                return walkNotFormula((Not) formula, graphicalRepresentation);
            case IMPL:
            case EQUIV:
                return walkBinaryFormula((BinaryOperator) formula, graphicalRepresentation);
            case AND:
            case OR:
                return walkNaryFormula((NAryOperator) formula, graphicalRepresentation);
            default:
                throw new IllegalArgumentException("Encountered unknown formula type " + formula.type());
        }
    }

    private GraphicalNode walkAtomicFormula(final Formula formula,
                                            final GraphicalRepresentation graphicalRepresentation) {
        final String label = formula.type() == FType.LITERAL ? litString((Literal) formula) : formula.toString();
        return addNode(formula, label, true, graphicalRepresentation);
    }

    private GraphicalNode walkPBConstraint(final PBConstraint pbc,
                                           final GraphicalRepresentation graphicalRepresentation) {
        final GraphicalNode pbNode = addNode(pbc, pbc.toString(), false, graphicalRepresentation);
        for (final Literal operand : pbc.operands()) {
            final GraphicalNode literalNode = addNode(operand, litString(operand), true, graphicalRepresentation);
            graphicalRepresentation.addEdge(new GraphicalEdge(pbNode, literalNode, edgeStyle(pbc, operand)));
        }
        return pbNode;
    }

    private GraphicalNode walkNotFormula(final Not not, final GraphicalRepresentation graphicalRepresentation) {
        final GraphicalNode node = addNode(not, "¬", false, graphicalRepresentation);
        final GraphicalNode operandNode = walkFormula(not.operand(), graphicalRepresentation);
        graphicalRepresentation.addEdge(new GraphicalEdge(node, operandNode, edgeStyle(not, not.operand())));
        return node;
    }

    private GraphicalNode walkBinaryFormula(final BinaryOperator op,
                                            final GraphicalRepresentation graphicalRepresentation) {
        final boolean isImpl = op.type() == FType.IMPL;
        final String label = isImpl ? "⇒" : "⇔";
        final GraphicalNode node = addNode(op, label, false, graphicalRepresentation);
        final GraphicalNode leftNode = walkFormula(op.left(), graphicalRepresentation);
        final GraphicalNode rightNode = walkFormula(op.right(), graphicalRepresentation);
        graphicalRepresentation
                .addEdge(new GraphicalEdge(node, leftNode, isImpl ? "l" : null, edgeStyle(op, op.left())));
        graphicalRepresentation
                .addEdge(new GraphicalEdge(node, rightNode, isImpl ? "r" : null, edgeStyle(op, op.right())));
        return node;
    }

    private GraphicalNode walkNaryFormula(final NAryOperator op,
                                          final GraphicalRepresentation graphicalRepresentation) {
        final String label = op.type() == FType.AND ? "∧" : "∨";
        final GraphicalNode node = addNode(op, label, false, graphicalRepresentation);
        for (final Formula operand : op) {
            final GraphicalNode operandNode = walkFormula(operand, graphicalRepresentation);
            graphicalRepresentation.addEdge(new GraphicalEdge(node, operandNode, edgeStyle(op, operand)));
        }
        return node;
    }

    private GraphicalNode addNode(final Formula formula, final String defaultLabel, final boolean terminal,
                                  final GraphicalRepresentation graphicalRepresentation) {
        final GraphicalNode node =
                new GraphicalNode(ID + graphicalRepresentation.getNodes().size(), labelOrDefault(formula, defaultLabel),
                        terminal, nodeStyle(formula));
        graphicalRepresentation.addNode(node);
        return node;
    }

    private static String litString(final Literal literal) {
        return (literal.phase() ? "" : "¬") + literal.name();
    }
}
