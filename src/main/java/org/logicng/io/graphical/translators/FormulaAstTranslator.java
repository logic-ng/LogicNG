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

public class FormulaAstTranslator extends GraphicalTranslator {

    FormulaAstTranslator(final GraphicalTranslatorBuilder<FormulaAstTranslator> builder) {
        super(builder.getBackgroundColor(), builder.isAlginTerminal(), builder.getEdgeStyle(), builder.getNodeStyle());
    }

    public static GraphicalTranslatorBuilder<FormulaAstTranslator> builder() {
        return new GraphicalTranslatorBuilder<>(FormulaAstTranslator::new);
    }

    public GraphicalRepresentation translate(final Formula formula) {
        return translate(formula, (f) -> this.nodeStyle);
    }

    public GraphicalRepresentation translate(final Formula formula, final StyleMapper<Formula> styleMapper) {
        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        walkFormula(formula, graphicalRepresentation, styleMapper);
        return graphicalRepresentation;
    }

    private GraphicalNode walkFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return walkAtomicFormula(formula, graphicalRepresentation, styleMapper);
            case PBC:
                return walkPBConstraint(formula, graphicalRepresentation, styleMapper);
            case NOT:
                return walkNotString((Not) formula, graphicalRepresentation, styleMapper);
            case IMPL:
            case EQUIV:
                return walkBinaryString((BinaryOperator) formula, graphicalRepresentation, styleMapper);
            case AND:
            case OR:
                return walkNaryString((NAryOperator) formula, graphicalRepresentation, styleMapper);
            default:
                throw new IllegalArgumentException("Cannot write the formula type " + formula.type());
        }
    }

    private GraphicalNode walkAtomicFormula(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final String label = formula.type() == FType.LITERAL ? litString((Literal) formula) : formula.toString();
        return addNode(formula, label, true, graphicalRepresentation, styleMapper);
    }

    private GraphicalNode walkPBConstraint(final Formula formula, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final GraphicalNode pbNode = addNode(formula, formula.toString(), false, graphicalRepresentation, styleMapper);
        for (final Literal operand : ((PBConstraint) formula).operands()) {
            final GraphicalNode literalNode = addNode(operand, litString(operand), true, graphicalRepresentation, styleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(pbNode, literalNode, this.edgeStyle));
        }
        return pbNode;
    }

    private GraphicalNode walkNotString(final Not not, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final GraphicalNode node = addNode(not, "¬", false, graphicalRepresentation, styleMapper);
        final GraphicalNode operandNode = walkFormula(not.operand(), graphicalRepresentation, styleMapper);
        graphicalRepresentation.addEdge(new GraphicalEdge(node, operandNode, this.edgeStyle));
        return node;
    }

    private GraphicalNode walkBinaryString(final BinaryOperator op, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final boolean isImpl = op.type() == FType.IMPL;
        final String label = isImpl ? "⇒" : "⇔";
        final GraphicalNode node = addNode(op, label, false, graphicalRepresentation, styleMapper);
        final GraphicalNode leftNode = walkFormula(op.left(), graphicalRepresentation, styleMapper);
        final GraphicalNode rightNode = walkFormula(op.right(), graphicalRepresentation, styleMapper);
        graphicalRepresentation.addEdge(new GraphicalEdge(node, leftNode, isImpl ? "l" : null, this.edgeStyle));
        graphicalRepresentation.addEdge(new GraphicalEdge(node, rightNode, isImpl ? "r" : null, this.edgeStyle));
        return node;
    }

    private GraphicalNode walkNaryString(final NAryOperator op, final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final String label = op.type() == FType.AND ? "∧" : "∨";
        final GraphicalNode node = addNode(op, label, false, graphicalRepresentation, styleMapper);
        for (final Formula operand : op) {
            final GraphicalNode operandNode = walkFormula(operand, graphicalRepresentation, styleMapper);
            graphicalRepresentation.addEdge(new GraphicalEdge(node, operandNode, this.edgeStyle));
        }
        return node;
    }

    private static GraphicalNode addNode(final Formula formula, final String label, final boolean terminal,
                                         final GraphicalRepresentation graphicalRepresentation, final StyleMapper<Formula> styleMapper) {
        final GraphicalNode node = new GraphicalNode(ID + graphicalRepresentation.getNodes().size(), label, terminal, styleMapper.computeStyle(formula));
        graphicalRepresentation.addNode(node);
        return node;
    }

    private static String litString(final Literal literal) {
        return (literal.phase() ? "" : "¬") + literal.name();
    }
}
