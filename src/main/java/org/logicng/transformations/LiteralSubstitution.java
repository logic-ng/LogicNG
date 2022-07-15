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

package org.logicng.transformations;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A formula transformation which substitutes literals by other literals.
 * <p>
 * Always the best fit is chosen.  So if there are two mappings for e.g.
 * {@code a -> b} and {@code ~a -> c}.  Then {@code ~a} will be mapped to
 * {@code c} and not to {@code ~b}.  On the other hand if there is only the
 * mapping {@code a -> b}, the literal {@code ~a} will be mapped to {@code ~b}.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class LiteralSubstitution implements FormulaTransformation {

    private final Map<Literal, Literal> substitution;

    /**
     * Generate a new formula substitution with a given literal-to-literal
     * mapping.
     * @param substitution a mapping from literals to literals
     */
    public LiteralSubstitution(final Map<Literal, Literal> substitution) {
        this.substitution = substitution;
    }

    /**
     * Generates a new formula substitution with an empty literal mapping.
     */
    public LiteralSubstitution() {
        this.substitution = new HashMap<>();
    }

    /**
     * Adds a new literal mapping to this substitution.
     * @param from the literal which should be substituted
     * @param to   the literal with which the substituted literal should be replaced
     */
    public void addSubstitution(final Literal from, final Literal to) {
        this.substitution.put(from, to);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case TRUE:
            case FALSE:
                return formula;
            case LITERAL:
                final Literal literal = (Literal) formula;
                Literal lit = this.substitution.get(literal);
                if (lit != null) {
                    return lit;
                }
                if (!literal.phase()) {
                    lit = this.substitution.get(literal.variable());
                    return lit != null ? lit.negate() : formula;
                }
                return formula;
            case NOT:
                return f.not(apply(((Not) formula).operand(), cache));
            case EQUIV:
            case IMPL:
                final BinaryOperator binOp = (BinaryOperator) formula;
                return f.binaryOperator(formula.type(), apply(binOp.left(), cache), apply(binOp.right(), cache));
            case OR:
            case AND:
                final List<Formula> operands = new ArrayList<>();
                for (final Formula op : formula) {
                    operands.add(apply(op, cache));
                }
                return f.naryOperator(formula.type(), operands);
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                final Literal[] originalOperands = pbc.operands();
                final Literal[] literals = new Literal[originalOperands.length];
                for (int i = 0; i < literals.length; i++) {
                    literals[i] = (Literal) apply(originalOperands[i], false);
                }
                return f.pbc(pbc.comparator(), pbc.rhs(), literals, pbc.coefficients());
            default:
                throw new IllegalArgumentException("Unknown formula type: " + formula.type());
        }
    }
}
