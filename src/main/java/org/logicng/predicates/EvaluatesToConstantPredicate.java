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

package org.logicng.predicates;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.And;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.Or;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Checks if the given formula is evaluated to false/true for a given (partial) assignment.
 * <p>
 * Note: If a partial assignment is given, the check only recognizes simple unsatisfiable/tautology cases
 * <ul>
 *     <li>operand of an AND/OR is false/true</li>
 *     <li>all operators of an OR/AND are false/true</li>
 *     <li>AND/OR has two operands with complementary negations</li>
 * </ul>
 * This evaluation differs from the standard formula evaluation {@link Formula#evaluate(Assignment)} in two ways. It
 * accepts partial assignments and it tries to avoid the generation of intermediate formula by the formula factory
 * objects in order to speed up the performance.
 * <p>
 * Example 01: When evaluation to false the formula (a | b) &amp; (~a | c) with partial assignment
 * [b -&gt; false, c -&gt; false] yields to a &amp; ~a which is recognized as unsatisfiable.
 * <p>
 * Example 02: When evaluation to true the formula (a &amp; b) | (~a &amp; c) with partial assignment
 * [b -&gt; false, c -&gt; false] yields to a | ~a which is recognized as a tautology.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class EvaluatesToConstantPredicate implements FormulaPredicate {

    private final boolean evaluatesToTrue;
    private final Map<Variable, Boolean> mapping;

    /**
     * Constructs a new evaluation predicate.
     * @param evaluatesToTrue {@code false} if the check aims for true, {@code false} if the check aims for false
     * @param mapping         the (partial) assignment
     */
    public EvaluatesToConstantPredicate(final boolean evaluatesToTrue, final Map<Variable, Boolean> mapping) {
        this.evaluatesToTrue = evaluatesToTrue;
        this.mapping = mapping;
    }

    /**
     * Returns the (partial) assignment.
     * @return the (partial) assignment
     */
    public Map<Variable, Boolean> getMapping() {
        return Collections.unmodifiableMap(this.mapping);
    }

    /**
     * Checks if the formula evaluates to false (or true) when the (partial) assignment is applied.
     * @param formula the formula
     * @param caching indicator for caching (currently not used)
     * @return {@code true} if applying the (partial) assignment yields to the specified constant, otherwise {@code false}
     */
    @Override
    public boolean test(final Formula formula, final boolean caching) {
        return innerTest(formula, true).type() == getConstantType(this.evaluatesToTrue);
    }

    /**
     * Restricts and possibly simplifies the formula by applying the (partial) assignment in order to decide if the restriction yields to the specified constant.
     * @param formula  the formula
     * @param topLevel indicator if the formula is the top level operator
     * @return Falsum resp. Verum if the (partial) assignment resulted not to the specified constant, otherwise the restricted and simplified formula
     */
    private Formula innerTest(final Formula formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case TRUE:
            case FALSE:
                return formula;
            case LITERAL:
                final Literal lit = (Literal) formula;
                final Boolean found = this.mapping.get(lit.variable());
                return found == null ? lit : f.constant(lit.phase() == found);
            case NOT:
                return handleNot((Not) formula, topLevel);
            case IMPL:
                return handleImplication((Implication) formula, topLevel);
            case EQUIV:
                return handleEquivalence((Equivalence) formula, topLevel);
            case OR:
                return handleOr((Or) formula, topLevel);
            case AND:
                return handleAnd((And) formula, topLevel);
            case PBC:
                return handlePBC((PBConstraint) formula);
            default:
                throw new IllegalArgumentException("Unknown formula type " + formula.type());
        }
    }

    private Formula handleNot(final Not formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        final Formula opResult = innerTest(formula.operand(), false);
        if (topLevel && !opResult.isConstantFormula()) {
            return f.constant(!this.evaluatesToTrue);
        }
        return opResult.isConstantFormula() ? f.constant(isFalsum(opResult)) : f.not(opResult);
    }

    private Formula handleImplication(final Implication formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        final Formula left = formula.left();
        final Formula right = formula.right();
        final Formula leftResult = innerTest(left, false);
        if (leftResult.isConstantFormula()) {
            if (this.evaluatesToTrue) {
                return isFalsum(leftResult) ? f.verum() : innerTest(right, topLevel);
            } else {
                return isVerum(leftResult) ? innerTest(right, topLevel) : f.verum();
            }
        }
        if (!this.evaluatesToTrue && topLevel) {
            return f.verum();
        }
        final Formula rightResult = innerTest(right, false);
        if (rightResult.isConstantFormula()) {
            return isVerum(rightResult) ? f.verum() : f.not(leftResult);
        }
        return f.implication(leftResult, rightResult);
    }

    private Formula handleEquivalence(final Equivalence formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        final Formula left = formula.left();
        final Formula right = formula.right();
        final Formula leftResult = innerTest(left, false);
        if (leftResult.isConstantFormula()) {
            return isVerum(leftResult) ? innerTest(right, topLevel) : innerTest(f.not(right), topLevel);
        }
        final Formula rightResult = innerTest(right, false);
        if (rightResult.isConstantFormula()) {
            if (topLevel) {
                return f.constant(!this.evaluatesToTrue);
            }
            return isVerum(rightResult) ? leftResult : f.not(leftResult);
        }
        return f.equivalence(leftResult, rightResult);
    }

    private Formula handleOr(final Or formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        final List<Formula> nops = new ArrayList<>();
        for (final Formula op : formula) {
            final Formula opResult = innerTest(op, !this.evaluatesToTrue && topLevel);
            if (isVerum(opResult)) {
                return f.verum();
            }
            if (!opResult.isConstantFormula()) {
                if (!this.evaluatesToTrue && topLevel) {
                    return f.verum();
                }
                nops.add(opResult);
            }
        }
        return f.or(nops);
    }

    private Formula handleAnd(final And formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        final List<Formula> nops = new ArrayList<>();
        for (final Formula op : formula) {
            final Formula opResult = innerTest(op, this.evaluatesToTrue && topLevel);
            if (isFalsum(opResult)) {
                return f.falsum();
            }
            if (!opResult.isConstantFormula()) {
                if (this.evaluatesToTrue && topLevel) {
                    return f.falsum();
                }
                nops.add(opResult);
            }
        }
        return f.and(nops);
    }

    private Formula handlePBC(final PBConstraint formula) {
        final FormulaFactory f = formula.factory();
        final Assignment assignment = new Assignment();
        for (final Map.Entry<Variable, Boolean> entry : this.mapping.entrySet()) {
            assignment.addLiteral(f.literal(entry.getKey().name(), entry.getValue()));
        }
        return formula.restrict(assignment);
    }

    private static FType getConstantType(final boolean constant) {
        return constant ? FType.TRUE : FType.FALSE;
    }

    private static boolean isFalsum(final Formula formula) {
        return formula.type() == FType.FALSE;
    }

    private static boolean isVerum(final Formula formula) {
        return formula.type() == FType.TRUE;
    }
}
