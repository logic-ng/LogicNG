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

package org.logicng.transformations.simplification;

import static org.logicng.formulas.FType.dual;

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;

/**
 * Negation simplifier.
 * <p>
 * Reduces the number of negations for a formula in a greedy manner.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class NegationSimplifier implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Formula nnf = formula.nnf();
        if (nnf.isAtomicFormula()) {
            return nnf;
        }
        final MinimizationResult result = minimize(nnf, true);
        return getSmallestFormula(Arrays.asList(formula, nnf, result.positiveResult), true);
    }

    private MinimizationResult minimize(final Formula formula, final boolean topLevel) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case LITERAL:
                final Literal lit = (Literal) formula;
                return new MinimizationResult(lit, lit.negate());
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                final List<MinimizationResult> opResults = new ArrayList<>(nary.numberOfOperands());
                for (final Formula op : formula) {
                    opResults.add(minimize(op, false));
                }
                final List<Formula> positiveOpResults = new ArrayList<>(opResults.size());
                final List<Formula> negativeOpResults = new ArrayList<>(opResults.size());
                for (final MinimizationResult result : opResults) {
                    positiveOpResults.add(result.getPositiveResult());
                    negativeOpResults.add(result.getNegativeResult());
                }
                final Formula smallestPositive = findSmallestPositive(formula.type(), positiveOpResults, negativeOpResults, topLevel, f);
                final Formula smallestNegative = findSmallestNegative(formula.type(), negativeOpResults, smallestPositive, topLevel, f);
                return new MinimizationResult(smallestPositive, smallestNegative);
            case FALSE:
            case TRUE:
            case NOT:
            case EQUIV:
            case IMPL:
            case PBC:
                throw new IllegalStateException("Unexpected LogicNG formula type: " + formula.type());
            default:
                throw new IllegalArgumentException("Unknown LogicNG formula type: " + formula.type());
        }
    }

    private Formula findSmallestPositive(final FType type, final List<Formula> positiveOpResults, final List<Formula> negativeOpResults, final boolean topLevel,
                                         final FormulaFactory f) {
        final Formula allPositive = f.naryOperator(type, positiveOpResults);
        final List<Formula> smallerPositiveOps = new ArrayList<>();
        final List<Formula> smallerNegativeOps = new ArrayList<>();
        for (int i = 0; i < positiveOpResults.size(); i++) {
            final Formula positiveOp = positiveOpResults.get(i);
            final Formula negativeOp = negativeOpResults.get(i);
            if (formattedLength(positiveOp, false) < formattedLength(negativeOp, false)) {
                smallerPositiveOps.add(positiveOp);
            } else {
                smallerNegativeOps.add(negativeOp);
            }
        }
        final Formula partialNegative = f.naryOperator(type, f.naryOperator(type, smallerPositiveOps),
                f.not(f.naryOperator(dual(type), smallerNegativeOps)));
        return getSmallestFormula(Arrays.asList(allPositive, partialNegative), topLevel);
    }

    private Formula findSmallestNegative(final FType type, final List<Formula> negativeOpResults, final Formula smallestPositive, final boolean topLevel, final FormulaFactory f) {
        final Formula negation = f.not(smallestPositive);
        final Formula flipped = f.naryOperator(dual(type), negativeOpResults);
        return getSmallestFormula(Arrays.asList(negation, flipped), topLevel);
    }

    private Formula getSmallestFormula(final Collection<Formula> formulas, final boolean topLevel) {
        assert !formulas.isEmpty();
        return formulas.stream().min(Comparator.comparingInt(formula -> formattedLength(formula, topLevel))).get();
    }

    private int formattedLength(final Formula formula, final boolean topLevel) {
        final int length = formula.toString().length();
        if (!topLevel && formula.type() == FType.OR) {
            return length + 2;
        } else {
            return length;
        }
    }

    private static class MinimizationResult {
        private final Formula positiveResult;
        private final Formula negativeResult;

        public MinimizationResult(final Formula positiveResult, final Formula negativeResult) {
            this.positiveResult = positiveResult;
            this.negativeResult = negativeResult;
        }

        public Formula getPositiveResult() {
            return this.positiveResult;
        }

        public Formula getNegativeResult() {
            return this.negativeResult;
        }
    }
}
