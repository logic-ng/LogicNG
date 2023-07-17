// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
import java.util.Comparator;
import java.util.List;

/**
 * Negation simplifier.
 * <p>
 * Reduces the number of negations for a formula in a greedy manner. The
 * criterion for the simplification is the length of the resulting formula.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class NegationSimplifier implements FormulaTransformation {

    private static final NegationSimplifier INSTANCE = new NegationSimplifier();

    /**
     * @deprecated In the next version, the standard constructor will be
     *             replaced by a private constructor. In order to instantiate an
     *             object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public NegationSimplifier() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static NegationSimplifier get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Formula nnf = formula.nnf();
        if (nnf.isAtomicFormula()) {
            return getSmallestFormula(true, formula, nnf);
        }
        final MinimizationResult result = minimize(nnf, true);
        return getSmallestFormula(true, formula, nnf, result.positiveResult);
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
                final Formula smallestPositive =
                        findSmallestPositive(formula.type(), positiveOpResults, negativeOpResults, topLevel, f);
                final Formula smallestNegative =
                        findSmallestNegative(formula.type(), negativeOpResults, smallestPositive, topLevel, f);
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

    private Formula findSmallestPositive(final FType type, final List<Formula> positiveOpResults,
                                         final List<Formula> negativeOpResults, final boolean topLevel,
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
        return getSmallestFormula(topLevel, allPositive, partialNegative);
    }

    private Formula findSmallestNegative(final FType type, final List<Formula> negativeOpResults,
                                         final Formula smallestPositive, final boolean topLevel,
                                         final FormulaFactory f) {
        final Formula negation = f.not(smallestPositive);
        final Formula flipped = f.naryOperator(dual(type), negativeOpResults);
        return getSmallestFormula(topLevel, negation, flipped);
    }

    private Formula getSmallestFormula(final boolean topLevel, final Formula... formulas) {
        assert formulas.length != 0;
        return Arrays.stream(formulas).min(Comparator.comparingInt(formula -> formattedLength(formula, topLevel)))
                .get();
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
