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
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Factor out simplification.
 * <p>
 * Reduces the length for a formula by applying factor out operations.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FactorOutSimplifier implements FormulaTransformation {

    private final RatingFunction<? extends Number> ratingFunction;

    /**
     * Constructs a new factor out simplification with the given rating function.
     * @param ratingFunction the rating function
     */
    public FactorOutSimplifier(final RatingFunction<? extends Number> ratingFunction) {
        this.ratingFunction = ratingFunction;
    }

    /**
     * Constructs a new factor out simplification with the default rating function {@link DefaultRatingFunction}.
     */
    public FactorOutSimplifier() {
        this(new DefaultRatingFunction());
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        Formula last;
        Formula simplified = formula;
        do {
            last = simplified;
            simplified = applyRec(last, cache);
        } while (!simplified.equals(last));
        return simplified;
    }

    private Formula applyRec(final Formula formula, final boolean cache) {
        switch (formula.type()) {
            case OR:
            case AND:
                final List<Formula> newOps = new ArrayList<>();
                for (final Formula op : formula) {
                    newOps.add(apply(op, cache));
                }
                final Formula newFormula = formula.factory().naryOperator(formula.type(), newOps);
                return newFormula instanceof NAryOperator ? simplify((NAryOperator) newFormula) : newFormula;
            case NOT:
                return apply(((Not) formula).operand(), cache).negate();
            case FALSE:
            case TRUE:
            case LITERAL:
            case IMPL:
            case EQUIV:
            case PBC:
                return formula;
            default:
                throw new IllegalStateException("Unknown formula type: " + formula.type());
        }
    }

    private Formula simplify(final NAryOperator formula) {
        final Formula simplified = factorOut(formula);
        return simplified == null || this.ratingFunction.apply(formula, true).doubleValue() <= this.ratingFunction.apply(simplified, true).doubleValue()
                ? formula : simplified;
    }

    private static Formula factorOut(final NAryOperator formula) {
        final Formula factorOutFormula = computeMaxOccurringSubformula(formula);
        if (factorOutFormula == null) {
            return null;
        }
        final FormulaFactory f = formula.factory();
        final FType type = formula.type();
        final List<Formula> formulasWithRemoved = new ArrayList<>();
        final List<Formula> unchangedFormulas = new ArrayList<>();
        for (final Formula operand : formula) {
            if (operand.type() == FType.LITERAL) {
                if (operand.equals(factorOutFormula)) {
                    formulasWithRemoved.add(f.constant(type == FType.OR));
                } else {
                    unchangedFormulas.add(operand);
                }
            } else if (operand.type() == FType.AND || operand.type() == FType.OR) {
                boolean removed = false;
                final List<Formula> newOps = new ArrayList<>();
                for (final Formula op : operand) {
                    if (!op.equals(factorOutFormula)) {
                        newOps.add(op);
                    } else {
                        removed = true;
                    }
                }
                (removed ? formulasWithRemoved : unchangedFormulas).add(f.naryOperator(operand.type(), newOps));
            } else {
                unchangedFormulas.add(operand);
            }
        }
        return f.naryOperator(type, f.naryOperator(type, unchangedFormulas),
                f.naryOperator(dual(type), factorOutFormula, f.naryOperator(type, formulasWithRemoved)));
    }

    private static Formula computeMaxOccurringSubformula(final NAryOperator formula) {
        final Map<Formula, Integer> formulaCounts = new HashMap<>();
        for (final Formula operand : formula) {
            if (operand.type() == FType.LITERAL) {
                formulaCounts.merge(operand, 1, Integer::sum);
            } else if (operand.type() == FType.AND || operand.type() == FType.OR) {
                for (final Formula subOperand : operand) {
                    formulaCounts.merge(subOperand, 1, Integer::sum);
                }
            }
        }
        final Pair<Formula, Integer> max = formulaCounts.entrySet().stream()
                .max(Comparator.comparingInt(Map.Entry::getValue))
                .map(e -> new Pair<>(e.getKey(), e.getValue()))
                .orElse(new Pair<>(null, 0));
        return max.second() < 2 ? null : max.first();
    }
}
