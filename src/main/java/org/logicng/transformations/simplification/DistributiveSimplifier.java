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

import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Not;
import org.logicng.formulas.cache.TransformationCacheEntry;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * A formula transformation which performs simplifications by applying the distributive laws.
 * @version 2.0.0
 * @since 1.3
 */
public final class DistributiveSimplifier implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final FormulaFactory f = formula.factory();
        final Formula result;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
            case PBC:
                result = formula;
                break;
            case EQUIV:
                final Equivalence equiv = (Equivalence) formula;
                result = f.equivalence(this.apply(equiv.left(), cache), this.apply(equiv.right(), cache));
                break;
            case IMPL:
                final Implication impl = (Implication) formula;
                result = f.implication(this.apply(impl.left(), cache), this.apply(impl.right(), cache));
                break;
            case NOT:
                final Not not = (Not) formula;
                result = f.not(this.apply(not.operand(), cache));
                break;
            case OR:
            case AND:
                result = distributeNAry(formula, cache, f);
                break;
            default:
                throw new IllegalStateException("Unknown formula type: " + formula.type());
        }
        if (cache) {
            formula.setTransformationCacheEntry(TransformationCacheEntry.DISTRIBUTIVE_SIMPLIFICATION, result);
        }
        return result;
    }

    private Formula distributeNAry(final Formula formula, final boolean cache, final FormulaFactory f) {
        final Formula result;
        final FType outerType = formula.type();
        final FType innerType = dual(outerType);
        final Set<Formula> operands = new LinkedHashSet<>();
        for (final Formula op : formula) {
            operands.add(this.apply(op, cache));
        }
        final Map<Formula, Set<Formula>> part2Operands = new LinkedHashMap<>();
        Formula mostCommon = null;
        int mostCommonAmount = 0;
        for (final Formula op : operands) {
            if (op.type() == innerType) {
                for (final Formula part : op) {
                    final Set<Formula> partOperands = part2Operands.computeIfAbsent(part, k -> new LinkedHashSet<>());
                    partOperands.add(op);
                    if (partOperands.size() > mostCommonAmount) {
                        mostCommon = part;
                        mostCommonAmount = partOperands.size();
                    }
                }
            }
        }
        if (mostCommon == null || mostCommonAmount == 1) {
            result = f.naryOperator(outerType, operands);
            return result;
        }
        operands.removeAll(part2Operands.get(mostCommon));
        final Set<Formula> relevantFormulas = new LinkedHashSet<>();
        for (final Formula preRelevantFormula : part2Operands.get(mostCommon)) {
            final Set<Formula> relevantParts = new LinkedHashSet<>();
            for (final Formula part : preRelevantFormula) {
                if (!part.equals(mostCommon)) {
                    relevantParts.add(part);
                }
            }
            relevantFormulas.add(f.naryOperator(innerType, relevantParts));
        }
        operands.add(f.naryOperator(innerType, mostCommon, f.naryOperator(outerType, relevantFormulas)));
        result = f.naryOperator(outerType, operands);
        return result;
    }
}
