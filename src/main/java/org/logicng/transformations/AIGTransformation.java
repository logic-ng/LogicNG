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

import static org.logicng.formulas.cache.TransformationCacheEntry.AIG;

import org.logicng.formulas.And;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Not;
import org.logicng.formulas.Or;
import org.logicng.formulas.cache.PredicateCacheEntry;

import java.util.LinkedHashSet;

/**
 * And-inverter-graph (AIG) transformation.  Returns the AIG of the given formula.
 * @version 1.0
 * @since 1.0
 */
public final class AIGTransformation implements FormulaTransformation {

    private FormulaFactory f;
    private boolean cache;

    @Override
    public Formula apply(final Formula formula, boolean cache) {
        this.f = formula.factory();
        this.cache = cache;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return formula;
            case NOT:
                return transformNot((Not) formula);
            case IMPL:
                return transformImplication((Implication) formula);
            case EQUIV:
                return transformEquivalence((Equivalence) formula);
            case AND:
                return transformAnd((And) formula);
            case OR:
                return transformOr((Or) formula);
            case PBC:
                return apply(formula.cnf(), cache);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private Formula transformNot(final Not not) {
        Formula aig = not.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = f.not(apply(not.operand(), cache));
            if (cache) {
                not.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformImplication(final Implication impl) {
        Formula aig = impl.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = f.not(f.and(apply(impl.left(), cache), f.not(apply(impl.right(), cache))));
            if (cache) {
                impl.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformEquivalence(final Equivalence equiv) {
        Formula aig = equiv.transformationCacheEntry(AIG);
        if (aig == null) {
            aig = f.and(f.not(f.and(apply(equiv.left(), cache), f.not(apply(equiv.right(), cache)))),
                    f.not(f.and(f.not(equiv.left()), equiv.right())));
            if (cache) {
                equiv.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformAnd(final And and) {
        Formula aig = and.transformationCacheEntry(AIG);
        if (aig == null) {
            final LinkedHashSet<Formula> nops = new LinkedHashSet<>(and.numberOfOperands());
            for (final Formula op : and) {
                nops.add(apply(op, cache));
            }
            aig = f.and(nops);
            if (cache) {
                and.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    private Formula transformOr(final Or or) {
        Formula aig = or.transformationCacheEntry(AIG);
        if (aig == null) {
            final LinkedHashSet<Formula> nops = new LinkedHashSet<>(or.numberOfOperands());
            for (final Formula op : or) {
                nops.add(f.not(apply(op, cache)));
            }
            aig = f.not(f.and(nops));
            if (cache) {
                or.setTransformationCacheEntry(AIG, aig);
                aig.setPredicateCacheEntry(PredicateCacheEntry.IS_AIG, true);
            }
        }
        return aig;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
