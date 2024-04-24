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

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_AIG;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Not;

/**
 * And-inverter-graph (AIG) predicate.  Returns {@code true} if the given formula is an AIG, {@code false} otherwise.
 * @version 1.0
 * @since 1.0
 */
public final class AIGPredicate implements FormulaPredicate {

    private final static AIGPredicate INSTANCE = new AIGPredicate();

    /**
     * Private empty constructor.  Singleton class.
     */
    private AIGPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static AIGPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_AIG);
        if (cached != Tristate.UNDEF) {
            return cached == Tristate.TRUE;
        }
        boolean result;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                result = true;
                break;
            case IMPL:
            case EQUIV:
            case OR:
            case PBC:
                result = false;
                break;
            case NOT:
                result = test(((Not) formula).operand(), cache);
                break;
            case AND:
                result = true;
                for (final Formula op : formula) {
                    if (!test(op, cache)) {
                        result = false;
                        break;
                    }
                }
                break;
            default:
                throw new IllegalArgumentException("Cannot compute AIG predicate on " + formula.type());
        }
        if (cache) {
            formula.setPredicateCacheEntry(IS_AIG, result);
        }
        return result;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
