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

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_DNF;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaPredicate;

/**
 * DNF predicate.  Indicates whether a formula is in DNF or not.
 * @version 1.0
 * @since 1.0
 */
public final class DNFPredicate implements FormulaPredicate {

    private final static DNFPredicate INSTANCE = new DNFPredicate();

    /**
     * Private empty constructor.  Singleton class.
     */
    private DNFPredicate() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the predicate.
     * @return the predicate instance
     */
    public static DNFPredicate get() {
        return INSTANCE;
    }

    @Override
    public boolean test(final Formula formula, final boolean cache) {
        final Tristate cached = formula.predicateCacheEntry(IS_DNF);
        if (cached != Tristate.UNDEF) {
            return cached == Tristate.TRUE;
        }
        boolean result;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return true;
            case NOT:
            case IMPL:
            case EQUIV:
            case PBC:
                return false;
            case OR:
                result = true;
                for (final Formula op : formula) {
                    if (!this.isDNFClause(op)) {
                        result = false;
                    }
                }
                break;
            case AND:
                result = this.isDNFClause(formula);
                break;
            default:
                throw new IllegalArgumentException("Cannot compute DNF predicate on " + formula.type());
        }
        if (cache) {
            formula.setPredicateCacheEntry(IS_DNF, result);
        }
        return result;
    }

    /**
     * Returns {@code true} if the given formula is a DNF minterm, {@code false} otherwise.
     * @param formula the formula
     * @return {@code true} if the given formula is a DNF minterm
     */
    private boolean isDNFClause(final Formula formula) {
        switch (formula.type()) {
            case TRUE:
            case FALSE:
            case LITERAL:
                return true;
            case IMPL:
            case EQUIV:
            case PBC:
            case NOT:
            case OR:
                return false;
            case AND:
                for (final Formula op : formula) {
                    if (op.type() != FType.LITERAL) {
                        return false;
                    }
                }
                return true;
            default:
                throw new IllegalArgumentException("Cannot compute DNF clause predicate on " + formula.type());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
