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

package org.logicng.formulas;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

import org.logicng.datastructures.Assignment;

import java.util.LinkedHashSet;

/**
 * Boolean conjunction.
 * <p>
 * Invariants:
 * - has at least two elements
 * - does not contain duplicates
 * - does not contain complementary literals
 * - does not contain constants
 * @version 2.0.0
 * @since 1.0
 */
public final class And extends NAryOperator {

    /**
     * Constructor.
     * @param operands the stream of operands
     * @param f        the factory which created this instance
     * @param isCNF    is {@code true} if the formula is in CNF, {@code false} otherwise
     */
    And(final LinkedHashSet<? extends Formula> operands, final FormulaFactory f, final boolean isCNF) {
        super(FType.AND, operands, f);
        if (isCNF) {
            this.setPredicateCacheEntry(IS_CNF, true);
            this.setTransformationCacheEntry(FACTORIZED_CNF, this);
        } else {
            this.setPredicateCacheEntry(IS_CNF, false);
        }
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        for (final Formula op : this.operands) {
            if (!op.evaluate(assignment)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return hashCode(31);
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a == object
        }
        if (other instanceof And) { // this is not really efficient... but should not be done anyway!
            return compareOperands(((And) other).operands);
        }
        return false;
    }
}
