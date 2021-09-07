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

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.PredicateCacheEntry;

import java.util.LinkedHashSet;

/**
 * Boolean disjunction.
 * <p>
 * Invariants:
 * - has at least two elements
 * - does not contain duplicates
 * - does not contain complementary literals
 * - does not contain constants
 * @version 2.2.0
 * @since 1.0
 */
public final class Or extends NAryOperator {

    /**
     * Constructor.
     * @param operands the list of operands
     * @param f        the factory which created this instance
     */
    Or(final LinkedHashSet<? extends Formula> operands, final FormulaFactory f) {
        super(FType.OR, operands, f);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        for (final Formula op : this.operands) {
            if (op.evaluate(assignment)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns {@code true} if this formula is a CNF clause, {@code false} otherwise.
     * @return {@code true} if this formula is a CNF clause
     */
    public boolean isCNFClause() {
        return this.f.predicateCacheEntry(this, PredicateCacheEntry.IS_CNF) == Tristate.TRUE;
    }

    @Override
    public int hashCode() {
        return hashCode(17);
    }

    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other instanceof Formula && this.f == ((Formula) other).f) {
            return false; // the same formula factory would have produced a == object
        }
        if (other instanceof Or) { // this is not really efficient... but should not be done anyway!
            return compareOperands(((Or) other).operands);
        }
        return false;
    }
}
