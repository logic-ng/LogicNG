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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import java.util.Collections;
import java.util.LinkedHashSet;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

/**
 * Boolean disjunction.
 * <p>
 * Invariants:
 * - has at least two elements
 * - does not contain duplicates
 * - does not contain complementary literals
 * - does not contain constants
 * @version 1.0
 * @since 1.0
 */
public final class Or extends NAryOperator {

  private final boolean isCNFClause;

  /**
   * Constructor.
   * @param operands the list of operands
   * @param f        the factory which created this instance
   * @param isClause is {@code true} if the formula is a clause, {@code false} otherwise
   */
  Or(final LinkedHashSet<? extends Formula> operands, final FormulaFactory f, boolean isClause) {
    super(FType.OR, operands, f);
    if (isClause) {
      this.setPredicateCacheEntry(IS_CNF, true);
      this.setTransformationCacheEntry(FACTORIZED_CNF, this);
      this.isCNFClause = true;
    } else {
      this.setPredicateCacheEntry(IS_CNF, false);
      this.isCNFClause = false;
    }
  }

  @Override
  public boolean evaluate(final Assignment assignment) {
    for (Formula op : operands)
      if (op.evaluate(assignment))
        return true;
    return false;
  }

  /**
   * Returns {@code true} if this formula is a CNF clause, {@code false} otherwise.
   * @return {@code true} if this formula is a CNF clause
   */
  public boolean isCNFClause() {
    return this.isCNFClause;
  }

  @Override
  public int hashCode() {
    return hashCode(17);
  }

  @Override
  public boolean equals(final Object other) {
    if (other == this)
      return true;
    if (other instanceof Formula && this.f == ((Formula) other).f)
      return false; // the same formula factory would have produced a == object
    if (other instanceof Or) { // this is not really efficient... but should not be done anyway!
      final LinkedHashSet<Formula> thisOps = new LinkedHashSet<>(this.operands.length);
      Collections.addAll(thisOps, this.operands);
      final LinkedHashSet<Formula> otherOps = new LinkedHashSet<>(((Or) other).operands.length);
      Collections.addAll(otherOps, ((Or) other).operands);
      return thisOps.equals(otherOps);
    }
    return false;
  }
}
