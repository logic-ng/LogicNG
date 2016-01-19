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
//  Copyright 2015 Christoph Zengler                                     //
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

package org.logicng.cardinalityconstraints;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.LinkedList;
import java.util.List;

/**
 * Encodes that at most one variable is assigned value true.  Uses the 2-product method due to Chen.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class CCAMOProduct extends CCAtMostOne {
  private final FormulaFactory f;
  private final CCAMOPure amo;

  /**
   * Constructs the naive AMO encoder.
   * @param f the formula factory
   */
  public CCAMOProduct(final FormulaFactory f) {
    this.f = f;
    this.amo = new CCAMOPure(f);
  }

  @Override
  public ImmutableFormulaList build(final Variable... vars) {
    if (vars.length < 2)
      return new ImmutableFormulaList(FType.AND);
    return new ImmutableFormulaList(FType.AND, this.productRec(vars));
  }

  private List<Formula> productRec(final Variable... vars) {
    final List<Formula> result = new LinkedList<>();
    int recBound = 20;
    int n = vars.length;
    int p = (int) Math.ceil(Math.sqrt(n));
    int q = (int) Math.ceil((double) n / (double) p);
    final Variable[] us = new Variable[p];
    for (int i = 0; i < us.length; i++)
      us[i] = this.f.newCCVariable();
    final Variable[] vs = new Variable[q];
    for (int i = 0; i < vs.length; i++)
      vs[i] = this.f.newCCVariable();
    if (us.length <= recBound)
      result.addAll(this.amo.build(us).toList());
    else
      result.addAll(this.productRec(us));
    if (vs.length <= recBound)
      result.addAll(this.amo.build(vs).toList());
    else
      result.addAll(this.productRec(vs));
    for (int i = 0; i < p; i++) {
      for (int j = 0; j < q; j++) {
        final int k = i * q + j;
        if (k >= 0 && k < n) {
          result.add(this.f.clause(this.f.literal(vars[k].name(), false), us[i]));
          result.add(this.f.clause(this.f.literal(vars[k].name(), false), vs[j]));
        }
      }
    }
    return result;
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
