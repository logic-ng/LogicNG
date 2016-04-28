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
//  Copyright 2015-2016 Christoph Zengler                                //
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

/*****************************************************************************************
 * Open-WBO -- Copyright (c) 2013-2015, Ruben Martins, Vasco Manquinho, Ines Lynce
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *****************************************************************************************/

package org.logicng.cardinalityconstraints;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.LinkedList;
import java.util.List;

/**
 * Encodes that exactly one variable is assigned value true.  Uses the Ladder/Regular encoding.
 * @version 1.0
 * @since 1.0
 */
public final class CCEXOLadder extends CCExactlyOne {

  private final FormulaFactory f;

  /**
   * Constructs a new Ladder encoder.
   * @param f the formula factory
   */
  public CCEXOLadder(final FormulaFactory f) {
    this.f = f;
  }

  @Override
  public ImmutableFormulaList build(final Variable... vars) {
    final List<Formula> result = new LinkedList<>();
    if (vars.length == 0)
      return new ImmutableFormulaList(FType.AND);
    if (vars.length == 1) {
      result.add(vars[0]);
      return new ImmutableFormulaList(FType.AND, result);
    }
    final Variable[] seqAuxiliary = new Variable[vars.length - 1];
    for (int i = 0; i < vars.length - 1; i++)
      seqAuxiliary[i] = this.f.newCCVariable();
    for (int i = 0; i < vars.length; i++) {
      if (i == 0) {
        result.add(this.f.clause(vars[0].negate(), seqAuxiliary[0]));
        result.add(this.f.clause(vars[0], seqAuxiliary[0].negate()));
      } else if (i == vars.length - 1) {
        result.add(this.f.clause(vars[i].negate(), seqAuxiliary[i - 1].negate()));
        result.add(this.f.clause(vars[i], seqAuxiliary[i - 1]));
      } else {
        result.add(this.f.clause(vars[i].negate(), seqAuxiliary[i]));
        result.add(this.f.clause(seqAuxiliary[i - 1].negate(), seqAuxiliary[i]));
        result.add(this.f.clause(vars[i].negate(), seqAuxiliary[i - 1].negate()));
        result.add(this.f.clause(vars[i], seqAuxiliary[i].negate(), seqAuxiliary[i - 1]));
      }
    }
    return new ImmutableFormulaList(FType.AND, result);
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
