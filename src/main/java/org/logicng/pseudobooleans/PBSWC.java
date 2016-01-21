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

package org.logicng.pseudobooleans;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.LinkedList;
import java.util.List;

/**
 * A sequential weight counter for the encoding of pseudo-Boolean constraints in CNF.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class PBSWC extends PBEncoder {

  private LNGVector<Literal> pbOutlits;
  private List<Formula> result;

  /**
   * Constructs a new sequential weight counter encoder.
   * @param f the formula factory
   */
  public PBSWC(final FormulaFactory f) {
    super(f);
    this.pbOutlits = new LNGVector<>();
    this.result = new LinkedList<>();
  }

  @Override
  public ImmutableFormulaList build(final Literal[] ls, final int[] cfs, int rhs) {
    if (rhs == Integer.MAX_VALUE)
      throw new IllegalArgumentException("Overflow in the Encoding");
    this.pbOutlits.clear();
    this.result.clear();
    final LNGVector<Literal> lits = new LNGVector<>(ls.length);
    for (Literal l : ls)
      lits.push(l);
    final LNGIntVector coeffs = new LNGIntVector(cfs.length);
    for (int cf : cfs)
      coeffs.push(cf);
    final LNGVector<Literal> simpLits = new LNGVector<>(lits.size());
    for (final Literal l : lits)
      simpLits.push(l);
    final LNGIntVector simpCoeffs = new LNGIntVector(coeffs);
    lits.clear();
    coeffs.clear();
    for (int i = 0; i < simpLits.size(); i++) {
      if (simpCoeffs.get(i) <= rhs) {
        lits.push(simpLits.get(i));
        coeffs.push(simpCoeffs.get(i));
      } else
        this.result.add(simpLits.get(i).negate());
    }
    if (lits.size() == 1) {
      this.result.add(lits.get(0).negate());
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    if (lits.size() == 0)
      return new ImmutableFormulaList(FType.AND, this.result);
    if (rhs == 0) {
      for (int i = 0; i < lits.size(); i++)
        this.result.add(lits.get(i).negate());
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    this.generateConstraint(rhs, lits, coeffs);
    return new ImmutableFormulaList(FType.AND, this.result);
  }

  @SuppressWarnings("unchecked")
  private void generateConstraint(int rhs, final LNGVector<Literal> lits, final LNGIntVector coeffs) {
    int n = lits.size();
    final LNGVector<LNGVector<Literal>> seqAuxiliary = new LNGVector<>(n + 1);
    for (int i = 0; i < n + 1; i++) {
      final LNGVector temp = new LNGVector();
      temp.growTo(rhs + 1);
      seqAuxiliary.push(temp);
    }
    for (int i = 1; i <= n; ++i)
      for (int j = 1; j <= rhs; ++j)
        seqAuxiliary.get(i).set(j, f.newPBVariable());

    for (int i = 1; i <= rhs; ++i)
      this.pbOutlits.push(seqAuxiliary.get(n).get(i));
    for (int i = 1; i <= n; i++) {
      int wi = coeffs.get(i - 1);
      assert wi <= rhs;
      for (int j = 1; j <= rhs; j++) {
        if (i >= 2 && i <= n && j <= rhs)
          this.result.add(f.clause(seqAuxiliary.get(i - 1).get(j).negate(), seqAuxiliary.get(i).get(j)));
        if (i <= n && j <= wi)
          this.result.add(f.clause(lits.get(i - 1).negate(), seqAuxiliary.get(i).get(j)));
        if (i >= 2 && i <= n && j <= rhs - wi)
          this.result.add(f.clause(seqAuxiliary.get(i - 1).get(j).negate(), lits.get(i - 1).negate(), seqAuxiliary.get(i).get(j + wi)));
      }
      if (i >= 2)
        this.result.add(f.clause(seqAuxiliary.get(i - 1).get(rhs + 1 - wi).negate(), lits.get(i - 1).negate()));
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
