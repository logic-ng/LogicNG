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

/**
 * PBLib       -- Copyright (c) 2012-2013  Peter Steinke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.pseudobooleans;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * The adder networks encoding for pseudo-Boolean constraints to CNF.
 * @version 1.1
 * @since 1.1
 */
final class PBAdderNetworks implements PBEncoding {

  private final FormulaFactory f;
  private List<Formula> formula;

  /**
   * Constructs a new pseudo-Boolean adder network.
   * @param f the formula factory
   */
  public PBAdderNetworks(final FormulaFactory f) {
    this.f = f;
  }

  private static int ldInt(int x) {
    int ldretutn = 0;
    for (int i = 0; i < 31; i++)
      if ((x & (1 << i)) > 0)
        ldretutn = i + 1;
    return ldretutn;
  }

  @Override
  public List<Formula> encode(LNGVector<Literal> lits, LNGIntVector coeffs, int rhs, List<Formula> formula) {
    this.formula = formula;
    final LNGVector<Literal> result = new LNGVector<>();
    final LNGVector<LinkedList<Literal>> buckets = new LNGVector<>();
    int nb = ldInt(rhs);
    for (int iBit = 0; iBit < nb; ++iBit) {
      buckets.push(new LinkedList<Literal>());
      result.push(null);
      for (int iVar = 0; iVar < lits.size(); iVar++)
        if (((1 << iBit) & coeffs.get(iVar)) != 0)
          buckets.back().push(lits.get(iVar));
    }
    this.adderTree(buckets, result);
    final LNGBooleanVector kBits = this.numToBits(buckets.size(), rhs);
    this.lessThanOrEqual(result, kBits, formula);
    return formula;
  }

  private void adderTree(final LNGVector<LinkedList<Literal>> buckets, final LNGVector<Literal> result) {
    Literal x;
    Literal y;
    Literal z;

    for (int i = 0; i < buckets.size(); i++) {
      if (buckets.get(i).isEmpty())
        continue;
      if (i == buckets.size() - 1 && buckets.get(i).size() >= 2) {
        buckets.push(new LinkedList<Literal>());
        result.push(null);
      }
      while (buckets.get(i).size() >= 3) {
        x = buckets.get(i).removeFirst();
        y = buckets.get(i).removeFirst();
        z = buckets.get(i).removeFirst();
        Literal xs = this.faSum(x, y, z);
        Literal xc = this.faCarry(x, y, z);
        buckets.get(i).add(xs);
        buckets.get(i + 1).add(xc);
        this.faExtra(xc, xs, x, y, z);
      }
      if (buckets.get(i).size() == 2) {
        x = buckets.get(i).removeFirst();
        y = buckets.get(i).removeFirst();
        buckets.get(i).add(this.haSum(x, y));
        buckets.get(i + 1).add(this.haCarry(x, y));
      }
      result.set(i, buckets.get(i).removeFirst());
    }
  }

  private LNGBooleanVector numToBits(int n, int num) {
    int number = num;
    final LNGBooleanVector bits = new LNGBooleanVector();
    for (int i = n - 1; i >= 0; i--) {
      int tmp = 1 << i;
      if (number < tmp)
        bits.push(false);
      else {
        bits.push(true);
        number -= tmp;
      }
    }
    bits.reverseInplace();
    return bits;
  }

  private void lessThanOrEqual(final LNGVector<Literal> xs, final LNGBooleanVector ys, final List<Formula> formula) {
    assert xs.size() == ys.size();
    final List<Literal> clause = new ArrayList<>();
    boolean skip;
    for (int i = 0; i < xs.size(); ++i) {
      if (ys.get(i) || xs.get(i) == null)
        continue;
      clause.clear();
      skip = false;
      for (int j = i + 1; j < xs.size(); ++j) {
        if (ys.get(j)) {
          if (xs.get(j) == null) {
            skip = true;
            break;
          }
          clause.add(xs.get(j).negate());
        } else {
          if (xs.get(j) == null)
            continue;
          clause.add(xs.get(j));
        }
      }
      if (skip)
        continue;
      clause.add(xs.get(i).negate());
      formula.add(this.f.clause(clause));
    }
  }

  private void faExtra(final Literal xc, final Literal xs, final Literal a, final Literal b, final Literal c) {
    this.formula.add(this.f.clause(xc.negate(), xs.negate(), a));
    this.formula.add(this.f.clause(xc.negate(), xs.negate(), b));
    this.formula.add(this.f.clause(xc.negate(), xs.negate(), c));
    this.formula.add(this.f.clause(xc, xs, a.negate()));
    this.formula.add(this.f.clause(xc, xs, b.negate()));
    this.formula.add(this.f.clause(xc, xs, c.negate()));
  }

  private Literal faCarry(final Literal a, final Literal b, final Literal c) {
    final Literal x = this.f.newPBVariable();
    this.formula.add(this.f.clause(b, c, x.negate()));
    this.formula.add(this.f.clause(a, c, x.negate()));
    this.formula.add(this.f.clause(a, b, x.negate()));
    this.formula.add(this.f.clause(b.negate(), c.negate(), x));
    this.formula.add(this.f.clause(a.negate(), c.negate(), x));
    this.formula.add(this.f.clause(a.negate(), b.negate(), x));
    return x;
  }

  private Literal faSum(final Literal a, final Literal b, final Literal c) {
    final Literal x = this.f.newPBVariable();
    this.formula.add(this.f.clause(a, b, c, x.negate()));
    this.formula.add(this.f.clause(a, b.negate(), c.negate(), x.negate()));
    this.formula.add(this.f.clause(a.negate(), b, c.negate(), x.negate()));
    this.formula.add(this.f.clause(a.negate(), b.negate(), c, x.negate()));
    this.formula.add(this.f.clause(a.negate(), b.negate(), c.negate(), x));
    this.formula.add(this.f.clause(a.negate(), b, c, x));
    this.formula.add(this.f.clause(a, b.negate(), c, x));
    this.formula.add(this.f.clause(a, b, c.negate(), x));
    return x;
  }

  private Literal haCarry(final Literal a, final Literal b) {
    final Literal x = this.f.newPBVariable();
    this.formula.add(this.f.clause(a, x.negate()));
    this.formula.add(this.f.clause(b, x.negate()));
    this.formula.add(this.f.clause(a.negate(), b.negate(), x));
    return x;
  }

  private Literal haSum(final Literal a, final Literal b) {
    final Literal x = this.f.newPBVariable();
    this.formula.add(this.f.clause(a.negate(), b.negate(), x.negate()));
    this.formula.add(this.f.clause(a, b, x.negate()));
    this.formula.add(this.f.clause(a.negate(), b, x));
    this.formula.add(this.f.clause(a, b.negate(), x));
    return x;
  }
}
