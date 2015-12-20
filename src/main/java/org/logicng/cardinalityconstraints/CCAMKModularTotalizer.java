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
import org.logicng.collections.LNGVector;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Encodes that at most 'rhs' literals can be assigned value true.  Uses the modular totalizer encoding for
 * translating the cardinality constraint into CNF.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class CCAMKModularTotalizer extends CCAtMostK {

  private final FormulaFactory f;
  private final Literal litUndef;
  private final Literal litError;

  private Literal h0;
  private LNGVector<Literal> cardinalityInlits;
  private LNGVector<Literal> cardinalityUpOutlits;
  private LNGVector<Literal> cardinalityLwOutlits;
  private int currentCardinalityRhs;
  private List<Formula> result;

  /**
   * Constructs a new modular totalizer.
   * @param f the formula factory
   */
  public CCAMKModularTotalizer(final FormulaFactory f) {
    this.f = f;
    this.litUndef = f.literal("RESERVED@LIT_UNDEF");
    this.litError = f.literal("RESERVED@LIT_ERROR");
    this.h0 = this.litUndef;
    this.currentCardinalityRhs = -1;
    this.cardinalityInlits = new LNGVector<>();
    this.cardinalityUpOutlits = new LNGVector<>();
    this.cardinalityLwOutlits = new LNGVector<>();
    this.result = new LinkedList<>();
  }

  @Override
  public ImmutableFormulaList build(final Collection<Literal> lits, int rhs) {
    if (rhs < 0)
      throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
    assert !lits.isEmpty();
    this.result.clear();
    this.cardinalityUpOutlits.clear();
    this.cardinalityLwOutlits.clear();
    if (rhs >= lits.size())
      return new ImmutableFormulaList(FType.AND);
    if (rhs == 0) {
      for (final Literal lit : lits)
        this.result.add(lit.negate());
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    assert rhs >= 1 && rhs < lits.size();
    int mod = (int) Math.ceil(Math.sqrt(rhs + 1.0));
    this.cardinalityUpOutlits = new LNGVector<>(lits.size() / mod);
    for (int i = 0; i < lits.size() / mod; i++)
      this.cardinalityUpOutlits.push(this.f.newCCLiteral());
    this.cardinalityLwOutlits = new LNGVector<>(mod - 1);
    for (int i = 0; i < mod - 1; i++)
      this.cardinalityLwOutlits.push(this.f.newCCLiteral());
    this.cardinalityInlits = new LNGVector<>(lits.size());
    for (final Literal lit : lits)
      this.cardinalityInlits.push(lit);
    this.currentCardinalityRhs = rhs + 1;
    if (this.cardinalityUpOutlits.size() == 0)
      this.cardinalityUpOutlits.push(this.h0);
    this.toCNF(mod, this.cardinalityUpOutlits, this.cardinalityLwOutlits, lits.size());
    assert this.cardinalityInlits.size() == 0;
    this.encodeOutput(rhs, mod);
    this.currentCardinalityRhs = rhs + 1;
    return new ImmutableFormulaList(FType.AND, this.result);
  }

  private void encodeOutput(int rhs, int mod) {
    assert this.cardinalityUpOutlits.size() != 0 || this.cardinalityLwOutlits.size() != 0;
    int ulimit = (rhs + 1) / mod;
    int llimit = (rhs + 1) - ulimit * mod;
    assert ulimit <= this.cardinalityUpOutlits.size();
    assert llimit <= this.cardinalityLwOutlits.size();
    for (int i = ulimit; i < this.cardinalityUpOutlits.size(); i++)
      this.result.add(this.cardinalityUpOutlits.get(i).negate());
    if (ulimit != 0 && llimit != 0) {
      for (int i = llimit - 1; i < this.cardinalityLwOutlits.size(); i++)
        this.result.add(this.f.clause(this.cardinalityUpOutlits.get(ulimit - 1).negate(), this.cardinalityLwOutlits.get(i).negate()));
    } else {
      if (ulimit == 0) {
        assert llimit != 0;
        for (int i = llimit - 1; i < this.cardinalityLwOutlits.size(); i++)
          this.result.add(this.cardinalityLwOutlits.get(i).negate());
      } else
        this.result.add(this.cardinalityUpOutlits.get(ulimit - 1).negate());
    }
  }

  private void toCNF(int mod, final LNGVector<Literal> ublits, final LNGVector<Literal> lwlits, int rhs) {
    LNGVector<Literal> lupper = new LNGVector<>();
    LNGVector<Literal> llower = new LNGVector<>();
    LNGVector<Literal> rupper = new LNGVector<>();
    LNGVector<Literal> rlower = new LNGVector<>();
    assert rhs > 1;
    int split = rhs / 2;
    int left = 1;
    int right = 1;
    if (split == 1) {
      assert this.cardinalityInlits.size() > 0;
      lupper.push(this.h0);
      llower.push(this.cardinalityInlits.back());
      this.cardinalityInlits.pop();
    } else {
      left = split / mod;
      for (int i = 0; i < left; i++)
        lupper.push(this.f.newCCLiteral());
      int limit = mod - 1;
      if (left % mod == 0 && split < mod - 1)
        limit = split;
      for (int i = 0; i < limit; i++)
        llower.push(this.f.newCCLiteral());
    }
    if (rhs - split == 1) {
      assert this.cardinalityInlits.size() > 0;
      rupper.push(this.h0);
      rlower.push(this.cardinalityInlits.back());
      this.cardinalityInlits.pop();
    } else {
      right = (rhs - split) / mod;
      for (int i = 0; i < right; i++)
        rupper.push(this.f.newCCLiteral());
      int limit = mod - 1;
      if (right % mod == 0 && rhs - split < mod - 1) {
        limit = rhs - split;
      }
      for (int i = 0; i < limit; i++)
        rlower.push(this.f.newCCLiteral());
    }
    if (lupper.size() == 0)
      lupper.push(this.h0);
    if (rupper.size() == 0)
      rupper.push(this.h0);
    this.adder(mod, ublits, lwlits, rupper, rlower, lupper, llower);
    if (left * mod + split - left * mod > 1)
      this.toCNF(mod, lupper, llower, left * mod + split - left * mod);
    if (right * mod + (rhs - split) - right * mod > 1)
      this.toCNF(mod, rupper, rlower, right * mod + (rhs - split) - right * mod);
  }

  private void adder(int mod, final LNGVector<Literal> upper, final LNGVector<Literal> lower,
                     final LNGVector<Literal> lupper, final LNGVector<Literal> llower, final LNGVector<Literal> rupper,
                     final LNGVector<Literal> rlower) {
    assert upper.size() != 0;
    assert lower.size() >= llower.size() && lower.size() >= rlower.size();
    Literal carry = this.litUndef;
    if (upper.get(0) != this.h0) // != is ok here - we are within the same formula factory
      carry = this.f.newCCLiteral();
    for (int i = 0; i <= llower.size(); i++) {
      for (int j = 0; j <= rlower.size(); j++) {
        if (i + j > this.currentCardinalityRhs + 1 && this.currentCardinalityRhs + 1 < mod)
          continue;
        if (i + j < mod) {
          if (i == 0 && j != 0) {
            if (upper.get(0) != this.h0)
              this.result.add(this.f.clause(rlower.get(j - 1).negate(), lower.get(i + j - 1), carry));
            else
              this.result.add(this.f.clause(rlower.get(j - 1).negate(), lower.get(i + j - 1)));
          } else if (j == 0 && i != 0) {
            if (upper.get(0) != this.h0)
              this.result.add(this.f.clause(llower.get(i - 1).negate(), lower.get(i + j - 1), carry));
            else
              this.result.add(this.f.clause(llower.get(i - 1).negate(), lower.get(i + j - 1)));
          } else if (i != 0) {
            if (upper.get(0) != this.h0)
              this.result.add(this.f.clause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get(i + j - 1), carry));
            else {
              assert i + j - 1 < lower.size();
              this.result.add(this.f.clause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get(i + j - 1)));
            }
          }
        } else if (i + j > mod) {
          assert i + j > 0;
          this.result.add(this.f.clause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get((i + j) % mod - 1)));
        } else {
          assert i + j == mod;
          assert carry != this.litUndef;
          this.result.add(this.f.clause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), carry));
        }
      }
    }
    if (upper.get(0) != this.h0) {
      this.finalAdder(mod, upper, lupper, rupper, carry);
    }
  }

  private void finalAdder(int mod, LNGVector<Literal> upper, LNGVector<Literal> lupper, LNGVector<Literal> rupper, Literal carry) {
    for (int i = 0; i <= lupper.size(); i++) {
      for (int j = 0; j <= rupper.size(); j++) {
        Literal a = this.litError;
        Literal b = this.litError;
        Literal c = this.litError;
        Literal d = this.litError;
        int closeMod = this.currentCardinalityRhs / mod;
        if (this.currentCardinalityRhs % mod != 0)
          closeMod++;
        if (mod * (i + j) > closeMod * mod)
          continue;
        if (i != 0)
          a = lupper.get(i - 1);
        if (j != 0)
          b = rupper.get(j - 1);
        if (i + j != 0 && i + j - 1 < upper.size())
          c = upper.get(i + j - 1);
        if (i + j < upper.size())
          d = upper.get(i + j);
        if (c != this.litUndef && c != this.litError) {
          final List<Literal> clause = new LinkedList<>();
          if (a != this.litUndef && a != this.litError)
            clause.add(a.negate());
          if (b != this.litUndef && b != this.litError)
            clause.add(b.negate());
          clause.add(c);
          if (clause.size() > 1)
            this.result.add(this.f.clause(clause));
        }
        final List<Literal> clause = new LinkedList<>();
        clause.add(carry.negate());
        if (a != this.litUndef && a != this.litError)
          clause.add(a.negate());
        if (b != this.litUndef && b != this.litError)
          clause.add(b.negate());
        if (d != this.litError && d != this.litUndef)
          clause.add(d);
        if (clause.size() > 1)
          this.result.add(this.f.clause(clause));
      }
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
