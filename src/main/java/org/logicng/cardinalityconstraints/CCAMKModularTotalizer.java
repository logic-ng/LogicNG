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
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * Encodes that at most 'rhs' variables can be assigned value true.  Uses the modular totalizer encoding for
 * translating the cardinality constraint into CNF.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class CCAMKModularTotalizer extends CCAtMostK {

  private final FormulaFactory f;
  private final Variable varUndef;
  private final Variable varError;

  private Variable h0;
  private LNGVector<Variable> cardinalityInvars;
  private LNGVector<Variable> cardinalityUpOutvars;
  private LNGVector<Variable> cardinalityLwOutvars;
  private int currentCardinalityRhs;
  private List<Formula> result;

  /**
   * Constructs a new modular totalizer.
   * @param f the formula factory
   */
  public CCAMKModularTotalizer(final FormulaFactory f) {
    this.f = f;
    this.varUndef = f.variable("RESERVED@VAR_UNDEF");
    this.varError = f.variable("RESERVED@VAR_ERROR");
    this.h0 = this.varUndef;
    this.currentCardinalityRhs = -1;
    this.cardinalityInvars = new LNGVector<>();
    this.cardinalityUpOutvars = new LNGVector<>();
    this.cardinalityLwOutvars = new LNGVector<>();
    this.result = new LinkedList<>();
  }

  @Override
  public ImmutableFormulaList build(final Collection<Variable> vars, int rhs) {
    if (rhs < 0)
      throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
    assert !vars.isEmpty();
    this.result.clear();
    this.cardinalityUpOutvars.clear();
    this.cardinalityLwOutvars.clear();
    if (rhs >= vars.size())
      return new ImmutableFormulaList(FType.AND);
    if (rhs == 0) {
      for (final Variable var : vars)
        this.result.add(var.negate());
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    assert rhs >= 1 && rhs < vars.size();
    int mod = (int) Math.ceil(Math.sqrt(rhs + 1.0));
    this.cardinalityUpOutvars = new LNGVector<>(vars.size() / mod);
    for (int i = 0; i < vars.size() / mod; i++)
      this.cardinalityUpOutvars.push(this.f.newCCVariable());
    this.cardinalityLwOutvars = new LNGVector<>(mod - 1);
    for (int i = 0; i < mod - 1; i++)
      this.cardinalityLwOutvars.push(this.f.newCCVariable());
    this.cardinalityInvars = new LNGVector<>(vars.size());
    for (final Variable var : vars)
      this.cardinalityInvars.push(var);
    this.currentCardinalityRhs = rhs + 1;
    if (this.cardinalityUpOutvars.size() == 0)
      this.cardinalityUpOutvars.push(this.h0);
    this.toCNF(mod, this.cardinalityUpOutvars, this.cardinalityLwOutvars, vars.size());
    assert this.cardinalityInvars.size() == 0;
    this.encodeOutput(rhs, mod);
    this.currentCardinalityRhs = rhs + 1;
    return new ImmutableFormulaList(FType.AND, this.result);
  }

  private void encodeOutput(int rhs, int mod) {
    assert this.cardinalityUpOutvars.size() != 0 || this.cardinalityLwOutvars.size() != 0;
    int ulimit = (rhs + 1) / mod;
    int llimit = (rhs + 1) - ulimit * mod;
    assert ulimit <= this.cardinalityUpOutvars.size();
    assert llimit <= this.cardinalityLwOutvars.size();
    for (int i = ulimit; i < this.cardinalityUpOutvars.size(); i++)
      this.result.add(this.cardinalityUpOutvars.get(i).negate());
    if (ulimit != 0 && llimit != 0) {
      for (int i = llimit - 1; i < this.cardinalityLwOutvars.size(); i++)
        this.result.add(this.f.clause(this.cardinalityUpOutvars.get(ulimit - 1).negate(), this.cardinalityLwOutvars.get(i).negate()));
    } else {
      if (ulimit == 0) {
        assert llimit != 0;
        for (int i = llimit - 1; i < this.cardinalityLwOutvars.size(); i++)
          this.result.add(this.cardinalityLwOutvars.get(i).negate());
      } else
        this.result.add(this.cardinalityUpOutvars.get(ulimit - 1).negate());
    }
  }

  private void toCNF(int mod, final LNGVector<Variable> ubvars, final LNGVector<Variable> lwvars, int rhs) {
    LNGVector<Variable> lupper = new LNGVector<>();
    LNGVector<Variable> llower = new LNGVector<>();
    LNGVector<Variable> rupper = new LNGVector<>();
    LNGVector<Variable> rlower = new LNGVector<>();
    assert rhs > 1;
    int split = rhs / 2;
    int left = 1;
    int right = 1;
    if (split == 1) {
      assert this.cardinalityInvars.size() > 0;
      lupper.push(this.h0);
      llower.push(this.cardinalityInvars.back());
      this.cardinalityInvars.pop();
    } else {
      left = split / mod;
      for (int i = 0; i < left; i++)
        lupper.push(this.f.newCCVariable());
      int limit = mod - 1;
      if (left % mod == 0 && split < mod - 1)
        limit = split;
      for (int i = 0; i < limit; i++)
        llower.push(this.f.newCCVariable());
    }
    if (rhs - split == 1) {
      assert this.cardinalityInvars.size() > 0;
      rupper.push(this.h0);
      rlower.push(this.cardinalityInvars.back());
      this.cardinalityInvars.pop();
    } else {
      right = (rhs - split) / mod;
      for (int i = 0; i < right; i++)
        rupper.push(this.f.newCCVariable());
      int limit = mod - 1;
      if (right % mod == 0 && rhs - split < mod - 1) {
        limit = rhs - split;
      }
      for (int i = 0; i < limit; i++)
        rlower.push(this.f.newCCVariable());
    }
    if (lupper.size() == 0)
      lupper.push(this.h0);
    if (rupper.size() == 0)
      rupper.push(this.h0);
    this.adder(mod, ubvars, lwvars, rupper, rlower, lupper, llower);
    if (left * mod + split - left * mod > 1)
      this.toCNF(mod, lupper, llower, left * mod + split - left * mod);
    if (right * mod + (rhs - split) - right * mod > 1)
      this.toCNF(mod, rupper, rlower, right * mod + (rhs - split) - right * mod);
  }

  private void adder(int mod, final LNGVector<Variable> upper, final LNGVector<Variable> lower,
                     final LNGVector<Variable> lupper, final LNGVector<Variable> llower, final LNGVector<Variable> rupper,
                     final LNGVector<Variable> rlower) {
    assert upper.size() != 0;
    assert lower.size() >= llower.size() && lower.size() >= rlower.size();
    Variable carry = this.varUndef;
    if (upper.get(0) != this.h0) // != is ok here - we are within the same formula factory
      carry = this.f.newCCVariable();
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
          assert carry != this.varUndef;
          this.result.add(this.f.clause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), carry));
        }
      }
    }
    if (upper.get(0) != this.h0) {
      this.finalAdder(mod, upper, lupper, rupper, carry);
    }
  }

  private void finalAdder(int mod, final LNGVector<Variable> upper, final LNGVector<Variable> lupper,
                          final LNGVector<Variable> rupper, final Variable carry) {
    for (int i = 0; i <= lupper.size(); i++) {
      for (int j = 0; j <= rupper.size(); j++) {
        Variable a = this.varError;
        Variable b = this.varError;
        Variable c = this.varError;
        Variable d = this.varError;
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
        if (c != this.varUndef && c != this.varError) {
          final List<Literal> clause = new LinkedList<>();
          if (a != this.varUndef && a != this.varError)
            clause.add(a.negate());
          if (b != this.varUndef && b != this.varError)
            clause.add(b.negate());
          clause.add(c);
          if (clause.size() > 1)
            this.result.add(this.f.clause(clause));
        }
        final List<Literal> clause = new LinkedList<>();
        clause.add(carry.negate());
        if (a != this.varUndef && a != this.varError)
          clause.add(a.negate());
        if (b != this.varUndef && b != this.varError)
          clause.add(b.negate());
        if (d != this.varError && d != this.varUndef)
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
