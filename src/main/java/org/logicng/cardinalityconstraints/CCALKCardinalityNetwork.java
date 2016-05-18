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

package org.logicng.cardinalityconstraints;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.OUTPUT_TO_INPUT;

/**
 * Encodes that at least 'rhs' variables are assigned value true.  Uses the cardinality network
 * encoding due to Asín, Nieuwenhuis, Oliveras, and Rodríguez-Carbonell .
 * @version 1.1
 * @since 1.1
 */
public final class CCALKCardinalityNetwork extends CCAtLeastK {

  private final FormulaFactory f;
  private List<Formula> result;
  private final CCSorting sorting;

  /**
   * Constructs a new cardinality encoder.
   * @param f the formula factory
   */
  public CCALKCardinalityNetwork(final FormulaFactory f) {
    this.f = f;
    this.result = new ArrayList<>();
    this.sorting = new CCSorting(f);
  }

  @Override
  public ImmutableFormulaList build(final Collection<Variable> vars, int rhs) {
    if (rhs < 0)
      throw new IllegalArgumentException("Invalid right hand side of cardinality constraint: " + rhs);
    this.result.clear();
    if (rhs > vars.size())
      return new ImmutableFormulaList(FType.AND, this.f.falsum());
    if (rhs == 0)
      return new ImmutableFormulaList(FType.AND);
    if (rhs == 1) {
      this.result.add(this.f.or(vars));
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    if (rhs == vars.size()) {
      for (final Variable var : vars)
        this.result.add(var);
      return new ImmutableFormulaList(FType.AND, this.result);
    }
    final LNGVector<Literal> input = new LNGVector<>();
    final LNGVector<Literal> output = new LNGVector<>();
    if (rhs > vars.size() / 2) {
      int geq = vars.size() - rhs;
      for (final Variable v : vars)
        input.push(v.negate());
      sorting.sort(geq, input, result, output, OUTPUT_TO_INPUT);
      for (int i = 0; i < geq; ++i)
        result.add(f.clause(output.get(i)));
    } else {
      for (final Variable v : vars)
        input.push(v);
      sorting.sort(rhs + 1, input, result, output, CCSorting.ImplicationDirection.INPUT_TO_OUTPUT);
      assert (output.size() > rhs);
      result.add(f.clause(output.get(rhs).negate()));
    }
    return new ImmutableFormulaList(FType.AND, this.result);
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
