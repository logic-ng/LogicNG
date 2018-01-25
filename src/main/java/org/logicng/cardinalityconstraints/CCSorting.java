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

package org.logicng.cardinalityconstraints;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.BOTH;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.INPUT_TO_OUTPUT;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.OUTPUT_TO_INPUT;

/**
 * Implementation of a sorting network.
 * @version 1.1
 * @since 1.1
 */
public final class CCSorting {

  /**
   * The implication direction.
   */
  public enum ImplicationDirection {
    INPUT_TO_OUTPUT, OUTPUT_TO_INPUT, BOTH
  }

  private final LNGVector<LNGVector<Literal>> auxVars;

  /**
   * Constructs a new sorting network.
   */
  public CCSorting() {
    this.auxVars = new LNGVector<>();
  }

  private static int counterSorterValue(int m, int n) {
    return 2 * n + (m - 1) * (2 * (n - 1) - 1) - (m - 2) - 2 * ((m - 1) * (m - 2) / 2);
  }

  private static int directSorterValue(int n) {
    if (n > 30)
      return Integer.MAX_VALUE;
    return (int) Math.pow(2, n) - 1;
  }

  /**
   * Generates a sorter encoding for the given input.
   * @param m         the the counter
   * @param input     the input literals
   * @param result    the result of the encoding
   * @param output    the output literals
   * @param direction the sorting direction
   */
  public void sort(int m, final LNGVector<Literal> input, final EncodingResult result, final LNGVector<Literal> output,
                   final ImplicationDirection direction) {
    assert m >= 0;
    if (m == 0) {
      output.clear();
      return;
    }
    int n = input.size();
    int m2 = m;
    if (m2 > n)
      m2 = n;
    if (n == 0) {
      output.clear();
      return;
    }
    if (n == 1) {
      output.clear();
      output.push(input.get(0));
      return;
    }
    if (n == 2) {
      output.clear();
      final Variable o1 = result.newVariable();
      if (m2 == 2) {
        final Variable o2 = result.newVariable();
        this.comparator(input.get(0), input.get(1), o1, o2, result, direction);
        output.push(o1);
        output.push(o2);
      } else {
        assert m2 == 1;
        this.comparator(input.get(0), input.get(1), o1, result, direction);
        output.push(o1);
      }
      return;
    }
    if (direction != INPUT_TO_OUTPUT) {
      this.recursiveSorter(m2, input, result, output, direction);
      return;
    }
    int counter = counterSorterValue(m2, n);
    int direct = directSorterValue(n);

    if (counter < direct)
      counterSorter(m2, input, result, output, direction);
    else
      directSorter(m2, input, result, output, direction);
  }

  private void comparator(final Literal x1, final Literal x2, final Literal y, final EncodingResult result,
                          final ImplicationDirection direction) {
    assert !x1.equals(x2);
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      result.addClause(x1.negate(), y);
      result.addClause(x2.negate(), y);
    }
    if (direction == OUTPUT_TO_INPUT || direction == BOTH)
      result.addClause(y.negate(), x1, x2);
  }

  private void comparator(final Literal x1, final Literal x2, final Literal y1, final Literal y2, final EncodingResult result,
                          final ImplicationDirection direction) {
    assert !x1.equals(x2);
    assert !y1.equals(y2);
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      result.addClause(x1.negate(), y1);
      result.addClause(x2.negate(), y1);
      result.addClause(x1.negate(), x2.negate(), y2);
    }
    if (direction == OUTPUT_TO_INPUT || direction == BOTH) {
      result.addClause(y1.negate(), x1, x2);
      result.addClause(y2.negate(), x1);
      result.addClause(y2.negate(), x2);
    }
  }

  private void recursiveSorter(int m, int l, final LNGVector<Literal> input, final EncodingResult result,
                               final LNGVector<Literal> output, final ImplicationDirection direction) {
    int n = input.size();
    assert output.size() == 0;
    assert n > 1;
    assert m <= n;
    final LNGVector<Literal> tmpLitsA = new LNGVector<>();
    final LNGVector<Literal> tmpLitsB = new LNGVector<>();
    final LNGVector<Literal> tmpLitsO1 = new LNGVector<>();
    final LNGVector<Literal> tmpLitsO2 = new LNGVector<>();

    for (int i = 0; i < l; i++)
      tmpLitsA.push(input.get(i));
    for (int i = l; i < n; i++)
      tmpLitsB.push(input.get(i));

    assert tmpLitsA.size() + tmpLitsB.size() == n;

    this.sort(m, tmpLitsA, result, tmpLitsO1, direction);
    this.sort(m, tmpLitsB, result, tmpLitsO2, direction);
    merge(m, tmpLitsO1, tmpLitsO2, result, output, direction);

    assert tmpLitsO1.size() == Math.min(l, m);
    assert tmpLitsO2.size() == Math.min(n - l, m);
    assert output.size() == m;
  }

  private void recursiveSorter(int m, final LNGVector<Literal> input, final EncodingResult result,
                               final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert m > 0;
    assert input.size() > 0;
    output.clear();
    int n = input.size();
    assert n > 1;
    int l = n / 2;
    this.recursiveSorter(m, l, input, result, output, direction);
  }

  private void counterSorter(int k, final LNGVector<Literal> x, final EncodingResult formula,
                             final LNGVector<Literal> output, final ImplicationDirection direction) {
    int n = x.size();
    auxVars.clear();
    for (int i = 0; i < n; i++)
      auxVars.push(new LNGVector<Literal>(k));

    for (int j = 0; j < k; j++)
      for (int i = j; i < n; i++)
        auxVars.get(i).set(j, formula.newVariable());
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      for (int i = 0; i < n; i++) {
        formula.addClause(x.get(i).negate(), auxVars.get(i).get(0));
        if (i > 0)
          formula.addClause(auxVars.get(i - 1).get(0).negate(), auxVars.get(i).get(0));
      }
      for (int j = 1; j < k; j++) {
        for (int i = j; i < n; i++) {
          formula.addClause(x.get(i).negate(), auxVars.get(i - 1).get(j - 1).negate(), auxVars.get(i).get(j));
          if (i > j)
            formula.addClause(auxVars.get(i - 1).get(j).negate(), auxVars.get(i).get(j));
        }
      }
    }
    assert direction == INPUT_TO_OUTPUT;
    output.clear();
    for (int i = 0; i < k; i++)
      output.push(auxVars.get(n - 1).get(i));
  }

  private void directSorter(int m, final LNGVector<Literal> input, final EncodingResult formula,
                            final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert direction == INPUT_TO_OUTPUT;
    int n = input.size();
    assert n < 20;
    int bitmask = 1;
    final LNGVector<Literal> clause = new LNGVector<>();
    output.clear();
    for (int i = 0; i < m; i++)
      output.push(formula.newVariable());
    while (bitmask < Math.pow(2, n)) {
      int count = 0;
      clause.clear();
      for (int i = 0; i < n; i++)
        if (((1 << i) & bitmask) != 0) {
          count++;
          if (count > m)
            break;
          clause.push(input.get(i).negate());
        }
      assert count > 0;
      if (count <= m) {
        clause.push(output.get(count - 1));
        formula.addClause(clause);
      }
      bitmask++;
    }
  }

  /**
   * Merges to input vectors.
   * @param m         parameter m
   * @param inputA    the first input vector
   * @param inputB    the second input vector
   * @param formula   the result formula
   * @param output    the output vector
   * @param direction the sorting direction
   */
  public void merge(int m, final LNGVector<Literal> inputA, final LNGVector<Literal> inputB, final EncodingResult formula,
                    final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert m >= 0;
    if (m == 0) {
      output.clear();
      return;
    }
    int a = inputA.size();
    int b = inputB.size();
    int n = a + b;
    int m2 = m;
    if (m2 > n)
      m2 = n;
    if (a == 0 || b == 0) {
      if (a == 0)
        output.replaceInplace(inputB);
      else
        output.replaceInplace(inputA);
      return;
    }
    if (direction != INPUT_TO_OUTPUT) {
      recursiveMerger(m2, inputA, inputA.size(), inputB, inputB.size(), formula, output, direction);
      return;
    }
    directMerger(m2, inputA, inputB, formula, output, direction);
  }

  private void recursiveMerger(int c, final LNGVector<Literal> inputA, int a, final LNGVector<Literal> inputB, int b,
                               final EncodingResult formula, final LNGVector<Literal> output,
                               final ImplicationDirection direction) {
    assert inputA.size() > 0;
    assert inputB.size() > 0;
    assert c > 0;
    output.clear();
    int a2 = a;
    int b2 = b;
    if (a2 > c)
      a2 = c;
    if (b2 > c)
      b2 = c;
    if (c == 1) {
      final Variable y = formula.newVariable();
      comparator(inputA.get(0), inputB.get(0), y, formula, direction);
      output.push(y);
      return;
    }
    if (a2 == 1 && b2 == 1) {
      assert c == 2;
      final Variable y1 = formula.newVariable();
      final Variable y2 = formula.newVariable();
      comparator(inputA.get(0), inputB.get(0), y1, y2, formula, direction);
      output.push(y1);
      output.push(y2);
      return;
    }
    final LNGVector<Literal> oddMerge = new LNGVector<>();
    final LNGVector<Literal> evenMerge = new LNGVector<>();
    final LNGVector<Literal> tmpLitsOddA = new LNGVector<>();
    final LNGVector<Literal> tmpLitsOddB = new LNGVector<>();
    final LNGVector<Literal> tmpLitsEvenA = new LNGVector<>();
    final LNGVector<Literal> tmpLitsEvenB = new LNGVector<>();

    for (int i = 0; i < a2; i = i + 2)
      tmpLitsOddA.push(inputA.get(i));
    for (int i = 0; i < b2; i = i + 2)
      tmpLitsOddB.push(inputB.get(i));
    for (int i = 1; i < a2; i = i + 2)
      tmpLitsEvenA.push(inputA.get(i));
    for (int i = 1; i < b2; i = i + 2)
      tmpLitsEvenB.push(inputB.get(i));

    merge(c / 2 + 1, tmpLitsOddA, tmpLitsOddB, formula, oddMerge, direction);
    merge(c / 2, tmpLitsEvenA, tmpLitsEvenB, formula, evenMerge, direction);

    assert oddMerge.size() > 0;

    output.push(oddMerge.get(0));

    int i = 1;
    int j = 0;
    while (true) {
      if (i < oddMerge.size() && j < evenMerge.size()) {
        if (output.size() + 2 <= c) {
          final Variable z0 = formula.newVariable();
          final Variable z1 = formula.newVariable();
          comparator(oddMerge.get(i), evenMerge.get(j), z0, z1, formula, direction);
          output.push(z0);
          output.push(z1);
          if (output.size() == c)
            break;
        } else if (output.size() + 1 == c) {
          final Variable z0 = formula.newVariable();
          comparator(oddMerge.get(i), evenMerge.get(j), z0, formula, direction);
          output.push(z0);
          break;
        }
      } else if (i >= oddMerge.size() && j >= evenMerge.size())
        break;
      else if (i >= oddMerge.size()) {
        assert j == evenMerge.size() - 1;
        output.push(evenMerge.back());
        break;
      } else {
        assert i == oddMerge.size() - 1;
        output.push(oddMerge.back());
        break;
      }
      i++;
      j++;
    }
    assert output.size() == a2 + b2 || output.size() == c;
  }

  private void directMerger(int m, final LNGVector<Literal> inputA, LNGVector<Literal> inputB, final EncodingResult formula,
                            LNGVector<Literal> output, final ImplicationDirection direction) {
    assert direction == INPUT_TO_OUTPUT;
    int a = inputA.size();
    int b = inputB.size();
    for (int i = 0; i < m; i++)
      output.push(formula.newVariable());
    int j = m < a ? m : a;
    for (int i = 0; i < j; i++)
      formula.addClause(inputA.get(i).negate(), output.get(i));
    j = m < b ? m : b;
    for (int i = 0; i < j; i++)
      formula.addClause(inputB.get(i).negate(), output.get(i));
    for (int i = 0; i < a; i++)
      for (int k = 0; k < b; k++)
        if (i + k + 1 < m)
          formula.addClause(inputA.get(i).negate(), inputB.get(k).negate(), output.get(i + k + 1));
  }
}
