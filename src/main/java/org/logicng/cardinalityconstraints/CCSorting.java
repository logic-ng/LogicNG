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

import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.BOTH;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.INPUT_TO_OUTPUT;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.OUTPUT_TO_INPUT;

/**
 * Implementation of a sorting network.
 * @version 1.1
 * @since 1.1
 */
final class CCSorting {

  enum ImplicationDirection {INPUT_TO_OUTPUT, OUTPUT_TO_INPUT, BOTH}

  private final FormulaFactory f;

  private final LNGVector<LNGVector<Literal>> s_auxs;
  private final Map<IntPair, Integer> recursive_sorter_values;
  private final Map<IntTriple, Integer> recursive_sorter_l_values;
  private final Map<IntTriple, Integer> recursive_merger_values;

  /**
   * Constructs a new sorting network.
   * @param f the formula factory
   */
  CCSorting(final FormulaFactory f) {
    this.f = f;
    this.s_auxs = new LNGVector<>();
    this.recursive_sorter_values = new HashMap<>();
    this.recursive_sorter_l_values = new HashMap<>();
    this.recursive_merger_values = new HashMap<>();
  }

  void sort(int m, final LNGVector<Literal> input, final List<Formula> result, final LNGVector<Literal> output,
            final ImplicationDirection direction) {
    assert (m >= 0);
    if (m == 0) {
      output.clear();
      return;
    }
    int n = input.size();
    if (m > n)
      m = n;
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
      final Variable o1 = this.f.newCCVariable();
      if (m == 2) {
        final Variable o2 = this.f.newCCVariable();
        this.comparator(input.get(0), input.get(1), o1, o2, result, direction);
        output.push(o1);
        output.push(o2);
      } else {
        assert (m == 1);
        this.comparator(input.get(0), input.get(1), o1, result, direction);
        output.push(o1);
      }

      return;
    }
    if (direction != INPUT_TO_OUTPUT) {
      this.recursive_sorter(m, input, result, output, direction);
      return;
    }
    int counter = counter_sorter_value(m, n);
    int direct = direct_sorter_value(n);
    int recursive = this.recursive_sorter_value(m, n, direction);

    if (counter < direct && counter < recursive)
      counter_sorter(m, input, result, output, direction);
    else if (direct < counter && direct < recursive)
      direct_sorter(m, input, result, output, direction);
    else
      this.recursive_sorter(m, input, result, output, direction);
  }

  private void comparator(final Literal x1, final Literal x2, final Literal y, final List<Formula> result,
                          final ImplicationDirection direction) {
    assert (!x1.equals(x2));
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      result.add(this.f.clause(x1.negate(), y));
      result.add(this.f.clause(x2.negate(), y));
    }
    if (direction == OUTPUT_TO_INPUT || direction == BOTH)
      result.add(this.f.clause(y.negate(), x1, x2));
  }

  private void comparator(final Literal x1, final Literal x2, final Literal y1, final Literal y2, final List<Formula> result,
                          final ImplicationDirection direction) {
    assert (!x1.equals(x2));
    assert (!y1.equals(y2));
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      result.add(this.f.clause(x1.negate(), y1));
      result.add(this.f.clause(x2.negate(), y1));
      result.add(this.f.clause(x1.negate(), x2.negate(), y2));
    }
    if (direction == OUTPUT_TO_INPUT || direction == BOTH) {
      result.add(this.f.clause(y1.negate(), x1, x2));
      result.add(this.f.clause(y2.negate(), x1));
      result.add(this.f.clause(y2.negate(), x2));
    }
  }

  private void recursive_sorter(int m, int l, final LNGVector<Literal> input, final List<Formula> result,
                                final LNGVector<Literal> output, final ImplicationDirection direction) {
    int n = input.size();
    assert (output.size() == 0);
    assert (n > 1);
    assert (m <= n);
    final LNGVector<Literal> tmp_lits_a = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_b = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_o1 = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_o2 = new LNGVector<>();

    for (int i = 0; i < l; i++)
      tmp_lits_a.push(input.get(i));
    for (int i = l; i < n; i++)
      tmp_lits_b.push(input.get(i));

    assert (tmp_lits_a.size() + tmp_lits_b.size() == n);

    this.sort(m, tmp_lits_a, result, tmp_lits_o1, direction);
    this.sort(m, tmp_lits_b, result, tmp_lits_o2, direction);
    merge(m, tmp_lits_o1, tmp_lits_o2, result, output, direction);

    assert (tmp_lits_o1.size() == Math.min(l, m));
    assert (tmp_lits_o2.size() == Math.min(n - l, m));
    assert (output.size() == m);
  }

  private void recursive_sorter(int m, final LNGVector<Literal> input, final List<Formula> result,
                                final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert (m > 0);
    assert (input.size() > 0);

    output.clear();

    int n = input.size();
    assert (n > 1);

    int l = 1;
    if (n > 100)
      l = n / 2;
    else {
      int min_value = this.recursive_sorter_value(m, n, l, direction);
      for (int i = 2; i < n; ++i) {
        int value = this.recursive_sorter_value(m, n, i, direction);
        if (value < min_value) {
          l = i;
          min_value = value;
        }
      }
    }
    this.recursive_sorter(m, l, input, result, output, direction);
  }

  private void counter_sorter(int k, final LNGVector<Literal> x, final List<Formula> formula,
                              final LNGVector<Literal> output, final ImplicationDirection direction) {
    int n = x.size();
    s_auxs.clear();
    for (int i = 0; i < n; i++)
      s_auxs.push(new LNGVector<Literal>(k));

    for (int j = 0; j < k; j++)
      for (int i = j; i < n; i++)
        s_auxs.get(i).set(j, f.newCCVariable());
    if (direction == INPUT_TO_OUTPUT || direction == BOTH) {
      for (int i = 0; i < n; ++i) {
        formula.add(f.clause(x.get(i).negate(), s_auxs.get(i).get(0)));
        if (i > 0)
          formula.add(f.clause(s_auxs.get(i - 1).get(0).negate(), s_auxs.get(i).get(0)));
      }
      for (int j = 1; j < k; ++j) {
        for (int i = j; i < n; ++i) {
          formula.add(f.clause(x.get(i).negate(), s_auxs.get(i - 1).get(j - 1).negate(), s_auxs.get(i).get(j)));
          if (i > j)
            formula.add(f.clause(s_auxs.get(i - 1).get(j).negate(), s_auxs.get(i).get(j)));
        }
      }
    }
    assert (direction == INPUT_TO_OUTPUT);
    output.clear();
    for (int i = 0; i < k; ++i)
      output.push(s_auxs.get(n - 1).get(i));
  }

  private void direct_sorter(int m, final LNGVector<Literal> input, final List<Formula> formula,
                             final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert (direction == INPUT_TO_OUTPUT);
    int n = input.size();
    assert (n < 20);
    int bitmask = 1;
    final List<Literal> clause = new ArrayList<>();
    output.clear();
    for (int i = 0; i < m; ++i)
      output.push(f.newCCVariable());
    while (bitmask < Math.pow(2, n)) {
      int count = 0;
      clause.clear();
      for (int i = 0; i < n; ++i)
        if (((1 << i) & bitmask) != 0) {
          count++;
          if (count > m)
            break;
          clause.add(input.get(i).negate());
        }
      assert (count > 0);
      if (count <= m) {
        clause.add(output.get(count - 1));
        formula.add(f.clause(clause));
      }
      bitmask++;
    }
  }

  private void merge(int m, final LNGVector<Literal> input_a, final LNGVector<Literal> input_b, final List<Formula> formula,
                     final LNGVector<Literal> output, final ImplicationDirection direction) {
    assert (m >= 0);
    if (m == 0) {
      output.clear();
      return;
    }
    int a = input_a.size();
    int b = input_b.size();
    int n = a + b;
    if (m > n)
      m = n;
    if (a == 0 || b == 0) {
      if (a == 0)
        output.replaceInplace(input_b);
      else
        output.replaceInplace(input_a);
      return;
    }
    if (direction != INPUT_TO_OUTPUT) {
      recursive_merger(m, input_a, input_a.size(), input_b, input_b.size(), formula, output, direction);
      return;
    }
    int direct = direct_merger_value(m, a, b);
    int recursive = recursive_merger_value(m, a, b, direction);
    if (direct < recursive) {
      direct_merger(m, input_a, input_b, formula, output, direction);
    } else {
      recursive_merger(m, input_a, input_a.size(), input_b, input_b.size(), formula, output, direction);
    }
  }

  private void recursive_merger(int c, final LNGVector<Literal> input_a, int a, final LNGVector<Literal> input_b, int b,
                                final List<Formula> formula, final LNGVector<Literal> output,
                                final ImplicationDirection direction) {
    assert (input_a.size() > 0);
    assert (input_b.size() > 0);
    assert (c > 0);
    output.clear();
    if (a > c)
      a = c;
    if (b > c)
      b = c;
    if (c == 1) {
      final Variable y = f.newCCVariable();
      comparator(input_a.get(0), input_b.get(0), y, formula, direction);
      output.push(y);
      return;
    }
    if (a == 1 && b == 1) {
      assert (c == 2);
      final Variable y1 = f.newCCVariable();
      final Variable y2 = f.newCCVariable();
      comparator(input_a.get(0), input_b.get(0), y1, y2, formula, direction);
      output.push(y1);
      output.push(y2);
      return;
    }
    final LNGVector<Literal> odd_merge = new LNGVector<>();
    final LNGVector<Literal> even_merge = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_odd_a = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_odd_b = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_even_a = new LNGVector<>();
    final LNGVector<Literal> tmp_lits_even_b = new LNGVector<>();

    for (int i = 0; i < a; i = i + 2)
      tmp_lits_odd_a.push(input_a.get(i));
    for (int i = 0; i < b; i = i + 2)
      tmp_lits_odd_b.push(input_b.get(i));
    for (int i = 1; i < a; i = i + 2)
      tmp_lits_even_a.push(input_a.get(i));
    for (int i = 1; i < b; i = i + 2)
      tmp_lits_even_b.push(input_b.get(i));

    merge(c / 2 + 1, tmp_lits_odd_a, tmp_lits_odd_b, formula, odd_merge, direction);
    merge(c / 2, tmp_lits_even_a, tmp_lits_even_b, formula, even_merge, direction);

    assert (odd_merge.size() > 0);

    output.push(odd_merge.get(0));

    int i = 1;
    int j = 0;
    while (true) {
      if (i < odd_merge.size() && j < even_merge.size()) {
        if (output.size() + 2 <= c) {
          final Variable z0 = f.newCCVariable();
          final Variable z1 = f.newCCVariable();
          comparator(odd_merge.get(i), even_merge.get(j), z0, z1, formula, direction);
          output.push(z0);
          output.push(z1);
          if (output.size() == c)
            break;
        } else if (output.size() + 1 == c) {
          final Variable z0 = f.newCCVariable();
          comparator(odd_merge.get(i), even_merge.get(j), z0, formula, direction);
          output.push(z0);
          break;
        }
      } else if (i >= odd_merge.size() && j >= even_merge.size())
        break;
      else if (i >= odd_merge.size()) {
        assert (j == even_merge.size() - 1);
        output.push(even_merge.back());
        break;
      } else {
        assert (i == odd_merge.size() - 1);
        output.push(odd_merge.back());
        break;
      }
      i++;
      j++;
    }
    assert (output.size() == a + b || output.size() == c);
  }

  private void direct_merger(int m, final LNGVector<Literal> input_a, LNGVector<Literal> input_b, final List<Formula> formula,
                             LNGVector<Literal> output, final ImplicationDirection direction) {
    assert (direction == INPUT_TO_OUTPUT);
    int a = input_a.size();
    int b = input_b.size();
    for (int i = 0; i < m; ++i)
      output.push(f.newCCVariable());
    int j = m < a ? m : a;
    for (int i = 0; i < j; ++i)
      formula.add(f.clause(input_a.get(i).negate(), output.get(i)));
    j = m < b ? m : b;
    for (int i = 0; i < j; ++i)
      formula.add(f.clause(input_b.get(i).negate(), output.get(i)));
    for (int i = 0; i < a; ++i)
      for (int k = 0; k < b; ++k)
        if (i + k + 1 < m)
          formula.add(f.clause(input_a.get(i).negate(), input_b.get(k).negate(), output.get(i + k + 1)));
  }

  private int recursive_sorter_value(int m, int n, int l, ImplicationDirection direction) {
    final Integer entry = this.recursive_sorter_l_values.get(new IntTriple(m, n, l));
    if (entry != null)
      return entry;
    final List<Formula> formula = new ArrayList<>();
    final LNGVector<Literal> input = new LNGVector<>();
    final LNGVector<Literal> output = new LNGVector<>();
    for (int i = 0; i < n; ++i)
      input.push(f.variable("v" + (i + 1)));
    int value = formula.size();
    recursive_sorter(m, l, input, formula, output, direction);
    recursive_sorter_l_values.put(new IntTriple(m, n, l), value);
    return value;
  }

  private int recursive_sorter_value(int m, int n, final ImplicationDirection direction) {
    final Integer entry = recursive_sorter_values.get(new IntPair(m, n));
    if (entry != null)
      return entry;
    final List<Formula> formula = new ArrayList<>();
    final LNGVector<Literal> input = new LNGVector<>();
    final LNGVector<Literal> output = new LNGVector<>();
    for (int i = 0; i < n; ++i)
      input.push(f.variable("v" + (i + 1)));
    recursive_sorter(m, input, formula, output, direction);
    int value = formula.size();
    recursive_sorter_values.put(new IntPair(m, n), value);
    return value;
  }

  private int counter_sorter_value(int m, int n) {
    return 2 * n + (m - 1) * (2 * (n - 1) - 1) - (m - 2) - 2 * ((m - 1) * (m - 2) / 2);
  }

  private int direct_sorter_value(int n) {
    if (n > 30)
      return Integer.MAX_VALUE;
    return (int) Math.pow(2, n) - 1;
  }

  private int direct_merger_value(int m, int a, int b) {
    return (a + b) * m - (m * m - m) / 2 - (a * a - a) / 2 - (b * b - b) / 2;
  }

  private int recursive_merger_value(int m, int a, int b, ImplicationDirection direction) {
    final Integer entry = recursive_merger_values.get(new IntTriple(m, a, b));
    if (entry != null)
      return entry;
    final List<Formula> formula = new ArrayList<>();
    final LNGVector<Literal> input_a = new LNGVector<>();
    final LNGVector<Literal> input_b = new LNGVector<>();
    final LNGVector<Literal> output = new LNGVector<>();
    for (int i = 0; i < a; ++i)
      input_a.push(f.variable("v" + (i + 1)));
    for (int i = 0; i < b; ++i)
      input_b.push(f.variable("v" + (i + a + 1)));
    recursive_merger(m, input_a, a, input_b, b, formula, output, direction);
    int value = formula.size();
    recursive_merger_values.put(new IntTriple(m, a, b), value);
    return value;
  }

  private static class IntPair {
    private final int a;
    private final int b;

    private IntPair(int a, int b) {
      this.a = a;
      this.b = b;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o)
        return true;
      if (!(o instanceof IntPair))
        return false;
      final IntPair intPair = (IntPair) o;
      return a == intPair.a && b == intPair.b;
    }

    @Override
    public int hashCode() {
      return Objects.hash(a, b);
    }

    @Override
    public String toString() {
      return "IntPair{a=" + a + ", b=" + b + '}';
    }
  }

  private static class IntTriple {
    private final int a;
    private final int b;
    private final int c;

    private IntTriple(int a, int b, int c) {
      this.a = a;
      this.b = b;
      this.c = c;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o)
        return true;
      if (!(o instanceof IntTriple))
        return false;
      final IntTriple intTriple = (IntTriple) o;
      return a == intTriple.a && b == intTriple.b && c == intTriple.c;
    }

    @Override
    public int hashCode() {
      return Objects.hash(a, b, c);
    }

    @Override
    public String toString() {
      return "IntTriple{a=" + a + ", b=" + b + ", c=" + c + '}';
    }
  }
}
