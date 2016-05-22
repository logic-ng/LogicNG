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

package org.logicng.pseudobooleans;

import org.logicng.cardinalityconstraints.CCSorting;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.INPUT_TO_OUTPUT;

/**
 * The binary merge encoding for pseudo-Boolean constraints to CNF due to Manthey, Philipp, and Steinke.
 * @version 1.1
 * @since 1.1
 */
public final class PBBinaryMerge extends PBEncoder {

  private static boolean USE_GAC_BINARY_MERGE = false;
  private static boolean BINARY_MERGE_NO_SUPPORT_FOR_SINGLE_BITS = true;
  private static boolean USE_WATCH_DOG_ENCODING_IN_BINARY_MERGER = true;
  private Literal true_lit;
  private final CCSorting sorting;

  /**
   * Constructs a new binary merge encoding.
   * @param f the formula factory
   */
  public PBBinaryMerge(final FormulaFactory f) {
    super(f);
    this.sorting = new CCSorting(f);
  }

  @Override
  public ImmutableFormulaList build(Literal[] lts, int[] cffs, int rhs) {
    final List<Formula> formula = new ArrayList<>();
    final LNGVector<Literal> lits = new LNGVector<>(lts);
    final LNGIntVector coeffs = new LNGIntVector(cffs);
    int maxWeight = maxWeight(coeffs);
    if (!USE_GAC_BINARY_MERGE)
      binary_merge(new LNGVector<>(lts), new LNGIntVector(cffs), rhs, maxWeight, lits.size(), formula, null);
    else {
      Literal x;
      boolean encode_complete_constrainte = false;
      for (int i = 0; i < lits.size(); ++i) {
        if (BINARY_MERGE_NO_SUPPORT_FOR_SINGLE_BITS && Double.compare(Math.floor(Math.log(coeffs.get(i)) / Math.log(2)), Math.log(coeffs.get(i)) / Math.log(2)) == 0) {
          encode_complete_constrainte = true;
          continue;
        }
        Literal tmpLit = lits.get(i);
        int tmpCoeff = coeffs.get(i);
        lits.set(i, lits.back());
        coeffs.set(i, coeffs.back());
        lits.pop();
        coeffs.pop();
        x = tmpLit;
        if (maxWeight == tmpCoeff) {
          int mw = 0;
          for (int j = 0; j < lits.size(); ++j)
            mw = Math.max(mw, coeffs.get(j));
          if (rhs <= 0)
            for (int j = 0; j < lits.size(); ++j)
              formula.add(f.clause(x.negate(), lits.get(j).negate()));
          else
            binary_merge(lits, coeffs, rhs - tmpCoeff, mw, lits.size(), formula, x.negate());
        } else {
          if (rhs <= 0)
            for (int j = 0; j < lits.size(); ++j)
              formula.add(f.clause(x.negate(), lits.get(j).negate()));
          binary_merge(lits, coeffs, rhs - tmpCoeff, maxWeight, lits.size(), formula, x.negate());
        }
        if (i < lits.size()) {
          lits.push(lits.get(i));
          lits.set(i, tmpLit);
          coeffs.push(coeffs.get(i));
          coeffs.set(i, tmpCoeff);
        }
      }
      if (BINARY_MERGE_NO_SUPPORT_FOR_SINGLE_BITS && encode_complete_constrainte)
        binary_merge(new LNGVector<>(lts), new LNGIntVector(cffs), rhs, maxWeight, lits.size(), formula, null);
    }
    return new ImmutableFormulaList(FType.AND, formula);
  }

  private int maxWeight(final LNGIntVector weights) {
    int maxweight = Integer.MIN_VALUE;
    for (int i = 0; i < weights.size(); i++)
      if (weights.get(i) > maxweight)
        maxweight = weights.get(i);
    return maxweight;
  }

  private void binary_merge(LNGVector<Literal> literals, LNGIntVector coefficients, int leq, int maxWeight, int n,
                            final List<Formula> formula, final Literal gac_lit) {
    int less_then = leq + 1;
    int p = (int) Math.floor(Math.log(maxWeight) / Math.log(2));
    int m = (int) Math.ceil((double) (less_then) / (Math.pow(2, p)));
    int new_less_thean = (int) (m * Math.pow(2, p));
    int T = (int) ((m * Math.pow(2, p))) - (less_then);
    true_lit = f.newPBVariable();
    formula.add(true_lit);
    LNGVector<LNGVector<Literal>> buckets = new LNGVector<>();
    int bit = 1;
    for (int i = 0; i <= p; i++) {
      buckets.push(new LNGVector<Literal>());
      if ((T & bit) != 0)
        buckets.get(i).push(true_lit);
      for (int j = 0; j < n; j++) {
        if ((coefficients.get(j) & bit) != 0) {
          if (gac_lit != null && coefficients.get(j) >= less_then)
            formula.add(f.clause(gac_lit, literals.get(j).negate()));
          else
            buckets.get(i).push(literals.get(j));
        }
      }
      bit = bit << 1;
    }
    LNGVector<LNGVector<Literal>> bucket_card = new LNGVector<>(p + 1);
    LNGVector<LNGVector<Literal>> bucket_merge = new LNGVector<>(p + 1);
    for (int i = 0; i < p + 1; i++) {
      bucket_card.push(new LNGVector<Literal>());
      bucket_merge.push(new LNGVector<Literal>());
    }
    assert (bucket_card.size() == buckets.size());
    LNGVector<Literal> carries = new LNGVector<>();
    for (int i = 0; i < buckets.size(); ++i) {
      int k = (int) Math.ceil(new_less_thean / Math.pow(2, i));
      if (USE_WATCH_DOG_ENCODING_IN_BINARY_MERGER)
        totalizer(buckets.get(i), bucket_card.get(i), formula);
      else
        sorting.sort(k, buckets.get(i), formula, bucket_card.get(i), INPUT_TO_OUTPUT);
      if (k <= buckets.get(i).size()) {
        assert (k == bucket_card.get(i).size() || USE_WATCH_DOG_ENCODING_IN_BINARY_MERGER);
        if (gac_lit != null)
          formula.add(f.clause(gac_lit, bucket_card.get(i).get(k - 1).negate()));
        else
          formula.add(f.clause(bucket_card.get(i).get(k - 1).negate()));
      }
      if (i > 0) {
        if (carries.size() > 0) {
          if (bucket_card.get(i).size() == 0) {
            bucket_merge.set(i, carries);
          } else {
            if (USE_WATCH_DOG_ENCODING_IN_BINARY_MERGER)
              unary_adder(bucket_card.get(i), carries, bucket_merge.get(i), formula);
            else
              sorting.merge(k, bucket_card.get(i), carries, formula, bucket_merge.get(i), INPUT_TO_OUTPUT);

            if (k == bucket_merge.get(i).size() || (USE_WATCH_DOG_ENCODING_IN_BINARY_MERGER && k <= bucket_merge.get(i).size())) {
              if (gac_lit != null)
                formula.add(f.clause(gac_lit, bucket_merge.get(i).get(k - 1).negate()));
              else
                formula.add(f.clause(bucket_merge.get(i).get(k - 1).negate()));
            }
          }
        } else {
          bucket_merge.get(i).replaceInplace(bucket_card.get(i));
        }
      }
      carries.clear();
      if (i == 0) {
        for (int j = 1; j < bucket_card.get(0).size(); j = j + 2)
          carries.push(bucket_card.get(0).get(j));
      } else {
        for (int j = 1; j < bucket_merge.get(i).size(); j = j + 2)
          carries.push(bucket_merge.get(i).get(j));
      }
    }
  }

  private void totalizer(final LNGVector<Literal> x, final LNGVector<Literal> u_x, final List<Formula> formula) {
    u_x.clear();
    if (x.size() == 0)
      return;
    if (x.size() == 1) {
      u_x.push(x.get(0));
    } else {
      for (int i = 0; i < x.size(); ++i)
        u_x.push(f.newPBVariable());
      final LNGVector<Literal> x_1 = new LNGVector<>();
      final LNGVector<Literal> x_2 = new LNGVector<>();
      int i = 0;
      for (; i < x.size() / 2; ++i)
        x_1.push(x.get(i));
      for (; i < x.size(); ++i)
        x_2.push(x.get(i));
      final LNGVector<Literal> u_x_1 = new LNGVector<>();
      final LNGVector<Literal> u_x_2 = new LNGVector<>();
      totalizer(x_1, u_x_1, formula);
      totalizer(x_2, u_x_2, formula);
      unary_adder(u_x_1, u_x_2, u_x, formula);
    }
  }

  private void unary_adder(final LNGVector<Literal> u, final LNGVector<Literal> v, final LNGVector<Literal> w,
                           final List<Formula> formula) {
    w.clear();
    if (u.size() == 0) {
      for (int i = 0; i < v.size(); ++i)
        w.push(v.get(i));
    } else if (v.size() == 0) {
      for (int i = 0; i < u.size(); ++i)
        w.push(u.get(i));
    } else {
      for (int i = 0; i < u.size() + v.size(); ++i)
        w.push(f.newPBVariable());
      for (int a = 0; a < u.size(); ++a)
        for (int b = 0; b < v.size(); ++b)
          formula.add(f.clause(u.get(a).negate(), v.get(b).negate(), w.get(a + b + 1)));
      for (int i = 0; i < v.size(); ++i)
        formula.add(f.clause(v.get(i).negate(), w.get(i)));
      for (int i = 0; i < u.size(); ++i)
        formula.add(f.clause(u.get(i), w.get(i)));
    }
  }
}
