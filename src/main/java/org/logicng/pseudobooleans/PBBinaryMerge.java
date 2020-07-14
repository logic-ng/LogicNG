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
//  Copyright 2015-20xx Christoph Zengler                                //
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

/*
 * PBLib       -- Copyright (c) 2012-2013  Peter Steinke
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.pseudobooleans;

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.INPUT_TO_OUTPUT;

import org.logicng.cardinalityconstraints.CCSorting;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.List;

/**
 * The binary merge encoding for pseudo-Boolean constraints to CNF due to Manthey, Philipp, and Steinke.
 * @version 2.0.0
 * @since 1.1
 */
public final class PBBinaryMerge implements PBEncoding {

    private final FormulaFactory f;
    private final PBConfig config;
    private final CCSorting sorting;

    /**
     * Constructs a new binary merge encoding.
     * @param f      the formula factory
     * @param config the configuration
     */
    PBBinaryMerge(final FormulaFactory f, final PBConfig config) {
        this.f = f;
        this.config = config;
        this.sorting = new CCSorting();
    }

    @Override
    public List<Formula> encode(final LNGVector<Literal> lts, final LNGIntVector cffs, final int rhs, final List<Formula> formula) {
        final LNGVector<Literal> lits = new LNGVector<>(lts.size());
        for (final Literal lit : lts) {
            lits.push(lit);
        }
        final LNGIntVector coeffs = new LNGIntVector(cffs);
        final int maxWeight = maxWeight(coeffs);
        if (!this.config.binaryMergeUseGAC) {
            binary_merge(lts, new LNGIntVector(cffs), rhs, maxWeight, lits.size(), formula, null);
        } else {
            Literal x;
            boolean encode_complete_constraint = false;
            for (int i = 0; i < lits.size(); i++) {
                if (this.config.binaryMergeNoSupportForSingleBit && Double.compare(Math.floor(Math.log(coeffs.get(i)) / Math.log(2)), Math.log(coeffs.get(i)) / Math.log(2)) == 0) {
                    encode_complete_constraint = true;
                    continue;
                }
                final Literal tmpLit = lits.get(i);
                final int tmpCoeff = coeffs.get(i);
                lits.set(i, lits.back());
                coeffs.set(i, coeffs.back());
                lits.pop();
                coeffs.pop();
                x = tmpLit;
                if (maxWeight == tmpCoeff) {
                    int mw = 0;
                    for (int j = 0; j < coeffs.size(); j++) {
                        mw = Math.max(mw, coeffs.get(j));
                    }
                    if (rhs - tmpCoeff <= 0) {
                        for (int j = 0; j < lits.size(); j++) {
                            formula.add(this.f.clause(x.negate(), lits.get(j).negate()));
                        }
                    } else {
                        binary_merge(lits, coeffs, rhs - tmpCoeff, mw, lits.size(), formula, x.negate());
                    }
                } else {
                    if (rhs - tmpCoeff <= 0) {
                        for (int j = 0; j < lits.size(); j++) {
                            formula.add(this.f.clause(x.negate(), lits.get(j).negate()));
                        }
                    }
                    binary_merge(lits, coeffs, rhs - tmpCoeff, maxWeight, lits.size(), formula, x.negate());
                }
                if (i < lits.size()) {
                    lits.push(lits.get(i));
                    lits.set(i, tmpLit);
                    coeffs.push(coeffs.get(i));
                    coeffs.set(i, tmpCoeff);
                }
            }
            if (this.config.binaryMergeNoSupportForSingleBit && encode_complete_constraint) {
                binary_merge(lts, new LNGIntVector(cffs), rhs, maxWeight, lits.size(), formula, null);
            }
        }
        return formula;
    }

    private int maxWeight(final LNGIntVector weights) {
        int maxweight = Integer.MIN_VALUE;
        for (int i = 0; i < weights.size(); i++) {
            if (weights.get(i) > maxweight) {
                maxweight = weights.get(i);
            }
        }
        return maxweight;
    }

    private void binary_merge(final LNGVector<Literal> literals, final LNGIntVector coefficients, final int leq, final int maxWeight, final int n,
                              final List<Formula> formula, final Literal gac_lit) {
        final int less_then = leq + 1;
        final int p = (int) Math.floor(Math.log(maxWeight) / Math.log(2));
        final int m = (int) Math.ceil(less_then / Math.pow(2, p));
        final int new_less_then = (int) (m * Math.pow(2, p));
        final int T = (int) ((m * Math.pow(2, p)) - less_then);

        final Literal true_lit = this.f.newPBVariable();
        formula.add(true_lit);
        final LNGVector<LNGVector<Literal>> buckets = new LNGVector<>();
        int bit = 1;
        for (int i = 0; i <= p; i++) {
            buckets.push(new LNGVector<>());
            if ((T & bit) != 0) {
                buckets.get(i).push(true_lit);
            }
            for (int j = 0; j < n; j++) {
                if ((coefficients.get(j) & bit) != 0) {
                    if (gac_lit != null && coefficients.get(j) >= less_then) {
                        formula.add(this.f.clause(gac_lit, literals.get(j).negate()));
                    } else {
                        buckets.get(i).push(literals.get(j));
                    }
                }
            }
            bit = bit << 1;
        }
        final LNGVector<LNGVector<Literal>> bucket_card = new LNGVector<>(p + 1);
        final LNGVector<LNGVector<Literal>> bucket_merge = new LNGVector<>(p + 1);
        for (int i = 0; i < p + 1; i++) {
            bucket_card.push(new LNGVector<>());
            bucket_merge.push(new LNGVector<>());
        }
        assert bucket_card.size() == buckets.size();
        final LNGVector<Literal> carries = new LNGVector<>();
        final EncodingResult tempResul = EncodingResult.resultForFormula(this.f);
        for (int i = 0; i < buckets.size(); i++) {
            final int k = (int) Math.ceil(new_less_then / Math.pow(2, i));
            if (this.config.binaryMergeUseWatchDog) {
                totalizer(buckets.get(i), bucket_card.get(i), formula);
            } else {
                this.sorting.sort(k, buckets.get(i), tempResul, bucket_card.get(i), INPUT_TO_OUTPUT);
                formula.addAll(tempResul.result());
            }
            if (k <= buckets.get(i).size()) {
                assert k == bucket_card.get(i).size() || this.config.binaryMergeUseWatchDog;
                if (gac_lit != null) {
                    formula.add(this.f.clause(gac_lit, bucket_card.get(i).get(k - 1).negate()));
                } else {
                    formula.add(this.f.clause(bucket_card.get(i).get(k - 1).negate()));
                }
            }
            if (i > 0) {
                if (carries.size() > 0) {
                    if (bucket_card.get(i).size() == 0) {
                        bucket_merge.set(i, carries);
                    } else {
                        if (this.config.binaryMergeUseWatchDog) {
                            unary_adder(bucket_card.get(i), carries, bucket_merge.get(i), formula);
                        } else {
                            this.sorting.merge(k, bucket_card.get(i), carries, tempResul, bucket_merge.get(i), INPUT_TO_OUTPUT);
                            formula.addAll(tempResul.result());
                        }
                        if (k == bucket_merge.get(i).size() || (this.config.binaryMergeUseWatchDog && k <= bucket_merge.get(i).size())) {
                            if (gac_lit != null) {
                                formula.add(this.f.clause(gac_lit, bucket_merge.get(i).get(k - 1).negate()));
                            } else {
                                formula.add(this.f.clause(bucket_merge.get(i).get(k - 1).negate()));
                            }
                        }
                    }
                } else {
                    bucket_merge.get(i).replaceInplace(bucket_card.get(i));
                }
            }
            carries.clear();
            if (i == 0) {
                for (int j = 1; j < bucket_card.get(0).size(); j = j + 2) {
                    carries.push(bucket_card.get(0).get(j));
                }
            } else {
                for (int j = 1; j < bucket_merge.get(i).size(); j = j + 2) {
                    carries.push(bucket_merge.get(i).get(j));
                }
            }
        }
    }

    private void totalizer(final LNGVector<Literal> x, final LNGVector<Literal> u_x, final List<Formula> formula) {
        u_x.clear();
        if (x.size() == 0) {
            return;
        }
        if (x.size() == 1) {
            u_x.push(x.get(0));
        } else {
            for (int i = 0; i < x.size(); i++) {
                u_x.push(this.f.newPBVariable());
            }
            final LNGVector<Literal> x_1 = new LNGVector<>();
            final LNGVector<Literal> x_2 = new LNGVector<>();
            int i = 0;
            for (; i < x.size() / 2; i++) {
                x_1.push(x.get(i));
            }
            for (; i < x.size(); i++) {
                x_2.push(x.get(i));
            }
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
            for (int i = 0; i < v.size(); i++) {
                w.push(v.get(i));
            }
        } else if (v.size() == 0) {
            for (int i = 0; i < u.size(); i++) {
                w.push(u.get(i));
            }
        } else {
            for (int i = 0; i < u.size() + v.size(); i++) {
                w.push(this.f.newPBVariable());
            }
            for (int a = 0; a < u.size(); a++) {
                for (int b = 0; b < v.size(); b++) {
                    formula.add(this.f.clause(u.get(a).negate(), v.get(b).negate(), w.get(a + b + 1)));
                }
            }
            for (int i = 0; i < v.size(); i++) {
                formula.add(this.f.clause(v.get(i).negate(), w.get(i)));
            }
            for (int i = 0; i < u.size(); i++) {
                formula.add(this.f.clause(u.get(i).negate(), w.get(i)));
            }
        }
    }
}
