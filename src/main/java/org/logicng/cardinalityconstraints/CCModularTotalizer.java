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
 */

package org.logicng.cardinalityconstraints;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * Modular Totalizer.
 * @version 2.0.0
 * @since 1.0
 */
public final class CCModularTotalizer {

    private final Variable varUndef;
    private final Variable varError;

    private final Variable h0;
    private LNGVector<Literal> inlits;
    private LNGVector<Literal> cardinalityUpOutvars;
    private LNGVector<Literal> cardinalityLwOutvars;
    private int currentCardinalityRhs;
    private EncodingResult result;
    private CCIncrementalData incData;

    /**
     * Constructs a new modular totalizer.
     */
    CCModularTotalizer(final FormulaFactory f) {
        this.varUndef = f.variable("RESERVED@VAR_UNDEF");
        this.varError = f.variable("RESERVED@VAR_ERROR");
        this.h0 = this.varUndef;
        this.currentCardinalityRhs = -1;
        this.inlits = new LNGVector<>();
    }

    /**
     * Builds an at-most-k constraint.
     * @param result the result of the encoding
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     */
    void buildAMK(final EncodingResult result, final Variable[] vars, final int rhs) {
        final int mod = this.initialize(result, rhs, vars.length);
        for (final Variable var : vars) {
            this.inlits.push(var);
        }
        this.toCNF(mod, this.cardinalityUpOutvars, this.cardinalityLwOutvars, vars.length);
        assert this.inlits.size() == 0;
        this.encodeOutput(rhs, mod);
        this.currentCardinalityRhs = rhs + 1;
        this.incData = new CCIncrementalData(result, CCConfig.AMK_ENCODER.MODULAR_TOTALIZER, rhs, this.cardinalityUpOutvars,
                this.cardinalityLwOutvars, mod);
    }

    /**
     * Builds an at-least-k constraint.
     * @param result the result of the encoding
     * @param vars   the variables of the constraint
     * @param rhs    the right hand side of the constraint
     */
    void buildALK(final EncodingResult result, final Variable[] vars, final int rhs) {
        final int newRHS = vars.length - rhs;
        final int mod = this.initialize(result, newRHS, vars.length);
        for (final Variable var : vars) {
            this.inlits.push(var.negate());
        }
        this.toCNF(mod, this.cardinalityUpOutvars, this.cardinalityLwOutvars, vars.length);
        assert this.inlits.size() == 0;
        this.encodeOutput(newRHS, mod);
        this.currentCardinalityRhs = newRHS + 1;
        this.incData = new CCIncrementalData(result, CCConfig.ALK_ENCODER.MODULAR_TOTALIZER, rhs, vars.length,
                this.cardinalityUpOutvars, this.cardinalityLwOutvars, mod);
    }

    /**
     * Returns the incremental data of this encoding.
     * @return the incremental data of this encoding
     */
    CCIncrementalData incrementalData() {
        return this.incData;
    }

    private int initialize(final EncodingResult result, final int rhs, final int n) {
        result.reset();
        this.result = result;
        this.cardinalityUpOutvars = new LNGVector<>();
        this.cardinalityLwOutvars = new LNGVector<>();
        final int mod = (int) Math.ceil(Math.sqrt(rhs + 1.0));
        this.cardinalityUpOutvars = new LNGVector<>(n / mod);
        for (int i = 0; i < n / mod; i++) {
            this.cardinalityUpOutvars.push(this.result.newVariable());
        }
        this.cardinalityLwOutvars = new LNGVector<>(mod - 1);
        for (int i = 0; i < mod - 1; i++) {
            this.cardinalityLwOutvars.push(this.result.newVariable());
        }
        this.inlits = new LNGVector<>(n);
        this.currentCardinalityRhs = rhs + 1;
        if (this.cardinalityUpOutvars.size() == 0) {
            this.cardinalityUpOutvars.push(this.h0);
        }
        return mod;
    }

    private void encodeOutput(final int rhs, final int mod) {
        assert this.cardinalityUpOutvars.size() != 0 || this.cardinalityLwOutvars.size() != 0;
        final int ulimit = (rhs + 1) / mod;
        final int llimit = (rhs + 1) - ulimit * mod;
        assert ulimit <= this.cardinalityUpOutvars.size();
        assert llimit <= this.cardinalityLwOutvars.size();
        for (int i = ulimit; i < this.cardinalityUpOutvars.size(); i++) {
            this.result.addClause(this.cardinalityUpOutvars.get(i).negate());
        }
        if (ulimit != 0 && llimit != 0) {
            for (int i = llimit - 1; i < this.cardinalityLwOutvars.size(); i++) {
                this.result.addClause(this.cardinalityUpOutvars.get(ulimit - 1).negate(), this.cardinalityLwOutvars.get(i).negate());
            }
        } else {
            if (ulimit == 0) {
                assert llimit != 0;
                for (int i = llimit - 1; i < this.cardinalityLwOutvars.size(); i++) {
                    this.result.addClause(this.cardinalityLwOutvars.get(i).negate());
                }
            } else {
                this.result.addClause(this.cardinalityUpOutvars.get(ulimit - 1).negate());
            }
        }
    }

    private void toCNF(final int mod, final LNGVector<Literal> ubvars, final LNGVector<Literal> lwvars, final int rhs) {
        final LNGVector<Literal> lupper = new LNGVector<>();
        final LNGVector<Literal> llower = new LNGVector<>();
        final LNGVector<Literal> rupper = new LNGVector<>();
        final LNGVector<Literal> rlower = new LNGVector<>();
        assert rhs > 1;
        final int split = rhs / 2;
        int left = 1;
        int right = 1;
        if (split == 1) {
            assert this.inlits.size() > 0;
            lupper.push(this.h0);
            llower.push(this.inlits.back());
            this.inlits.pop();
        } else {
            left = split / mod;
            for (int i = 0; i < left; i++) {
                lupper.push(this.result.newVariable());
            }
            int limit = mod - 1;
            if (left % mod == 0 && split < mod - 1) {
                limit = split;
            }
            for (int i = 0; i < limit; i++) {
                llower.push(this.result.newVariable());
            }
        }
        if (rhs - split == 1) {
            assert this.inlits.size() > 0;
            rupper.push(this.h0);
            rlower.push(this.inlits.back());
            this.inlits.pop();
        } else {
            right = (rhs - split) / mod;
            for (int i = 0; i < right; i++) {
                rupper.push(this.result.newVariable());
            }
            int limit = mod - 1;
            if (right % mod == 0 && rhs - split < mod - 1) {
                limit = rhs - split;
            }
            for (int i = 0; i < limit; i++) {
                rlower.push(this.result.newVariable());
            }
        }
        if (lupper.size() == 0) {
            lupper.push(this.h0);
        }
        if (rupper.size() == 0) {
            rupper.push(this.h0);
        }
        this.adder(mod, ubvars, lwvars, rupper, rlower, lupper, llower);
        int val = left * mod + split - left * mod;
        if (val > 1) {
            this.toCNF(mod, lupper, llower, val);
        }
        val = right * mod + (rhs - split) - right * mod;
        if (val > 1) {
            this.toCNF(mod, rupper, rlower, val);
        }
    }

    private void adder(final int mod, final LNGVector<Literal> upper, final LNGVector<Literal> lower,
                       final LNGVector<Literal> lupper, final LNGVector<Literal> llower, final LNGVector<Literal> rupper,
                       final LNGVector<Literal> rlower) {
        assert upper.size() != 0;
        assert lower.size() >= llower.size() && lower.size() >= rlower.size();
        Variable carry = this.varUndef;
        if (upper.get(0) != this.h0) // != is ok here - we are within the same formula factory
        {
            carry = this.result.newVariable();
        }
        for (int i = 0; i <= llower.size(); i++) {
            for (int j = 0; j <= rlower.size(); j++) {
                if (i + j > this.currentCardinalityRhs + 1 && this.currentCardinalityRhs + 1 < mod) {
                    continue;
                }
                if (i + j < mod) {
                    if (i == 0 && j != 0) {
                        if (upper.get(0) != this.h0) {
                            this.result.addClause(rlower.get(j - 1).negate(), lower.get(i + j - 1), carry);
                        } else {
                            this.result.addClause(rlower.get(j - 1).negate(), lower.get(i + j - 1));
                        }
                    } else if (j == 0 && i != 0) {
                        if (upper.get(0) != this.h0) {
                            this.result.addClause(llower.get(i - 1).negate(), lower.get(i + j - 1), carry);
                        } else {
                            this.result.addClause(llower.get(i - 1).negate(), lower.get(i + j - 1));
                        }
                    } else if (i != 0) {
                        if (upper.get(0) != this.h0) {
                            this.result.addClause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get(i + j - 1), carry);
                        } else {
                            assert i + j - 1 < lower.size();
                            this.result.addClause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get(i + j - 1));
                        }
                    }
                } else if (i + j > mod) {
                    assert i + j > 0;
                    this.result.addClause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), lower.get((i + j) % mod - 1));
                } else {
                    assert i + j == mod;
                    assert carry != this.varUndef;
                    this.result.addClause(llower.get(i - 1).negate(), rlower.get(j - 1).negate(), carry);
                }
            }
        }
        if (upper.get(0) != this.h0) {
            this.finalAdder(mod, upper, lupper, rupper, carry);
        }
    }

    private void finalAdder(final int mod, final LNGVector<Literal> upper, final LNGVector<Literal> lupper,
                            final LNGVector<Literal> rupper, final Variable carry) {
        for (int i = 0; i <= lupper.size(); i++) {
            for (int j = 0; j <= rupper.size(); j++) {
                Literal a = this.varError;
                Literal b = this.varError;
                Literal c = this.varError;
                Literal d = this.varError;
                int closeMod = this.currentCardinalityRhs / mod;
                if (this.currentCardinalityRhs % mod != 0) {
                    closeMod++;
                }
                if (mod * (i + j) > closeMod * mod) {
                    continue;
                }
                if (i != 0) {
                    a = lupper.get(i - 1);
                }
                if (j != 0) {
                    b = rupper.get(j - 1);
                }
                if (i + j != 0 && i + j - 1 < upper.size()) {
                    c = upper.get(i + j - 1);
                }
                if (i + j < upper.size()) {
                    d = upper.get(i + j);
                }
                if (c != this.varUndef && c != this.varError) {
                    final LNGVector<Literal> clause = new LNGVector<>();
                    if (a != this.varUndef && a != this.varError) {
                        clause.push(a.negate());
                    }
                    if (b != this.varUndef && b != this.varError) {
                        clause.push(b.negate());
                    }
                    clause.push(c);
                    if (clause.size() > 1) {
                        this.result.addClause(clause);
                    }
                }
                final LNGVector<Literal> clause = new LNGVector<>();
                clause.push(carry.negate());
                if (a != this.varUndef && a != this.varError) {
                    clause.push(a.negate());
                }
                if (b != this.varUndef && b != this.varError) {
                    clause.push(b.negate());
                }
                if (d != this.varError && d != this.varUndef) {
                    clause.push(d);
                }
                if (clause.size() > 1) {
                    this.result.addClause(clause);
                }
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
