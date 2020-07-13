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

package org.logicng.solvers.maxsat.encodings;

import static org.logicng.solvers.sat.MiniSatStyleSolver.LIT_UNDEF;
import static org.logicng.solvers.sat.MiniSatStyleSolver.mkLit;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

import org.logicng.collections.LNGIntVector;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * Encodes that at most 'rhs' literals can be assigned value true.  Uses the modular totalizer encoding for
 * translating the cardinality constraint into CNF.
 * @version 2.0.0
 * @since 1.0
 */
public class ModularTotalizer extends Encoding {

    protected static final int LIT_ERROR = -2;

    protected final int h0;
    protected final LNGIntVector cardinalityUpoutlits;
    protected final LNGIntVector cardinalityLwoutlits;
    protected int modulo;
    protected LNGIntVector cardinalityInlits;
    protected int currentCardinalityRhs;

    /**
     * Constructs a new modular totalizer.
     */
    ModularTotalizer() {
        this.h0 = LIT_UNDEF;
        this.modulo = -1;
        this.currentCardinalityRhs = -1;
        this.cardinalityInlits = new LNGIntVector();
        this.cardinalityUpoutlits = new LNGIntVector();
        this.cardinalityLwoutlits = new LNGIntVector();
    }

    /**
     * Sets the modulo value.
     * @param m the modulo value
     */
    void setModulo(final int m) {
        this.modulo = m;
    }

    /**
     * Returns {@code true} if an encoding was created, {@code false} otherwise.
     * @return {@code true} if an encoding was created
     */
    boolean hasCreatedEncoding() {
        return this.hasEncoding;
    }

    /**
     * Encodes a cardinality constraint.
     * @param s    the solver
     * @param lits the literals of the constraint
     * @param rhs  the right hand side of the constraint
     */
    public void encode(final MiniSatStyleSolver s, final LNGIntVector lits, final int rhs) {
        assert lits.size() > 0;
        this.hasEncoding = false;
        this.cardinalityUpoutlits.clear();
        this.cardinalityLwoutlits.clear();
        if (rhs == 0) {
            for (int i = 0; i < lits.size(); i++) {
                addUnitClause(s, not(lits.get(i)));
            }
            return;
        }
        assert rhs >= 1 && rhs <= lits.size();
        if (rhs == lits.size()) {
            return;
        }
        this.hasEncoding = true;
        int mod = (int) Math.ceil(Math.sqrt(rhs + 1.0));
        if (this.modulo == -1) {
            this.modulo = mod;
        } else {
            mod = this.modulo;
        }
        for (int i = 0; i < lits.size() / mod; i++) {
            final int p = mkLit(s.nVars(), false);
            MaxSAT.newSATVariable(s);
            this.cardinalityUpoutlits.push(p);
        }
        for (int i = 0; i < mod - 1; i++) {
            final int p = mkLit(s.nVars(), false);
            MaxSAT.newSATVariable(s);
            this.cardinalityLwoutlits.push(p);
        }
        this.cardinalityInlits = new LNGIntVector(lits);
        this.currentCardinalityRhs = rhs + 1;
        if (this.cardinalityUpoutlits.size() == 0) {
            this.cardinalityUpoutlits.push(this.h0);
        }
        this.toCNF(s, mod, this.cardinalityUpoutlits, this.cardinalityLwoutlits, lits.size());
        assert this.cardinalityInlits.size() == 0;
        this.update(s, rhs);
    }

    /**
     * Updates the right hand side of the current constraint.
     * @param s   the solver
     * @param rhs the new right hand side
     */
    public void update(final MiniSatStyleSolver s, final int rhs) {
        assert this.currentCardinalityRhs != -1;
        assert this.hasEncoding;
        this.encodeOutput(s, rhs);
        this.currentCardinalityRhs = rhs + 1;
    }

    protected void encodeOutput(final MiniSatStyleSolver s, final int rhs) {
        assert this.hasEncoding;
        assert this.cardinalityUpoutlits.size() != 0 || this.cardinalityLwoutlits.size() != 0;
        final int mod = this.modulo;
        final int ulimit = (rhs + 1) / mod;
        final int llimit = (rhs + 1) - ulimit * mod;
        assert ulimit <= this.cardinalityUpoutlits.size();
        assert llimit <= this.cardinalityLwoutlits.size();
        for (int i = ulimit; i < this.cardinalityUpoutlits.size(); i++) {
            addUnitClause(s, not(this.cardinalityUpoutlits.get(i)));
        }
        if (ulimit != 0 && llimit != 0) {
            for (int i = llimit - 1; i < this.cardinalityLwoutlits.size(); i++) {
                addBinaryClause(s, not(this.cardinalityUpoutlits.get(ulimit - 1)), not(this.cardinalityLwoutlits.get(i)));
            }
        } else {
            if (ulimit == 0) {
                assert llimit != 0;
                for (int i = llimit - 1; i < this.cardinalityLwoutlits.size(); i++) {
                    addUnitClause(s, not(this.cardinalityLwoutlits.get(i)));
                }
            } else {
                addUnitClause(s, not(this.cardinalityUpoutlits.get(ulimit - 1)));
            }
        }
    }

    protected void toCNF(final MiniSatStyleSolver s, final int mod, final LNGIntVector ublits, final LNGIntVector lwlits, final int rhs) {
        final LNGIntVector lupper = new LNGIntVector();
        final LNGIntVector llower = new LNGIntVector();
        final LNGIntVector rupper = new LNGIntVector();
        final LNGIntVector rlower = new LNGIntVector();
        assert rhs > 1;
        final int split = rhs / 2;
        int left = 1;
        int right = 1;
        if (split == 1) {
            assert this.cardinalityInlits.size() > 0;
            lupper.push(this.h0);
            llower.push(this.cardinalityInlits.back());
            this.cardinalityInlits.pop();
        } else {
            left = split / mod;
            for (int i = 0; i < left; i++) {
                final int p = mkLit(s.nVars(), false);
                MaxSAT.newSATVariable(s);
                lupper.push(p);
            }
            int limit = mod - 1;
            if (left % mod == 0 && split < mod - 1) {
                limit = split;
            }
            for (int i = 0; i < limit; i++) {
                final int p = mkLit(s.nVars(), false);
                MaxSAT.newSATVariable(s);
                llower.push(p);
            }
        }
        if (rhs - split == 1) {
            assert this.cardinalityInlits.size() > 0;
            rupper.push(this.h0);
            rlower.push(this.cardinalityInlits.back());
            this.cardinalityInlits.pop();
        } else {
            right = (rhs - split) / mod;
            for (int i = 0; i < right; i++) {
                final int p = mkLit(s.nVars(), false);
                MaxSAT.newSATVariable(s);
                rupper.push(p);
            }
            int limit = mod - 1;
            if (right % mod == 0 && rhs - split < mod - 1) {
                limit = rhs - split;
            }
            for (int i = 0; i < limit; i++) {
                final int p = mkLit(s.nVars(), false);
                MaxSAT.newSATVariable(s);
                rlower.push(p);
            }
        }
        if (lupper.size() == 0) {
            lupper.push(this.h0);
        }
        if (rupper.size() == 0) {
            rupper.push(this.h0);
        }
        this.adder(s, mod, ublits, lwlits, rupper, rlower, lupper, llower);
        if (left * mod + split - left * mod > 1) {
            this.toCNF(s, mod, lupper, llower, left * mod + split - left * mod);
        }
        if (right * mod + (rhs - split) - right * mod > 1) {
            this.toCNF(s, mod, rupper, rlower, right * mod + (rhs - split) - right * mod);
        }
    }

    protected void adder(final MiniSatStyleSolver s, final int mod, final LNGIntVector upper, final LNGIntVector lower,
                         final LNGIntVector lupper, final LNGIntVector llower, final LNGIntVector rupper,
                         final LNGIntVector rlower) {
        assert upper.size() != 0;
        assert lower.size() >= llower.size() && lower.size() >= rlower.size();
        int carry = LIT_UNDEF;
        if (upper.get(0) != this.h0) {
            carry = mkLit(s.nVars(), false);
            MaxSAT.newSATVariable(s);
        }
        for (int i = 0; i <= llower.size(); i++) {
            for (int j = 0; j <= rlower.size(); j++) {
                if (i + j > this.currentCardinalityRhs + 1 && this.currentCardinalityRhs + 1 < this.modulo) {
                    continue;
                }
                if (i + j < mod) {
                    if (i == 0 && j != 0) {
                        if (upper.get(0) != this.h0) {
                            addTernaryClause(s, not(rlower.get(j - 1)), lower.get(i + j - 1), carry);
                        } else {
                            addBinaryClause(s, not(rlower.get(j - 1)), lower.get(i + j - 1));
                        }
                    } else if (j == 0 && i != 0) {
                        if (upper.get(0) != this.h0) {
                            addTernaryClause(s, not(llower.get(i - 1)), lower.get(i + j - 1), carry);
                        } else {
                            addBinaryClause(s, not(llower.get(i - 1)), lower.get(i + j - 1));
                        }
                    } else if (i != 0) {
                        if (upper.get(0) != this.h0) {
                            addQuaternaryClause(s, not(llower.get(i - 1)), not(rlower.get(j - 1)), lower.get(i + j - 1), carry);
                        } else {
                            assert i + j - 1 < lower.size();
                            addTernaryClause(s, not(llower.get(i - 1)), not(rlower.get(j - 1)), lower.get(i + j - 1));
                        }
                    }
                } else if (i + j > mod) {
                    assert i + j > 0;
                    addTernaryClause(s, not(llower.get(i - 1)), not(rlower.get(j - 1)), lower.get((i + j) % mod - 1));
                } else {
                    assert i + j == mod;
                    assert carry != LIT_UNDEF;
                    addTernaryClause(s, not(llower.get(i - 1)), not(rlower.get(j - 1)), carry);
                }
            }
        }
        if (upper.get(0) != this.h0) {
            for (int i = 0; i <= lupper.size(); i++) {
                for (int j = 0; j <= rupper.size(); j++) {
                    int a = LIT_ERROR;
                    int b = LIT_ERROR;
                    int c = LIT_ERROR;
                    int d = LIT_ERROR;
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
                    if (c != LIT_UNDEF && c != LIT_ERROR) {
                        final LNGIntVector clause = new LNGIntVector();
                        if (a != LIT_UNDEF && a != LIT_ERROR) {
                            clause.push(not(a));
                        }
                        if (b != LIT_UNDEF && b != LIT_ERROR) {
                            clause.push(not(b));
                        }
                        clause.push(c);
                        if (clause.size() > 1) {
                            s.addClause(clause, null);
                        }
                    }
                    final LNGIntVector clause = new LNGIntVector();
                    clause.push(not(carry));
                    if (a != LIT_UNDEF && a != LIT_ERROR) {
                        clause.push(not(a));
                    }
                    if (b != LIT_UNDEF && b != LIT_ERROR) {
                        clause.push(not(b));
                    }
                    if (d != LIT_ERROR && d != LIT_UNDEF) {
                        clause.push(d);
                    }
                    if (clause.size() > 1) {
                        s.addClause(clause, null);
                    }
                }
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
