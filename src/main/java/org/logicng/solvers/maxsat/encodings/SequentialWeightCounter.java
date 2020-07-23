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

import static org.logicng.solvers.maxsat.algorithms.MaxSAT.newSATVariable;
import static org.logicng.solvers.sat.MiniSatStyleSolver.LIT_UNDEF;
import static org.logicng.solvers.sat.MiniSatStyleSolver.mkLit;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * A sequential weight counter for the encoding of pseudo-Boolean constraints in CNF.
 * @version 2.0.0
 * @since 1.0
 */
public class SequentialWeightCounter extends Encoding {

    protected final LNGIntVector pbOutlits;
    protected final LNGIntVector unitLits;
    protected final LNGIntVector unitCoeffs;
    protected int currentPbRhs;
    protected int currentLitBlocking;
    protected LNGVector<LNGIntVector> seqAuxiliaryInc;
    protected LNGIntVector litsInc;
    protected LNGIntVector coeffsInc;

    /**
     * Constructs a new sequential weight counter encoder.
     */
    SequentialWeightCounter() {
        this.currentPbRhs = -1;
        this.currentLitBlocking = LIT_UNDEF;
        this.pbOutlits = new LNGIntVector();
        this.unitLits = new LNGIntVector();
        this.unitCoeffs = new LNGIntVector();
        this.seqAuxiliaryInc = new LNGVector<>();
        this.litsInc = new LNGIntVector();
        this.coeffsInc = new LNGIntVector();
    }

    /**
     * Updates the assumptions with the unit literals.
     * @param assumptions the current assumptions
     */
    void updateAssumptions(final LNGIntVector assumptions) {
        assumptions.push(not(this.currentLitBlocking));
        for (int i = 0; i < this.unitLits.size(); i++) {
            assumptions.push(not(this.unitLits.get(i)));
        }
    }

    /**
     * Returns {@code true} if an encoding was created, {@code false} otherwise.
     * @return {@code true} if an encoding was created
     */
    boolean hasCreatedEncoding() {
        return this.hasEncoding;
    }

    /**
     * Encodes the pseudo-Boolean constraint
     * @param s      the solver
     * @param lits   the literals of the constraint
     * @param coeffs the coefficients of the constraints
     * @param rhs    the right hand side of the constraint
     */
    public void encode(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs, final int rhs) {
        if (rhs == Integer.MAX_VALUE) {
            throw new IllegalArgumentException("Overflow in the encoding.");
        }
        this.hasEncoding = false;
        final LNGIntVector simpLits = new LNGIntVector(lits);
        final LNGIntVector simpCoeffs = new LNGIntVector(coeffs);
        lits.clear();
        coeffs.clear();
        for (int i = 0; i < simpLits.size(); i++) {
            if (simpCoeffs.get(i) <= rhs) {
                lits.push(simpLits.get(i));
                coeffs.push(simpCoeffs.get(i));
            } else {
                addUnitClause(s, not(simpLits.get(i)));
            }
        }
        if (lits.size() == 1) {
            addUnitClause(s, not(lits.get(0)));
            return;
        }
        if (lits.size() == 0) {
            return;
        }
        final int n = lits.size();
        final LNGIntVector[] seqAuxiliary = new LNGIntVector[n + 1];
        for (int i = 0; i < n + 1; i++) {
            seqAuxiliary[i] = new LNGIntVector();
            seqAuxiliary[i].growTo(rhs + 1, -1);
        }
        for (int i = 1; i <= n; ++i) {
            for (int j = 1; j <= rhs; ++j) {
                seqAuxiliary[i].set(j, mkLit(s.nVars(), false));
                newSATVariable(s);
            }
        }
        for (int i = 1; i <= rhs; ++i) {
            this.pbOutlits.push(seqAuxiliary[n].get(i));
        }
        for (int i = 1; i <= n; i++) {
            final int wi = coeffs.get(i - 1);
            assert wi <= rhs;
            for (int j = 1; j <= rhs; j++) {
                if (i >= 2 && i <= n && j <= rhs) {
                    addBinaryClause(s, not(seqAuxiliary[i - 1].get(j)), seqAuxiliary[i].get(j));
                }
                if (i <= n && j <= wi) {
                    addBinaryClause(s, not(lits.get(i - 1)), seqAuxiliary[i].get(j));
                }
                if (i >= 2 && i <= n && j <= rhs - wi) {
                    addTernaryClause(s, not(seqAuxiliary[i - 1].get(j)), not(lits.get(i - 1)), seqAuxiliary[i].get(j + wi));
                }
            }
            if (i >= 2) {
                addBinaryClause(s, not(seqAuxiliary[i - 1].get(rhs + 1 - wi)), not(lits.get(i - 1)));
            }
        }
        this.currentPbRhs = rhs;
        this.hasEncoding = true;
    }

    /**
     * Incremental construction of the SWC encoding.
     * @param s           the solver
     * @param lits        the literals of the constraint
     * @param coeffs      the coefficients of the constraint
     * @param rhs         the right hand size of the constraint
     * @param assumptions the current assumptions
     * @param size        the size
     */
    public void encode(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs,
                       final int rhs, final LNGIntVector assumptions, final int size) {
        if (rhs == Integer.MAX_VALUE) {
            throw new IllegalArgumentException("Overflow in the encoding.");
        }
        this.hasEncoding = false;
        final LNGIntVector simpLits = new LNGIntVector(lits);
        final LNGIntVector simpCoeffs = new LNGIntVector(coeffs);
        lits.clear();
        coeffs.clear();
        final LNGIntVector simpUnitLits = new LNGIntVector(this.unitLits);
        final LNGIntVector simpUnitCoeffs = new LNGIntVector(this.unitCoeffs);
        this.unitLits.clear();
        this.unitCoeffs.clear();
        for (int i = 0; i < simpUnitLits.size(); i++) {
            if (simpUnitCoeffs.get(i) <= rhs) {
                lits.push(simpUnitLits.get(i));
                coeffs.push(simpUnitCoeffs.get(i));
            } else {
                this.unitLits.push(simpUnitLits.get(i));
                this.unitCoeffs.push(simpUnitCoeffs.get(i));
            }
        }
        for (int i = 0; i < simpLits.size(); i++) {
            if (simpCoeffs.get(i) <= rhs) {
                lits.push(simpLits.get(i));
                coeffs.push(simpCoeffs.get(i));
            } else {
                this.unitLits.push(simpLits.get(i));
                this.unitCoeffs.push(simpCoeffs.get(i));
            }
        }
        if (lits.size() == 1) {
            for (int i = 0; i < this.unitLits.size(); i++) {
                assumptions.push(not(this.unitLits.get(i)));
            }
            this.unitLits.push(lits.get(0));
            this.unitCoeffs.push(coeffs.get(0));
            return;
        }
        if (lits.size() == 0) {
            for (int i = 0; i < this.unitLits.size(); i++) {
                assumptions.push(not(this.unitLits.get(i)));
            }
            return;
        }
        final int n = lits.size();
        this.seqAuxiliaryInc = new LNGVector<>(size + 1);
        for (int i = 0; i <= n; i++) {
            this.seqAuxiliaryInc.set(i, new LNGIntVector());
            this.seqAuxiliaryInc.get(i).growTo(rhs + 1, -1);
        }
        for (int i = 1; i <= n; ++i) {
            for (int j = 1; j <= rhs; ++j) {
                this.seqAuxiliaryInc.get(i).set(j, mkLit(s.nVars(), false));
                newSATVariable(s);
            }
        }
        final int blocking = mkLit(s.nVars(), false);
        newSATVariable(s);
        this.currentLitBlocking = blocking;
        assumptions.push(not(blocking));
        for (int i = 1; i <= n; i++) {
            final int wi = coeffs.get(i - 1);
            assert rhs >= wi;
            for (int j = 1; j <= rhs; j++) {
                if (i >= 2 && i <= n && j <= rhs) {
                    addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), this.seqAuxiliaryInc.get(i).get(j));
                }
                if (i <= n && j <= wi) {
                    addBinaryClause(s, not(lits.get(i - 1)), this.seqAuxiliaryInc.get(i).get(j));
                }
                if (i >= 2 && i <= n && j <= rhs - wi) {
                    addTernaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), not(lits.get(i - 1)), this.seqAuxiliaryInc.get(i).get(j + wi));
                }
            }
            if (i >= 2) {
                addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(rhs + 1 - wi)), not(lits.get(i - 1)), blocking);
            }
        }
        for (int i = 0; i < this.unitLits.size(); i++) {
            assumptions.push(not(this.unitLits.get(i)));
        }
        this.currentPbRhs = rhs;
        this.hasEncoding = true;
        this.litsInc = new LNGIntVector(lits);
        this.coeffsInc = new LNGIntVector(coeffs);
    }

    /**
     * Updates the 'rhs' of an already existent pseudo-Boolean encoding.  This method allows for all learned clauses
     * from previous iterations to be kept in the next iteration.
     * @param s   the solver
     * @param rhs the new right hand side
     */
    public void update(final MiniSatStyleSolver s, final int rhs) {
        assert this.currentPbRhs != -1;
        for (int i = rhs; i < this.currentPbRhs; i++) {
            addUnitClause(s, not(this.pbOutlits.get(i)));
        }
        this.currentPbRhs = rhs;
    }

    /**
     * Incremental update of the SWC encoding.
     * @param s   the solver
     * @param rhs the new right hand side
     */
    public void updateInc(final MiniSatStyleSolver s, final int rhs) {
        if (this.currentLitBlocking != LIT_UNDEF) {
            addUnitClause(s, this.currentLitBlocking);
        }
        final int n = this.litsInc.size();
        final int offset = this.currentPbRhs + 1;
        assert this.currentPbRhs < rhs;
        for (int i = 1; i <= n; i++) {
            for (int j = offset; j <= rhs; j++) {
                this.seqAuxiliaryInc.get(i).push(LIT_UNDEF);
            }
        }
        for (int i = 1; i <= n; ++i) {
            for (int j = offset; j <= rhs; ++j) {
                assert this.seqAuxiliaryInc.get(i).size() > j;
                this.seqAuxiliaryInc.get(i).set(j, mkLit(s.nVars(), false));
                newSATVariable(s);
            }
        }
        for (int i = 1; i < this.litsInc.size(); i++) {
            assert this.seqAuxiliaryInc.get(i).size() == rhs + 1;
        }
        this.currentLitBlocking = mkLit(s.nVars(), false);
        newSATVariable(s);
        for (int i = 1; i <= n; i++) {
            final int wi = this.coeffsInc.get(i - 1);
            assert wi > 0;
            assert rhs >= wi;
            for (int j = 1; j <= rhs; j++) {
                if (i >= 2 && i <= n && j <= rhs && j >= offset) {
                    assert this.seqAuxiliaryInc.get(i).size() > j;
                    addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), this.seqAuxiliaryInc.get(i).get(j));
                }
                if (i >= 2 && i <= n && j <= rhs - wi && j >= offset - wi) {
                    addTernaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), not(this.litsInc.get(i - 1)), this.seqAuxiliaryInc.get(i).get(j + wi));
                }
            }
            if (i >= 2) {
                assert this.seqAuxiliaryInc.get(i - 1).size() > rhs + 1 - wi;
                assert rhs + 1 - wi > 0;
                assert i - 1 < this.litsInc.size();
                addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(rhs + 1 - wi)), not(this.litsInc.get(i - 1)), this.currentLitBlocking);
            }
        }
        this.currentPbRhs = rhs;
    }

    /**
     * Joins two pseudo boolean constraints.  The given constraint is added to the current one.
     * @param s      the solver
     * @param lits   the literals of the constraint
     * @param coeffs the coefficients of the constraint
     */
    void join(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs) {
        assert this.currentLitBlocking != LIT_UNDEF;
        final int rhs = this.currentPbRhs;
        if (rhs == Integer.MAX_VALUE) {
            throw new IllegalArgumentException("Overflow in the encoding.");
        }
        final LNGIntVector simpUnitLits = new LNGIntVector(this.unitLits);
        final LNGIntVector simpUnitCoeffs = new LNGIntVector(this.unitCoeffs);
        this.unitLits.clear();
        this.unitCoeffs.clear();
        final int lhsJoin = this.litsInc.size();
        for (int i = 0; i < simpUnitLits.size(); i++) {
            if (simpUnitCoeffs.get(i) <= rhs) {
                this.litsInc.push(simpUnitLits.get(i));
                this.coeffsInc.push(simpUnitCoeffs.get(i));
            } else {
                this.unitLits.push(simpUnitLits.get(i));
                this.unitCoeffs.push(simpUnitCoeffs.get(i));
            }
        }
        for (int i = 0; i < lits.size(); i++) {
            if (coeffs.get(i) <= rhs) {
                this.litsInc.push(lits.get(i));
                this.coeffsInc.push(coeffs.get(i));
            } else {
                this.unitLits.push(lits.get(i));
                this.unitCoeffs.push(coeffs.get(i));
            }
        }
        if (this.litsInc.size() == lhsJoin) {
            return;
        }
        final int n = this.litsInc.size();
        assert this.seqAuxiliaryInc.get(lhsJoin).size() > 0;
        for (int i = lhsJoin + 1; i <= n; i++) {
            this.seqAuxiliaryInc.set(i, new LNGIntVector());
            this.seqAuxiliaryInc.get(i).growTo(rhs + 1, -1);
        }
        for (int i = lhsJoin + 1; i <= n; ++i) {
            for (int j = 1; j <= rhs; ++j) {
                this.seqAuxiliaryInc.get(i).set(j, mkLit(s.nVars(), false));
                newSATVariable(s);
            }
        }
        for (int i = 1; i <= n; i++) {
            assert this.seqAuxiliaryInc.get(i).size() == rhs + 1;
        }
        for (int i = lhsJoin; i <= n; i++) {
            final int wi = this.coeffsInc.get(i - 1);
            assert wi > 0;
            assert wi <= rhs;
            for (int j = 1; j <= rhs; j++) {
                assert this.seqAuxiliaryInc.get(i).size() > j;
                assert this.seqAuxiliaryInc.get(i - 1).size() > j;
                addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), this.seqAuxiliaryInc.get(i).get(j));
                if (j <= wi) {
                    assert this.seqAuxiliaryInc.get(i).size() > j;
                    assert i - 1 < this.litsInc.size() && i - 1 >= 0;
                    addBinaryClause(s, not(this.litsInc.get(i - 1)), this.seqAuxiliaryInc.get(i).get(j));
                }
                if (j <= rhs - wi) {
                    addTernaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(j)), not(this.litsInc.get(i - 1)), this.seqAuxiliaryInc.get(i).get(j + wi));
                }
            }
            if (i > lhsJoin) {
                assert rhs + 1 - wi >= 0;
                assert this.seqAuxiliaryInc.get(i - 1).size() > rhs + 1 - wi;
                assert i - 1 < this.litsInc.size();
                addBinaryClause(s, not(this.seqAuxiliaryInc.get(i - 1).get(rhs + 1 - wi)), not(this.litsInc.get(i - 1)), this.currentLitBlocking);
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
