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
import org.logicng.collections.LNGVector;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.sat.MiniSatStyleSolver;

/**
 * Encodes that at most 'rhs' literals can be assigned value true.  Uses the totalizer encoding for
 * translating the cardinality constraint into CNF.
 * @version 2.0.0
 * @since 1.0
 */
public class Totalizer extends Encoding {

    protected final LNGVector<LNGIntVector> totalizerIterativeLeft;
    protected final LNGVector<LNGIntVector> totalizerIterativeRight;
    protected final LNGVector<LNGIntVector> totalizerIterativeOutput;
    protected final LNGIntVector totalizerIterativeRhs;
    protected final int blocking;
    protected final LNGIntVector cardinalityOutlits;
    protected LNGIntVector cardinalityInlits;
    protected MaxSATConfig.IncrementalStrategy incrementalStrategy;
    protected int currentCardinalityRhs;
    protected boolean joinMode;

    /**
     * Constructs a new totalizer with a given incremental strategy.
     * @param strategy the incremental strategy
     */
    Totalizer(final MaxSATConfig.IncrementalStrategy strategy) {
        this.blocking = LIT_UNDEF;
        this.joinMode = false;
        this.currentCardinalityRhs = -1;
        this.incrementalStrategy = strategy;
        this.totalizerIterativeLeft = new LNGVector<>();
        this.totalizerIterativeRight = new LNGVector<>();
        this.totalizerIterativeOutput = new LNGVector<>();
        this.totalizerIterativeRhs = new LNGIntVector();
        this.cardinalityInlits = new LNGIntVector();
        this.cardinalityOutlits = new LNGIntVector();
    }

    /**
     * Updates the right hand side.
     * @param s   the solver
     * @param rhs the new right hand side
     */
    public void update(final MiniSatStyleSolver s, final int rhs) {
        final LNGIntVector assumptions = new LNGIntVector();
        this.update(s, rhs, assumptions);
    }

    /**
     * Returns {@code true} if an encoding was created, {@code false} otherwise.
     * @return {@code true} if an encoding was created
     */
    boolean hasCreatedEncoding() {
        return this.hasEncoding;
    }

    /**
     * Sets the incremental strategy.
     * @param incremental the incremental strategy
     */
    public void setIncremental(final MaxSATConfig.IncrementalStrategy incremental) {
        this.incrementalStrategy = incremental;
    }

    /**
     * Returns the incremental strategy.
     * @return the incremental strategy
     */
    public MaxSATConfig.IncrementalStrategy incremental() {
        return this.incrementalStrategy;
    }

    /**
     * Joins two constraints.  The given constraint is added to the current one.
     * @param s    the solver
     * @param lits the literals of the constraint
     * @param rhs  the right hand side of the constraint
     */
    void join(final MiniSatStyleSolver s, final LNGIntVector lits, final int rhs) {
        assert this.incrementalStrategy == MaxSATConfig.IncrementalStrategy.ITERATIVE;
        final LNGIntVector leftCardinalityOutlits = new LNGIntVector(this.cardinalityOutlits);
        final int oldCardinality = this.currentCardinalityRhs;
        if (lits.size() > 1) {
            this.build(s, lits, Math.min(rhs, lits.size()));
        } else {
            assert lits.size() == 1;
            this.cardinalityOutlits.clear();
            this.cardinalityOutlits.push(lits.get(0));
        }
        final LNGIntVector rightCardinalityOutlits = new LNGIntVector(this.cardinalityOutlits);
        this.cardinalityOutlits.clear();
        for (int i = 0; i < leftCardinalityOutlits.size() + rightCardinalityOutlits.size(); i++) {
            final int p = mkLit(s.nVars(), false);
            MaxSAT.newSATVariable(s);
            this.cardinalityOutlits.push(p);
        }
        this.currentCardinalityRhs = rhs;
        this.adder(s, leftCardinalityOutlits, rightCardinalityOutlits, this.cardinalityOutlits);
        this.currentCardinalityRhs = oldCardinality;
    }

    /**
     * Updates the right hand side of a constraint.
     * @param s           the solver
     * @param rhs         the new right hand side
     * @param assumptions the assumptions
     * @throws IllegalStateException if the incremental strategy is unknown
     */
    public void update(final MiniSatStyleSolver s, final int rhs, final LNGIntVector assumptions) {
        assert this.hasEncoding;
        switch (this.incrementalStrategy) {
            case NONE:
                for (int i = rhs; i < this.cardinalityOutlits.size(); i++) {
                    addUnitClause(s, not(this.cardinalityOutlits.get(i)));
                }
                break;
            case ITERATIVE:
                this.incremental(s, rhs);
                assumptions.clear();
                for (int i = rhs; i < this.cardinalityOutlits.size(); i++) {
                    assumptions.push(not(this.cardinalityOutlits.get(i)));
                }
                break;
            default:
                throw new IllegalStateException("Unknown incremental strategy: " + this.incrementalStrategy);
        }
    }

    /**
     * Builds the cardinality constraint.
     * @param s    the solver
     * @param lits the literals of the constraint
     * @param rhs  the right hand side of the constraint
     */
    public void build(final MiniSatStyleSolver s, final LNGIntVector lits, final int rhs) {
        this.cardinalityOutlits.clear();
        this.hasEncoding = false;
        if (rhs == 0) {
            for (int i = 0; i < lits.size(); i++) {
                addUnitClause(s, not(lits.get(i)));
            }
            return;
        }
        assert rhs >= 1 && rhs <= lits.size();
        if (this.incrementalStrategy == MaxSATConfig.IncrementalStrategy.NONE && rhs == lits.size()) {
            return;
        }
        if (rhs == lits.size() && !this.joinMode) {
            return;
        }
        for (int i = 0; i < lits.size(); i++) {
            final int p = mkLit(s.nVars(), false);
            MaxSAT.newSATVariable(s);
            this.cardinalityOutlits.push(p);
        }
        this.cardinalityInlits = new LNGIntVector(lits);
        this.currentCardinalityRhs = rhs;
        this.toCNF(s, this.cardinalityOutlits);
        assert this.cardinalityInlits.size() == 0;
        if (!this.joinMode) {
            this.joinMode = true;
        }
        this.hasEncoding = true;
    }

    protected void toCNF(final MiniSatStyleSolver s, final LNGIntVector lits) {
        final LNGIntVector left = new LNGIntVector();
        final LNGIntVector right = new LNGIntVector();
        assert lits.size() > 1;
        final int split = lits.size() / 2;
        for (int i = 0; i < lits.size(); i++) {
            if (i < split) {
                if (split == 1) {
                    assert this.cardinalityInlits.size() > 0;
                    left.push(this.cardinalityInlits.back());
                    this.cardinalityInlits.pop();
                } else {
                    final int p = mkLit(s.nVars(), false);
                    MaxSAT.newSATVariable(s);
                    left.push(p);
                }
            } else {
                if (lits.size() - split == 1) {
                    assert this.cardinalityInlits.size() > 0;
                    right.push(this.cardinalityInlits.back());
                    this.cardinalityInlits.pop();
                } else {
                    final int p = mkLit(s.nVars(), false);
                    MaxSAT.newSATVariable(s);
                    right.push(p);
                }
            }
        }
        this.adder(s, left, right, lits);
        if (left.size() > 1) {
            this.toCNF(s, left);
        }
        if (right.size() > 1) {
            this.toCNF(s, right);
        }
    }

    protected void adder(final MiniSatStyleSolver s, final LNGIntVector left, final LNGIntVector right, final LNGIntVector output) {
        assert output.size() == left.size() + right.size();
        if (this.incrementalStrategy == MaxSATConfig.IncrementalStrategy.ITERATIVE) {
            this.totalizerIterativeLeft.push(new LNGIntVector(left));
            this.totalizerIterativeRight.push(new LNGIntVector(right));
            this.totalizerIterativeOutput.push(new LNGIntVector(output));
            this.totalizerIterativeRhs.push(this.currentCardinalityRhs);
        }
        for (int i = 0; i <= left.size(); i++) {
            for (int j = 0; j <= right.size(); j++) {
                if (i == 0 && j == 0) {
                    continue;
                }
                if (i + j > this.currentCardinalityRhs + 1) {
                    continue;
                }
                if (i == 0) {
                    addBinaryClause(s, not(right.get(j - 1)), output.get(j - 1), this.blocking);
                } else if (j == 0) {
                    addBinaryClause(s, not(left.get(i - 1)), output.get(i - 1), this.blocking);
                } else {
                    addTernaryClause(s, not(left.get(i - 1)), not(right.get(j - 1)), output.get(i + j - 1), this.blocking);
                }
            }
        }
    }

    protected void incremental(final MiniSatStyleSolver s, final int rhs) {
        for (int z = 0; z < this.totalizerIterativeRhs.size(); z++) {
            for (int i = 0; i <= this.totalizerIterativeLeft.get(z).size(); i++) {
                for (int j = 0; j <= this.totalizerIterativeRight.get(z).size(); j++) {
                    if (i == 0 && j == 0) {
                        continue;
                    }
                    if (i + j > rhs + 1 || i + j <= this.totalizerIterativeRhs.get(z) + 1) {
                        continue;
                    }
                    if (i == 0) {
                        addBinaryClause(s, not(this.totalizerIterativeRight.get(z).get(j - 1)), this.totalizerIterativeOutput.get(z).get(j - 1));
                    } else if (j == 0) {
                        addBinaryClause(s, not(this.totalizerIterativeLeft.get(z).get(i - 1)), this.totalizerIterativeOutput.get(z).get(i - 1));
                    } else {
                        addTernaryClause(s, not(this.totalizerIterativeLeft.get(z).get(i - 1)), not(this.totalizerIterativeRight.get(z).get(j - 1)), this.totalizerIterativeOutput.get(z).get(i + j - 1));
                    }
                }
            }
            this.totalizerIterativeRhs.set(z, rhs);
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
