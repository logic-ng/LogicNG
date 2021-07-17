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
 * Glucose -- Copyright (c) 2009-2014, Gilles Audemard, Laurent Simon
 * CRIL - Univ. Artois, France
 * LRI  - Univ. Paris Sud, France (2009-2013)
 * Labri - Univ. Bordeaux, France
 * <p>
 * Syrup (Glucose Parallel) -- Copyright (c) 2013-2014, Gilles Audemard, Laurent Simon
 * CRIL - Univ. Artois, France
 * Labri - Univ. Bordeaux, France
 * <p>
 * Glucose sources are based on MiniSat (see below MiniSat copyrights). Permissions and copyrights of
 * Glucose (sources until 2013, Glucose 3.0, single core) are exactly the same as Minisat on which it
 * is based on. (see below).
 * <p>
 * Glucose-Syrup sources are based on another copyright. Permissions and copyrights for the parallel
 * version of Glucose-Syrup (the "Software") are granted, free of charge, to deal with the Software
 * without restriction, including the rights to use, copy, modify, merge, publish, distribute,
 * sublicence, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * - The above and below copyrights notices and this permission notice shall be included in all
 * copies or substantial portions of the Software;
 * - The parallel version of Glucose (all files modified since Glucose 3.0 releases, 2013) cannot
 * be used in any competitive event (sat competitions/evaluations) without the express permission of
 * the authors (Gilles Audemard / Laurent Simon). This is also the case for any competitive event
 * using Glucose Parallel as an embedded SAT engine (single core or not).
 * <p>
 * <p>
 * --------------- Original Minisat Copyrights
 * <p>
 * Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 * Copyright (c) 2007-2010, Niklas Sorensson
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.solvers.sat;

import static org.logicng.handlers.Handler.start;
import static org.logicng.handlers.SATHandler.finishSolving;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.datastructures.LNGBoundedIntQueue;
import org.logicng.solvers.datastructures.LNGBoundedLongQueue;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.datastructures.MSWatcher;

/**
 * Glucose 4.0 solver.
 * @version 2.1.0
 * @since 1.0
 */
public class GlucoseSyrup extends MiniSatStyleSolver {

    /**
     * the ratio of clauses which will be removed
     */
    protected static final int RATIO_REMOVE_CLAUSES = 2;

    /**
     * the lower bound for blocking restarts
     */
    protected static final int LB_BLOCKING_RESTART = 10000;

    // external solver configuration
    protected final GlucoseConfig glucoseConfig;

    // internal solver state
    protected LNGVector<LNGVector<MSWatcher>> watchesBin;
    protected LNGIntVector permDiff;
    protected LNGIntVector lastDecisionLevel;
    protected LNGBoundedLongQueue lbdQueue;
    protected LNGBoundedIntQueue trailQueue;
    protected LNGBooleanVector assump;
    protected int myflag;
    protected long analyzeLBD;
    protected int analyzeSzWithoutSelectors;
    protected int nbclausesbeforereduce;
    protected int conflicts;
    protected int conflictsRestarts;
    protected double sumLBD;
    protected int curRestart;

    // solver configuration
    protected int lbLBDMinimizingClause;
    protected int lbLBDFrozenClause;
    protected int lbSizeMinimizingClause;
    protected int firstReduceDB;
    protected int specialIncReduceDB;
    protected int incReduceDB;
    protected double factorK;
    protected double factorR;
    protected int sizeLBDQueue;
    protected int sizeTrailQueue;
    protected boolean reduceOnSize;
    protected int reduceOnSizeSize;
    protected double maxVarDecay;

    /**
     * Constructs a new Glucose 2 solver with the default values for solver configuration.  By default, incremental mode
     * is activated.
     */
    GlucoseSyrup() {
        this(MiniSatConfig.builder().build(), GlucoseConfig.builder().build());
    }

    /**
     * Constructs a new Glucose solver with a given solver configuration.
     * @param config        the MiniSat configuration
     * @param glucoseConfig the Glucose configuration
     */
    public GlucoseSyrup(final MiniSatConfig config, final GlucoseConfig glucoseConfig) {
        super(config);
        this.glucoseConfig = glucoseConfig;
        this.initializeGlucose();
    }

    /**
     * Initializes the additional parameters.
     */
    protected void initializeGlucose() {
        this.initializeGlucoseConfig();
        this.watchesBin = new LNGVector<>();
        this.permDiff = new LNGIntVector();
        this.lastDecisionLevel = new LNGIntVector();
        this.lbdQueue = new LNGBoundedLongQueue();
        this.trailQueue = new LNGBoundedIntQueue();
        this.assump = new LNGBooleanVector();
        this.lbdQueue.initSize(this.sizeLBDQueue);
        this.trailQueue.initSize(this.sizeTrailQueue);
        this.myflag = 0;
        this.analyzeBtLevel = 0;
        this.analyzeLBD = 0;
        this.analyzeSzWithoutSelectors = 0;
        this.nbclausesbeforereduce = this.firstReduceDB;
        this.conflicts = 0;
        this.conflictsRestarts = 0;
        this.sumLBD = 0;
        this.curRestart = 1;
    }

    /**
     * Initializes the glucose configuration.
     */
    protected void initializeGlucoseConfig() {
        this.lbLBDMinimizingClause = this.glucoseConfig.lbLBDMinimizingClause;
        this.lbLBDFrozenClause = this.glucoseConfig.lbLBDFrozenClause;
        this.lbSizeMinimizingClause = this.glucoseConfig.lbSizeMinimizingClause;
        this.firstReduceDB = this.glucoseConfig.firstReduceDB;
        this.specialIncReduceDB = this.glucoseConfig.specialIncReduceDB;
        this.incReduceDB = this.glucoseConfig.incReduceDB;
        this.factorK = this.glucoseConfig.factorK;
        this.factorR = this.glucoseConfig.factorR;
        this.sizeLBDQueue = this.glucoseConfig.sizeLBDQueue;
        this.sizeTrailQueue = this.glucoseConfig.sizeTrailQueue;
        this.reduceOnSize = this.glucoseConfig.reduceOnSize;
        this.reduceOnSizeSize = this.glucoseConfig.reduceOnSizeSize;
        this.maxVarDecay = this.glucoseConfig.maxVarDecay;
    }

    @Override
    public int newVar(final boolean sign, final boolean dvar) {
        final int v = nVars();
        final MSVariable newVar = new MSVariable(sign);
        this.watches.push(new LNGVector<>());
        this.watches.push(new LNGVector<>());
        this.watchesBin.push(new LNGVector<>());
        this.watchesBin.push(new LNGVector<>());
        this.vars.push(newVar);
        this.seen.push(false);
        this.permDiff.push(0);
        this.assump.push(false);
        newVar.setDecision(dvar);
        insertVarOrder(v);
        return v;
    }

    @Override
    public boolean addClause(final LNGIntVector ps, final Proposition proposition) {
        assert decisionLevel() == 0;
        int p;
        int i;
        int j;
        if (this.config.proofGeneration) {
            final LNGIntVector vec = new LNGIntVector(ps.size());
            for (i = 0; i < ps.size(); i++) {
                vec.push((var(ps.get(i)) + 1) * (-2 * (sign(ps.get(i)) ? 1 : 0) + 1));
            }
            this.pgOriginalClauses.push(new ProofInformation(vec, proposition));
        }
        if (!this.ok) {
            return false;
        }
        ps.sort();

        boolean flag = false;
        LNGIntVector oc = null;
        if (this.config.proofGeneration) {
            oc = new LNGIntVector();
            for (i = 0, p = LIT_UNDEF; i < ps.size(); i++) {
                oc.push(ps.get(i));
                if (value(ps.get(i)) == Tristate.TRUE || ps.get(i) == not(p) || value(ps.get(i)) == Tristate.FALSE) {
                    flag = true;
                }
            }
        }

        for (i = 0, j = 0, p = LIT_UNDEF; i < ps.size(); i++) {
            if (value(ps.get(i)) == Tristate.TRUE || ps.get(i) == not(p)) {
                return true;
            } else if (value(ps.get(i)) != Tristate.FALSE && ps.get(i) != p) {
                p = ps.get(i);
                ps.set(j++, p);
            }
        }
        ps.removeElements(i - j);

        if (flag) {
            LNGIntVector vec = new LNGIntVector(ps.size() + 1);
            vec.push(1);
            for (i = 0; i < ps.size(); i++) {
                vec.push((var(ps.get(i)) + 1) * (-2 * (sign(ps.get(i)) ? 1 : 0) + 1));
            }
            this.pgProof.push(vec);

            vec = new LNGIntVector(oc.size());
            vec.push(-1);
            for (i = 0; i < oc.size(); i++) {
                vec.push((var(oc.get(i)) + 1) * (-2 * (sign(oc.get(i)) ? 1 : 0) + 1));
            }
            this.pgProof.push(vec);
        }

        if (ps.size() == 0) {
            this.ok = false;
            if (this.config.proofGeneration) {
                this.pgProof.push(new LNGIntVector(1, 0));
            }
            return false;
        } else if (ps.size() == 1) {
            uncheckedEnqueue(ps.get(0), null);
            this.ok = propagate() == null;
            if (!this.ok && this.config.proofGeneration) {
                this.pgProof.push(new LNGIntVector(1, 0));
            }
            return this.ok;
        } else {
            final MSClause c = new MSClause(ps, false);
            this.clauses.push(c);
            attachClause(c);
        }
        return true;
    }

    @Override
    public Tristate solve(final SATHandler handler) {
        if (this.config.incremental && this.config.proofGeneration) {
            throw new IllegalStateException("Cannot use incremental and proof generation at the same time");
        }
        this.handler = handler;
        start(handler);
        this.model.clear();
        this.conflict.clear();
        if (!this.ok) {
            return Tristate.FALSE;
        }
        for (int i = 0; i < this.assumptions.size(); i++) {
            this.assump.set(var(this.assumptions.get(i)), !sign(this.assumptions.get(i)));
        }

        Tristate status = Tristate.UNDEF;
        while (status == Tristate.UNDEF && !this.canceledByHandler) {
            status = search();
        }

        if (this.config.proofGeneration && this.assumptions.empty()) {
            if (status == Tristate.FALSE) {
                this.pgProof.push(new LNGIntVector(1, 0));
            }
        }

        if (status == Tristate.TRUE) {
            this.model = new LNGBooleanVector(this.vars.size());
            for (final MSVariable v : this.vars) {
                this.model.push(v.assignment() == Tristate.TRUE);
            }
        } else if (status == Tristate.FALSE && this.conflict.size() == 0) {
            this.ok = false;
        }
        finishSolving(handler);
        cancelUntil(0);
        this.handler = null;
        this.canceledByHandler = false;
        for (int i = 0; i < this.assumptions.size(); i++) {
            this.assump.set(var(this.assumptions.get(i)), false);
        }
        return status;
    }

    @Override
    public void reset() {
        super.initialize();
        this.initializeGlucose();
    }

    @Override
    public int[] saveState() {
        throw new UnsupportedOperationException("The Glucose solver does not support state loading/saving");
    }

    @Override
    public void loadState(final int[] state) {
        throw new UnsupportedOperationException("The Glucose solver does not support state loading/saving");
    }

    @Override
    protected void uncheckedEnqueue(final int lit, final MSClause reason) {
        assert value(lit) == Tristate.UNDEF;
        final MSVariable var = v(lit);
        var.assign(Tristate.fromBool(!sign(lit)));
        var.setReason(reason);
        var.setLevel(decisionLevel());
        this.trail.push(lit);
    }

    @Override
    protected void attachClause(final MSClause c) {
        assert c.size() > 1;
        if (c.size() == 2) {
            this.watchesBin.get(not(c.get(0))).push(new MSWatcher(c, c.get(1)));
            this.watchesBin.get(not(c.get(1))).push(new MSWatcher(c, c.get(0)));
        } else {
            this.watches.get(not(c.get(0))).push(new MSWatcher(c, c.get(1)));
            this.watches.get(not(c.get(1))).push(new MSWatcher(c, c.get(0)));
        }
        if (c.learnt()) {
            this.learntsLiterals += c.size();
        } else {
            this.clausesLiterals += c.size();
        }
    }

    @Override
    protected void detachClause(final MSClause c) {
        assert c.size() > 1;
        if (c.size() == 2) {
            this.watchesBin.get(not(c.get(0))).remove(new MSWatcher(c, c.get(1)));
            this.watchesBin.get(not(c.get(1))).remove(new MSWatcher(c, c.get(0)));
        } else {
            this.watches.get(not(c.get(0))).remove(new MSWatcher(c, c.get(1)));
            this.watches.get(not(c.get(1))).remove(new MSWatcher(c, c.get(0)));
        }
        if (c.learnt()) {
            this.learntsLiterals -= c.size();
        } else {
            this.clausesLiterals -= c.size();
        }
    }

    @Override
    protected void removeClause(final MSClause c) {
        if (this.config.proofGeneration) {
            final LNGIntVector vec = new LNGIntVector(c.size());
            vec.push(-1);
            for (int i = 0; i < c.size(); i++) {
                vec.push((var(c.get(i)) + 1) * (-2 * (sign(c.get(i)) ? 1 : 0) + 1));
            }
            this.pgProof.push(vec);
        }

        detachClause(c);
        if (locked(c)) {
            v(c.get(0)).setReason(null);
        }
    }

    @Override
    protected MSClause propagate() {
        MSClause confl = null;
        int numProps = 0;
        while (this.qhead < this.trail.size()) {
            final int p = this.trail.get(this.qhead++);
            final LNGVector<MSWatcher> ws = this.watches.get(p);
            int iInd = 0;
            int jInd = 0;
            numProps++;
            final LNGVector<MSWatcher> wbin = this.watchesBin.get(p);
            for (int k = 0; k < wbin.size(); k++) {
                final int imp = wbin.get(k).blocker();
                if (value(imp) == Tristate.FALSE) {
                    return wbin.get(k).clause();
                }
                if (value(imp) == Tristate.UNDEF) {
                    uncheckedEnqueue(imp, wbin.get(k).clause());
                }
            }
            while (iInd < ws.size()) {
                final MSWatcher i = ws.get(iInd);
                final int blocker = i.blocker();
                if (value(blocker) == Tristate.TRUE) {
                    ws.set(jInd++, i);
                    iInd++;
                    continue;
                }
                final MSClause c = i.clause();
                assert !c.oneWatched();
                final int falseLit = not(p);
                if (c.get(0) == falseLit) {
                    c.set(0, c.get(1));
                    c.set(1, falseLit);
                }
                assert c.get(1) == falseLit;
                iInd++;
                final int first = c.get(0);
                final MSWatcher w = new MSWatcher(c, first);
                if (first != blocker && value(first) == Tristate.TRUE) {
                    ws.set(jInd++, w);
                    continue;
                }
                boolean foundWatch = false;
                if (this.incremental) {
                    int choosenPos = -1;
                    for (int k = 2; k < c.size(); k++) {
                        if (value(c.get(k)) != Tristate.FALSE) {
                            if (decisionLevel() > this.assumptions.size()) {
                                choosenPos = k;
                                break;
                            } else {
                                choosenPos = k;
                                if (value(c.get(k)) == Tristate.TRUE || !isSelector(var(c.get(k)))) {
                                    break;
                                }
                            }
                        }
                    }
                    if (choosenPos != -1) {
                        c.set(1, c.get(choosenPos));
                        c.set(choosenPos, falseLit);
                        this.watches.get(not(c.get(1))).push(w);
                        foundWatch = true;
                    }
                } else {
                    for (int k = 2; k < c.size() && !foundWatch; k++) {
                        if (value(c.get(k)) != Tristate.FALSE) {
                            c.set(1, c.get(k));
                            c.set(k, falseLit);
                            this.watches.get(not(c.get(1))).push(w);
                            foundWatch = true;
                        }
                    }
                }
                if (!foundWatch) {
                    ws.set(jInd++, w);
                    if (value(first) == Tristate.FALSE) {
                        confl = c;
                        this.qhead = this.trail.size();
                        while (iInd < ws.size()) {
                            ws.set(jInd++, ws.get(iInd++));
                        }
                    } else {
                        uncheckedEnqueue(first, c);
                    }
                }
            }
            ws.removeElements(iInd - jInd);
        }
        this.simpDBProps -= numProps;
        return confl;
    }

    @Override
    protected boolean litRedundant(final int p, final int abstractLevels) {
        this.analyzeStack.clear();
        this.analyzeStack.push(p);
        final int top = this.analyzeToClear.size();
        while (this.analyzeStack.size() > 0) {
            assert v(this.analyzeStack.back()).reason() != null;
            final MSClause c = v(this.analyzeStack.back()).reason();
            this.analyzeStack.pop();
            if (c.size() == 2 && value(c.get(0)) == Tristate.FALSE) {
                assert value(c.get(1)) == Tristate.TRUE;
                final int tmp = c.get(0);
                c.set(0, c.get(1));
                c.set(1, tmp);
            }
            for (int i = 1; i < c.size(); i++) {
                final int q = c.get(i);
                if (!this.seen.get(var(q)) && v(q).level() > 0) {
                    if (v(q).reason() != null && (abstractLevel(var(q)) & abstractLevels) != 0) {
                        this.seen.set(var(q), true);
                        this.analyzeStack.push(q);
                        this.analyzeToClear.push(q);
                    } else {
                        for (int j = top; j < this.analyzeToClear.size(); j++) {
                            this.seen.set(var(this.analyzeToClear.get(j)), false);
                        }
                        this.analyzeToClear.removeElements(this.analyzeToClear.size() - top);
                        return false;
                    }
                }
            }
        }
        return true;
    }

    @Override
    protected void analyzeFinal(final int p, final LNGIntVector outConflict) {
        outConflict.clear();
        outConflict.push(p);
        if (decisionLevel() == 0) {
            return;
        }
        this.seen.set(var(p), true);
        int x;
        MSVariable v;
        for (int i = this.trail.size() - 1; i >= this.trailLim.get(0); i--) {
            x = var(this.trail.get(i));
            if (this.seen.get(x)) {
                v = this.vars.get(x);
                if (v.reason() == null) {
                    assert v.level() > 0;
                    outConflict.push(not(this.trail.get(i)));
                } else {
                    final MSClause c = v.reason();
                    for (int j = c.size() == 2 ? 0 : 1; j < c.size(); j++) {
                        if (v(c.get(j)).level() > 0) {
                            this.seen.set(var(c.get(j)), true);
                        }
                    }
                }
                this.seen.set(x, false);
            }
        }
        this.seen.set(var(p), false);
    }

    @Override
    protected void reduceDB() {
        int i;
        int j;
        this.learnts.manualSort(MSClause.glucoseComparator);
        if (this.learnts.get(this.learnts.size() / RATIO_REMOVE_CLAUSES).lbd() <= 3) {
            this.nbclausesbeforereduce += this.specialIncReduceDB;
        }
        if (this.learnts.back().lbd() <= 5) {
            this.nbclausesbeforereduce += this.specialIncReduceDB;
        }
        int limit = this.learnts.size() / 2;
        for (i = j = 0; i < this.learnts.size(); i++) {
            final MSClause c = this.learnts.get(i);
            if (c.lbd() > 2 && c.size() > 2 && c.canBeDel() && !locked(c) && (i < limit)) {
                removeClause(this.learnts.get(i));
            } else {
                if (!c.canBeDel()) {
                    limit++;
                }
                c.setCanBeDel(true);
                this.learnts.set(j++, this.learnts.get(i));
            }
        }
        this.learnts.removeElements(i - j);
    }

    @Override
    protected void removeSatisfied(final LNGVector<MSClause> cs) {
        int i;
        int j;
        for (i = j = 0; i < cs.size(); i++) {
            final MSClause c = cs.get(i);
            if (satisfied(c)) {
                removeClause(cs.get(i));
            } else {
                cs.set(j++, cs.get(i));
            }
        }
        cs.removeElements(i - j);
    }

    @Override
    protected boolean satisfied(final MSClause c) {
        if (this.incremental) {
            return (value(c.get(0)) == Tristate.TRUE) || (value(c.get(1)) == Tristate.TRUE);
        }
        for (int i = 0; i < c.size(); i++) {
            if (value(c.get(i)) == Tristate.TRUE) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected boolean simplify() {
        assert decisionLevel() == 0;
        if (!this.ok) {
            return false;
        } else {
            final MSClause cr = propagate();
            if (cr != null) {
                return this.ok = false;
            }
        }
        if (nAssigns() == this.simpDBAssigns || (this.simpDBProps > 0)) {
            return true;
        }
        removeSatisfied(this.learnts);
        if (this.shouldRemoveSatsisfied) {
            removeSatisfied(this.clauses);
        }
        rebuildOrderHeap();
        this.simpDBAssigns = nAssigns();
        this.simpDBProps = this.clausesLiterals + this.learntsLiterals;
        return true;
    }

    /**
     * Computes the LBD for a given vector of literals.
     * @param lits the vector of literals
     * @param e    parameter for incremental mode
     * @return the LBD
     */
    protected long computeLBD(final LNGIntVector lits, final int e) {
        int end = e;
        long nblevels = 0;
        this.myflag++;
        if (this.incremental) {
            if (end == -1) {
                end = lits.size();
            }
            long nbDone = 0;
            for (int i = 0; i < lits.size(); i++) {
                if (nbDone >= end) {
                    break;
                }
                if (isSelector(var(lits.get(i)))) {
                    continue;
                }
                nbDone++;
                final int l = v(lits.get(i)).level();
                if (this.permDiff.get(l) != this.myflag) {
                    this.permDiff.set(l, this.myflag);
                    nblevels++;
                }
            }
        } else {
            for (int i = 0; i < lits.size(); i++) {
                final int l = v(lits.get(i)).level();
                if (this.permDiff.get(l) != this.myflag) {
                    this.permDiff.set(l, this.myflag);
                    nblevels++;
                }
            }
        }
        if (!this.reduceOnSize) {
            return nblevels;
        }
        if (lits.size() < this.reduceOnSizeSize) {
            return lits.size();
        }
        return lits.size() + nblevels;
    }

    /**
     * Computes the LBD for a given clause
     * @param c the clause
     * @return the LBD
     */
    protected long computeLBD(final MSClause c) {
        long nblevels = 0;
        this.myflag++;
        if (this.incremental) {
            long nbDone = 0;
            for (int i = 0; i < c.size(); i++) {
                if (nbDone >= c.sizeWithoutSelectors()) {
                    break;
                }
                if (isSelector(var(c.get(i)))) {
                    continue;
                }
                nbDone++;
                final int l = v(c.get(i)).level();
                if (this.permDiff.get(l) != this.myflag) {
                    this.permDiff.set(l, this.myflag);
                    nblevels++;
                }
            }
        } else {
            for (int i = 0; i < c.size(); i++) {
                final int l = v(c.get(i)).level();
                if (this.permDiff.get(l) != this.myflag) {
                    this.permDiff.set(l, this.myflag);
                    nblevels++;
                }
            }
        }
        if (!this.reduceOnSize) {
            return nblevels;
        }
        if (c.size() < this.reduceOnSizeSize) {
            return c.size();
        }
        return c.size() + nblevels;
    }

    /**
     * Returns {@code true} if a given variable is a selector variable, {@code false} otherwise.
     * @param v the variable
     * @return {@code true} if the given variable is a selector variable
     */
    protected boolean isSelector(final int v) {
        return this.incremental && this.assump.get(v);
    }

    /**
     * A special clause minimization by binary resolution for small clauses.
     * @param outLearnt the vector where the new learnt 1-UIP clause is stored
     */
    protected void minimisationWithBinaryResolution(final LNGIntVector outLearnt) {
        final long lbd = computeLBD(outLearnt, -1);
        int p = not(outLearnt.get(0));
        if (lbd <= this.lbLBDMinimizingClause) {
            this.myflag++;
            for (int i = 1; i < outLearnt.size(); i++) {
                this.permDiff.set(var(outLearnt.get(i)), this.myflag);
            }
            int nb = 0;
            for (final MSWatcher wbin : this.watchesBin.get(p)) {
                final int imp = wbin.blocker();
                if (this.permDiff.get(var(imp)) == this.myflag && value(imp) == Tristate.TRUE) {
                    nb++;
                    this.permDiff.set(var(imp), this.myflag - 1);
                }
            }
            int l = outLearnt.size() - 1;
            if (nb > 0) {
                for (int i = 1; i < outLearnt.size() - nb; i++) {
                    if (this.permDiff.get(var(outLearnt.get(i))) != this.myflag) {
                        p = outLearnt.get(l);
                        outLearnt.set(l, outLearnt.get(i));
                        outLearnt.set(i, p);
                        l--;
                        i--;
                    }
                }
                outLearnt.removeElements(nb);
            }
        }
    }

    /**
     * The main search procedure of the CDCL algorithm.
     * @return a {@link Tristate} representing the result.  {@code FALSE} if the formula is UNSAT, {@code TRUE} if the
     * formula is SAT, and {@code UNKNOWN} if the state is not known yet (restart)
     */
    protected Tristate search() {
        assert this.ok;
        final LNGIntVector learntClause = new LNGIntVector();
        final LNGIntVector selectors = new LNGIntVector();
        boolean blocked = false;
        this.selectionOrderIdx = 0;
        while (true) {
            final MSClause confl = propagate();
            if (confl != null) {
                if (this.handler != null && !this.handler.detectedConflict()) {
                    this.canceledByHandler = true;
                    return Tristate.UNDEF;
                }
                this.conflicts++;
                this.conflictsRestarts++;
                if (this.conflicts % 5000 == 0 && this.varDecay < this.maxVarDecay) {
                    this.varDecay += 0.01;
                }
                if (decisionLevel() == 0) {
                    return Tristate.FALSE;
                }
                this.trailQueue.push(this.trail.size());
                if (this.conflictsRestarts > LB_BLOCKING_RESTART && this.lbdQueue.valid() && this.trail.size() > this.factorR * this.trailQueue.avg()) {
                    this.lbdQueue.fastClear();
                    if (!blocked) {
                        blocked = true;
                    }
                }
                learntClause.clear();
                selectors.clear();
                analyze(confl, learntClause, selectors);
                this.lbdQueue.push(this.analyzeLBD);
                this.sumLBD += this.analyzeLBD;
                cancelUntil(this.analyzeBtLevel);
                if (this.analyzeBtLevel < this.selectionOrder.size()) {
                    this.selectionOrderIdx = this.analyzeBtLevel;
                }

                if (this.config.proofGeneration) {
                    final LNGIntVector vec = new LNGIntVector(learntClause.size() + 1);
                    vec.push(1);
                    for (int i = 0; i < learntClause.size(); i++) {
                        vec.push((var(learntClause.get(i)) + 1) * (-2 * (sign(learntClause.get(i)) ? 1 : 0) + 1));
                    }
                    this.pgProof.push(vec);
                }

                if (learntClause.size() == 1) {
                    uncheckedEnqueue(learntClause.get(0), null);
                } else {
                    final MSClause cr = new MSClause(learntClause, true);
                    cr.setLBD(this.analyzeLBD);
                    cr.setOneWatched(false);
                    cr.setSizeWithoutSelectors(this.analyzeSzWithoutSelectors);
                    this.learnts.push(cr);
                    attachClause(cr);
                    claBumpActivity(cr);
                    uncheckedEnqueue(learntClause.get(0), cr);
                }
                varDecayActivity();
                claDecayActivity();
            } else {
                if (this.lbdQueue.valid() && (this.lbdQueue.avg() * this.factorK) > (this.sumLBD / this.conflictsRestarts)) {
                    this.lbdQueue.fastClear();
                    int bt = 0;
                    if (this.incremental) {
                        bt = Math.min(decisionLevel(), this.assumptions.size());
                    }
                    cancelUntil(bt);
                    return Tristate.UNDEF;
                }
                if (decisionLevel() == 0 && !simplify()) {
                    return Tristate.FALSE;
                }
                if (this.conflicts >= (this.curRestart * this.nbclausesbeforereduce) && this.learnts.size() > 0) {
                    this.curRestart = (this.conflicts / this.nbclausesbeforereduce) + 1;
                    reduceDB();
                    this.nbclausesbeforereduce += this.incReduceDB;
                }
                int next = LIT_UNDEF;
                while (decisionLevel() < this.assumptions.size()) {
                    final int p = this.assumptions.get(decisionLevel());
                    if (value(p) == Tristate.TRUE) {
                        this.trailLim.push(this.trail.size());
                    } else if (value(p) == Tristate.FALSE) {
                        analyzeFinal(not(p), this.conflict);
                        return Tristate.FALSE;
                    } else {
                        next = p;
                        break;
                    }
                }
                if (next == LIT_UNDEF) {
                    next = pickBranchLit();
                    if (next == LIT_UNDEF) {
                        return Tristate.TRUE;
                    }
                }
                this.trailLim.push(this.trail.size());
                uncheckedEnqueue(next, null);
            }
        }
    }

    /**
     * Analyzes a given conflict clause wrt. the current solver state.  A 1-UIP clause is created during this procedure
     * and the new backtracking level is stored in the solver state.
     * @param conflictClause the conflict clause to start the resolution analysis with
     * @param outLearnt      the vector where the new learnt 1-UIP clause is stored
     * @param selectors      a vector of selector variables
     */
    protected void analyze(final MSClause conflictClause, final LNGIntVector outLearnt,
                           final LNGIntVector selectors) {
        MSClause c = conflictClause;
        int pathC = 0;
        int p = LIT_UNDEF;
        outLearnt.push(-1);
        int index = this.trail.size() - 1;
        do {
            assert c != null;
            if (p != LIT_UNDEF && c.size() == 2 && value(c.get(0)) == Tristate.FALSE) {
                assert value(c.get(1)) == Tristate.TRUE;
                final int tmp = c.get(0);
                c.set(0, c.get(1));
                c.set(1, tmp);
            }
            if (c.learnt()) {
                claBumpActivity(c);
            } else {
                if (!c.seen()) {
                    c.setSeen(true);
                }
            }
            if (c.learnt() && c.lbd() > 2) {
                final long nblevels = computeLBD(c);
                if (nblevels + 1 < c.lbd()) {
                    if (c.lbd() <= this.lbLBDFrozenClause) {
                        c.setCanBeDel(false);
                    }
                    c.setLBD(nblevels);
                }
            }
            for (int j = (p == LIT_UNDEF) ? 0 : 1; j < c.size(); j++) {
                final int q = c.get(j);
                if (!this.seen.get(var(q)) && v(q).level() != 0) {
                    if (!isSelector(var(q))) {
                        varBumpActivity(var(q));
                    }
                    this.seen.set(var(q), true);
                    if (v(q).level() >= decisionLevel()) {
                        pathC++;
                        if (!isSelector(var(q)) && (v(q).reason() != null) && v(q).reason().learnt()) {
                            this.lastDecisionLevel.push(q);
                        }
                    } else {
                        if (isSelector(var(q))) {
                            assert value(q) == Tristate.FALSE;
                            selectors.push(q);
                        } else {
                            outLearnt.push(q);
                        }
                    }
                }
            }
            while (!this.seen.get(var(this.trail.get(index--)))) {
            }
            p = this.trail.get(index + 1);
            c = v(p).reason();
            this.seen.set(var(p), false);
            pathC--;
        } while (pathC > 0);
        outLearnt.set(0, not(p));
        simplifyClause(outLearnt, selectors);
    }

    /**
     * Minimizes a given learnt clause depending on the minimization method of the solver configuration.
     * @param outLearnt the learnt clause which should be minimized
     * @param selectors a vector of selector variables
     */
    protected void simplifyClause(final LNGIntVector outLearnt, final LNGIntVector selectors) {
        int i;
        int j;
        for (i = 0; i < selectors.size(); i++) {
            outLearnt.push(selectors.get(i));
        }
        this.analyzeToClear = new LNGIntVector(outLearnt);
        if (this.ccminMode == MiniSatConfig.ClauseMinimization.DEEP) {
            int abstractLevel = 0;
            for (i = 1; i < outLearnt.size(); i++) {
                abstractLevel |= abstractLevel(var(outLearnt.get(i)));
            }
            for (i = j = 1; i < outLearnt.size(); i++) {
                if (v(outLearnt.get(i)).reason() == null || !litRedundant(outLearnt.get(i), abstractLevel)) {
                    outLearnt.set(j++, outLearnt.get(i));
                }
            }
        } else if (this.ccminMode == MiniSatConfig.ClauseMinimization.BASIC) {
            for (i = j = 1; i < outLearnt.size(); i++) {
                final MSVariable v = v(outLearnt.get(i));
                if (v.reason() == null) {
                    outLearnt.set(j++, outLearnt.get(i));
                } else {
                    final MSClause c = v(outLearnt.get(i)).reason();
                    for (int k = c.size() == 2 ? 0 : 1; k < c.size(); k++) {
                        if (!this.seen.get(var(c.get(k))) && v(c.get(k)).level() > 0) {
                            outLearnt.set(j++, outLearnt.get(i));
                            break;
                        }
                    }
                }
            }
        } else {
            i = j = outLearnt.size();
        }
        outLearnt.removeElements(i - j);
        if (!this.incremental && outLearnt.size() <= this.lbSizeMinimizingClause) {
            minimisationWithBinaryResolution(outLearnt);
        }
        this.analyzeBtLevel = 0;
        if (outLearnt.size() > 1) {
            int max = 1;
            for (int k = 2; k < outLearnt.size(); k++) {
                if (v(outLearnt.get(k)).level() > v(outLearnt.get(max)).level()) {
                    max = k;
                }
            }
            final int p = outLearnt.get(max);
            outLearnt.set(max, outLearnt.get(1));
            outLearnt.set(1, p);
            this.analyzeBtLevel = v(p).level();
        }
        this.analyzeSzWithoutSelectors = 0;
        if (this.incremental) {
            for (int k = 0; k < outLearnt.size(); k++) {
                if (!isSelector(var(outLearnt.get(k)))) {
                    this.analyzeSzWithoutSelectors++;
                } else if (k > 0) {
                    break;
                }
            }
        } else {
            this.analyzeSzWithoutSelectors = outLearnt.size();
        }
        this.analyzeLBD = computeLBD(outLearnt, outLearnt.size() - selectors.size());
        if (this.lastDecisionLevel.size() > 0) {
            for (int k = 0; k < this.lastDecisionLevel.size(); k++) {
                if ((v(this.lastDecisionLevel.get(k)).reason()).lbd() < this.analyzeLBD) {
                    varBumpActivity(var(this.lastDecisionLevel.get(k)));
                }
            }
            this.lastDecisionLevel.clear();
        }
        for (int m = 0; m < this.analyzeToClear.size(); m++) {
            this.seen.set(var(this.analyzeToClear.get(m)), false);
        }
        for (int m = 0; m < selectors.size(); m++) {
            this.seen.set(var(selectors.get(m)), false);
        }
    }

    @Override
    protected boolean isRotatable(final int lit) {
        if (!super.isRotatable(lit)) {
            return false;
        }
        for (final MSWatcher watcher : this.watchesBin.get(not(lit))) {
            if (isUnit(lit, watcher.clause())) {
                return false;
            }
        }
        return true;
    }
}

