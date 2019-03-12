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

/**************************************************************************
 * Copyright (C) 2012 - 2014 Armin Biere JKU Linz
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 ****************************************************************************/

package org.logicng.solvers.sat;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGLongPriorityQueue;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.datastructures.CLClause;
import org.logicng.solvers.datastructures.CLFrame;
import org.logicng.solvers.datastructures.CLOccs;
import org.logicng.solvers.datastructures.CLVar;
import org.logicng.solvers.datastructures.CLWatch;
import org.logicng.util.Pair;

/**
 * A complete Reimplementation of the CleaneLing solver.
 * @version 1.3
 * @since 1.0
 * @deprecated CleaneLing does not support many of the features of MiniSat in LogicNG.
 * With an upcoming 2.0 Release of LogicNG CleaneLing will be removed.
 */
public final class CleaneLingSolver extends CleaneLingStyleSolver {

    private enum Simplifier {NOSIMP, TOPSIMP, BLOCK, ELIM}

    private boolean dense;
    private boolean schedule;
    private int touched;
    private LNGIntVector original;
    private LNGVector<CLClause> clauses;
    private LNGVector<CLOccs[]> occs;
    private LNGLongPriorityQueue candsElim;
    private LNGLongPriorityQueue candsBlock;
    private LNGVector<Pair<CLClause, Integer>> tostrengthen;
    private int distilled;
    private LNGIntVector extension;
    private Simplifier simplifier;

    /**
     * Constructs a new CleaneLing solver with a given configuration.
     * @param config the configuration
     */
    public CleaneLingSolver(final CleaneLingConfig config) {
        super(config);
        this.initializeOriginalSolver();
    }

    /**
     * Initializes the internal solver state.
     */
    private void initializeOriginalSolver() {
        this.dense = false;
        this.schedule = false;
        this.touched = 0;
        this.distilled = 0;
        this.original = new LNGIntVector();
        this.clauses = new LNGVector<>();
        this.occs = new LNGVector<>();
        this.candsElim = new LNGLongPriorityQueue();
        this.candsBlock = new LNGLongPriorityQueue();
        this.tostrengthen = new LNGVector<>();
        this.extension = new LNGIntVector();
        this.simplifier = Simplifier.NOSIMP;
    }

    @Override
    public void addlit(final int lit) {
        this.original.push(lit);
        if (lit != 0) {
            importLit(lit);
            this.addedlits.push(lit);
        } else {
            if (!trivialClause()) { newPushConnectClause(); }
            this.addedlits.clear();
        }
    }

    @Override
    public Tristate solve(final SATHandler handler) {
        this.handler = handler;
        if (this.handler != null) { this.handler.startedSolving(); }
        this.model.clear();
        initLimits();
        biasPhases();
        Tristate res;
        while (true) {
            if ((res = search()) != UNDEF || this.canceledByHandler) { break; } else if ((res = simplify()) != UNDEF || this.canceledByHandler) {
                break;
            } else {
                updateLimits();
            }
        }
        switch (res) {
            case TRUE:
                extend();
                break;
            case FALSE:
                break;
            default:
                break;
        }
        if (res == TRUE) { for (int i = 0; i < this.vals.size(); i++) { this.model.push(this.vals.get(i) == VALUE_TRUE); } }
        if (this.handler != null) { this.handler.finishedSolving(); }
        this.handler = null;
        this.canceledByHandler = false;
        backtrack();
        return res;
    }

    @Override
    public void reset() {
        super.reset();
        this.initializeOriginalSolver();
    }

    @Override
    protected void importLit(final int lit) {
        final int idx = Math.abs(lit);
        assert lit != 0;
        int newIdx;
        while (idx >= (newIdx = this.vars.size())) {
            this.vars.push(new CLVar());
            this.vals.push((byte) 0);
            this.phases.push((byte) 0);
            this.watches.push(new LNGVector<CLWatch>());
            this.watches.push(new LNGVector<CLWatch>());
            this.occs.push(new CLOccs[]{new CLOccs(), new CLOccs()});
            if (newIdx == 0) { continue; }
            this.decisions.push(newIdx);
        }
    }

    @Override
    protected void newPushConnectClause(final boolean redundant, final int glue) {
        final CLClause c = newClause(redundant, glue);
        this.clauses.push(c);
        connectClause(c);
    }

    @Override
    protected void assign(final int lit, final CLClause reason) {
        final CLVar v = var(lit);
        assert val(lit) == VALUE_UNASSIGNED;
        final int idx = Math.abs(lit);
        final byte s = sign(lit);
        this.vals.set(idx, s);
        if (this.simplifier == Simplifier.NOSIMP) { this.phases.set(idx, s); }
        v.setLevel(this.level);
        if (this.level == 0) {
            this.stats.varsFixed++;
            if (v.state() == CLVar.State.ELIMINATED) {
                assert this.stats.varsEliminated > 0;
                this.stats.varsEliminated--;
            } else { assert v.state() == CLVar.State.FREE; }
            v.setState(CLVar.State.FIXED);
        }
        this.trail.push(lit);
        v.setReason(reason);
        if (v.reason() != null) {
            assert !reason.forcing();
            reason.setForcing(true);
            if (reason.redundant() && !reason.important()) { this.limits.reduceForcing++; }
            updateGlue(reason);
        }
    }

    @Override
    protected void unassign(final int lit) {
        assert this.level > 0;
        final CLClause reason;
        final CLVar v = var(lit);
        this.vals.set(Math.abs(lit), VALUE_UNASSIGNED);
        assert v.level() == this.level;
        v.setLevel(Integer.MAX_VALUE);
        reason = v.reason();
        if (reason != null) {
            assert reason.forcing();
            reason.setForcing(false);
            if (reason.redundant() && !reason.important()) {
                assert this.limits.reduceForcing > 0;
                this.limits.reduceForcing--;
            }
        }
        final int idx = Math.abs(lit);
        if (!this.decisions.contains(idx)) { this.decisions.push(idx); }
    }

    @Override
    protected void initLimits() {
        newRestartLimit();
        if (this.limits.simpSteps == 0) { this.limits.simpSteps = this.config.simpint; }
        if (this.stats.simplifications == 0) {
            assert this.config.boost > 0;
            if (this.limits.simpSteps >= Integer.MAX_VALUE / this.config.boost) { this.limits.simpSteps = Integer.MAX_VALUE; } else { this.limits.simpSteps *= this.config.boost; }
        }
        if (this.limits.searchConflicts == 0 && this.config.searchfirst) { this.limits.searchConflicts = this.config.searchint; }
        this.limits.simpRemovedVars = 0;
    }

    @Override
    protected void updateLimits() {
        if (this.config.simpgeom) {
            if (this.limits.simpInc >= Integer.MAX_VALUE / 2) { this.limits.simpInc = Integer.MAX_VALUE; } else if (this.limits.simpInc == 0) {
                this.limits.simpInc = this.config.simpint;
            } else { this.limits.simpInc *= 2; }
        } else {
            if (this.limits.simpInc >= Integer.MAX_VALUE - this.config.simpint) { this.limits.simpInc = Integer.MAX_VALUE; } else { this.limits.simpInc += this.config.simpint; }
        }
        this.limits.simpSteps = this.limits.simpInc;
        if (this.config.stepslim != 0 && this.limits.simpSteps > this.config.stepslim) { this.limits.simpSteps = this.config.stepslim; }

        if (this.limits.searchInc == 0) { this.limits.searchInc = this.config.searchint; }
        assert this.limits.searchInc != 0;
        if (this.limits.searchConflicts != 0) {
            int inc = this.limits.searchInc;
            final long removed = this.limits.simpRemovedVars;
            if (removed > 0 && remainingVars() != 0) {
                final long reduction = (100 * removed) / remainingVars();
                if (reduction > 1) { inc /= reduction; }
            }
            if (this.limits.searchInc >= Integer.MAX_VALUE - inc) { this.limits.searchInc = Integer.MAX_VALUE; } else { this.limits.searchInc += inc; }
        } else { assert !this.config.searchfirst; }
        this.limits.searchConflicts = this.limits.searchInc;
    }

    @Override
    protected CLClause newClause(final boolean redundant, final int glue) {
        final CLClause c = new CLClause();
        assert glue == 0 || redundant;
        assert eachVariableOccursOnlyOnce();
        if (this.config.gluered) { c.setGlue(glue); } else { assert c.glue() == 0; }
        c.setImportant(glue <= this.config.gluekeep);
        c.setRedundant(redundant);
        c.setActivity(this.stats.conflicts);
        for (int i = 0; i < this.addedlits.size(); i++) { c.lits().push(this.addedlits.get(i)); }
        if (redundant) { this.stats.clausesRedundant++; } else { this.stats.clausesIrredundant++; }
        return c;
    }

    @Override
    protected void connectClause(final CLClause c) {
        if (c.satisfied()) { return; }

        final int size = c.size();
        final boolean binary = size == 2;
        for (int p = 0; p < 2; p++) {
            for (int q = p + 1; q < c.lits().size(); q++) {
                final int lit = c.lits().get(q);
                final int litP = c.lits().get(p);
                final int cmp = var(lit).level() - var(litP).level();
                if (cmp > 0 || (cmp == 0 && (val(lit) > val(litP)))) {
                    c.lits().set(p, lit);
                    c.lits().set(q, litP);
                }
            }
        }
        final int l0 = c.lits().get(0);
        final int l1 = l0 != 0 ? c.lits().get(1) : 0;
        final int newLevel = (l0 != 0 && l1 != 0) ? Math.min(var(l0).level(), var(l1).level()) : 0;
        if (newLevel != Integer.MAX_VALUE) { backtrack(newLevel); }
        if (size >= 2) {
            addWatch(l0, l1, binary, c);
            addWatch(l1, l0, binary, c);
            if (this.dense) { connectOccs(c); }
        }
        boolean ignore = false;
        int lit = 0;
        int other;
        int tmp;
        int p;
        for (p = 0; p < c.lits().size(); p++) {
            other = c.lits().get(p);
            tmp = val(other);
            if (tmp == VALUE_TRUE) {
                ignore = true;
                break;
            }
            if (tmp == VALUE_UNASSIGNED) {
                if (lit != 0) { ignore = true; } else { lit = other; }
            }
        }
        if (!ignore) {
            if (lit == 0) {
                assert this.level == 0;
                if (this.empty == null) { this.empty = c; }
            } else { assign(lit, c); }
        }
        if (c.redundant() && c.important()) { this.limits.reduceImportant++; }
    }

    @Override
    protected CLClause bcp() {
        int visits = 0;
        int propagations = 0;
        CLClause conflict = this.empty;
        while (conflict == null && this.next < this.trail.size()) {
            propagations++;
            final int lit = -this.trail.get(this.next++);
            this.stats.steps++;
            final LNGVector<CLWatch> ws = watches(lit);
            final LNGVector<CLWatch> newWS = new LNGVector<>();
            int i;
            for (i = 0; conflict == null && i < ws.size(); i++) {
                final CLWatch w = ws.get(i);
                newWS.push(w);
                int other = w.blit();
                byte v = val(other);
                if (v == VALUE_TRUE) { continue; }
                final CLClause clause = w.clause();
                if (this.ignore != null) {
                    if (this.ignore == clause) { continue; }
                    if (clause.redundant()) {
                        if (!w.binary()) { visits++; }
                        continue;
                    }
                }
                if (w.binary()) {
                    if (v == VALUE_FALSE) { conflict = clause; } else { assign(other, clause); }
                } else {
                    visits++;
                    if (clause.dumped()) {
                        newWS.pop();
                        continue;
                    }
                    int p;
                    if (clause.lits().get(0) == lit) {
                        final int temp = clause.lits().get(0);
                        clause.lits().set(0, clause.lits().get(1));
                        clause.lits().set(1, temp);
                    }
                    assert clause.lits().get(1) == lit;
                    for (p = 2; p < clause.lits().size(); p++) {
                        other = clause.lits().get(p);
                        if (val(other) >= 0) { break; }
                    }
                    if (p == clause.size()) { other = 0; }
                    if (other != 0) {
                        clause.lits().set(p, lit);
                        clause.lits().set(1, other);
                        addWatch(other, clause.lits().get(0), false, clause);
                        newWS.pop();
                    } else {
                        other = clause.lits().get(0);
                        v = val(other);
                        if (v == VALUE_FALSE) { conflict = clause; } else if (v != VALUE_TRUE) { assign(other, clause); } else { newWS.back().setBlit(other); }
                    }
                }
            }
            if (conflict != null) { while (i < ws.size()) { newWS.push(ws.get(i++)); } }
            ws.replaceInplace(newWS);
        }
        if (conflict != null && this.simplifier == Simplifier.NOSIMP) { this.stats.conflicts++; }
        this.stats.propagations += propagations;
        this.stats.steps += visits;
        return conflict;
    }

    @Override
    protected void minimizeClause() {
        final int learned = this.addedlits.size();
        this.stats.litsLearned += learned;
        final LNGIntVector newAddedLits = new LNGIntVector(this.addedlits.size());
        for (int i = 0; i < this.addedlits.size(); i++) {
            final int lit = this.addedlits.get(i);
            if (!minimizeLit(-lit)) { newAddedLits.push(lit); }
        }
        this.addedlits = newAddedLits;
        this.stats.litsMinimized += learned - this.addedlits.size();
    }

    @Override
    protected void analyze(final CLClause r) {
        CLClause reason = r;
        if (this.empty != null) {
            assert this.level == 0;
            return;
        }
        updateGlue(reason);
        assert this.addedlits.empty();
        int lit = 0;
        int open = 0;
        int it = this.trail.size();
        while (true) {
            bumpClause(reason);
            for (int p = 0; p < reason.lits().size(); p++) {
                lit = reason.lits().get(p);
                if (pullLit(lit)) { open++; }
            }
            while (it > 0 && marked(lit = -this.trail.get(--it)) == 0) { assert var(lit).level() == this.level; }
            if (it == 0 || --open == 0) { break; }
            reason = var(lit).reason();
            assert reason != null;
        }

        assert lit != 0;
        this.addedlits.push(lit);
        minimizeClause();
        unmark();
        final int glue = unmarkFrames();
        this.stats.gluesCount++;
        this.stats.gluesSum += glue;
        this.stats.sizes += this.addedlits.size();
        newPushConnectClause(true, glue);
        this.addedlits.clear();
        this.scoreIncrement *= this.config.scincfact / 1000.0;
        if (this.simplifier == Simplifier.NOSIMP && this.level == 0 && this.empty == null) {
            this.limits.searchConflicts += this.config.itsimpdel;
            this.stats.iterations++;
        }
    }

    @Override
    protected boolean restarting() {
        return this.config.restart && this.stats.conflicts >= this.limits.restart;
    }

    @Override
    protected void restart() {
        this.stats.restartsCount++;
        int nextDecision = 0;
        while (nextDecision == 0 && !this.decisions.empty()) {
            final int lit = this.decisions.top();
            if (val(lit) != 0) { this.decisions.pop(lit); } else { nextDecision = lit; }
        }
        if (nextDecision != 0) {
            int newLevel;
            if (this.config.reusetrail) {
                final double nextDecisionPriority = this.decisions.priority(nextDecision);
                for (newLevel = 0; newLevel < this.level; newLevel++) {
                    final CLFrame frame = this.control.get(newLevel + 1);
                    final int decision = Math.abs(frame.decision());
                    if (this.decisions.priority(decision) < nextDecisionPriority) { break; }
                }
                if (newLevel != 0) {
                    this.stats.restartsReuseCount++;
                    this.stats.restartsReuseSum += (100 * newLevel) / this.level;
                }
            } else {
                newLevel = 0; // Do not reuse trail.  Back track to the top.
            }
            backtrack(newLevel);
        }
        newRestartLimit();
    }

    @Override
    protected Tristate search() {
        long conflicts = 0;
        CLClause conflict;
        Tristate res = UNDEF;
        while (res == UNDEF) {
            if (this.empty != null) { res = FALSE; } else if ((conflict = bcp()) != null) {
                if (this.handler != null && !this.handler.detectedConflict()) {
                    this.canceledByHandler = true;
                    return UNDEF;
                }
                analyze(conflict);
                conflicts++;
            } else if (conflicts >= this.limits.searchConflicts) { break; } else if (reducing()) { reduce(); } else if (restarting()) {
                restart();
            } else if (!decide()) { res = TRUE; }
        }
        return res;
    }

    /**
     * Initializes the initial phases of literals.
     */
    private void biasPhases() {
        final double[] score = new double[2 * (maxvar() + 1)];
        for (final CLClause c : this.clauses) {
            if (c.redundant() || c.satisfied()) { continue; }
            double inc = 1;
            for (int size = c.size(); size > 0; size--) { inc /= 2.0; }
            for (int i = 0; i < c.lits().size(); i++) {
                final int p = c.lits().get(i);
                if (p > 0) { score[p * 2] += inc; } else { score[(-p * 2) - 1] += inc; }
            }
        }
        for (int idx = 1; idx <= maxvar(); idx++) { this.phases.set(idx, (score[idx * 2] > score[(idx * 2) - 1]) ? (byte) 1 : (byte) -1); }
    }

    /**
     * Returns the occurrence list for a given literal.
     * @param lit the literal
     * @return the occurrence list for the literal
     */
    private CLOccs occs(final int lit) {
        return this.occs.get(Math.abs(lit))[lit < 0 ? 0 : 1];
    }

    /**
     * Updates and pushes a literal as candidate to simplify.
     * @param l the literal
     */
    private void touch(final int l) {
        int lit = l;
        if (lit < 0) { lit = -lit; }
        final long newPriority = (long) occs(lit).count() + occs(-lit).count();
        if (!var(lit).free()) { return; }
        final LNGLongPriorityQueue queue = currentCands();
        queue.update(lit, -newPriority);
        if (this.schedule && !queue.contains(lit)) { queue.push(lit); }
    }

    /**
     * Returns a list of candidates to simplify depending on the current simplifier.
     * @return the list of candidates (literals)
     */
    private LNGLongPriorityQueue currentCands() {
        switch (this.simplifier) {
            case BLOCK:
                return this.candsBlock;
            default:
                return this.candsElim;
        }
    }

    /**
     * Touches literals in newly top-level satisfied clauses using the literals not touched yet.
     */
    private void touchFixed() {
        assert this.dense;
        assert this.level == 0;
        assert this.schedule;
        while (this.touched < this.trail.size()) {
            final int lit = this.trail.get(this.touched++);
            assert val(lit) > 0;
            assert var(lit).level() == 0;
            final CLOccs os = occs(lit);
            for (final CLClause c : os) {
                for (int i = 0; i < c.lits().size(); i++) {
                    final int other = c.lits().get(i);
                    if (val(other) == VALUE_TRUE) { continue; }
                    assert other != lit;
                    touch(other);
                }
            }
        }
    }

    /**
     * Adds a clause to a literal's occurrence list.
     * @param lit the literal
     * @param c   the clause
     */
    private void addOcc(final int lit, final CLClause c) {
        assert this.dense;
        occs(lit).add(c);
        touch(lit);
    }

    /**
     * Connects a clause through a full occurrence list.
     * @param c the clause
     */
    private void connectOccs(final CLClause c) {
        assert this.dense;
        for (int i = 0; i < c.lits().size(); i++) { addOcc(c.lits().get(i), c); }
    }

    /**
     * Connects all clauses through full occurrence lists.
     */
    private void connectOccs() {
        assert !this.dense;
        this.dense = true;
        for (final CLClause c : this.clauses) { if (!c.redundant()) { connectOccs(c); } }
    }

    /**
     * Decrements full occurrence for a literal.
     * @param lit the literal
     */
    private void decOcc(final int lit) {
        assert this.dense;
        occs(lit).dec();
        touch(lit);
    }

    /**
     * Updates the glue value for a given clause.
     * @param c the clause
     */
    private void updateGlue(final CLClause c) {
        if (!this.config.glueupdate) { return; }
        if (!this.config.gluered) {
            assert c.glue() == 0;
            return;
        }
        assert this.frames.empty();
        for (int i = 0; i < c.lits().size(); i++) { markFrame(c.lits().get(i)); }
        final int newGlue = unmarkFrames();
        if (newGlue >= c.glue()) { return; }
        c.setGlue(newGlue);
        this.stats.gluesSum += newGlue;
        this.stats.gluesCount++;
        this.stats.gluesUpdates++;
    }

    /**
     * Dumps a given clause.
     * @param c the clause
     */
    private void dumpClause(final CLClause c) {
        if (c.dumped()) { return; }
        if (c.redundant()) {
            assert this.stats.clausesRedundant > 0;
            this.stats.clausesRedundant--;
        } else {
            assert this.stats.clausesIrredundant > 0;
            this.stats.clausesIrredundant--;
            if (this.dense) { for (int i = 0; i < c.lits().size(); i++) { decOcc(c.lits().get(i)); } }
        }
        c.setDumped(true);
    }

    /**
     * Deletes a given clause (for this implementation this is equivalent to dumping the clause).
     * @param c the clause
     */
    private void deleteClause(final CLClause c) {
        dumpClause(c);
    }

    /**
     * Bumps a clause and increments it's activity.
     * @param c the clause
     */
    private void bumpClause(final CLClause c) {
        switch (this.config.cbump) {
            case INC:
                c.setActivity(c.activity() + 1);
                break;
            case LRU:
                c.setActivity(this.stats.conflicts);
                break;
            case AVG:
                c.setActivity((this.stats.conflicts + c.activity()) / 2);
                break;
            case NONE:
            default:
                assert this.config.cbump == CleaneLingConfig.ClauseBumping.NONE;
                break;
        }
    }

    /**
     * Returns {@code true} if a reduction should be performed, {@code false} otherwise.
     * @return {@code true} if a reduction should be performed
     */
    private boolean reducing() {
        if (this.limits.reduceRedundant == 0) { this.limits.reduceRedundant = this.config.redinit; }
        long limit = this.limits.reduceRedundant;
        limit += this.limits.reduceForcing;
        limit += this.limits.reduceImportant;
        return limit <= this.stats.clausesRedundant;
    }

    /**
     * Reduces the number of redundant clauses.
     */
    private void reduce() {
        this.stats.reductions++;
        final LNGVector<CLClause> candidates = new LNGVector<>();
        for (final CLClause c : this.clauses) { if (c.redundant() && !c.important() && !c.forcing()) { candidates.push(c); } }
        final int keep = candidates.size() / 2;
        candidates.sort(CLClause.comp);
        for (int i = keep; i < candidates.size(); i++) { candidates.get(i).setRemove(true); }
        for (int idx = 1; idx <= maxvar(); idx++) {
            for (int sign = -1; sign <= 1; sign += 2) {
                final LNGVector<CLWatch> ws = watches(sign * idx);
                final LNGVector<CLWatch> newWs = new LNGVector<>(ws.size());
                for (final CLWatch w : ws) { if (!w.clause().remove()) { newWs.push(w); } }
                ws.replaceInplace(newWs);
            }
        }
        int j = 0;
        int i;
        for (i = 0; i < this.clauses.size(); i++) {
            final CLClause c = this.clauses.get(i);
            if (i == this.distilled) { this.distilled = j; }
            if (!c.remove()) { this.clauses.set(j++, c); }
        }
        if (i == this.distilled) { this.distilled = j; }
        this.clauses.shrinkTo(j);
        long reduced = 0;
        for (int k = keep; k < candidates.size(); k++) {
            deleteClause(candidates.get(k));
            reduced++;
        }
        this.stats.clausesReduced += reduced;
        candidates.release();
        this.limits.reduceRedundant += this.config.redinc;
    }

    /**
     * Returns the number of remaining variables without fixed and eliminated.
     * @return the number of remaining variables
     */
    private int remainingVars() {
        int res = maxvar();
        res -= this.stats.varsFixed;
        res -= this.stats.varsEquivalent;
        res -= this.stats.varsEliminated;
        return res;
    }

    /**
     * Removes a literal in a clause.
     * @param c      the clause
     * @param remove the literal to remove
     */
    private void strengthen(final CLClause c, final int remove) {
        if (c.dumped() || satisfied(c)) { return; }
        assert this.addedlits.empty();
        for (int i = 0; i < c.lits().size(); i++) {
            final int lit = c.lits().get(i);
            if (lit != remove && val(lit) == 0) { this.addedlits.push(lit); }
        }
        newPushConnectClause();
        this.addedlits.clear();
        dumpClause(c);
    }

    /**
     * Returns {@code true} if a given clause is satisfied, {@code false} otherwise.
     * @param c the clause
     * @return {@code true} if a given clause is satisfied
     */
    private boolean satisfied(final CLClause c) {
        if (c.satisfied()) { return true; }
        for (int i = 0; i < c.lits().size(); i++) {
            if (val(c.lits().get(i)) == VALUE_TRUE) {
                if (this.level == 0) { c.setSatisfied(true); }
                return true;
            }
        }
        return false;
    }

    /**
     * Reduces simplification steps for large formulas.
     * @return the size penalty
     */
    private int sizePenalty() {
        long numClauses = (long) this.stats.clausesIrredundant / this.config.sizepen;
        int logres = 0;
        while (numClauses != 0 && logres < this.config.sizemaxpen) {
            numClauses >>= 1;
            logres++;
        }
        return 1 << logres;
    }

    /**
     * Distills units and subsumes/strengthens clauses.
     */
    private void distill() {
        final long steps = this.limits.simpSteps;
        assert this.level == 0;
        assert !this.config.plain;
        if (!this.config.distill) { return; }
        final long limit = this.stats.steps + steps / sizePenalty();
        boolean changed = false;
        boolean done = this.clauses.empty();
        if (this.distilled >= this.clauses.size()) { this.distilled = 0; }
        while (this.empty == null && !done && this.stats.steps++ < limit) {
            final CLClause c = this.clauses.get(this.distilled);
            if (!c.redundant() && !c.dumped() && c.large() && !satisfied(c)) {
                CLClause conflict = null;
                assert this.ignore == null;
                this.ignore = c;
                int lit;
                for (int p = 0; conflict == null && p < c.lits().size(); p++) {
                    lit = c.lits().get(p);
                    final byte v = val(lit);
                    if (v == VALUE_FALSE) { continue; }
                    if (v == VALUE_TRUE) {
                        this.stats.distillStrengthened++;
                        strengthen(c, lit);
                        changed = true;
                        break;
                    }
                    assume(-lit);
                    conflict = bcp();
                }
                this.ignore = null;
                if (conflict != null) {
                    analyze(conflict);

                    if (this.level == 0) {
                        assert this.next < this.trail.size();
                        this.stats.distillUnits++;
                        changed = true;
                        conflict = bcp();
                        if (conflict != null) {
                            analyze(conflict);
                            assert this.empty != null;
                        }
                    } else {
                        this.stats.distillSubsumed++;
                        dumpClause(c);
                    }
                }
                backtrack();
            }
            if (++this.distilled == this.clauses.size()) {
                if (changed) { changed = false; } else { done = true; }
                this.distilled = 0;
            }
        }
    }

    /**
     * Tries to resolve to clauses with a given pivot literal.  Returns {@code true} if the resolvent is not trivial,
     * {@code false} otherwise.
     * @param c     the first clause
     * @param pivot the pivot literal
     * @param d     the second clause
     * @return {@code true} if the resolvent is not trivial
     */
    private boolean tryResolve(final CLClause c, final int pivot, final CLClause d) {
        assert !c.dumped() && !c.satisfied();
        assert !d.dumped() && !d.satisfied();
        boolean res = true;
        assert this.seen.empty();
        this.stats.steps++;
        for (int i = 0; i < c.lits().size(); i++) {
            final int lit = c.lits().get(i);
            if (lit == pivot) { continue; }
            assert marked(lit) == 0;
            mark(lit);
        }
        this.stats.steps++;
        for (int p = 0; res && p < d.lits().size(); p++) {
            final int lit = d.lits().get(p);
            if (lit == -pivot) { continue; }
            final int m = marked(lit);
            if (m > 0) { continue; }
            if (m < 0) { res = false; } else { mark(lit); }
        }
        unmark();
        return res;
    }

    /**
     * Backward subsumes from clause.
     * @param c      the clause
     * @param ignore the literal to ignore
     */
    private void backward(final CLClause c, final int ignore) {
        int minlit = 0;
        int minoccs = Integer.MAX_VALUE;
        int litoccs;
        this.stats.steps++;
        for (int i = 0; i < c.lits().size(); i++) {
            final int lit = c.lits().get(i);
            if (lit == ignore) { continue; }
            if (val(lit) < 0) { continue; }
            litoccs = occs(lit).count();
            if (minlit != 0 && minoccs >= litoccs) { continue; }
            minlit = lit;
            minoccs = litoccs;
        }
        if (minoccs >= this.config.bwocclim) { return; }
        assert minlit != 0;
        for (int i = 0; i < c.lits().size(); i++) { mark(c.lits().get(i)); }
        final CLOccs os = occs(minlit);
        for (final CLClause d : os) {
            if (d == c) { continue; }
            int lit;
            int count = this.seen.size();
            int negated = 0;
            this.stats.steps++;
            for (int p = 0; count != 0 && p < d.lits().size(); p++) {
                lit = d.lits().get(p);
                final int m = marked(lit);
                if (m == 0) { continue; }
                assert count > 0;
                count--;
                if (m > 0) { continue; }
                assert m < 0;
                if (negated != 0) {
                    count = Integer.MAX_VALUE;
                    break;
                }
                negated = lit;
            }
            if (count != 0) { continue; }
            if (negated != 0) {
                this.tostrengthen.push(new Pair<>(d, negated));
                this.stats.backwardStrengthened++;
            } else {
                this.stats.backwardSubsumed++;
                dumpClause(d);
            }
        }
        unmark();
    }

    /**
     * Backward subsumes from clauses in the occurrence list of a given literal.
     * @param lit the literal
     */
    private void backward(final int lit) {
        assert this.level == 0;
        assert this.dense;
        assert this.tostrengthen.empty();
        final CLOccs os = occs(lit);
        for (final CLClause c : os) {
            assert !c.redundant();
            this.stats.steps++;
            if (c.dumped()) { continue; }
            if (c.size() >= this.config.bwclslim) { continue; }
            if (satisfied(c)) { continue; }
            backward(c, lit);
        }
        while (!this.tostrengthen.empty()) {
            final Pair<CLClause, Integer> cplp = this.tostrengthen.back();
            this.tostrengthen.pop();
            strengthen(cplp.first(), cplp.second());
        }
    }

    /**
     * Tries bounded variable elimination on a candidate literal.
     * @param cand the literal
     * @return {@code true} if resolution of all positive with all negative occurrences of the candidate produces at most
     * as many non trivial resolvents as the number of positive plus negative occurrences, {@code false} otherwise
     */
    private boolean tryElim(final int cand) {
        assert var(cand).free();
        final CLOccs p = occs(cand);
        final CLOccs n = occs(-cand);
        long limit = (long) p.count() + n.count();
        for (int i = 0; limit >= 0 && i < p.clauses().size(); i++) {
            final CLClause c = p.clauses().get(i);
            assert !c.redundant();
            this.stats.steps++;
            if (c.dumped() || satisfied(c)) { continue; }
            for (int j = 0; limit >= 0 && j < n.clauses().size(); j++) {
                final CLClause d = n.clauses().get(j);
                assert !d.redundant();
                this.stats.steps++;
                if (d.dumped() || satisfied(d)) { continue; }
                if (tryResolve(c, cand, d)) { limit--; }
            }
        }
        return limit >= 0;
    }

    /**
     * Performs resolution on two given clauses and a pivot literal.
     * @param c     the first clause
     * @param pivot the pivot literal
     * @param d     the second clause
     */
    private void doResolve(final CLClause c, final int pivot, final CLClause d) {
        assert !c.dumped() && !c.satisfied();
        assert !d.dumped() && !d.satisfied();
        assert this.addedlits.empty();
        this.stats.steps++;
        for (int i = 0; i < c.lits().size(); i++) {
            final int lit = c.lits().get(i);
            if (lit != pivot) { this.addedlits.push(lit); }
        }
        this.stats.steps++;
        for (int i = 0; i < d.lits().size(); i++) {
            final int lit = d.lits().get(i);
            if (lit != -pivot) { this.addedlits.push(lit); }
        }
        if (!trivialClause()) { newPushConnectClause(); }
        this.addedlits.clear();
    }

    /**
     * Pushes and logs a literal to the extension.
     * @param lit the literal
     */
    private void pushExtension(final int lit) {
        this.extension.push(lit);
    }

    /**
     * Pushes and logs a clause and its blocking literal to the extension.
     * @param c    the clause
     * @param blit the blocking literal
     */
    private void pushExtension(final CLClause c, final int blit) {
        pushExtension(0);
        for (int i = 0; i < c.lits().size(); i++) {
            final int lit = c.lits().get(i);
            if (lit != blit) { pushExtension(lit); }
        }
        pushExtension(blit);
    }

    /**
     * Performs blocking variable elimination on a candidate.
     * @param cand the candidate literal
     */
    private void doElim(final int cand) {
        assert this.schedule;
        assert var(cand).free();
        final CLOccs p = occs(cand);
        final CLOccs n = occs(-cand);
        for (final CLClause c : p) {
            this.stats.steps++;
            if (c.dumped() || satisfied(c)) { continue; }
            for (final CLClause d : n) {
                this.stats.steps++;
                if (d.dumped() || satisfied(d)) { continue; }
                doResolve(c, cand, d);
            }
        }
        final int extend;
        final CLOccs e;
        if (p.count() < n.count()) {
            extend = cand;
            e = p;
        } else {
            extend = -cand;
            e = n;
        }
        for (final CLClause c : e) {
            if (c.dumped() || satisfied(c)) { continue; }
            this.stats.steps++;
            pushExtension(c, extend);
        }
        pushExtension(0);
        pushExtension(-extend);
        while (!p.clauses().empty()) {
            final CLClause c = p.clauses().back();
            this.stats.steps++;
            p.clauses().pop();
            if (c.satisfied() || c.dumped()) { continue; }
            this.stats.clausesEliminated++;
            dumpClause(c);
        }
        p.clauses().release();
        while (!n.clauses().empty()) {
            final CLClause c = n.clauses().back();
            this.stats.steps++;
            n.clauses().pop();
            if (c.satisfied() || c.dumped()) { continue; }
            this.stats.clausesEliminated++;
            dumpClause(c);
        }
        n.clauses().release();
        var(cand).setState(CLVar.State.ELIMINATED);
        this.stats.varsEliminated++;
        final CLClause conflict = bcp();
        if (conflict != null) {
            analyze(conflict);
            assert this.empty != null;
        }
        touchFixed();
    }

    /**
     * Returns {@code true} if a given clause contains an eliminated variable, {@code false} otherwise.
     * @param c the clause
     * @return {@code true} if a given clause contains an eliminated variable
     */
    private boolean containsEliminated(final CLClause c) {
        for (int i = 0; i < c.lits().size(); i++) { if (var(c.lits().get(i)).state() == CLVar.State.ELIMINATED) { return true; } }
        return false;
    }

    /**
     * Dumps redundant clauses with eliminated variables.
     */
    private void dumpEliminatedRedundant() {
        for (final CLClause c : this.clauses) {
            if (!c.redundant() || c.satisfied() || c.dumped()) { continue; }
            if (containsEliminated(c)) { dumpClause(c); }
        }
    }

    /**
     * Updates the candidates priority queue for new rounds of simplification.
     */
    private void updateCands() {
        if (!this.dense) { connectOccs(); }
        assert !this.schedule;
        this.schedule = true;
        if (this.schedule = currentCands().empty()) { for (int idx = 1; idx <= maxvar(); idx++) { touch(idx); } }
        this.schedule = true;
        touchFixed();
    }

    /**
     * Returns {@code true} if a given candidate literal should not be eliminated, {@code false} otherwise.
     * @param cand the candidate literal
     * @return {@code true} if a given candidate literal should not be eliminated
     */
    private boolean donotelim(final int cand) {
        int sign;
        if (occs(cand).count() > this.config.elmpocclim1) { return true; }
        if (occs(-cand).count() > this.config.elmpocclim1) { return true; }
        if (occs(cand).count() > this.config.elmpocclim2 && occs(-cand).count() > this.config.elmpocclim2) { return true; }
        for (sign = -1; sign <= 1; sign += 2) {
            final CLOccs os = occs(sign * cand);
            for (final CLClause c : os) {
                assert !c.redundant();
                if (c.size() >= this.config.elmclslim) { return true; }
                for (int i = 0; i < c.lits().size(); i++) { if (occs(c.lits().get(i)).count() >= this.config.elmocclim) { return true; } }
            }
        }
        return false;
    }

    /**
     * (Bounded) Variable Elimination = (B)VE = Clause Distribution = DP.
     * <p>
     * This is the same bounded variable elimination technique as in the SatElite paper except that it performs
     * subsumption and self-subsuming resolution on-the-fly while trying to eliminate a candidate variable.  It is also
     * limited by the number of resolution steps.
     */
    private void elim() {
        final long steps = this.limits.simpSteps;
        assert this.level == 0;
        assert !this.config.plain;
        if (!this.config.elim) { return; }
        assert this.simplifier == Simplifier.TOPSIMP;
        this.simplifier = Simplifier.ELIM;
        updateCands();
        final long limit;
        if (this.stats.simplifications <= this.config.elmrtc) { limit = Long.MAX_VALUE; } else {
            limit = this.stats.steps + 10 * steps / sizePenalty();
        }
        while (this.empty == null && !this.candsElim.empty() && this.stats.steps++ < limit) {
            final int cand = this.candsElim.top();
            final long priority = this.candsElim.priority(cand);
            this.candsElim.pop(cand);
            if (priority == 0 || !var(cand).free() || donotelim(cand)) { continue; }
            backward(cand);
            backward(-cand);
            if (tryElim(cand)) { doElim(cand); }
        }
        assert this.schedule;
        this.schedule = false;
        assert this.simplifier == Simplifier.ELIM;
        this.simplifier = Simplifier.TOPSIMP;
        dumpEliminatedRedundant();
    }

    /**
     * Returns {@code true} if all resolvents of a given clause and a blocking literal are tautological, {@code false} otherwise.
     * @param c    the clause
     * @param blit the blocking literal
     * @return {@code true} if all resolvents with 'c' on 'blit' are tautological
     */
    private boolean blockClause(final CLClause c, final int blit) {
        if (c.dumped() || satisfied(c)) { return false; }
        final CLOccs os = occs(-blit);
        for (final CLClause d : os) {
            assert !d.redundant();
            this.stats.steps++;
            if (d.dumped() || satisfied(d)) { continue; }
            if (tryResolve(c, blit, d)) { return false; }
        }
        return true;
    }

    /**
     * Finds and removes all blocked clauses blocked on a given blocking literal.
     * @param blit the blocking literal
     */
    private void blockLit(final int blit) {
        final CLOccs os = occs(blit);
        for (final CLClause c : os) {
            assert !c.redundant();
            if (!blockClause(c, blit)) { continue; }
            this.stats.clausesBlocked++;
            pushExtension(c, blit);
            dumpClause(c);
        }
    }

    /**
     * Blocked Clause Elimination.
     */
    private void block() {
        final long steps = this.limits.simpSteps;
        assert this.level == 0;
        assert !this.config.plain;
        if (!this.config.block) { return; }
        if (this.config.blkwait >= this.stats.simplifications) { return; }
        assert this.simplifier == Simplifier.TOPSIMP;
        this.simplifier = Simplifier.BLOCK;
        updateCands();
        final long limit;
        if (this.stats.simplifications <= this.config.blkrtc) { limit = Long.MAX_VALUE; } else { limit = this.stats.steps + 10 * steps / sizePenalty(); }
        while (this.empty == null && !this.candsBlock.empty() && this.stats.steps++ < limit) {
            final int cand = this.candsBlock.top();
            final long priority = this.candsBlock.priority(cand);
            this.candsBlock.pop(cand);
            if (priority == 0 || !var(cand).free()) { continue; }
            blockLit(cand);
            blockLit(-cand);
        }
        assert this.schedule;
        this.schedule = false;
        assert this.simplifier == Simplifier.BLOCK;
        this.simplifier = Simplifier.TOPSIMP;
    }

    /**
     * Disconnects all clauses.
     */
    private void disconnectClauses() {
        this.limits.reduceImportant = 0;
        for (int idx = 1; idx <= maxvar(); idx++) {
            for (int sign = -1; sign <= 1; sign += 2) {
                final int lit = sign * idx;
                watches(lit).release();
                occs(lit).release();
            }
        }
        this.dense = false;
    }

    /**
     * Removes false literals from a given clause.
     * @param c the clause
     * @return the new clause
     */
    private CLClause reduceClause(final CLClause c) {
        int lit;
        int i;
        for (i = 0; i < c.lits().size(); i++) {
            lit = c.lits().get(i);
            if (val(lit) < 0) { break; }
        }
        if (i == c.lits().size()) { return c; }
        assert this.addedlits.empty();
        for (i = 0; i < c.lits().size(); i++) {
            lit = c.lits().get(i);
            if (val(lit) >= 0) { this.addedlits.push(lit); }
        }
        final boolean redundant = c.redundant();
        final int glue = c.glue();
        deleteClause(c);
        final CLClause res = newClause(redundant, glue);
        this.addedlits.clear();
        return res;
    }

    /**
     * Collect garbage (dumped) clauses.
     */
    private void collectClauses() {
        assert this.level == 0;
        int i;
        int j = 0;
        long collected = 0;
        for (i = j; i < this.clauses.size(); i++) {
            if (i == this.distilled) { this.distilled = j; }
            final CLClause c = this.clauses.get(i);
            if (c.forcing() || !(c.dumped() || satisfied(c))) { this.clauses.set(j++, reduceClause(c)); } else {
                deleteClause(c);
                collected++;
            }
        }
        if (i == this.distilled) { this.distilled = j; }
        this.clauses.shrinkTo(j);
        this.stats.clausesCollected += collected;
    }

    /**
     * Connects all clauses.
     */
    private void connectClauses() {
        assert !this.dense;
        for (final CLClause c : this.clauses) { connectClause(c); }
    }

    /**
     * Garbage collection.
     */
    private void collect() {
        disconnectClauses();
        collectClauses();
        connectClauses();
    }

    /**
     * Performs bounded inprocessing.
     * @return the status after simplification
     */
    private Tristate simplify() {
        assert this.simplifier == Simplifier.NOSIMP;
        this.simplifier = Simplifier.TOPSIMP;
        this.stats.simplifications++;
        backtrack();
        final int varsBefore = remainingVars();
        if (!this.config.plain) {
            if (this.empty == null) { distill(); }
            if (this.empty == null) { block(); }
            if (this.empty == null) { elim(); }
        }
        collect();
        assert this.simplifier == Simplifier.TOPSIMP;
        this.simplifier = Simplifier.NOSIMP;
        final int varsAfter = remainingVars();
        assert varsBefore >= varsAfter;
        this.limits.simpRemovedVars = varsBefore - varsAfter;
        return this.empty != null ? FALSE : UNDEF;
    }

    /**
     * Extends a partial to a full assignment.
     */
    private void extend() {
        while (!this.extension.empty()) {
            final int lit = this.extension.back();
            int other;
            boolean satisfied = false;
            while ((other = this.extension.back()) != 0) {
                this.extension.pop();
                if (val(other) == VALUE_TRUE) { satisfied = true; }
            }
            this.extension.pop();
            if (!satisfied) { this.vals.set(Math.abs(lit), sign(lit)); }
        }
    }

    /**
     * Returns {@code true} if each variable occurs only once in the current clause, {@code false} otherwise.
     * @return {@code true} if each variable occurs only once in the current clause
     */
    private boolean eachVariableOccursOnlyOnce() {
        int lit;
        for (int i = 0; i < this.addedlits.size(); i++) {
            lit = this.addedlits.get(i);
            assert marked(lit) == 0;
            assert marked(-lit) == 0;
        }
        boolean res = true;
        for (int i = 0; i < this.addedlits.size(); i++) {
            lit = this.addedlits.get(i);
            if (marked(lit) != 0 || marked(-lit) != 0) { res = false; } else { mark(lit); }
        }
        unmark();
        return res;
    }
}

