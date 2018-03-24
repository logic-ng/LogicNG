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

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

/**
 * A complete Reimplementation of the CleaneLing solver.
 * @version 1.3
 * @since 1.0
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
  public void addlit(int lit) {
    original.push(lit);
    if (lit != 0) {
      importLit(lit);
      addedlits.push(lit);
    } else {
      if (!trivialClause())
        newPushConnectClause();
      addedlits.clear();
    }
  }

  @Override
  public Tristate solve(final SATHandler handler) {
    this.handler = handler;
    if (this.handler != null)
      this.handler.startedSolving();
    model.clear();
    initLimits();
    biasPhases();
    Tristate res;
    while (true)
      if ((res = search()) != UNDEF || this.canceledByHandler)
        break;
      else if ((res = simplify()) != UNDEF || this.canceledByHandler)
        break;
      else updateLimits();
    switch (res) {
      case TRUE:
        extend();
        break;
      case FALSE:
        break;
      default:
        break;
    }
    if (res == TRUE)
      for (int i = 0; i < this.vals.size(); i++)
        model.push(this.vals.get(i) == VALUE_TRUE);
    if (this.handler != null)
      this.handler.finishedSolving();
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
  protected void importLit(int lit) {
    int idx = Math.abs(lit);
    assert lit != 0;
    int newIdx;
    while (idx >= (newIdx = vars.size())) {
      vars.push(new CLVar());
      vals.push((byte) 0);
      phases.push((byte) 0);
      watches.push(new LNGVector<CLWatch>());
      watches.push(new LNGVector<CLWatch>());
      occs.push(new CLOccs[]{new CLOccs(), new CLOccs()});
      if (newIdx == 0)
        continue;
      decisions.push(newIdx);
    }
  }

  @Override
  protected void newPushConnectClause(boolean redundant, int glue) {
    final CLClause c = newClause(redundant, glue);
    clauses.push(c);
    connectClause(c);
  }

  @Override
  protected void assign(int lit, final CLClause reason) {
    final CLVar v = var(lit);
    assert val(lit) == VALUE_UNASSIGNED;
    int idx = Math.abs(lit);
    final byte s = sign(lit);
    vals.set(idx, s);
    if (simplifier == Simplifier.NOSIMP)
      phases.set(idx, s);
    v.setLevel(level);
    if (level == 0) {
      stats.varsFixed++;
      if (v.state() == CLVar.State.ELIMINATED) {
        assert stats.varsEliminated > 0;
        stats.varsEliminated--;
      } else
        assert v.state() == CLVar.State.FREE;
      v.setState(CLVar.State.FIXED);
    }
    trail.push(lit);
    v.setReason(reason);
    if (v.reason() != null) {
      assert !reason.forcing();
      reason.setForcing(true);
      if (reason.redundant() && !reason.important())
        limits.reduceForcing++;
      updateGlue(reason);
    }
  }

  @Override
  protected void unassign(int lit) {
    assert level > 0;
    final CLClause reason;
    final CLVar v = var(lit);
    vals.set(Math.abs(lit), VALUE_UNASSIGNED);
    assert v.level() == level;
    v.setLevel(Integer.MAX_VALUE);
    reason = v.reason();
    if (reason != null) {
      assert reason.forcing();
      reason.setForcing(false);
      if (reason.redundant() && !reason.important()) {
        assert limits.reduceForcing > 0;
        limits.reduceForcing--;
      }
    }
    final int idx = Math.abs(lit);
    if (!decisions.contains(idx))
      decisions.push(idx);
  }

  @Override
  protected void initLimits() {
    newRestartLimit();
    if (limits.simpSteps == 0)
      limits.simpSteps = config.simpint;
    if (stats.simplifications == 0) {
      assert config.boost > 0;
      if (limits.simpSteps >= Integer.MAX_VALUE / config.boost)
        limits.simpSteps = Integer.MAX_VALUE;
      else limits.simpSteps *= config.boost;
    }
    if (limits.searchConflicts == 0 && config.searchfirst)
      limits.searchConflicts = config.searchint;
    limits.simpRemovedVars = 0;
  }

  @Override
  protected void updateLimits() {
    if (config.simpgeom) {
      if (limits.simpInc >= Integer.MAX_VALUE / 2)
        limits.simpInc = Integer.MAX_VALUE;
      else if (limits.simpInc == 0)
        limits.simpInc = config.simpint;
      else limits.simpInc *= 2;
    } else {
      if (limits.simpInc >= Integer.MAX_VALUE - config.simpint)
        limits.simpInc = Integer.MAX_VALUE;
      else limits.simpInc += config.simpint;
    }
    limits.simpSteps = limits.simpInc;
    if (config.stepslim != 0 && limits.simpSteps > config.stepslim)
      limits.simpSteps = config.stepslim;

    if (limits.searchInc == 0)
      limits.searchInc = config.searchint;
    assert limits.searchInc != 0;
    if (limits.searchConflicts != 0) {
      int inc = limits.searchInc;
      long removed = limits.simpRemovedVars;
      if (removed > 0 && remainingVars() != 0) {
        long reduction = (100 * removed) / remainingVars();
        if (reduction > 1)
          inc /= reduction;
      }
      if (limits.searchInc >= Integer.MAX_VALUE - inc)
        limits.searchInc = Integer.MAX_VALUE;
      else
        limits.searchInc += inc;
    } else
      assert !config.searchfirst;
    limits.searchConflicts = limits.searchInc;
  }

  @Override
  protected CLClause newClause(boolean redundant, int glue) {
    CLClause c = new CLClause();
    assert glue == 0 || redundant;
    assert eachVariableOccursOnlyOnce();
    if (config.gluered)
      c.setGlue(glue);
    else
      assert c.glue() == 0;
    c.setImportant(glue <= config.gluekeep);
    c.setRedundant(redundant);
    c.setActivity(stats.conflicts);
    for (int i = 0; i < addedlits.size(); i++)
      c.lits().push(addedlits.get(i));
    if (redundant)
      stats.clausesRedundant++;
    else
      stats.clausesIrredundant++;
    return c;
  }

  @Override
  protected void connectClause(final CLClause c) {
    if (c.satisfied())
      return;

    int size = c.size();
    boolean binary = size == 2;
    for (int p = 0; p < 2; p++)
      for (int q = p + 1; q < c.lits().size(); q++) {
        int lit = c.lits().get(q);
        int litP = c.lits().get(p);
        int cmp = var(lit).level() - var(litP).level();
        if (cmp > 0 || (cmp == 0 && (val(lit) > val(litP)))) {
          c.lits().set(p, lit);
          c.lits().set(q, litP);
        }
      }
    int l0 = c.lits().get(0);
    int l1 = l0 != 0 ? c.lits().get(1) : 0;
    int newLevel = (l0 != 0 && l1 != 0) ? Math.min(var(l0).level(), var(l1).level()) : 0;
    if (newLevel != Integer.MAX_VALUE)
      backtrack(newLevel);
    if (size >= 2) {
      addWatch(l0, l1, binary, c);
      addWatch(l1, l0, binary, c);
      if (dense)
        connectOccs(c);
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
        if (lit != 0)
          ignore = true;
        else
          lit = other;
      }
    }
    if (!ignore) {
      if (lit == 0) {
        assert level == 0;
        if (empty == null)
          empty = c;
      } else assign(lit, c);
    }
    if (c.redundant() && c.important())
      limits.reduceImportant++;
  }

  @Override
  protected CLClause bcp() {
    int visits = 0;
    int propagations = 0;
    CLClause conflict = empty;
    while (conflict == null && next < trail.size()) {
      propagations++;
      int lit = -trail.get(next++);
      stats.steps++;
      LNGVector<CLWatch> ws = watches(lit);
      LNGVector<CLWatch> newWS = new LNGVector<>();
      int i;
      for (i = 0; conflict == null && i < ws.size(); i++) {
        CLWatch w = ws.get(i);
        newWS.push(w);
        int other = w.blit();
        byte v = val(other);
        if (v == VALUE_TRUE)
          continue;
        CLClause clause = w.clause();
        if (ignore != null) {
          if (ignore == clause)
            continue;
          if (clause.redundant()) {
            if (!w.binary())
              visits++;
            continue;
          }
        }
        if (w.binary()) {
          if (v == VALUE_FALSE)
            conflict = clause;
          else
            assign(other, clause);
        } else {
          visits++;
          if (clause.dumped()) {
            newWS.pop();
            continue;
          }
          int p;
          if (clause.lits().get(0) == lit) {
            int temp = clause.lits().get(0);
            clause.lits().set(0, clause.lits().get(1));
            clause.lits().set(1, temp);
          }
          assert clause.lits().get(1) == lit;
          for (p = 2; p < clause.lits().size(); p++) {
            other = clause.lits().get(p);
            if (val(other) >= 0)
              break;
          }
          if (p == clause.size())
            other = 0;
          if (other != 0) {
            clause.lits().set(p, lit);
            clause.lits().set(1, other);
            addWatch(other, clause.lits().get(0), false, clause);
            newWS.pop();
          } else {
            other = clause.lits().get(0);
            v = val(other);
            if (v == VALUE_FALSE)
              conflict = clause;
            else if (v != VALUE_TRUE)
              assign(other, clause);
            else
              newWS.back().setBlit(other);
          }
        }
      }
      if (conflict != null)
        while (i < ws.size())
          newWS.push(ws.get(i++));
      ws.replaceInplace(newWS);
    }
    if (conflict != null && simplifier == Simplifier.NOSIMP)
      stats.conflicts++;
    stats.propagations += propagations;
    stats.steps += visits;
    return conflict;
  }

  @Override
  protected void minimizeClause() {
    int learned = addedlits.size();
    stats.litsLearned += learned;
    LNGIntVector newAddedLits = new LNGIntVector(addedlits.size());
    for (int i = 0; i < addedlits.size(); i++) {
      final int lit = addedlits.get(i);
      if (!minimizeLit(-lit))
        newAddedLits.push(lit);
    }
    addedlits = newAddedLits;
    stats.litsMinimized += learned - addedlits.size();
  }

  @Override
  protected void analyze(final CLClause r) {
    CLClause reason = r;
    if (empty != null) {
      assert level == 0;
      return;
    }
    updateGlue(reason);
    assert addedlits.empty();
    int lit = 0;
    int open = 0;
    int it = trail.size();
    while (true) {
      bumpClause(reason);
      for (int p = 0; p < reason.lits().size(); p++) {
        lit = reason.lits().get(p);
        if (pullLit(lit))
          open++;
      }
      while (it > 0 && marked(lit = -trail.get(--it)) == 0)
        assert var(lit).level() == level;
      if (it == 0 || --open == 0)
        break;
      reason = var(lit).reason();
      assert reason != null;
    }

    assert lit != 0;
    addedlits.push(lit);
    minimizeClause();
    unmark();
    int glue = unmarkFrames();
    stats.gluesCount++;
    stats.gluesSum += glue;
    stats.sizes += addedlits.size();
    newPushConnectClause(true, glue);
    addedlits.clear();
    scoreIncrement *= config.scincfact / 1000.0;
    if (simplifier == Simplifier.NOSIMP && level == 0 && empty == null) {
      limits.searchConflicts += config.itsimpdel;
      stats.iterations++;
    }
  }

  @Override
  protected boolean restarting() {
    return config.restart && stats.conflicts >= limits.restart;
  }

  @Override
  protected void restart() {
    stats.restartsCount++;
    int nextDecision = 0;
    while (nextDecision == 0 && !decisions.empty()) {
      int lit = decisions.top();
      if (val(lit) != 0)
        decisions.pop(lit);
      else nextDecision = lit;
    }
    if (nextDecision != 0) {
      int newLevel;
      if (config.reusetrail) {
        double nextDecisionPriority = decisions.priority(nextDecision);
        for (newLevel = 0; newLevel < level; newLevel++) {
          CLFrame frame = control.get(newLevel + 1);
          int decision = Math.abs(frame.decision());
          if (decisions.priority(decision) < nextDecisionPriority)
            break;
        }
        if (newLevel != 0) {
          stats.restartsReuseCount++;
          stats.restartsReuseSum += (100 * newLevel) / level;
        }
      } else
        newLevel = 0; // Do not reuse trail.  Back track to the top.
      backtrack(newLevel);
    }
    newRestartLimit();
  }

  @Override
  protected Tristate search() {
    long conflicts = 0;
    CLClause conflict;
    Tristate res = UNDEF;
    while (res == UNDEF)
      if (empty != null)
        res = FALSE;
      else if ((conflict = bcp()) != null) {
        if (handler != null && !handler.detectedConflict()) {
          canceledByHandler = true;
          return UNDEF;
        }
        analyze(conflict);
        conflicts++;
      } else if (conflicts >= limits.searchConflicts)
        break;
      else if (reducing())
        reduce();
      else if (restarting())
        restart();
      else if (!decide())
        res = TRUE;
    return res;
  }

  /**
   * Initializes the initial phases of literals.
   */
  private void biasPhases() {
    final double[] score = new double[2 * (maxvar() + 1)];
    for (final CLClause c : clauses) {
      if (c.redundant() || c.satisfied())
        continue;
      double inc = 1;
      for (int size = c.size(); size > 0; size--)
        inc /= 2.0;
      for (int i = 0; i < c.lits().size(); i++) {
        final int p = c.lits().get(i);
        if (p > 0)
          score[p * 2] += inc;
        else
          score[(-p * 2) - 1] += inc;
      }
    }
    for (int idx = 1; idx <= maxvar(); idx++)
      phases.set(idx, (score[idx * 2] > score[(idx * 2) - 1]) ? (byte) 1 : (byte) -1);
  }

  /**
   * Returns the occurrence list for a given literal.
   * @param lit the literal
   * @return the occurrence list for the literal
   */
  private CLOccs occs(int lit) {
    return occs.get(Math.abs(lit))[lit < 0 ? 0 : 1];
  }

  /**
   * Updates and pushes a literal as candidate to simplify.
   * @param l the literal
   */
  private void touch(int l) {
    int lit = l;
    if (lit < 0)
      lit = -lit;
    long newPriority = (long) occs(lit).count() + occs(-lit).count();
    if (!var(lit).free())
      return;
    final LNGLongPriorityQueue queue = currentCands();
    queue.update(lit, -newPriority);
    if (schedule && !queue.contains(lit))
      queue.push(lit);
  }

  /**
   * Returns a list of candidates to simplify depending on the current simplifier.
   * @return the list of candidates (literals)
   */
  private LNGLongPriorityQueue currentCands() {
    switch (simplifier) {
      case BLOCK:
        return candsBlock;
      default:
        return candsElim;
    }
  }

  /**
   * Touches literals in newly top-level satisfied clauses using the literals not touched yet.
   */
  private void touchFixed() {
    assert dense;
    assert level == 0;
    assert schedule;
    while (touched < trail.size()) {
      int lit = trail.get(touched++);
      assert val(lit) > 0;
      assert var(lit).level() == 0;
      final CLOccs os = occs(lit);
      for (final CLClause c : os) {
        for (int i = 0; i < c.lits().size(); i++) {
          final int other = c.lits().get(i);
          if (val(other) == VALUE_TRUE)
            continue;
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
  private void addOcc(int lit, final CLClause c) {
    assert dense;
    occs(lit).add(c);
    touch(lit);
  }

  /**
   * Connects a clause through a full occurrence list.
   * @param c the clause
   */
  private void connectOccs(final CLClause c) {
    assert dense;
    for (int i = 0; i < c.lits().size(); i++)
      addOcc(c.lits().get(i), c);
  }

  /**
   * Connects all clauses through full occurrence lists.
   */
  private void connectOccs() {
    assert !dense;
    dense = true;
    for (final CLClause c : clauses)
      if (!c.redundant())
        connectOccs(c);
  }

  /**
   * Decrements full occurrence for a literal.
   * @param lit the literal
   */
  private void decOcc(int lit) {
    assert dense;
    occs(lit).dec();
    touch(lit);
  }

  /**
   * Updates the glue value for a given clause.
   * @param c the clause
   */
  private void updateGlue(final CLClause c) {
    if (!config.glueupdate)
      return;
    if (!config.gluered) {
      assert c.glue() == 0;
      return;
    }
    assert frames.empty();
    for (int i = 0; i < c.lits().size(); i++)
      markFrame(c.lits().get(i));
    int newGlue = unmarkFrames();
    if (newGlue >= c.glue())
      return;
    c.setGlue(newGlue);
    stats.gluesSum += newGlue;
    stats.gluesCount++;
    stats.gluesUpdates++;
  }

  /**
   * Dumps a given clause.
   * @param c the clause
   */
  private void dumpClause(final CLClause c) {
    if (c.dumped())
      return;
    if (c.redundant()) {
      assert stats.clausesRedundant > 0;
      stats.clausesRedundant--;
    } else {
      assert stats.clausesIrredundant > 0;
      stats.clausesIrredundant--;
      if (dense)
        for (int i = 0; i < c.lits().size(); i++)
          decOcc(c.lits().get(i));
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
    switch (config.cbump) {
      case INC:
        c.setActivity(c.activity() + 1);
        break;
      case LRU:
        c.setActivity(stats.conflicts);
        break;
      case AVG:
        c.setActivity((stats.conflicts + c.activity()) / 2);
        break;
      case NONE:
      default:
        assert config.cbump == CleaneLingConfig.ClauseBumping.NONE;
        break;
    }
  }

  /**
   * Returns {@code true} if a reduction should be performed, {@code false} otherwise.
   * @return {@code true} if a reduction should be performed
   */
  private boolean reducing() {
    if (limits.reduceRedundant == 0)
      limits.reduceRedundant = config.redinit;
    long limit = limits.reduceRedundant;
    limit += limits.reduceForcing;
    limit += limits.reduceImportant;
    return limit <= stats.clausesRedundant;
  }

  /**
   * Reduces the number of redundant clauses.
   */
  private void reduce() {
    stats.reductions++;
    LNGVector<CLClause> candidates = new LNGVector<>();
    for (final CLClause c : clauses)
      if (c.redundant() && !c.important() && !c.forcing())
        candidates.push(c);
    int keep = candidates.size() / 2;
    candidates.sort(CLClause.comp);
    for (int i = keep; i < candidates.size(); i++)
      candidates.get(i).setRemove(true);
    for (int idx = 1; idx <= maxvar(); idx++)
      for (int sign = -1; sign <= 1; sign += 2) {
        LNGVector<CLWatch> ws = watches(sign * idx);
        LNGVector<CLWatch> newWs = new LNGVector<>(ws.size());
        for (final CLWatch w : ws)
          if (!w.clause().remove())
            newWs.push(w);
        ws.replaceInplace(newWs);
      }
    int j = 0;
    int i;
    for (i = 0; i < clauses.size(); i++) {
      final CLClause c = clauses.get(i);
      if (i == distilled)
        distilled = j;
      if (!c.remove())
        clauses.set(j++, c);
    }
    if (i == distilled)
      distilled = j;
    clauses.shrinkTo(j);
    long reduced = 0;
    for (int k = keep; k < candidates.size(); k++) {
      deleteClause(candidates.get(k));
      reduced++;
    }
    stats.clausesReduced += reduced;
    candidates.release();
    limits.reduceRedundant += config.redinc;
  }

  /**
   * Returns the number of remaining variables without fixed and eliminated.
   * @return the number of remaining variables
   */
  private int remainingVars() {
    int res = maxvar();
    res -= stats.varsFixed;
    res -= stats.varsEquivalent;
    res -= stats.varsEliminated;
    return res;
  }

  /**
   * Removes a literal in a clause.
   * @param c      the clause
   * @param remove the literal to remove
   */
  private void strengthen(final CLClause c, int remove) {
    if (c.dumped() || satisfied(c))
      return;
    assert addedlits.empty();
    for (int i = 0; i < c.lits().size(); i++) {
      final int lit = c.lits().get(i);
      if (lit != remove && val(lit) == 0)
        addedlits.push(lit);
    }
    newPushConnectClause();
    addedlits.clear();
    dumpClause(c);
  }

  /**
   * Returns {@code true} if a given clause is satisfied, {@code false} otherwise.
   * @param c the clause
   * @return {@code true} if a given clause is satisfied
   */
  private boolean satisfied(final CLClause c) {
    if (c.satisfied())
      return true;
    for (int i = 0; i < c.lits().size(); i++)
      if (val(c.lits().get(i)) == VALUE_TRUE) {
        if (level == 0)
          c.setSatisfied(true);
        return true;
      }
    return false;
  }

  /**
   * Reduces simplification steps for large formulas.
   * @return the size penalty
   */
  private int sizePenalty() {
    long numClauses = (long) stats.clausesIrredundant / config.sizepen;
    int logres = 0;
    while (numClauses != 0 && logres < config.sizemaxpen) {
      numClauses >>= 1;
      logres++;
    }
    return 1 << logres;
  }

  /**
   * Distills units and subsumes/strengthens clauses.
   */
  private void distill() {
    long steps = limits.simpSteps;
    assert level == 0;
    assert !config.plain;
    if (!config.distill)
      return;
    long limit = stats.steps + steps / sizePenalty();
    boolean changed = false;
    boolean done = clauses.empty();
    if (distilled >= clauses.size())
      distilled = 0;
    while (empty == null && !done && stats.steps++ < limit) {
      CLClause c = clauses.get(distilled);
      if (!c.redundant() && !c.dumped() && c.large() && !satisfied(c)) {
        CLClause conflict = null;
        assert ignore == null;
        ignore = c;
        int lit;
        for (int p = 0; conflict == null && p < c.lits().size(); p++) {
          lit = c.lits().get(p);
          byte v = val(lit);
          if (v == VALUE_FALSE)
            continue;
          if (v == VALUE_TRUE) {
            stats.distillStrengthened++;
            strengthen(c, lit);
            changed = true;
            break;
          }
          assume(-lit);
          conflict = bcp();
        }
        ignore = null;
        if (conflict != null) {
          analyze(conflict);

          if (level == 0) {
            assert next < trail.size();
            stats.distillUnits++;
            changed = true;
            conflict = bcp();
            if (conflict != null) {
              analyze(conflict);
              assert empty != null;
            }
          } else {
            stats.distillSubsumed++;
            dumpClause(c);
          }
        }
        backtrack();
      }
      if (++distilled == clauses.size()) {
        if (changed)
          changed = false;
        else done = true;
        distilled = 0;
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
  private boolean tryResolve(CLClause c, int pivot, CLClause d) {
    assert !c.dumped() && !c.satisfied();
    assert !d.dumped() && !d.satisfied();
    boolean res = true;
    assert seen.empty();
    stats.steps++;
    for (int i = 0; i < c.lits().size(); i++) {
      final int lit = c.lits().get(i);
      if (lit == pivot)
        continue;
      assert marked(lit) == 0;
      mark(lit);
    }
    stats.steps++;
    for (int p = 0; res && p < d.lits().size(); p++) {
      final int lit = d.lits().get(p);
      if (lit == -pivot)
        continue;
      int m = marked(lit);
      if (m > 0)
        continue;
      if (m < 0)
        res = false;
      else
        mark(lit);
    }
    unmark();
    return res;
  }

  /**
   * Backward subsumes from clause.
   * @param c      the clause
   * @param ignore the literal to ignore
   */
  private void backward(CLClause c, int ignore) {
    int minlit = 0;
    int minoccs = Integer.MAX_VALUE;
    int litoccs;
    stats.steps++;
    for (int i = 0; i < c.lits().size(); i++) {
      final int lit = c.lits().get(i);
      if (lit == ignore)
        continue;
      if (val(lit) < 0)
        continue;
      litoccs = occs(lit).count();
      if (minlit != 0 && minoccs >= litoccs)
        continue;
      minlit = lit;
      minoccs = litoccs;
    }
    if (minoccs >= config.bwocclim)
      return;
    assert minlit != 0;
    for (int i = 0; i < c.lits().size(); i++)
      mark(c.lits().get(i));
    CLOccs os = occs(minlit);
    for (final CLClause d : os) {
      if (d == c)
        continue;
      int lit;
      int count = seen.size();
      int negated = 0;
      stats.steps++;
      for (int p = 0; count != 0 && p < d.lits().size(); p++) {
        lit = d.lits().get(p);
        int m = marked(lit);
        if (m == 0)
          continue;
        assert count > 0;
        count--;
        if (m > 0)
          continue;
        assert m < 0;
        if (negated != 0) {
          count = Integer.MAX_VALUE;
          break;
        }
        negated = lit;
      }
      if (count != 0)
        continue;
      if (negated != 0) {
        tostrengthen.push(new Pair<>(d, negated));
        stats.backwardStrengthened++;
      } else {
        stats.backwardSubsumed++;
        dumpClause(d);
      }
    }
    unmark();
  }

  /**
   * Backward subsumes from clauses in the occurrence list of a given literal.
   * @param lit the literal
   */
  private void backward(int lit) {
    assert level == 0;
    assert dense;
    assert tostrengthen.empty();
    CLOccs os = occs(lit);
    for (final CLClause c : os) {
      assert !c.redundant();
      stats.steps++;
      if (c.dumped())
        continue;
      if (c.size() >= config.bwclslim)
        continue;
      if (satisfied(c))
        continue;
      backward(c, lit);
    }
    while (!tostrengthen.empty()) {
      Pair<CLClause, Integer> cplp = tostrengthen.back();
      tostrengthen.pop();
      strengthen(cplp.first(), cplp.second());
    }
  }

  /**
   * Tries bounded variable elimination on a candidate literal.
   * @param cand the literal
   * @return {@code true} if resolution of all positive with all negative occurrences of the candidate produces at most
   * as many non trivial resolvents as the number of positive plus negative occurrences, {@code false} otherwise
   */
  private boolean tryElim(int cand) {
    assert var(cand).free();
    CLOccs p = occs(cand);
    CLOccs n = occs(-cand);
    long limit = (long) p.count() + n.count();
    for (int i = 0; limit >= 0 && i < p.clauses().size(); i++) {
      CLClause c = p.clauses().get(i);
      assert !c.redundant();
      stats.steps++;
      if (c.dumped() || satisfied(c))
        continue;
      for (int j = 0; limit >= 0 && j < n.clauses().size(); j++) {
        CLClause d = n.clauses().get(j);
        assert !d.redundant();
        stats.steps++;
        if (d.dumped() || satisfied(d))
          continue;
        if (tryResolve(c, cand, d))
          limit--;
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
  private void doResolve(CLClause c, int pivot, CLClause d) {
    assert !c.dumped() && !c.satisfied();
    assert !d.dumped() && !d.satisfied();
    assert addedlits.empty();
    stats.steps++;
    for (int i = 0; i < c.lits().size(); i++) {
      final int lit = c.lits().get(i);
      if (lit != pivot)
        addedlits.push(lit);
    }
    stats.steps++;
    for (int i = 0; i < d.lits().size(); i++) {
      final int lit = d.lits().get(i);
      if (lit != -pivot)
        addedlits.push(lit);
    }
    if (!trivialClause())
      newPushConnectClause();
    addedlits.clear();
  }

  /**
   * Pushes and logs a literal to the extension.
   * @param lit the literal
   */
  private void pushExtension(int lit) {
    extension.push(lit);
  }

  /**
   * Pushes and logs a clause and its blocking literal to the extension.
   * @param c    the clause
   * @param blit the blocking literal
   */
  private void pushExtension(CLClause c, int blit) {
    pushExtension(0);
    for (int i = 0; i < c.lits().size(); i++) {
      final int lit = c.lits().get(i);
      if (lit != blit)
        pushExtension(lit);
    }
    pushExtension(blit);
  }

  /**
   * Performs blocking variable elimination on a candidate.
   * @param cand the candidate literal
   */
  private void doElim(int cand) {
    assert schedule;
    assert var(cand).free();
    CLOccs p = occs(cand);
    CLOccs n = occs(-cand);
    for (final CLClause c : p) {
      stats.steps++;
      if (c.dumped() || satisfied(c))
        continue;
      for (final CLClause d : n) {
        stats.steps++;
        if (d.dumped() || satisfied(d))
          continue;
        doResolve(c, cand, d);
      }
    }
    int extend;
    CLOccs e;
    if (p.count() < n.count()) {
      extend = cand;
      e = p;
    } else {
      extend = -cand;
      e = n;
    }
    for (final CLClause c : e) {
      if (c.dumped() || satisfied(c))
        continue;
      stats.steps++;
      pushExtension(c, extend);
    }
    pushExtension(0);
    pushExtension(-extend);
    while (!p.clauses().empty()) {
      CLClause c = p.clauses().back();
      stats.steps++;
      p.clauses().pop();
      if (c.satisfied() || c.dumped())
        continue;
      stats.clausesEliminated++;
      dumpClause(c);
    }
    p.clauses().release();
    while (!n.clauses().empty()) {
      CLClause c = n.clauses().back();
      stats.steps++;
      n.clauses().pop();
      if (c.satisfied() || c.dumped())
        continue;
      stats.clausesEliminated++;
      dumpClause(c);
    }
    n.clauses().release();
    var(cand).setState(CLVar.State.ELIMINATED);
    stats.varsEliminated++;
    CLClause conflict = bcp();
    if (conflict != null) {
      analyze(conflict);
      assert empty != null;
    }
    touchFixed();
  }

  /**
   * Returns {@code true} if a given clause contains an eliminated variable, {@code false} otherwise.
   * @param c the clause
   * @return {@code true} if a given clause contains an eliminated variable
   */
  private boolean containsEliminated(CLClause c) {
    for (int i = 0; i < c.lits().size(); i++)
      if (var(c.lits().get(i)).state() == CLVar.State.ELIMINATED)
        return true;
    return false;
  }

  /**
   * Dumps redundant clauses with eliminated variables.
   */
  private void dumpEliminatedRedundant() {
    for (final CLClause c : clauses) {
      if (!c.redundant() || c.satisfied() || c.dumped())
        continue;
      if (containsEliminated(c))
        dumpClause(c);
    }
  }

  /**
   * Updates the candidates priority queue for new rounds of simplification.
   */
  private void updateCands() {
    if (!dense)
      connectOccs();
    assert !schedule;
    schedule = true;
    if (schedule = currentCands().empty())
      for (int idx = 1; idx <= maxvar(); idx++)
        touch(idx);
    schedule = true;
    touchFixed();
  }

  /**
   * Returns {@code true} if a given candidate literal should not be eliminated, {@code false} otherwise.
   * @param cand the candidate literal
   * @return {@code true} if a given candidate literal should not be eliminated
   */
  private boolean donotelim(int cand) {
    int sign;
    if (occs(cand).count() > config.elmpocclim1)
      return true;
    if (occs(-cand).count() > config.elmpocclim1)
      return true;
    if (occs(cand).count() > config.elmpocclim2 && occs(-cand).count() > config.elmpocclim2)
      return true;
    for (sign = -1; sign <= 1; sign += 2) {
      CLOccs os = occs(sign * cand);
      for (final CLClause c : os) {
        assert !c.redundant();
        if (c.size() >= config.elmclslim)
          return true;
        for (int i = 0; i < c.lits().size(); i++)
          if (occs(c.lits().get(i)).count() >= config.elmocclim)
            return true;
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
    long steps = limits.simpSteps;
    assert level == 0;
    assert !config.plain;
    if (!config.elim)
      return;
    assert simplifier == Simplifier.TOPSIMP;
    simplifier = Simplifier.ELIM;
    updateCands();
    long limit;
    if (stats.simplifications <= config.elmrtc)
      limit = Long.MAX_VALUE;
    else {
      limit = stats.steps + 10 * steps / sizePenalty();
    }
    while (empty == null && !candsElim.empty() && stats.steps++ < limit) {
      int cand = candsElim.top();
      long priority = candsElim.priority(cand);
      candsElim.pop(cand);
      if (priority == 0 || !var(cand).free() || donotelim(cand))
        continue;
      backward(cand);
      backward(-cand);
      if (tryElim(cand))
        doElim(cand);
    }
    assert schedule;
    schedule = false;
    assert simplifier == Simplifier.ELIM;
    simplifier = Simplifier.TOPSIMP;
    dumpEliminatedRedundant();
  }

  /**
   * Returns {@code true} if all resolvents of a given clause and a blocking literal are tautological, {@code false} otherwise.
   * @param c    the clause
   * @param blit the blocking literal
   * @return {@code true} if all resolvents with 'c' on 'blit' are tautological
   */
  private boolean blockClause(CLClause c, int blit) {
    if (c.dumped() || satisfied(c))
      return false;
    CLOccs os = occs(-blit);
    for (final CLClause d : os) {
      assert !d.redundant();
      stats.steps++;
      if (d.dumped() || satisfied(d))
        continue;
      if (tryResolve(c, blit, d))
        return false;
    }
    return true;
  }

  /**
   * Finds and removes all blocked clauses blocked on a given blocking literal.
   * @param blit the blocking literal
   */
  private void blockLit(int blit) {
    CLOccs os = occs(blit);
    for (final CLClause c : os) {
      assert !c.redundant();
      if (!blockClause(c, blit))
        continue;
      stats.clausesBlocked++;
      pushExtension(c, blit);
      dumpClause(c);
    }
  }

  /**
   * Blocked Clause Elimination.
   */
  private void block() {
    long steps = limits.simpSteps;
    assert level == 0;
    assert !config.plain;
    if (!config.block)
      return;
    if (config.blkwait >= stats.simplifications)
      return;
    assert simplifier == Simplifier.TOPSIMP;
    simplifier = Simplifier.BLOCK;
    updateCands();
    long limit;
    if (stats.simplifications <= config.blkrtc)
      limit = Long.MAX_VALUE;
    else
      limit = stats.steps + 10 * steps / sizePenalty();
    while (empty == null && !candsBlock.empty() && stats.steps++ < limit) {
      int cand = candsBlock.top();
      long priority = candsBlock.priority(cand);
      candsBlock.pop(cand);
      if (priority == 0 || !var(cand).free())
        continue;
      blockLit(cand);
      blockLit(-cand);
    }
    assert schedule;
    schedule = false;
    assert simplifier == Simplifier.BLOCK;
    simplifier = Simplifier.TOPSIMP;
  }

  /**
   * Disconnects all clauses.
   */
  private void disconnectClauses() {
    limits.reduceImportant = 0;
    for (int idx = 1; idx <= maxvar(); idx++)
      for (int sign = -1; sign <= 1; sign += 2) {
        int lit = sign * idx;
        watches(lit).release();
        occs(lit).release();
      }
    dense = false;
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
      if (val(lit) < 0)
        break;
    }
    if (i == c.lits().size())
      return c;
    assert addedlits.empty();
    for (i = 0; i < c.lits().size(); i++) {
      lit = c.lits().get(i);
      if (val(lit) >= 0)
        addedlits.push(lit);
    }
    boolean redundant = c.redundant();
    int glue = c.glue();
    deleteClause(c);
    CLClause res = newClause(redundant, glue);
    addedlits.clear();
    return res;
  }

  /**
   * Collect garbage (dumped) clauses.
   */
  private void collectClauses() {
    assert level == 0;
    int i;
    int j = 0;
    long collected = 0;
    for (i = j; i < clauses.size(); i++) {
      if (i == distilled)
        distilled = j;
      CLClause c = clauses.get(i);
      if (c.forcing() || !(c.dumped() || satisfied(c)))
        clauses.set(j++, reduceClause(c));
      else {
        deleteClause(c);
        collected++;
      }
    }
    if (i == distilled)
      distilled = j;
    clauses.shrinkTo(j);
    stats.clausesCollected += collected;
  }

  /**
   * Connects all clauses.
   */
  private void connectClauses() {
    assert !dense;
    for (final CLClause c : clauses)
      connectClause(c);
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
    assert simplifier == Simplifier.NOSIMP;
    simplifier = Simplifier.TOPSIMP;
    stats.simplifications++;
    backtrack();
    int varsBefore = remainingVars();
    if (!config.plain) {
      if (empty == null)
        distill();
      if (empty == null)
        block();
      if (empty == null)
        elim();
    }
    collect();
    assert simplifier == Simplifier.TOPSIMP;
    simplifier = Simplifier.NOSIMP;
    int varsAfter = remainingVars();
    assert varsBefore >= varsAfter;
    limits.simpRemovedVars = varsBefore - varsAfter;
    return empty != null ? FALSE : UNDEF;
  }

  /**
   * Extends a partial to a full assignment.
   */
  private void extend() {
    while (!extension.empty()) {
      int lit = extension.back();
      int other;
      boolean satisfied = false;
      while ((other = extension.back()) != 0) {
        extension.pop();
        if (val(other) == VALUE_TRUE)
          satisfied = true;
      }
      extension.pop();
      if (!satisfied)
        vals.set(Math.abs(lit), sign(lit));
    }
  }

  /**
   * Returns {@code true} if each variable occurs only once in the current clause, {@code false} otherwise.
   * @return {@code true} if each variable occurs only once in the current clause
   */
  private boolean eachVariableOccursOnlyOnce() {
    int lit;
    for (int i = 0; i < addedlits.size(); i++) {
      lit = addedlits.get(i);
      assert marked(lit) == 0;
      assert marked(-lit) == 0;
    }
    boolean res = true;
    for (int i = 0; i < addedlits.size(); i++) {
      lit = addedlits.get(i);
      if (marked(lit) != 0 || marked(-lit) != 0)
        res = false;
      else
        mark(lit);
    }
    unmark();
    return res;
  }
}

