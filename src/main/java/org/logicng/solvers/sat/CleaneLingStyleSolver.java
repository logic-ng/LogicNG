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

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGByteVector;
import org.logicng.collections.LNGDoublePriorityQueue;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.datastructures.CLClause;
import org.logicng.solvers.datastructures.CLFrame;
import org.logicng.solvers.datastructures.CLVar;
import org.logicng.solvers.datastructures.CLWatch;

/**
 * The super class for all CleaneLing-style solvers.
 * @version 1.1
 * @since 1.0
 */
public abstract class CleaneLingStyleSolver {

  public static final byte VALUE_TRUE = 1;
  public static final byte VALUE_FALSE = -1;
  public static final byte VALUE_UNASSIGNED = 0;
  protected final CleaneLingConfig config;
  protected int level;
  protected int next;
  protected double scoreIncrement;
  protected LNGVector<CLVar> vars;
  protected LNGByteVector vals;
  protected LNGByteVector phases;
  protected LNGDoublePriorityQueue decisions;
  protected LNGVector<CLFrame> control;
  protected LNGVector<LNGVector<CLWatch>> watches;
  protected LNGIntVector trail;
  protected LNGIntVector addedlits;
  protected LNGIntVector seen;
  protected LNGIntVector frames;
  protected CLClause ignore;
  protected CLStats stats;
  protected CLLimits limits;
  protected CLClause empty;
  protected LNGBooleanVector model;

  // SAT handler
  protected SATHandler handler;
  protected boolean canceledByHandler;

  /**
   * Constructs a new CleaneLing-style solver with a given configuration.
   * @param config the configuration
   */
  protected CleaneLingStyleSolver(final CleaneLingConfig config) {
    this.config = config;
    this.initialize();
  }

  /**
   * Returns the sign of a literal.
   * @param lit the literal
   * @return -1 for a negative literal, 1 for a positive literal
   */
  protected static byte sign(int lit) {
    return lit < 0 ? (byte) -1 : (byte) 1;
  }

  /**
   * Returns the next number in the luby sequence.
   * @param i the base value
   * @return the next number in the luby sequence
   */
  protected static long luby(long i) {
    long res = 0;
    long k;
    for (k = 1; res == 0 && k < 64; k++) {
      if (i == (1L << k) - 1)
        res = 1L << (k - 1);
    }
    k = 1;
    while (res == 0) {
      if ((1L << (k - 1)) <= i && i < (1L << k) - 1)
        res = luby(i - (1L << (k - 1)) + 1);
      k++;
    }
    return res;
  }

  /**
   * Initializes the internal solver state.
   */
  private void initialize() {
    this.level = 0;
    this.next = 0;
    this.empty = null;
    this.scoreIncrement = 1;
    this.ignore = null;
    this.vars = new LNGVector<>();
    this.vals = new LNGByteVector();
    this.phases = new LNGByteVector();
    this.decisions = new LNGDoublePriorityQueue();
    this.control = new LNGVector<>();
    this.trail = new LNGIntVector();
    this.addedlits = new LNGIntVector(100);
    this.seen = new LNGIntVector();
    this.frames = new LNGIntVector();
    this.watches = new LNGVector<>();
    this.stats = new CLStats();
    this.limits = new CLLimits();
    this.model = new LNGBooleanVector();
    control.push(new CLFrame());
  }

  /**
   * Adds a literal to the solver.  The literal 0 terminates a clause.
   * @param lit the literal
   */
  public void addlit(int lit) {
    if (lit != 0) {
      importLit(lit);
      addedlits.push(lit);
    } else {
      if (!trivialClause())
        newPushConnectClause();
      addedlits.clear();
    }
  }

  /**
   * Solves the formula currently stored in the solver.  Returns {@link Tristate#TRUE} if the formula is satisfiable (SAT),
   * {@link Tristate#FALSE} if the formula is unsatisfiable (UNSAT), or {@link Tristate#UNDEF} if the computation was canceled
   * by a {@link SATHandler}.  If {@code null} is passed as handler, the solver will run until the satisfiability is decided.
   * @param handler a sat handler
   * @return {@link Tristate#TRUE} if the formula is satisfiable, {@link Tristate#FALSE} if the formula is not satisfiable, or
   * {@link Tristate#UNDEF} if the computation was canceled.
   */
  public abstract Tristate solve(final SATHandler handler);

  /**
   * Returns the current model of the solver or an empty vector if there is none.
   * @return the current model of the solver
   */
  public LNGBooleanVector model() {
    return this.model;
  }

  /**
   * Resets the solver.
   */
  public void reset() {
    this.initialize();
  }

  /**
   * Imports a given literal.
   * @param lit the literal
   */
  protected void importLit(int lit) {
    int idx = Math.abs(lit);
    assert lit != 0;
    int newIdx;
    while (idx >= (newIdx = vars.size())) {
      vars.push(new CLVar());
      vals.push((byte) 0);
      phases.push((byte) 1);
      watches.push(new LNGVector<CLWatch>());
      watches.push(new LNGVector<CLWatch>());
      if (newIdx == 0)
        continue;
      decisions.push(newIdx);
    }
  }

  /**
   * Returns {@code true} if the current clause is a trivial clause, {@code false} otherwise.
   * @return {@code true} if the current clause is a trivial clause
   */
  protected boolean trivialClause() {
    boolean res = false;
    final LNGIntVector newAddedLits = new LNGIntVector(addedlits.size());
    for (int i = 0; i < addedlits.size(); i++) {
      final int lit = addedlits.get(i);
      assert lit != 0;
      int m = marked(lit);
      if (m < 0) {
        res = true;
        break;
      } else if (m == 0) {
        newAddedLits.push(lit);
        mark(lit);
      }
    }
    addedlits = newAddedLits;
    unmark();
    return res;
  }

  /**
   * Stores a new original clause.
   */
  protected void newPushConnectClause() {
    newPushConnectClause(false, 0);
  }

  /**
   * Stores a new clause.
   * @param redundant {@code true} if the clause is learnt, {@code false} if it is a original clause
   * @param glue      the glue value
   */
  protected abstract void newPushConnectClause(boolean redundant, int glue);

  /**
   * Returns the maximum variable index.
   * @return the maximum variable index
   */
  protected int maxvar() {
    int res = vars.size();
    if (res != 0) {
      assert res > 1;
      res--;
    }
    return res;
  }

  /**
   * Returns the variable for a given literal.
   * @param lit the literal
   * @return the variable for the literal
   */
  protected CLVar var(int lit) {
    int idx = Math.abs(lit);
    assert 0 < idx && idx < vars.size();
    return vars.get(idx);
  }

  /**
   * Returns the value for a given literal.
   * @param lit the literal
   * @return the value as byte (-1 = false, 1 = true)
   */
  protected byte val(int lit) {
    byte res = vals.get(Math.abs(lit));
    if (lit < 0)
      res = (byte) -res;
    return res;
  }

  /**
   * Returns whether a given literal is marked or not.
   * @param lit the literal
   * @return whether a given literal is marked or not
   */
  protected int marked(int lit) {
    final int res = var(lit).mark();
    return (lit < 0) ? -res : res;
  }

  /**
   * Marks a given literal.
   * @param lit the literal
   */
  protected void mark(int lit) {
    final CLVar v = var(lit);
    assert v.mark() == 0;
    v.setMark(sign(lit));
    seen.push(lit);
  }

  /**
   * Unmarks all variables up to level 0.
   */
  protected void unmark() {
    unmark(0);
  }

  /**
   * Unmarks all variables up to a given level
   * @param level the level
   */
  protected void unmark(int level) {
    assert level <= seen.size();
    while (level < seen.size()) {
      final int lit = seen.back();
      seen.pop();
      final CLVar v = var(lit);
      assert v.mark() == sign(lit);
      v.setMark(0);
    }
  }

  /**
   * Returns the watchers for a given literal.
   * @param lit the literal
   * @return the watchers for the literal
   */
  protected LNGVector<CLWatch> watches(int lit) {
    return watches.get(lit < 0 ? -lit * 2 - 1 : lit * 2);
  }

  /**
   * Adds a new watcher for a given literal.
   * @param lit    the literal
   * @param blit   the blocking literal
   * @param binary indicates whether it is a binary clause or not
   * @param clause the watched clause
   */
  protected void addWatch(int lit, int blit, boolean binary, final CLClause clause) {
    watches(lit).push(new CLWatch(blit, binary, clause));
  }

  /**
   * Marks the frame for a given literal's decision level.
   * @param lit the literal
   * @return {@code true} if the frame was newly marked, {@code false} if the frame was already marked
   */
  protected boolean markFrame(int lit) {
    final int currentlevel = var(lit).level();
    final CLFrame frame = control.get(currentlevel);
    if (frame.mark())
      return false;
    frame.setMark(true);
    frames.push(currentlevel);
    return true;
  }

  /**
   * Unmarks all frames.
   * @return the number of unmarked frames
   */
  protected int unmarkFrames() {
    final int res = frames.size();
    while (!frames.empty()) {
      final CLFrame f = control.get(frames.back());
      frames.pop();
      assert f.mark();
      f.setMark(false);
    }
    return res;
  }

  /**
   * Backtracks to level 0.
   */
  public void backtrack() {
    backtrack(0);
  }

  /**
   * Backtracks to a given level
   * @param newLevel the level
   */
  protected void backtrack(int newLevel) {
    assert 0 <= newLevel && newLevel <= level;
    if (newLevel == level)
      return;
    CLFrame f = control.back();
    while (f.level() > newLevel) {
      assert f.level() == level;
      assert f.trail() < trail.size();
      while (f.trail() < trail.size()) {
        int lit = trail.back();
        assert var(lit).level() == f.level();
        trail.pop();
        unassign(lit);
      }
      assert level > 0;
      level--;
      trail.shrinkTo(f.trail());
      next = f.trail();
      control.pop();
      f = control.back();
    }
    assert newLevel == level;
  }

  /**
   * Assigns a given literal.
   * @param lit    the literal
   * @param reason the reason
   */
  protected abstract void assign(int lit, final CLClause reason);

  /**
   * Unassigns a given literal
   * @param lit the literal
   */
  protected abstract void unassign(int lit);

  /**
   * Returns {@code true} if all assignments were propagated, {@code false} otherwise.
   * @return {@code true} if all assignments were propagated
   */
  protected boolean propagated() {
    return next == trail.size();
  }

  /**
   * Rescores all variables.
   */
  protected void rescore() {
    double maxScore = scoreIncrement;
    for (int idx = 1; idx < vars.size(); idx++) {
      double p = decisions.priority(idx);
      if (p > maxScore)
        maxScore = p;
    }
    double factor = 1 / maxScore;
    decisions.rescore(factor);
    scoreIncrement *= factor;
  }

  /**
   * Bump a literal's activity.
   * @param lit the literal
   */
  protected void bumpLit(int lit) {
    final double maxPriority = 1e300;
    int idx = Math.abs(lit);
    double oldPriority;
    if (scoreIncrement > maxPriority || (oldPriority = decisions.priority(idx)) > maxPriority) {
      rescore();
      oldPriority = decisions.priority(idx);
    }
    double newPriority = oldPriority + scoreIncrement;
    decisions.update(idx, newPriority);
  }

  /**
   * Analyzes a given literal.
   * @param lit the literal
   * @return the result of the analysis
   */
  protected boolean pullLit(int lit) {
    if (val(lit) == VALUE_TRUE)
      return false;
    if (marked(lit) != 0)
      return false;
    mark(lit);
    bumpLit(lit);
    if (var(lit).level() == level)
      return true;
    markFrame(lit);
    addedlits.push(lit);
    return false;
  }

  /**
   * Minimize the first UIP clause by trying to remove a given literal.
   * @param root the literal
   * @return {@code true} if the literal can be removed, {@code false} otherwise
   */
  protected boolean minimizeLit(int root) {
    assert marked(root) != 0;
    CLClause reason = var(root).reason();
    if (reason == null)
      return false;
    int oldSeenSize = seen.size();
    int nextSeen = oldSeenSize;
    boolean res = true;
    int lit = root;
    while (true) {
      int other;
      for (int p = 0; res && p < reason.lits().size(); p++) {
        other = reason.lits().get(p);
        if (other == lit)
          continue;
        assert val(other) == VALUE_FALSE;
        if (marked(other) != 0)
          continue;
        CLVar v = var(other);
        if (v.reason() == null)
          res = false;
        else if (!control.get(v.level()).mark()) {
          res = false;
        } else
          mark(other);
      }
      if (!res || nextSeen == seen.size())
        break;
      lit = -seen.get(nextSeen++);
      reason = var(lit).reason();
      assert reason != null;
    }
    if (!res)
      unmark(oldSeenSize);
    return res;
  }

  /**
   * Computes a new restart limit.
   */
  protected void newRestartLimit() {
    long newInterval = config.restartint * luby(stats.restartsCount + 1);
    if (newInterval > limits.maxRestartInterval)
      limits.maxRestartInterval = newInterval;
    limits.restart = stats.conflicts + newInterval;
  }

  /**
   * Assumes a given decision.
   * @param decision the decision
   */
  protected void assume(int decision) {
    assert propagated();
    level++;
    int height = trail.size();
    control.push(new CLFrame(decision, level, height));
    assert level + 1 == control.size();
    assign(decision, null);
  }

  /**
   * Checks if there are unassigned literals left.
   * @return {@code false} if all literals are assigned, {@code true} otherwise
   */
  protected boolean decide() {
    assert propagated();
    int decision = 0;
    while (decision == 0 && !decisions.empty()) {
      int lit = decisions.top();
      decisions.pop(lit);
      if (val(lit) == 0)
        decision = lit;
    }
    if (decision == 0)
      return false;
    assert decision > 0;
    if (phases.get(decision) < 0)
      decision = -decision;
    stats.decisions++;
    stats.levels += level;
    assume(decision);
    return true;
  }

  /**
   * Initializes the limits.
   */
  protected abstract void initLimits();

  /**
   * Updates the limits
   */
  protected abstract void updateLimits();

  /**
   * Creates a new clause.
   * @param redundant {@code true} if the clause is redundant (learnt), {@code false} otherwise
   * @param glue      the glue value
   * @return the new clause
   */
  protected abstract CLClause newClause(boolean redundant, int glue);

  /**
   * Connects a given clause (initializes its watchers).
   * @param c the clause
   */
  protected abstract void connectClause(final CLClause c);

  /**
   * Performs boolean constraint propagation.
   * @return a conflicting clause or {@code null} if there was no conflict
   */
  protected abstract CLClause bcp();

  /**
   * Minimizes the current clause.
   */
  protected abstract void minimizeClause();

  /**
   * Performs conflict analysis for a given conflicting clause.
   * @param reason the conflicting clause
   */
  protected abstract void analyze(CLClause reason);

  /**
   * Returns {@code true} if a restart should be performed, {@code false} otherwise.
   * @return {@code true} if a restart should be performed
   */
  protected abstract boolean restarting();

  /**
   * Restarts the solver.
   */
  protected abstract void restart();

  /**
   * The main CDCL algorithm.
   * @return the state of the search
   */
  protected abstract Tristate search();

  /**
   * Limits for the solver.
   */
  protected static final class CLLimits {
    long restart;
    long maxRestartInterval;
    long reduceRedundant;
    long reduceForcing;
    long reduceImportant;
    int simpRemovedVars;
    long simpSteps;
    long simpInc;
    long searchConflicts;
    int searchInc;
  }

  /**
   * The stats for the solver.
   */
  protected static final class CLStats {
    int conflicts;
    int decisions;
    int levels;
    int iterations;
    int propagations;
    int reductions;
    int simplifications;
    int steps;
    int sizes;
    int gluesSum;
    int gluesCount;
    int gluesUpdates;
    long restartsCount;
    long restartsReuseCount;
    long restartsReuseSum;
    int clausesIrredundant;
    int clausesRedundant;
    int clausesCollected;
    int clausesReduced;
    int clausesEliminated;
    int clausesBlocked;
    int backwardSubsumed;
    int backwardStrengthened;
    int distillUnits;
    int distillSubsumed;
    int distillStrengthened;
    int varsFixed;
    int varsEquivalent;
    int varsEliminated;
    int litsLearned;
    int litsMinimized;
  }
}
