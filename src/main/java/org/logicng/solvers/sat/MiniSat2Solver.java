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

/******************************************************************************************
 * MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************************/

package org.logicng.solvers.sat;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneType;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.datastructures.MSWatcher;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;

/**
 * A solver based on MiniSAT 2.2.0.  If the incremental mode is deactivated, this version should behave exactly
 * like the C++ version.
 * <p>
 * If the incremental mode is activated, this solver allows to save and load the solver state in an efficient manner.
 * Therefore clause deletion and simplifications are deactivated in this mode.  This mode is most efficient on small
 * to mid-size industrial formulas (up to 50,000 variables, 100,000 clauses).  Whenever you have lots of small formulas
 * to solve or need the ability to add and delete formulas from the solver, we recommend to consider this mode.
 * @version 1.3
 * @since 1.0
 */
public class MiniSat2Solver extends MiniSatStyleSolver {

  private LNGIntVector unitClauses;
  private double learntsizeAdjustConfl;
  private int learntsizeAdjustCnt;
  private int learntsizeAdjustStartConfl;
  private double learntsizeAdjustInc;
  private double maxLearnts;

  private Stack<Integer> backboneCandidates;
  private LNGIntVector backboneAssumptions;
  private HashMap<Integer, Tristate> backboneMap;
  private boolean computingBackbone;

  /**
   * Constructs a new MiniSAT 2 solver with the default values for solver configuration.  By default, incremental mode
   * is activated.
   */
  public MiniSat2Solver() {
    this(new MiniSatConfig.Builder().build());
  }

  /**
   * Constructs a new MiniSAT 2 solver with a given solver configuration.
   * @param config the solver configuration
   */
  public MiniSat2Solver(final MiniSatConfig config) {
    super(config);
    this.initializeMiniSAT();
  }

  /**
   * Initializes the additional parameters.
   */
  private void initializeMiniSAT() {
    this.unitClauses = new LNGIntVector();
    this.learntsizeAdjustConfl = 0;
    this.learntsizeAdjustCnt = 0;
    this.learntsizeAdjustStartConfl = 100;
    this.learntsizeAdjustInc = 1.5;
    this.maxLearnts = 0;
    this.computingBackbone = false;
  }

  @Override
  public int newVar(final boolean sign, final boolean dvar) {
    final int v = this.vars.size();
    final MSVariable newVar = new MSVariable(sign);
    this.vars.push(newVar);
    this.watches.push(new LNGVector<MSWatcher>());
    this.watches.push(new LNGVector<MSWatcher>());
    this.seen.push(false);
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

    if (ps.empty()) {
      this.ok = false;
      return false;
    } else if (ps.size() == 1) {
      uncheckedEnqueue(ps.get(0), null);
      this.ok = propagate() == null;
      if (this.incremental) {
        this.unitClauses.push(ps.get(0));
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
    this.handler = handler;
    if (this.handler != null) {
      this.handler.startedSolving();
    }
    this.model.clear();
    this.conflict.clear();
    if (!this.ok) {
      return Tristate.FALSE;
    }
    this.learntsizeAdjustConfl = this.learntsizeAdjustStartConfl;
    this.learntsizeAdjustCnt = (int) this.learntsizeAdjustConfl;
    this.maxLearnts = this.clauses.size() * this.learntsizeFactor;
    Tristate status = Tristate.UNDEF;
    int currRestarts = 0;
    while (status == Tristate.UNDEF && !this.canceledByHandler) {
      final double restBase = luby(this.restartInc, currRestarts);
      status = search((int) (restBase * this.restartFirst));
      currRestarts++;
    }

    if (this.config.proofGeneration) {
      if (status == Tristate.FALSE) {
        this.pgProof.push(new LNGIntVector(1, 0));
      }
    }

    if (status == Tristate.TRUE) {
      this.model = new LNGBooleanVector(this.vars.size());
      for (final MSVariable v : this.vars) {
        this.model.push(v.assignment() == Tristate.TRUE);
      }
    } else if (status == Tristate.FALSE && this.conflict.empty()) {
      this.ok = false;
    }
    if (this.handler != null) {
      this.handler.finishedSolving();
    }
    cancelUntil(0);
    this.handler = null;
    this.canceledByHandler = false;
    return status;
  }

  @Override
  public void reset() {
    super.initialize();
    this.initializeMiniSAT();
  }

  /**
   * Saves and returns the solver state expressed as an integer array which stores the length of the internal data
   * structures.  The array has length 5 and has the following layout:
   * <p>
   * {@code | current solver state | #vars | #clauses | #learnt clauses | #unit clauses | #pg original | #pg proof}
   * @return the current solver state
   */
  @Override
  public int[] saveState() {
    if (!this.incremental) {
      throw new IllegalStateException("Cannot save a state when the incremental mode is deactivated");
    }
    final int[] state;
    state = new int[7];
    state[0] = this.ok ? 1 : 0;
    state[1] = this.vars.size();
    state[2] = this.clauses.size();
    state[3] = this.learnts.size();
    state[4] = this.unitClauses.size();
    if (this.config.proofGeneration) {
      state[5] = this.pgOriginalClauses.size();
      state[6] = this.pgProof.size();
    }
    return state;
  }

  @Override
  public void loadState(final int[] state) {
    if (!this.incremental) {
      throw new IllegalStateException("Cannot load a state when the incremental mode is deactivated");
    }
    int i;
    completeBacktrack();
    this.ok = state[0] == 1;
    final int newVarsSize = Math.min(state[1], this.vars.size());
    for (i = this.vars.size() - 1; i >= newVarsSize; i--) {
      this.orderHeap.remove(this.name2idx.remove(this.idx2name.remove(i)));
    }
    this.vars.shrinkTo(newVarsSize);
    final int newClausesSize = Math.min(state[2], this.clauses.size());
    for (i = this.clauses.size() - 1; i >= newClausesSize; i--) {
      simpleRemoveClause(this.clauses.get(i));
    }
    this.clauses.shrinkTo(newClausesSize);
    final int newLearntsSize = Math.min(state[3], this.learnts.size());
    for (i = this.learnts.size() - 1; i >= newLearntsSize; i--) {
      simpleRemoveClause(this.learnts.get(i));
    }
    this.learnts.shrinkTo(newLearntsSize);
    this.watches.shrinkTo(newVarsSize * 2);
    this.unitClauses.shrinkTo(state[4]);
    for (i = 0; this.ok && i < this.unitClauses.size(); i++) {
      uncheckedEnqueue(this.unitClauses.get(i), null);
      this.ok = propagate() == null;
    }
    if (this.config.proofGeneration) {
      final int newPgOriginalSize = Math.min(state[5], this.pgOriginalClauses.size());
      this.pgOriginalClauses.shrinkTo(newPgOriginalSize);
      final int newPgProofSize = Math.min(state[6], this.pgProof.size());
      this.pgProof.shrinkTo(newPgProofSize);
    }
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
    this.watches.get(not(c.get(0))).push(new MSWatcher(c, c.get(1)));
    this.watches.get(not(c.get(1))).push(new MSWatcher(c, c.get(0)));
    if (c.learnt()) {
      this.learntsLiterals += c.size();
    } else {
      this.clausesLiterals += c.size();
    }
  }

  @Override
  protected void detachClause(final MSClause c) {
    assert c.size() > 1;
    this.watches.get(not(c.get(0))).remove(new MSWatcher(c, c.get(1)));
    this.watches.get(not(c.get(1))).remove(new MSWatcher(c, c.get(0)));
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
      while (iInd < ws.size()) {
        final MSWatcher i = ws.get(iInd);
        final int blocker = i.blocker();
        if (value(blocker) == Tristate.TRUE) {
          ws.set(jInd++, i);
          iInd++;
          continue;
        }
        final MSClause c = i.clause();
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
        for (int k = 2; k < c.size() && !foundWatch; k++) {
          if (value(c.get(k)) != Tristate.FALSE) {
            c.set(1, c.get(k));
            c.set(k, falseLit);
            this.watches.get(not(c.get(1))).push(w);
            foundWatch = true;
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
          for (int j = 1; j < c.size(); j++) {
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
  protected void cancelUntil(final int level) {
    if (decisionLevel() > level) {
      if (!this.config.fastBackboneComputation) {
        for (int c = this.trail.size() - 1; c >= this.trailLim.get(level); c--) {
          final int x = var(this.trail.get(c));
          final MSVariable v = this.vars.get(x);
          v.assign(Tristate.UNDEF);
          v.setPolarity(sign(this.trail.get(c)));
          insertVarOrder(x);
        }
      } else {
        for (int c = this.trail.size() - 1; c >= this.trailLim.get(level); c--) {
          final int x = var(this.trail.get(c));
          final MSVariable v = this.vars.get(x);
          v.assign(Tristate.UNDEF);
          v.setPolarity(!this.computingBackbone && sign(this.trail.get(c)));
          insertVarOrder(x);
        }
      }
      this.qhead = this.trailLim.get(level);
      this.trail.removeElements(this.trail.size() - this.trailLim.get(level));
      this.trailLim.removeElements(this.trailLim.size() - level);
    }
  }

  @Override
  protected void reduceDB() {
    int i;
    int j;
    final double extraLim = this.claInc / this.learnts.size();
    this.learnts.manualSort(MSClause.minisatComparator);
    for (i = j = 0; i < this.learnts.size(); i++) {
      final MSClause c = this.learnts.get(i);
      if (c.size() > 2 && !locked(c) && (i < this.learnts.size() / 2 || c.activity() < extraLim)) {
        removeClause(this.learnts.get(i));
      } else {
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
        assert value(c.get(0)) == Tristate.UNDEF && value(c.get(1)) == Tristate.UNDEF;
        if (!this.config.proofGeneration) {
          // This simplification does not work with proof generation
          for (int k = 2; k < c.size(); k++) {
            if (value(c.get(k)) == Tristate.FALSE) {
              c.set(k--, c.get(c.size() - 1));
              c.pop();
            }
          }
        }
        cs.set(j++, cs.get(i));
      }
    }
    cs.removeElements(i - j);
  }

  @Override
  protected boolean satisfied(final MSClause c) {
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
    if (!this.ok || propagate() != null) {
      this.ok = false;
      return false;
    }
    if (nAssigns() == this.simpDBAssigns || (this.simpDBProps > 0)) {
      return true;
    }
    removeSatisfied(this.learnts);
    if (this.removeSatisfied) {
      removeSatisfied(this.clauses);
    }
    rebuildOrderHeap();
    this.simpDBAssigns = nAssigns();
    this.simpDBProps = this.clausesLiterals + this.learntsLiterals;
    return true;
  }

  /**
   * The main search procedure of the CDCL algorithm.
   * @param nofConflicts the number of conflicts till the next restart
   * @return a {@link Tristate} representing the result.  {@code FALSE} if the formula is UNSAT, {@code TRUE} if the
   * formula is SAT, and {@code UNDEF} if the state is not known yet (restart) or the handler canceled the computation
   */
  private Tristate search(final int nofConflicts) {
    if (!this.ok) {
      return Tristate.FALSE;
    }
    int conflictC = 0;
    while (true) {
      final MSClause confl = propagate();
      if (confl != null) {
        if (this.handler != null && !this.handler.detectedConflict()) {
          this.canceledByHandler = true;
          return Tristate.UNDEF;
        }
        conflictC++;
        if (decisionLevel() == 0) {
          return Tristate.FALSE;
        }
        final LNGIntVector learntClause = new LNGIntVector();
        analyze(confl, learntClause);
        cancelUntil(this.analyzeBtLevel);

        if (this.config.proofGeneration) {
          final LNGIntVector vec = new LNGIntVector(learntClause.size());
          vec.push(1);
          for (int i = 0; i < learntClause.size(); i++) {
            vec.push((var(learntClause.get(i)) + 1) * (-2 * (sign(learntClause.get(i)) ? 1 : 0) + 1));
          }
          this.pgProof.push(vec);
        }

        if (learntClause.size() == 1) {
          uncheckedEnqueue(learntClause.get(0), null);
          this.unitClauses.push(learntClause.get(0));
        } else {
          final MSClause cr = new MSClause(learntClause, true);
          this.learnts.push(cr);
          attachClause(cr);
          if (!this.incremental) {
            claBumpActivity(cr);
          }
          uncheckedEnqueue(learntClause.get(0), cr);
        }
        varDecayActivity();
        if (!this.incremental) {
          claDecayActivity();
        }
        if (--this.learntsizeAdjustCnt == 0) {
          this.learntsizeAdjustConfl *= this.learntsizeAdjustInc;
          this.learntsizeAdjustCnt = (int) this.learntsizeAdjustConfl;
          this.maxLearnts *= this.learntsizeInc;
        }
      } else {
        if (nofConflicts >= 0 && conflictC >= nofConflicts) {
          cancelUntil(0);
          return Tristate.UNDEF;
        }
        if (!this.incremental) {
          if (decisionLevel() == 0 && !simplify()) {
            return Tristate.FALSE;
          }
          if (this.learnts.size() - nAssigns() >= this.maxLearnts) {
            reduceDB();
          }
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
   */
  private void analyze(final MSClause conflictClause, final LNGIntVector outLearnt) {
    MSClause c = conflictClause;
    int pathC = 0;
    int p = LIT_UNDEF;
    outLearnt.push(-1);
    int index = this.trail.size() - 1;
    do {
      assert c != null;
      if (!this.incremental && c.learnt()) {
        claBumpActivity(c);
      }
      for (int j = (p == LIT_UNDEF) ? 0 : 1; j < c.size(); j++) {
        final int q = c.get(j);
        if (!this.seen.get(var(q)) && v(q).level() > 0) {
          varBumpActivity(var(q));
          this.seen.set(var(q), true);
          if (v(q).level() >= decisionLevel()) {
            pathC++;
          } else {
            outLearnt.push(q);
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
    simplifyClause(outLearnt);
  }

  /**
   * Minimizes a given learnt clause depending on the minimization method of the solver configuration.
   * @param outLearnt the learnt clause which should be minimized
   */
  private void simplifyClause(final LNGIntVector outLearnt) {
    int i;
    int j;
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
        if (v(outLearnt.get(i)).reason() == null) {
          outLearnt.set(j++, outLearnt.get(i));
        } else {
          final MSClause c = v(outLearnt.get(i)).reason();
          for (int k = 1; k < c.size(); k++) {
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
    for (int l = 0; l < this.analyzeToClear.size(); l++) {
      this.seen.set(var(this.analyzeToClear.get(l)), false);
    }
  }

  /**
   * Performs an unconditional backtrack to level zero.
   */
  private void completeBacktrack() {
    for (int v = 0; v < this.vars.size(); v++) {
      final MSVariable var = this.vars.get(v);
      var.assign(Tristate.UNDEF);
      var.setReason(null);
      if (!this.orderHeap.inHeap(v) && var.decision()) {
        this.orderHeap.insert(v);
      }
    }
    this.trail.clear();
    this.trailLim.clear();
    this.qhead = 0;
  }

  /**
   * Performs a simple removal of clauses used during the loading of an older state.
   * @param c the clause to remove
   */
  private void simpleRemoveClause(final MSClause c) {
    this.watches.get(not(c.get(0))).remove(new MSWatcher(c, c.get(1)));
    this.watches.get(not(c.get(1))).remove(new MSWatcher(c, c.get(0)));
  }

  ///// Backbone Stuff /////

  /**
   * Computes the backbone of the given variables with respect to the formulas added to the solver.
   * @param variables variables to test
   * @param type      backbone type
   * @return the backbone projected to the relevant variables or {@code null} if the formula on the solver with the restrictions are not satisfiable
   */
  public Backbone computeBackbone(final Collection<Variable> variables, final BackboneType type) {
    final boolean sat = solve(null) == Tristate.TRUE;
    if (sat) {
      this.computingBackbone = true;
      final List<Integer> relevantVarIndices = getRelevantVarIndices(variables);
      initBackboneDS(relevantVarIndices);
      computeBackbone(relevantVarIndices, type);
      final Backbone backbone = buildBackbone(variables, type);
      this.computingBackbone = false;
      return backbone;
    } else {
      return Backbone.unsatBackbone();
    }
  }

  /**
   * Returns a list of relevant variable indices. A relevant variable is known by the solver.
   * @param variables variables to convert and filter
   * @return list of relevant variable indices
   */
  private List<Integer> getRelevantVarIndices(final Collection<Variable> variables) {
    final List<Integer> relevantVarIndices = new ArrayList<>(variables.size());
    for (final Variable var : variables) {
      final Integer idx = this.name2idx.get(var.name());
      // Note: Unknown variables are variables added to the solver yet. Thus, these are optional variables and can
      // be left out for the backbone computation.
      if (idx != null) {
        relevantVarIndices.add(idx);
      }
    }
    return relevantVarIndices;
  }

  /**
   * Initializes the internal solver state for backbones.
   * @param variables to test
   */
  private void initBackboneDS(final List<Integer> variables) {
    this.backboneCandidates = new Stack<>();
    this.backboneAssumptions = new LNGIntVector(variables.size());
    this.backboneMap = new HashMap<>();
    for (final Integer var : variables) {
      this.backboneMap.put(var, Tristate.UNDEF);
    }
  }

  /**
   * Computes the backbone for the given variables.
   * @param variables variables to test
   */
  private void computeBackbone(final List<Integer> variables, final BackboneType type) {
    final Stack<Integer> candidates = createInitialCandidates(variables, type);
    while (candidates.size() > 0) {
      final int lit = candidates.pop();
      if (solveWithLit(lit)) {
        refineUpperBound();
      } else {
        addBackboneLiteral(lit);
      }
    }
  }

  /**
   * Creates the initial candidate literals for the backbone computation.
   * @param variables variables to test
   * @return initial candidates
   */
  private Stack<Integer> createInitialCandidates(final List<Integer> variables, final BackboneType type) {
    for (final Integer var : variables) {
      if (isUPZeroLit(var)) {
        final int backboneLit = mkLit(var, !this.model.get(var));
        addBackboneLiteral(backboneLit);
      } else {
        final boolean modelPhase = this.model.get(var);
        if (isBothOrNegativeType(type) && !modelPhase || isBothOrPositiveType(type) && modelPhase) {
          final int lit = mkLit(var, !modelPhase);
          if (!this.config.bbInitialUBCheckForRotatableLiterals || !isRotatable(lit)) {
            this.backboneCandidates.add(lit);
          }
        }
      }
    }
    return this.backboneCandidates;
  }

  /**
   * Refines the upper bound by optional checks (UP zero literal, complement model literal, rotatable literal).
   */
  private void refineUpperBound() {
    for (final Integer lit : new ArrayList<>(this.backboneCandidates)) {
      final int var = var(lit);
      if (isUPZeroLit(var)) {
        this.backboneCandidates.remove(lit);
        addBackboneLiteral(lit);
      } else if (this.config.bbCheckForComplementModelLiterals && this.model.get(var) == sign(lit)) {
        this.backboneCandidates.remove(lit);
      } else if (this.config.bbCheckForRotatableLiterals && isRotatable(lit)) {
        this.backboneCandidates.remove(lit);
      }
    }
  }

  /**
   * Tests the given literal with the formula on the solver for satisfiability.
   * @param lit literal to test
   * @return {@code true} if satisfiable, otherwise {@code false}
   */
  private boolean solveWithLit(final int lit) {
    this.backboneAssumptions.push(not(lit));
    final boolean sat = solve(null, this.backboneAssumptions) == Tristate.TRUE;
    this.backboneAssumptions.pop();
    return sat;
  }

  /**
   * Builds the backbone object from the computed backbone literals.
   * @param variables relevant variables
   * @return backbone
   */
  private Backbone buildBackbone(final Collection<Variable> variables, final BackboneType type) {
    final SortedSet<Variable> posBackboneVars = isBothOrPositiveType(type) ? new TreeSet<Variable>() : null;
    final SortedSet<Variable> negBackboneVars = isBothOrNegativeType(type) ? new TreeSet<Variable>() : null;
    final SortedSet<Variable> optionalVars = isBothType(type) ? new TreeSet<Variable>() : null;
    for (final Variable var : variables) {
      final Integer idx = this.name2idx.get(var.name());
      if (idx == null) {
        if (isBothType(type)) {
          optionalVars.add(var);
        }
      } else {
        switch (this.backboneMap.get(idx)) {
          case TRUE:
            if (isBothOrPositiveType(type)) {
              posBackboneVars.add(var);
            }
            break;
          case FALSE:
            if (isBothOrNegativeType(type)) {
              negBackboneVars.add(var);
            }
            break;
          case UNDEF:
            if (isBothType(type)) {
              optionalVars.add(var);
            }
            break;
          default:
            throw new IllegalStateException("Unknown tristate: " + this.backboneMap.get(idx));
        }
      }
    }
    return Backbone.satBackbone(posBackboneVars, negBackboneVars, optionalVars);
  }

  /**
   * Tests the given variable whether it is a unit propagated literal on level 0.
   * <p>
   * Assumption: The formula on the solver has successfully been tested to be satisfiable before.
   * @param var variable index to test
   * @return {@code true} if the variable is a unit propagated literal on level 0, otherwise {@code false}
   */
  private boolean isUPZeroLit(final int var) {
    return this.vars.get(var).level() == 0;
  }

  /**
   * Tests the given literal whether it is unit in the given clause.
   * @param lit    literal to test
   * @param clause clause containing the literal
   * @return {@code true} if the literal is unit, {@code false} otherwise
   */
  private boolean isUnit(final int lit, final MSClause clause) {
    for (int i = 0; i < clause.size(); ++i) {
      final int clauseLit = clause.get(i);
      if (lit != clauseLit && this.model.get(var(clauseLit)) != sign(clauseLit)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Tests the given literal whether it is rotatable in the current model.
   * @param lit literal to test
   * @return {@code true} if the literal is rotatable, otherwise {@code false}
   */
  private boolean isRotatable(final int lit) {
    // A rotatable literal MUST NOT be a unit propagated literal
    if (v(lit).reason() != null) {
      return false;
    }
    // A rotatable literal MUST NOT be unit
    for (final MSWatcher watcher : this.watches.get(not(lit))) {
      if (isUnit(lit, watcher.clause())) {
        return false;
      }
    }
    return true;
  }

  /**
   * Adds the given literal to the backbone result and optionally adds the literal to the solver.
   * @param lit literal to add
   */
  private void addBackboneLiteral(final int lit) {
    this.backboneMap.put(var(lit), sign(lit) ? Tristate.FALSE : Tristate.TRUE);
    this.backboneAssumptions.push(lit);
  }

  private boolean isBothOrPositiveType(final BackboneType type) {
    return type == BackboneType.POSITIVE_AND_NEGATIVE || type == BackboneType.ONLY_POSITIVE;
  }

  private boolean isBothOrNegativeType(final BackboneType type) {
    return type == BackboneType.POSITIVE_AND_NEGATIVE || type == BackboneType.ONLY_NEGATIVE;
  }

  private boolean isBothType(final BackboneType type) {
    return type == BackboneType.POSITIVE_AND_NEGATIVE;
  }
}
