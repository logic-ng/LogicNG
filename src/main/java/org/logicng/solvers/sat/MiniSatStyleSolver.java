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

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.datastructures.LNGHeap;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.datastructures.MSWatcher;

import java.util.Map;
import java.util.TreeMap;

/**
 * The super class for all MiniSAT-style solvers.
 * @version 1.3
 * @since 1.0
 */
public abstract class MiniSatStyleSolver {

  /**
   * The undefined literal
   */
  public static final int LIT_UNDEF = -1;

  // external solver configuration
  protected final MiniSatConfig config;

  // internal solver state
  protected boolean ok;
  protected int qhead;
  protected LNGVector<MSClause> clauses;
  protected LNGVector<MSClause> learnts;
  protected LNGVector<LNGVector<MSWatcher>> watches;
  protected LNGVector<MSVariable> vars;
  protected LNGHeap orderHeap;
  protected LNGIntVector trail;
  protected LNGIntVector trailLim;
  protected LNGBooleanVector model;
  protected LNGIntVector conflict;
  protected LNGIntVector assumptions;
  protected LNGBooleanVector seen;
  protected LNGIntVector analyzeStack;
  protected LNGIntVector analyzeToClear;
  protected int analyzeBtLevel;
  protected double claInc;
  protected int simpDBAssigns;
  protected int simpDBProps;
  protected int clausesLiterals;
  protected int learntsLiterals;

  // solver configuration
  protected double varDecay;
  protected double varInc;
  protected MiniSatConfig.ClauseMinimization ccminMode;
  protected int restartFirst;
  protected double restartInc;
  protected double clauseDecay;
  protected boolean removeSatisfied;
  protected double learntsizeFactor;
  protected double learntsizeInc;
  protected boolean incremental;

  // mapping of variable names to variable indices
  protected Map<String, Integer> name2idx;
  protected Map<Integer, String> idx2name;

  // SAT handler
  protected SATHandler handler;
  protected boolean canceledByHandler;

  // Proof generating information
  protected LNGVector<ProofInformation> pgOriginalClauses;
  protected LNGVector<LNGIntVector> pgProof;

  /**
   * Constructs a new MiniSAT-style solver with a given configuration.
   * @param config the configuration
   */
  protected MiniSatStyleSolver(final MiniSatConfig config) {
    this.config = config;
    this.initialize();
  }

  /**
   * Creates a literal for a given variable number and literal.
   * @param var  the variable number
   * @param sign {@code true} if the literal is negative, {@code false} otherwise
   * @return the literal (as integer value)
   */
  public static int mkLit(int var, boolean sign) {
    return var + var + (sign ? 1 : 0);
  }

  /**
   * Negates a given literal.
   * @param lit the literal
   * @return the negated literal
   */
  public static int not(int lit) {
    return lit ^ 1;
  }

  /**
   * Returns {@code true} if a given literal is negated, {@code false} otherwise.
   * @param lit the literal
   * @return {@code true} if the literal is negated
   */
  public static boolean sign(int lit) {
    return (lit & 1) == 1;
  }

  /**
   * Returns the variable index for a given literal.
   * @param lit the literal
   * @return the variable index of the literal
   */
  public static int var(int lit) {
    return lit >> 1;
  }

  /**
   * Computes the next number in the Luby sequence.
   * @param y the restart increment
   * @param x the current number of restarts
   * @return the next number in the Luby sequence
   */
  protected static double luby(double y, int x) {
    int intX = x;
    int size = 1;
    int seq = 0;
    while (size < intX + 1) {
      seq++;
      size = 2 * size + 1;
    }
    while (size - 1 != intX) {
      size = (size - 1) >> 1;
      seq--;
      intX = intX % size;
    }
    return Math.pow(y, seq);
  }

  /**
   * Initializes the internal solver state.
   */
  protected void initialize() {
    this.initializeConfig();
    this.ok = true;
    this.qhead = 0;
    this.clauses = new LNGVector<>();
    this.learnts = new LNGVector<>();
    this.watches = new LNGVector<>();
    this.vars = new LNGVector<>();
    this.orderHeap = new LNGHeap(this);
    this.trail = new LNGIntVector();
    this.trailLim = new LNGIntVector();
    this.model = new LNGBooleanVector();
    this.conflict = new LNGIntVector();
    this.assumptions = new LNGIntVector();
    this.seen = new LNGBooleanVector();
    this.analyzeStack = new LNGIntVector();
    this.analyzeToClear = new LNGIntVector();
    this.analyzeBtLevel = 0;
    this.claInc = 1;
    this.simpDBAssigns = -1;
    this.simpDBProps = 0;
    this.clausesLiterals = 0;
    this.learntsLiterals = 0;
    this.name2idx = new TreeMap<>();
    this.idx2name = new TreeMap<>();
    this.canceledByHandler = false;
    if (this.config.proofGeneration) {
      this.pgOriginalClauses = new LNGVector<>();
      this.pgProof = new LNGVector<>();
    }
  }

  /**
   * Initializes the solver configuration.
   */
  private void initializeConfig() {
    this.varDecay = this.config.varDecay;
    this.varInc = this.config.varInc;
    this.ccminMode = this.config.clauseMin;
    this.restartFirst = this.config.restartFirst;
    this.restartInc = this.config.restartInc;
    this.clauseDecay = this.config.clauseDecay;
    this.removeSatisfied = this.config.removeSatisfied;
    this.learntsizeFactor = this.config.learntsizeFactor;
    this.learntsizeInc = this.config.learntsizeInc;
    this.incremental = this.config.incremental;
  }

  /**
   * Returns the variable for a given literal.
   * @param lit the literal
   * @return the variable of the literal
   */
  protected MSVariable v(int lit) {
    return this.vars.get(lit >> 1);
  }

  /**
   * Returns the assigned value of a given literal.
   * @param lit the literal
   * @return the assigned value of the literal
   */
  protected Tristate value(int lit) {
    return sign(lit) ? Tristate.negate(this.v(lit).assignment()) : this.v(lit).assignment();
  }

  /**
   * Compares two variables by their activity.
   * @param x the first variable
   * @param y the second variable
   * @return {@code true} if the first variable's activity is larger then the second one's
   */
  public boolean lt(int x, int y) {
    return this.vars.get(x).activity() > this.vars.get(y).activity();
  }

  /**
   * Returns the variable index for a given variable name.
   * @param name the variable name
   * @return the variable index for the name
   */
  public int idxForName(final String name) {
    final Integer id = this.name2idx.get(name);
    return id == null ? -1 : id;
  }

  /**
   * Returns the name for a given variable index.
   * @param var the variable index
   * @return the name for the index
   */
  public String nameForIdx(int var) {
    return this.idx2name.get(var);
  }

  /**
   * Adds a new variable name with a given variable index to this solver.
   * @param name the variable name
   * @param id   the variable index
   */
  public void addName(final String name, int id) {
    this.name2idx.put(name, id);
    this.idx2name.put(id, name);
  }

  /**
   * Adds a new variable to the solver.
   * @param sign the initial polarity of the new variable, {@code true} if negative, {@code false} if positive
   * @param dvar {@code true} if this variable can be used as a decision variable, {@code false} if it should not be
   *             used as a decision variable
   * @return the index of the new variable
   */
  public abstract int newVar(boolean sign, boolean dvar);

  /**
   * Adds a unit clause to the solver.
   * @param lit         the unit clause's literal
   * @param proposition a proposition (if required for proof tracing)
   * @return {@code true} if the clause was added successfully, {@code false} otherwise
   */
  public boolean addClause(int lit, final Proposition proposition) {
    final LNGIntVector unit = new LNGIntVector(1);
    unit.push(lit);
    return this.addClause(unit, proposition);
  }

  /**
   * Adds a clause to the solver.
   * @param ps          the literals of the clause
   * @param proposition a proposition (if required for proof tracing)
   * @return {@code true} if the clause was added successfully, {@code false} otherwise
   */
  public abstract boolean addClause(final LNGIntVector ps, final Proposition proposition);

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
   * Solves the formula currently stored in the solver together with the given assumption literals.  Returns
   * {@link Tristate#TRUE} if the formula and the assumptions are satisfiable (SAT), {@link Tristate#FALSE} if the formula and the
   * assumptions are not satisfiable together (UNSAT), or {@link Tristate#UNDEF} if the computation was canceled by a
   * {@link SATHandler}. If {@code null} is passed as handler, the solver will run until the satisfiability is decided.
   * @param handler     a sat handler
   * @param assumptions the assumptions as a given vector of literals
   * @return {@link Tristate#TRUE} if the formula and the assumptions are satisfiable, {@link Tristate#FALSE} if they are
   * not satisfiable, or {@link Tristate#UNDEF} if the computation was canceled.
   */
  public Tristate solve(final SATHandler handler, final LNGIntVector assumptions) {
    this.assumptions = new LNGIntVector(assumptions);
    final Tristate result = this.solve(handler);
    this.assumptions.clear();
    return result;
  }

  /**
   * Resets the solver state.
   */
  public abstract void reset();

  /**
   * Returns the current model of the solver or an empty vector if there is none.
   * @return the current model of the solver
   */
  public LNGBooleanVector model() {
    return this.model;
  }

  /**
   * Returns the current conflict of the solver or an empty vector if there is none.
   * @return the current conflict of the solver
   */
  public LNGIntVector conflict() {
    return this.conflict;
  }

  /**
   * Saves and returns the solver state expressed as an integer array which stores the length of the internal data
   * structures.
   * @return the current solver state
   * @throws UnsupportedOperationException if the solver does not support state saving/loading
   * @throws IllegalStateException         if the solver is not in incremental mode
   */
  public abstract int[] saveState();

  /**
   * Loads a given state in the solver.
   * <p>
   * ATTENTION: You can only load a state which was created by this instance of the solver before the current state.
   * Only the sized of the internal data structures are stored, meaning you can track back in time and restore a solver
   * state with fewer variables and/or fewer clauses.  It is not possible to import a solver state from another solver
   * or another solving execution.
   * @param state the solver state to load
   * @throws UnsupportedOperationException if the solver does not support state saving/loading
   * @throws IllegalStateException         if the solver is not in incremental mode
   */
  public abstract void loadState(int[] state);

  /**
   * Returns the number of variables of the solver.
   * @return the number of variables of the solver
   */
  public int nVars() {
    return this.vars.size();
  }

  /**
   * Returns the mapping from variable names to internal solver indices.
   * @return the mapping from variable names to internal solver indices
   */
  public Map<String, Integer> name2idx() {
    return name2idx;
  }

  /**
   * Returns the number of assigned variables.
   * @return the number of assigned variables
   */
  protected int nAssigns() {
    return this.trail.size();
  }

  /**
   * Returns the current decision level of the solver.
   * @return the current decision level of the solver
   */
  protected int decisionLevel() {
    return this.trailLim.size();
  }

  /**
   * Helper function used to maintain an abstraction of levels involved during conflict analysis.
   * @param x a variable index
   * @return the abstraction of levels
   */
  protected int abstractLevel(int x) {
    return 1 << (this.vars.get(x).level() & 31);
  }

  /**
   * Inserts a variable (given by its index) into the heap of decision variables.
   * @param x the variable index
   */
  protected void insertVarOrder(int x) {
    if (!this.orderHeap.inHeap(x) && this.vars.get(x).decision())
      this.orderHeap.insert(x);
  }

  /**
   * Picks the next branching literal.
   * @return the literal or -1 if there are no unassigned literals left
   */
  protected int pickBranchLit() {
    int next = -1;
    while (next == -1 || this.vars.get(next).assignment() != Tristate.UNDEF || !this.vars.get(next).decision())
      if (this.orderHeap.empty())
        return -1;
      else
        next = this.orderHeap.removeMin();
    return mkLit(next, this.vars.get(next).polarity());
  }

  /**
   * Decays the variable activity increment by the variable decay factor.
   */
  protected void varDecayActivity() {
    this.varInc *= (1 / this.varDecay);
  }

  /**
   * Bumps the activity of the variable at a given index.
   * @param v the variable index
   */
  protected void varBumpActivity(int v) {
    this.varBumpActivity(v, this.varInc);
  }

  /**
   * Bumps the activity of the variable at a given index by a given value.
   * @param v   the variable index
   * @param inc the increment value
   */
  protected void varBumpActivity(int v, double inc) {
    final MSVariable var = this.vars.get(v);
    var.incrementActivity(inc);
    if (var.activity() > 1e100) {
      for (final MSVariable variable : this.vars)
        variable.rescaleActivity();
      this.varInc *= 1e-100;
    }
    if (this.orderHeap.inHeap(v))
      this.orderHeap.decrease(v);
  }

  /**
   * Rebuilds the heap of decision variables.
   */
  protected void rebuildOrderHeap() {
    final LNGIntVector vs = new LNGIntVector();
    for (int v = 0; v < this.nVars(); v++)
      if (this.vars.get(v).decision() && this.vars.get(v).assignment() == Tristate.UNDEF)
        vs.push(v);
    this.orderHeap.build(vs);
  }

  /**
   * Returns {@code true} if the given clause is locked and therefore cannot be removed, {@code false} otherwise.
   * @param c the clause
   * @return {@code true} if the given clause is locked
   */
  protected boolean locked(final MSClause c) {
    return value(c.get(0)) == Tristate.TRUE && v(c.get(0)).reason() != null && v(c.get(0)).reason() == c;
  }

  /**
   * Decays the clause activity increment by the clause decay factor.
   */
  protected void claDecayActivity() {
    claInc *= (1 / clauseDecay);
  }

  /**
   * Bumps the activity of the given clause.
   * @param c the clause
   */
  protected void claBumpActivity(final MSClause c) {
    c.incrementActivity(claInc);
    if (c.activity() > 1e20) {
      for (final MSClause clause : learnts)
        clause.rescaleActivity();
      claInc *= 1e-20;
    }
  }

  /**
   * Assigns a literal (= a variable to the respective value).
   * @param lit    the literal
   * @param reason the reason clause of the assignment (conflict resolution) or {@code null} if it was a decision
   */
  protected abstract void uncheckedEnqueue(int lit, MSClause reason);

  /**
   * Attaches a given clause to the solver (i.e. the watchers for this clause are initialized).
   * @param c the clause
   */
  protected abstract void attachClause(final MSClause c);

  /**
   * Detaches a given clause (e.g. removes all watchers pointing to this clause).
   * @param c the clause
   */
  protected abstract void detachClause(final MSClause c);

  /**
   * Removes a given clause.
   * @param c the clause to remove
   */
  protected abstract void removeClause(final MSClause c);

  /**
   * Performs unit propagation.
   * @return the conflicting clause if a conflict arose during unit propagation or {@code null} if there was none
   */
  protected abstract MSClause propagate();

  /**
   * Returns {@code true} if a given literal is redundant in the current conflict analysis, {@code false} otherwise.
   * @param p              the literal
   * @param abstractLevels an abstraction of levels
   * @return {@code true} if a given literal is redundant in the current conflict analysis
   */
  protected abstract boolean litRedundant(int p, int abstractLevels);

  /**
   * Analysis the final conflict if there were assumptions.
   * @param p           the conflicting literal
   * @param outConflict the vector to store the final conflict
   */
  protected abstract void analyzeFinal(int p, final LNGIntVector outConflict);

  /**
   * Performs backtracking up to a given level.
   * @param level the level
   */
  protected abstract void cancelUntil(int level);

  /**
   * Reduces the database of learnt clauses.  Only clauses of the first half of the clauses with the most activity
   * are possibly removed.  A clause is only removed if it is not locked, i.e. is the reason of an assignment for a
   * variable.
   */
  protected abstract void reduceDB();

  /**
   * Removes all clauses which are satisfied under the current assignment of a set of clauses.
   * @param cs the set of clauses
   */
  protected abstract void removeSatisfied(final LNGVector<MSClause> cs);

  /**
   * Returns {@code true} if a given clause is satisfied under the current assignment, {@code false} otherwise.
   * @param c the clause
   * @return {@code true} if a given clause is satisfied under the current assignment
   */
  protected abstract boolean satisfied(final MSClause c);

  /**
   * Simplifies the database of clauses.  This method is only executed on level 0.  All learnt clauses which are
   * satisfied on level 0 are removed.  Depending on the configuration of the solver, also original clauses which are
   * satisfied at level 0 are removed.
   * @return {@code true} if simplification was successful and no conflict was found, {@code false} if a conflict was
   * found during the simplification
   */
  protected abstract boolean simplify();

  /**
   * Returns the original clauses for proof generation.
   * @return the original clauses for proof generation
   */
  public LNGVector<ProofInformation> pgOriginalClauses() {
    return this.pgOriginalClauses;
  }

  /**
   * Returns the proof clauses for proof generation.
   * @return the proof clauses for proof generation
   */
  public LNGVector<LNGIntVector> pgProof() {
    return this.pgProof;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("ok            ").append(ok).append(System.lineSeparator());
    sb.append("qhead         ").append(qhead).append(System.lineSeparator());
    sb.append("#clauses      ").append(clauses.size()).append(System.lineSeparator());
    sb.append("#learnts      ").append(learnts.size()).append(System.lineSeparator());
    sb.append("#watches      ").append(watches.size()).append(System.lineSeparator());
    sb.append("#vars         ").append(vars.size()).append(System.lineSeparator());
    sb.append("#orderheap    ").append(orderHeap.size()).append(System.lineSeparator());
    sb.append("#trail        ").append(trail.size()).append(System.lineSeparator());
    sb.append("#trailLim     ").append(trailLim.size()).append(System.lineSeparator());

    sb.append("model         ").append(model).append(System.lineSeparator());
    sb.append("conflict      ").append(conflict).append(System.lineSeparator());
    sb.append("assumptions   ").append(assumptions).append(System.lineSeparator());
    sb.append("#seen         ").append(seen.size()).append(System.lineSeparator());
    sb.append("#stack        ").append(analyzeStack.size()).append(System.lineSeparator());
    sb.append("#toclear      ").append(analyzeToClear.size()).append(System.lineSeparator());

    sb.append("claInc        ").append(claInc).append(System.lineSeparator());
    sb.append("simpDBAssigns ").append(simpDBAssigns).append(System.lineSeparator());
    sb.append("simpDBProps   ").append(simpDBProps).append(System.lineSeparator());
    sb.append("#clause lits  ").append(clausesLiterals).append(System.lineSeparator());
    sb.append("#learnts lits ").append(learntsLiterals).append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * Class containing the information required for generating a proof.
   */
  public static class ProofInformation {
    private final LNGIntVector clause;
    private final Proposition proposition;

    /**
     * Constructor.
     * @param clause      the clause
     * @param proposition the proposition
     */
    public ProofInformation(LNGIntVector clause, Proposition proposition) {
      this.clause = clause;
      this.proposition = proposition;
    }

    /**
     * Returns the clause.
     * @return the clause
     */
    public LNGIntVector clause() {
      return clause;
    }

    /**
     * Returns the proposition.
     * @return the proposition
     */
    public Proposition proposition() {
      return proposition;
    }

    @Override
    public String toString() {
      return "ProofInformation{" +
              "clause=" + clause +
              ", proposition=" + proposition +
              '}';
    }
  }
}
