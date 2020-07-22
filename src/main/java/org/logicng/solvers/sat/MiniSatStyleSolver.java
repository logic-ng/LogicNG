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
 */

package org.logicng.solvers.sat;

import static org.logicng.datastructures.Tristate.UNDEF;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneType;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.datastructures.LNGHeap;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.datastructures.MSWatcher;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * The super class for all MiniSAT-style solvers.
 * @version 2.0.0
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
    protected boolean shouldRemoveSatsisfied;
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

    // backbone computation
    protected Stack<Integer> backboneCandidates;
    protected LNGIntVector backboneAssumptions;
    protected HashMap<Integer, Tristate> backboneMap;
    protected boolean computingBackbone;

    // Selection order
    protected LNGIntVector selectionOrder;
    protected int selectionOrderIdx;

    protected double learntsizeAdjustConfl;
    protected int learntsizeAdjustCnt;
    protected int learntsizeAdjustStartConfl;
    protected double learntsizeAdjustInc;
    protected double maxLearnts;

    /**
     * Constructs a new MiniSAT-style solver with a given configuration.
     * @param config the configuration
     */
    protected MiniSatStyleSolver(final MiniSatConfig config) {
        this.config = config;
        this.initialize();
    }

    /**
     * Returns the name-to-index mapping for variables.
     * @return the name-to-index mapping
     */
    public Map<String, Integer> getName2idx() {
        return this.name2idx;
    }

    /**
     * Creates a literal for a given variable number and literal.
     * @param var  the variable number
     * @param sign {@code true} if the literal is negative, {@code false} otherwise
     * @return the literal (as integer value)
     */
    public static int mkLit(final int var, final boolean sign) {
        return var + var + (sign ? 1 : 0);
    }

    /**
     * Negates a given literal.
     * @param lit the literal
     * @return the negated literal
     */
    public static int not(final int lit) {
        return lit ^ 1;
    }

    /**
     * Returns {@code true} if a given literal is negated, {@code false} otherwise.
     * @param lit the literal
     * @return {@code true} if the literal is negated
     */
    public static boolean sign(final int lit) {
        return (lit & 1) == 1;
    }

    /**
     * Returns the variable index for a given literal.
     * @param lit the literal
     * @return the variable index of the literal
     */
    public static int var(final int lit) {
        return lit >> 1;
    }

    /**
     * Computes the next number in the Luby sequence.
     * @param y the restart increment
     * @param x the current number of restarts
     * @return the next number in the Luby sequence
     */
    protected static double luby(final double y, final int x) {
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
        this.computingBackbone = false;
        this.selectionOrder = new LNGIntVector();
        this.selectionOrderIdx = 0;
    }

    /**
     * Initializes the solver configuration.
     */
    protected void initializeConfig() {
        this.varDecay = this.config.varDecay;
        this.varInc = this.config.varInc;
        this.ccminMode = this.config.clauseMin;
        this.restartFirst = this.config.restartFirst;
        this.restartInc = this.config.restartInc;
        this.clauseDecay = this.config.clauseDecay;
        this.shouldRemoveSatsisfied = this.config.removeSatisfied;
        this.learntsizeFactor = this.config.learntsizeFactor;
        this.learntsizeInc = this.config.learntsizeInc;
        this.incremental = this.config.incremental;
    }

    /**
     * Returns the variable for a given literal.
     * @param lit the literal
     * @return the variable of the literal
     */
    protected MSVariable v(final int lit) {
        return this.vars.get(lit >> 1);
    }

    /**
     * Returns the assigned value of a given literal.
     * @param lit the literal
     * @return the assigned value of the literal
     */
    protected Tristate value(final int lit) {
        return sign(lit) ? Tristate.negate(this.v(lit).assignment()) : this.v(lit).assignment();
    }

    /**
     * Compares two variables by their activity.
     * @param x the first variable
     * @param y the second variable
     * @return {@code true} if the first variable's activity is larger then the second one's
     */
    public boolean lt(final int x, final int y) {
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
    public String nameForIdx(final int var) {
        return this.idx2name.get(var);
    }

    /**
     * Adds a new variable name with a given variable index to this solver.
     * @param name the variable name
     * @param id   the variable index
     */
    public void addName(final String name, final int id) {
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
    public boolean addClause(final int lit, final Proposition proposition) {
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
     * Returns {@code false} if this solver is known to be in a conflicting state, otherwise {@code true}.
     * @return {@code false} if this solver is known to be in a conflicting state, otherwise {@code true}
     */
    public boolean ok() {
        return this.ok;
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
        return this.name2idx;
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
    protected int abstractLevel(final int x) {
        return 1 << (this.vars.get(x).level() & 31);
    }

    /**
     * Inserts a variable (given by its index) into the heap of decision variables.
     * @param x the variable index
     */
    protected void insertVarOrder(final int x) {
        if (!this.orderHeap.inHeap(x) && this.vars.get(x).decision()) {
            this.orderHeap.insert(x);
        }
    }

    /**
     * Picks the next branching literal.
     * @return the literal or -1 if there are no unassigned literals left
     */
    protected int pickBranchLit() {
        if (this.selectionOrder.size() > 0 && this.selectionOrderIdx < this.selectionOrder.size()) {
            while (this.selectionOrderIdx < this.selectionOrder.size()) {
                final int lit = this.selectionOrder.get(this.selectionOrderIdx++);
                final int var = var(lit);
                final MSVariable msVariable = this.vars.get(var);
                if (msVariable.assignment() == UNDEF) {
                    return lit;
                }
            }
        }
        int next = -1;
        while (next == -1 || this.vars.get(next).assignment() != UNDEF || !this.vars.get(next).decision()) {
            if (this.orderHeap.empty()) {
                return -1;
            } else {
                next = this.orderHeap.removeMin();
            }
        }
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
    protected void varBumpActivity(final int v) {
        this.varBumpActivity(v, this.varInc);
    }

    /**
     * Bumps the activity of the variable at a given index by a given value.
     * @param v   the variable index
     * @param inc the increment value
     */
    protected void varBumpActivity(final int v, final double inc) {
        final MSVariable var = this.vars.get(v);
        var.incrementActivity(inc);
        if (var.activity() > 1e100) {
            for (final MSVariable variable : this.vars) {
                variable.rescaleActivity();
            }
            this.varInc *= 1e-100;
        }
        if (this.orderHeap.inHeap(v)) {
            this.orderHeap.decrease(v);
        }
    }

    /**
     * Rebuilds the heap of decision variables.
     */
    protected void rebuildOrderHeap() {
        final LNGIntVector vs = new LNGIntVector();
        for (int v = 0; v < this.nVars(); v++) {
            if (this.vars.get(v).decision() && this.vars.get(v).assignment() == UNDEF) {
                vs.push(v);
            }
        }
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
        this.claInc *= (1 / this.clauseDecay);
    }

    /**
     * Bumps the activity of the given clause.
     * @param c the clause
     */
    protected void claBumpActivity(final MSClause c) {
        c.incrementActivity(this.claInc);
        if (c.activity() > 1e20) {
            for (final MSClause clause : this.learnts) {
                clause.rescaleActivity();
            }
            this.claInc *= 1e-20;
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

    protected void cancelUntil(final int level) {
        if (decisionLevel() > level) {
            if (!this.computingBackbone) {
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

    protected void decayActivities() {
        varDecayActivity();
        if (!this.incremental) {
            claDecayActivity();
        }
        if (--this.learntsizeAdjustCnt == 0) {
            this.learntsizeAdjustConfl *= this.learntsizeAdjustInc;
            this.learntsizeAdjustCnt = (int) this.learntsizeAdjustConfl;
            this.maxLearnts *= this.learntsizeInc;
        }
    }

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
        sb.append("ok            ").append(this.ok).append(System.lineSeparator());
        sb.append("qhead         ").append(this.qhead).append(System.lineSeparator());
        sb.append("#clauses      ").append(this.clauses.size()).append(System.lineSeparator());
        sb.append("#learnts      ").append(this.learnts.size()).append(System.lineSeparator());
        sb.append("#watches      ").append(this.watches.size()).append(System.lineSeparator());
        sb.append("#vars         ").append(this.vars.size()).append(System.lineSeparator());
        sb.append("#orderheap    ").append(this.orderHeap.size()).append(System.lineSeparator());
        sb.append("#trail        ").append(this.trail.size()).append(System.lineSeparator());
        sb.append("#trailLim     ").append(this.trailLim.size()).append(System.lineSeparator());

        sb.append("model         ").append(this.model).append(System.lineSeparator());
        sb.append("conflict      ").append(this.conflict).append(System.lineSeparator());
        sb.append("assumptions   ").append(this.assumptions).append(System.lineSeparator());
        sb.append("#seen         ").append(this.seen.size()).append(System.lineSeparator());
        sb.append("#stack        ").append(this.analyzeStack.size()).append(System.lineSeparator());
        sb.append("#toclear      ").append(this.analyzeToClear.size()).append(System.lineSeparator());

        sb.append("claInc        ").append(this.claInc).append(System.lineSeparator());
        sb.append("simpDBAssigns ").append(this.simpDBAssigns).append(System.lineSeparator());
        sb.append("simpDBProps   ").append(this.simpDBProps).append(System.lineSeparator());
        sb.append("#clause lits  ").append(this.clausesLiterals).append(System.lineSeparator());
        sb.append("#learnts lits ").append(this.learntsLiterals).append(System.lineSeparator());
        return sb.toString();
    }

    /**
     * Class containing the information required for generating a proof.
     */
    public static class ProofInformation {
        protected final LNGIntVector clause;
        protected final Proposition proposition;

        /**
         * Constructor.
         * @param clause      the clause
         * @param proposition the proposition
         */
        public ProofInformation(final LNGIntVector clause, final Proposition proposition) {
            this.clause = clause;
            this.proposition = proposition;
        }

        /**
         * Returns the clause.
         * @return the clause
         */
        public LNGIntVector clause() {
            return this.clause;
        }

        /**
         * Returns the proposition.
         * @return the proposition
         */
        public Proposition proposition() {
            return this.proposition;
        }

        @Override
        public String toString() {
            return "ProofInformation{" +
                    "clause=" + this.clause +
                    ", proposition=" + this.proposition +
                    '}';
        }
    }

    /**
     * Returns the unit propagated literals on level zero.
     * @return unit propagated literal on level zero
     */
    public LNGIntVector upZeroLiterals() {
        final LNGIntVector upZeroLiterals = new LNGIntVector();
        for (int i = 0; i < this.trail.size(); ++i) {
            final int lit = this.trail.get(i);
            if (v(lit).level() > 0) {
                break;
            } else {
                upZeroLiterals.push(lit);
            }
        }
        return upZeroLiterals;
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
    protected List<Integer> getRelevantVarIndices(final Collection<Variable> variables) {
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
    protected void initBackboneDS(final List<Integer> variables) {
        this.backboneCandidates = new Stack<>();
        this.backboneAssumptions = new LNGIntVector(variables.size());
        this.backboneMap = new HashMap<>();
        for (final Integer var : variables) {
            this.backboneMap.put(var, UNDEF);
        }
    }

    /**
     * Computes the backbone for the given variables.
     * @param variables variables to test
     * @param type      the type of the backbone
     */
    protected void computeBackbone(final List<Integer> variables, final BackboneType type) {
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
     * @param type      the type of the backbone
     * @return initial candidates
     */
    protected Stack<Integer> createInitialCandidates(final List<Integer> variables, final BackboneType type) {
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
    protected void refineUpperBound() {
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
    protected boolean solveWithLit(final int lit) {
        this.backboneAssumptions.push(not(lit));
        final boolean sat = solve(null, this.backboneAssumptions) == Tristate.TRUE;
        this.backboneAssumptions.pop();
        return sat;
    }

    /**
     * Builds the backbone object from the computed backbone literals.
     * @param variables relevant variables
     * @param type      the type of the backbone
     * @return backbone
     */
    protected Backbone buildBackbone(final Collection<Variable> variables, final BackboneType type) {
        final SortedSet<Variable> posBackboneVars = isBothOrPositiveType(type) ? new TreeSet<>() : null;
        final SortedSet<Variable> negBackboneVars = isBothOrNegativeType(type) ? new TreeSet<>() : null;
        final SortedSet<Variable> optionalVars = isBothType(type) ? new TreeSet<>() : null;
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
    protected boolean isUPZeroLit(final int var) {
        return this.vars.get(var).level() == 0;
    }

    /**
     * Tests the given literal whether it is unit in the given clause.
     * @param lit    literal to test
     * @param clause clause containing the literal
     * @return {@code true} if the literal is unit, {@code false} otherwise
     */
    protected boolean isUnit(final int lit, final MSClause clause) {
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
    protected boolean isRotatable(final int lit) {
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
    protected void addBackboneLiteral(final int lit) {
        this.backboneMap.put(var(lit), sign(lit) ? Tristate.FALSE : Tristate.TRUE);
        this.backboneAssumptions.push(lit);
    }

    protected boolean isBothOrPositiveType(final BackboneType type) {
        return type == BackboneType.POSITIVE_AND_NEGATIVE || type == BackboneType.ONLY_POSITIVE;
    }

    protected boolean isBothOrNegativeType(final BackboneType type) {
        return type == BackboneType.POSITIVE_AND_NEGATIVE || type == BackboneType.ONLY_NEGATIVE;
    }

    protected boolean isBothType(final BackboneType type) {
        return type == BackboneType.POSITIVE_AND_NEGATIVE;
    }

    /**
     * Returns the clauses loaded on the solver.
     * @return the clauses loaded on the solver
     */
    public LNGVector<MSClause> clauses() {
        return this.clauses;
    }

    /**
     * Returns the variables known by the solver.
     * @return the variables
     */
    public LNGVector<MSVariable> variables() {
        return this.vars;
    }

    /**
     * Sets the variable's selection order that is used to solve the formula on the solver.
     * <p>
     * If a custom selection order is set, the solver will pick a variable from the custom order in order to branch on it during the search.
     * The given polarity in the selection order is used as assignment for the variable.
     * If all variables in the custom order are already assigned, the solver falls back to the activity based variable selection.
     * @param selectionOrder the custom selection order
     */
    public void setSelectionOrder(final List<? extends Literal> selectionOrder) {
        this.selectionOrder.clear();
        for (final Literal literal : selectionOrder) {
            final Integer var = this.name2idx.get(literal.name());
            if (var != null) {
                this.selectionOrder.push(mkLit(var, !literal.phase()));
            }
        }
    }

    /**
     * Resets a previously set selection order.
     */
    public void resetSelectionOrder() {
        this.selectionOrder.clear();
    }
}
