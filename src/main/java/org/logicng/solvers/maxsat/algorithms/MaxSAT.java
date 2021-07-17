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

package org.logicng.solvers.maxsat.algorithms;

import static org.logicng.handlers.Handler.start;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.SolverType;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.sat.MiniSatStyleSolver.LIT_UNDEF;
import static org.logicng.solvers.sat.MiniSatStyleSolver.mkLit;
import static org.logicng.solvers.sat.MiniSatStyleSolver.sign;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.datastructures.MSHardClause;
import org.logicng.solvers.datastructures.MSSoftClause;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.GlucoseSyrup;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.Locale;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Super class for the MaxSAT solvers.
 * @version 2.0.0
 * @since 1.0
 */
public abstract class MaxSAT {

    /**
     * The MaxSAT problem type: {@code UNWEIGHTED} or {@code WEIGHTED}.
     */
    public enum ProblemType {
        UNWEIGHTED, WEIGHTED
    }

    /**
     * The MaxSAT result type: {@code SATISFIABLE}, {@code UNSATISFIABLE}, {@code OPTIMUM}, or {@code UNDEF}.
     */
    public enum MaxSATResult {
        UNSATISFIABLE, OPTIMUM, UNDEF
    }

    protected final LNGBooleanVector model;
    final LNGVector<MSSoftClause> softClauses;
    final LNGVector<MSHardClause> hardClauses;
    final LNGIntVector orderWeights;
    final SolverType solverType;
    protected Verbosity verbosity;
    protected MaxSATHandler handler;
    int hardWeight;
    ProblemType problemType;
    int nbVars;
    int nbSoft;
    int nbHard;
    int nbInitialVariables;
    int nbCores;
    int nbSymmetryClauses;
    long sumSizeCores;
    int nbSatisfiable;
    int ubCost;
    int lbCost;
    int currentWeight;

    /**
     * Constructor.
     * @param config the solver configuration
     */
    protected MaxSAT(final MaxSATConfig config) {
        this.hardWeight = 0;
        this.hardClauses = new LNGVector<>();
        this.softClauses = new LNGVector<>();
        this.hardWeight = Integer.MAX_VALUE;
        this.problemType = ProblemType.UNWEIGHTED;
        this.nbVars = 0;
        this.nbSoft = 0;
        this.nbHard = 0;
        this.nbInitialVariables = 0;
        this.currentWeight = 1;
        this.model = new LNGBooleanVector();
        this.ubCost = 0;
        this.lbCost = 0;
        this.nbSymmetryClauses = 0;
        this.nbCores = 0;
        this.nbSatisfiable = 0;
        this.sumSizeCores = 0;
        this.orderWeights = new LNGIntVector();
        this.solverType = config.solverType;
        this.handler = null;
    }

    /**
     * Creates a new variable in the SAT solver.
     * @param s the SAT solver
     */
    public static void newSATVariable(final MiniSatStyleSolver s) {
        s.newVar(true, true);
    }

    /**
     * Solves the formula that is currently loaded in the SAT solver with a set of assumptions.
     * @param s           the SAT solver
     * @param satHandler  a SAT handler
     * @param assumptions the assumptions
     * @return the result of the solving process
     */
    public static Tristate searchSATSolver(final MiniSatStyleSolver s, final SATHandler satHandler, final LNGIntVector assumptions) {
        return s.solve(satHandler, assumptions);
    }

    /**
     * Solves the formula without assumptions.
     * @param s          the SAT solver
     * @param satHandler a SAT handler
     * @return the result of the solving process
     */
    public static Tristate searchSATSolver(final MiniSatStyleSolver s, final SATHandler satHandler) {
        return s.solve(satHandler);
    }

    /**
     * The main MaxSAT solving method.
     * @param handler a MaxSAT handler
     * @return the result of the solving process
     * @throws IllegalArgumentException if the configuration was not valid
     */
    public final MaxSATResult search(final MaxSATHandler handler) {
        this.handler = handler;
        start(handler);
        final MaxSATResult result = search();
        if (handler != null) {
            handler.finishedSolving();
        }
        this.handler = null;
        return result;
    }

    /**
     * The main MaxSAT solving method.
     * @return the result of the solving process
     * @throws IllegalArgumentException if the configuration was not valid
     */
    public abstract MaxSATResult search();

    /**
     * Returns the number of variables in the working MaxSAT formula.
     * @return the number of variables in the working MaxSAT formula
     */
    public int nVars() {
        return this.nbVars;
    }

    /**
     * Returns the number of soft clauses in the working MaxSAT formula.
     * @return the number of soft clauses in the working MaxSAT formula
     */
    public int nSoft() {
        return this.nbSoft;
    }

    /**
     * Returns the number of hard clauses in the working MaxSAT formula.
     * @return the number of hard clauses in the working MaxSAT formula
     */
    public int nHard() {
        return this.nbHard;
    }

    /**
     * Increases the number of variables in the working MaxSAT formula.
     */
    public void newVar() {
        this.nbVars++;
    }

    /**
     * Adds a new hard clause to the hard clause database.
     * @param lits the literals of the hard clause
     */
    public void addHardClause(final LNGIntVector lits) {
        this.hardClauses.push(new MSHardClause(lits));
        this.nbHard++;
    }

    /**
     * Adds a new soft clause to the soft clause database.
     * @param weight the weight of the soft clause
     * @param lits   the literals of the soft clause
     */
    public void addSoftClause(final int weight, final LNGIntVector lits) {
        final LNGIntVector rVars = new LNGIntVector();
        this.softClauses.push(new MSSoftClause(lits, weight, LIT_UNDEF, rVars));
        this.nbSoft++;
    }

    /**
     * Adds a new soft clause to the soft clause database with predefined relaxation variables.
     * @param weight the weight of the soft clause
     * @param lits   the literals of the soft clause
     * @param vars   the relaxation variables of the soft clause
     */
    public void addSoftClause(final int weight, final LNGIntVector lits, final LNGIntVector vars) {
        this.softClauses.push(new MSSoftClause(lits, weight, LIT_UNDEF, vars));
        this.nbSoft++;
    }

    /**
     * Creates a new literal to be used in the working MaxSAT formula.
     * @param sign the sign of the literal
     * @return the new literal
     */
    public int newLiteral(final boolean sign) {
        final int p = mkLit(this.nVars(), sign);
        this.newVar();
        return p;
    }

    /**
     * Sets the problem type.
     * @param type the problem type
     */
    public void setProblemType(final ProblemType type) {
        this.problemType = type;
    }

    /**
     * Initializes 'ubCost' to the sum of weights of the soft clauses
     * @param weight the weight
     */
    public void updateSumWeights(final int weight) {
        if (weight != this.hardWeight) {
            this.ubCost += weight;
        }
    }

    /**
     * Initializes the current weight to the maximum weight of the soft clauses.
     * @param weight the weight
     */
    public void setCurrentWeight(final int weight) {
        if (weight > this.currentWeight && weight != this.hardWeight) {
            this.currentWeight = weight;
        }
    }

    /**
     * Returns the current weight.
     * @return the current weight
     */
    public int currentWeight() {
        return this.currentWeight;
    }

    /**
     * Creates an empty SAT Solver.
     * @return the empty SAT solver
     */
    public MiniSatStyleSolver newSATSolver() {
        switch (this.solverType) {
            case GLUCOSE:
                return new GlucoseSyrup(MiniSatConfig.builder().incremental(true).build(),
                        GlucoseConfig.builder().build());
            case MINISAT:
                return new MiniSat2Solver(MiniSatConfig.builder().incremental(false).build());
            default:
                throw new IllegalStateException("Unknown solver type: " + this.solverType);
        }
    }

    /**
     * Saves the current model found by the SAT solver.
     * @param currentModel the model found by the solver
     */
    public void saveModel(final LNGBooleanVector currentModel) {
        assert this.nbInitialVariables != 0;
        assert currentModel.size() != 0;
        this.model.clear();
        for (int i = 0; i < this.nbInitialVariables; i++) {
            this.model.push(currentModel.get(i));
        }
    }

    /**
     * Computes the cost of a given model. The cost of a model is the sum of the weights of the unsatisfied soft
     * clauses.  If a weight is specified, then it only considers the sum of the weights of the unsatisfied soft clauses
     * with the specified weight.
     * @param currentModel the model
     * @param weight       the weight
     * @return the cost of the given model
     */
    public int computeCostModel(final LNGBooleanVector currentModel, final int weight) {
        assert currentModel.size() != 0;
        int currentCost = 0;
        for (int i = 0; i < nSoft(); i++) {
            boolean unsatisfied = true;
            for (int j = 0; j < this.softClauses.get(i).clause().size(); j++) {
                if (weight != Integer.MAX_VALUE && this.softClauses.get(i).weight() != weight) {
                    unsatisfied = false;
                    continue;
                }
                assert var(this.softClauses.get(i).clause().get(j)) < currentModel.size();
                if ((sign(this.softClauses.get(i).clause().get(j)) && !currentModel.get(var(this.softClauses.get(i).clause().get(j))))
                        || (!sign(this.softClauses.get(i).clause().get(j)) && currentModel.get(var(this.softClauses.get(i).clause().get(j))))) {
                    unsatisfied = false;
                    break;
                }
            }
            if (unsatisfied) {
                currentCost += this.softClauses.get(i).weight();
            }
        }
        return currentCost;
    }

    /**
     * Tests if the MaxSAT formula has lexicographical optimization criterion.
     * @param cache is indicates whether the result should be cached.
     * @return {@code true} if the formula has lexicographical optimization criterion
     */
    public boolean isBMO(final boolean cache) {
        assert this.orderWeights.size() == 0;
        boolean bmo = true;
        final SortedSet<Integer> partitionWeights = new TreeSet<>();
        final SortedMap<Integer, Integer> nbPartitionWeights = new TreeMap<>();
        for (int i = 0; i < nSoft(); i++) {
            final int weight = this.softClauses.get(i).weight();
            partitionWeights.add(weight);
            nbPartitionWeights.merge(weight, 1, Integer::sum);
        }
        for (final int i : partitionWeights) {
            this.orderWeights.push(i);
        }
        this.orderWeights.sortReverse();
        long totalWeights = 0;
        for (int i = 0; i < this.orderWeights.size(); i++) {
            totalWeights += this.orderWeights.get(i) * nbPartitionWeights.get(this.orderWeights.get(i));
        }
        for (int i = 0; i < this.orderWeights.size(); i++) {
            totalWeights -= this.orderWeights.get(i) * nbPartitionWeights.get(this.orderWeights.get(i));
            if (this.orderWeights.get(i) < totalWeights) {
                bmo = false;
                break;
            }
        }
        if (!cache) {
            this.orderWeights.clear();
        }
        return bmo;
    }

    /**
     * Returns the stats of this solver instance.
     * @return the stats of this solver instance
     */
    public Stats stats() {
        return new Stats();
    }

    /**
     * Returns the optimal result of the solver.
     * @return the optimal result of the solver
     */
    public int result() {
        return this.ubCost;
    }

    /**
     * Returns the model of the solver.
     * @return the model of the solver
     */
    public LNGBooleanVector model() {
        return this.model;
    }

    /**
     * Returns the current SAT handler or {@code null} if no MaxSAT handler was given.
     * @return the current SAT handler
     */
    SATHandler satHandler() {
        return this.handler == null ? null : this.handler.satHandler();
    }

    boolean foundLowerBound(final int lowerBound, final Assignment model) {
        return this.handler == null || this.handler.foundLowerBound(lowerBound, model);
    }

    boolean foundUpperBound(final int upperBound, final Assignment model) {
        return this.handler == null || this.handler.foundUpperBound(upperBound, model);
    }

    /**
     * The MaxSAT solver statistics.
     */
    public class Stats {
        protected final int ubC;
        protected final int nbS;
        protected final int nbC;
        protected final double avgCS;
        protected final int nbSC;

        protected Stats() {
            this.ubC = MaxSAT.this.model.size() == 0 ? -1 : MaxSAT.this.ubCost;
            this.nbS = MaxSAT.this.nbSatisfiable;
            this.nbC = MaxSAT.this.nbCores;
            this.avgCS = MaxSAT.this.nbCores != 0 ? (double) MaxSAT.this.sumSizeCores / MaxSAT.this.nbCores : 0.0;
            this.nbSC = MaxSAT.this.nbSymmetryClauses;
        }

        /**
         * Returns the best solution or -1 if there is none.
         * @return the best solution or -1 if there is none
         */
        public int bestSolution() {
            return this.ubC;
        }

        /**
         * Returns the number of SAT calls.
         * @return the number of SAT calls
         */
        public int satCalls() {
            return this.nbS;
        }

        /**
         * Returns the number of UNSAT calls (cores).
         * @return the number of UNSAT calls (cores)
         */
        public int unsatCalls() {
            return this.nbC;
        }

        /**
         * Returns the average core size.
         * @return the average core size
         */
        public double averageCoreSize() {
            return this.avgCS;
        }

        /**
         * Returns the number of symmetry clauses.
         * @return the number of symmetry clauses
         */
        public int symmetryClauses() {
            return this.nbSC;
        }

        @Override
        public String toString() {
            return String.format(Locale.ENGLISH,
                    "MaxSAT.Stats{best solution=%d, #sat calls=%d, #unsat calls=%d, average core size=%.2f, #symmetry clauses=%d}",
                    this.ubC, this.nbS, this.nbC, this.avgCS, this.nbSC);
        }
    }
}
