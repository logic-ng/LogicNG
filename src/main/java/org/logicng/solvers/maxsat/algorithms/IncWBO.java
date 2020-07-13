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

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.WeightStrategy;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;
import static org.logicng.solvers.sat.MiniSatStyleSolver.sign;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.solvers.maxsat.encodings.Encoder;
import org.logicng.util.Pair;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.TreeMap;

/**
 * Incremental WBO solver.
 * @version 2.0.0
 * @since 1.0
 */
public class IncWBO extends WBO {

    protected final Encoder encoder;
    protected final LNGBooleanVector incSoft;
    protected final PrintStream output;
    protected boolean firstBuild;

    /**
     * Constructs a new solver with default values.
     */
    public IncWBO() {
        this(MaxSATConfig.builder().build());
    }

    /**
     * Constructs a new solver with a given configuration.
     * @param config the configuration
     */
    public IncWBO(final MaxSATConfig config) {
        super(config);
        this.solver = null;
        this.verbosity = config.verbosity;
        this.nbCurrentSoft = 0;
        this.weightStrategy = config.weightStrategy;
        this.symmetryStrategy = config.symmetry;
        this.symmetryBreakingLimit = config.limit;
        this.firstBuild = true;
        this.coreMapping = new TreeMap<>();
        this.assumptions = new LNGIntVector();
        this.indexSoftCore = new LNGIntVector();
        this.softMapping = new LNGVector<>();
        this.relaxationMapping = new LNGVector<>();
        this.duplicatedSymmetryClauses = new HashSet<>();
        this.encoder = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
        this.encoder.setAMOEncoding(config.amoEncoding);
        this.incSoft = new LNGBooleanVector();
        this.output = config.output;
    }

    @Override
    public MaxSATResult search() {
        this.nbInitialVariables = nVars();
        if (this.currentWeight == 1) {
            this.problemType = ProblemType.UNWEIGHTED;
            this.weightStrategy = WeightStrategy.NONE;
        }
        if (this.symmetryStrategy) {
            initSymmetry();
        }
        if (this.problemType == ProblemType.UNWEIGHTED || this.weightStrategy == WeightStrategy.NONE) {
            return this.normalSearch();
        } else if (this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY) {
            return this.weightSearch();
        }
        throw new IllegalArgumentException("Unknown problem type.");
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }

    protected void incrementalBuildWeightSolver(final WeightStrategy strategy) {
        assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
        if (this.firstBuild) {
            this.solver = newSATSolver();
            for (int i = 0; i < nVars(); i++) {
                newSATVariable(this.solver);
            }
            for (int i = 0; i < nHard(); i++) {
                this.solver.addClause(this.hardClauses.get(i).clause(), null);
            }
            if (this.symmetryStrategy) {
                this.symmetryBreaking();
            }
            this.firstBuild = false;
        }
        LNGIntVector clause;
        this.nbCurrentSoft = 0;
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() >= this.currentWeight && this.softClauses.get(i).weight() != 0) {
                this.nbCurrentSoft++;
                clause = new LNGIntVector(this.softClauses.get(i).clause());
                for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                    clause.push(this.softClauses.get(i).relaxationVars().get(j));
                }
                clause.push(this.softClauses.get(i).assumptionVar());
                this.solver.addClause(clause, null);
            }
        }
    }

    protected void relaxCore(final LNGIntVector conflict, final int weightCore) {
        assert conflict.size() > 0;
        assert weightCore > 0;
        final LNGIntVector lits = new LNGIntVector();
        for (int i = 0; i < conflict.size(); i++) {
            final int indexSoft = this.coreMapping.get(conflict.get(i));
            if (this.softClauses.get(indexSoft).weight() == weightCore) {
                final LNGIntVector clause = new LNGIntVector(this.softClauses.get(indexSoft).clause());
                final LNGIntVector vars = new LNGIntVector(this.softClauses.get(indexSoft).relaxationVars());
                final int p = newLiteral(false);
                newSATVariable(this.solver);
                vars.push(p);
                lits.push(p);
                addSoftClause(weightCore, clause, vars);
                final int l = newLiteral(false);
                newSATVariable(this.solver);
                this.softClauses.get(nSoft() - 1).setAssumptionVar(l);
                this.coreMapping.put(l, nSoft() - 1);
                this.incSoft.set(indexSoft, true);
                this.incSoft.push(false);
                for (int j = 0; j < vars.size(); j++) {
                    clause.push(vars.get(j));
                }
                clause.push(l);
                this.solver.addClause(clause, null);
                clause.clear();
                clause.push(this.softClauses.get(indexSoft).assumptionVar());
                this.solver.addClause(clause, null);
                if (this.symmetryStrategy) {
                    this.softMapping.push(new LNGIntVector(this.softMapping.get(indexSoft)));
                    this.softMapping.get(indexSoft).clear();
                    this.relaxationMapping.push(new LNGIntVector(this.relaxationMapping.get(indexSoft)));
                    this.relaxationMapping.get(indexSoft).clear();
                    symmetryLog(nSoft() - 1);
                }
            } else {
                assert this.softClauses.get(indexSoft).weight() - weightCore > 0;
                this.softClauses.get(indexSoft).setWeight(this.softClauses.get(indexSoft).weight() - weightCore);
                LNGIntVector clause = new LNGIntVector(this.softClauses.get(indexSoft).clause());
                LNGIntVector vars = new LNGIntVector(this.softClauses.get(indexSoft).relaxationVars());
                addSoftClause(this.softClauses.get(indexSoft).weight(), clause, vars);
                if (this.symmetryStrategy) {
                    this.softMapping.push(new LNGIntVector(this.softMapping.get(indexSoft)));
                    this.softMapping.get(indexSoft).clear();
                    this.relaxationMapping.push(new LNGIntVector(this.relaxationMapping.get(indexSoft)));
                    this.relaxationMapping.get(indexSoft).clear();
                }
                this.incSoft.set(indexSoft, true);
                int l = newLiteral(false);
                newSATVariable(this.solver);
                this.softClauses.get(nSoft() - 1).setAssumptionVar(l);
                this.coreMapping.put(l, nSoft() - 1);
                this.incSoft.push(false);
                for (int j = 0; j < vars.size(); j++) {
                    clause.push(vars.get(j));
                }
                clause.push(l);
                this.solver.addClause(clause, null);
                clause.clear();
                vars.clear();
                clause = new LNGIntVector(this.softClauses.get(indexSoft).clause());
                vars = new LNGIntVector(this.softClauses.get(indexSoft).relaxationVars());
                l = newLiteral(false);
                newSATVariable(this.solver);
                vars.push(l);
                lits.push(l);
                addSoftClause(weightCore, clause, vars);
                l = newLiteral(false);
                newSATVariable(this.solver);
                this.softClauses.get(nSoft() - 1).setAssumptionVar(l);
                this.coreMapping.put(l, nSoft() - 1);
                this.incSoft.push(false);
                for (int j = 0; j < vars.size(); j++) {
                    clause.push(vars.get(j));
                }
                clause.push(l);
                this.solver.addClause(clause, null);
                clause.clear();
                clause.push(this.softClauses.get(indexSoft).assumptionVar());
                this.solver.addClause(clause, null);
                if (this.symmetryStrategy) {
                    this.softMapping.push(new LNGIntVector());
                    this.relaxationMapping.push(new LNGIntVector());
                    symmetryLog(nSoft() - 1);
                }
            }
        }
        this.encoder.encodeAMO(this.solver, lits);
        this.nbVars = this.solver.nVars();
        if (this.symmetryStrategy) {
            this.symmetryBreaking();
        }
        this.sumSizeCores += conflict.size();
    }

    @Override
    protected void symmetryBreaking() {
        if (this.indexSoftCore.size() != 0 && this.nbSymmetryClauses < this.symmetryBreakingLimit) {
            final LNGIntVector[] coreIntersection = new LNGIntVector[this.nbCores];
            final LNGIntVector[] coreIntersectionCurrent = new LNGIntVector[this.nbCores];
            for (int i = 0; i < this.nbCores; i++) {
                coreIntersection[i] = new LNGIntVector();
                coreIntersectionCurrent[i] = new LNGIntVector();
            }
            final LNGIntVector coreList = new LNGIntVector();
            for (int i = 0; i < this.indexSoftCore.size(); i++) {
                final int p = this.indexSoftCore.get(i);
                final LNGIntVector addCores = new LNGIntVector();
                for (int j = 0; j < this.softMapping.get(p).size() - 1; j++) {
                    final int core = this.softMapping.get(p).get(j);
                    addCores.push(core);
                    if (coreIntersection[core].size() == 0) {
                        coreList.push(core);
                    }
                    assert j < this.relaxationMapping.get(p).size();
                    assert var(this.relaxationMapping.get(p).get(j)) > this.nbInitialVariables;
                    coreIntersection[core].push(this.relaxationMapping.get(p).get(j));
                }
                for (int j = 0; j < addCores.size(); j++) {
                    final int core = addCores.get(j);
                    final int b = this.softMapping.get(p).size() - 1;
                    assert b < this.relaxationMapping.get(p).size();
                    assert var(this.relaxationMapping.get(p).get(b)) > this.nbInitialVariables;
                    coreIntersectionCurrent[core].push(this.relaxationMapping.get(p).get(b));
                }
                for (int k = 0; k < coreList.size(); k++) {
                    for (int m = 0; m < coreIntersection[coreList.get(k)].size(); m++) {
                        for (int j = m + 1; j < coreIntersectionCurrent[coreList.get(k)].size(); j++) {
                            final LNGIntVector clause = new LNGIntVector();
                            clause.push(not(coreIntersection[coreList.get(k)].get(m)));
                            clause.push(not(coreIntersectionCurrent[coreList.get(k)].get(j)));
                            Pair<Integer, Integer> symClause = new Pair<>(var(coreIntersection[coreList.get(k)].get(m)), var(coreIntersectionCurrent[coreList.get(k)].get(j)));
                            if (var(coreIntersection[coreList.get(k)].get(m)) > var(coreIntersectionCurrent[coreList.get(k)].get(j))) {
                                symClause = new Pair<>(var(coreIntersectionCurrent[coreList.get(k)].get(j)), var(coreIntersection[coreList.get(k)].get(m)));
                            }
                            if (!this.duplicatedSymmetryClauses.contains(symClause)) {
                                this.duplicatedSymmetryClauses.add(symClause);
                                this.solver.addClause(clause, null);
                                this.nbSymmetryClauses++;
                                if (this.symmetryBreakingLimit == this.nbSymmetryClauses) {
                                    break;
                                }
                            }
                        }
                        if (this.symmetryBreakingLimit == this.nbSymmetryClauses) {
                            break;
                        }
                    }
                    if (this.symmetryBreakingLimit == this.nbSymmetryClauses) {
                        break;
                    }
                }
                if (this.symmetryBreakingLimit == this.nbSymmetryClauses) {
                    break;
                }
            }
        }
        this.indexSoftCore.clear();
    }

    @Override
    protected MaxSATResult weightSearch() {
        assert this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY;
        final Tristate unsatResult = unsatSearch();
        if (unsatResult == UNDEF) {
            return MaxSATResult.UNDEF;
        } else if (unsatResult == FALSE) {
            return MaxSATResult.UNSATISFIABLE;
        }
        initAssumptions(this.assumptions);
        updateCurrentWeight(this.weightStrategy);
        this.incrementalBuildWeightSolver(this.weightStrategy);
        this.incSoft.growTo(nSoft(), false);
        while (true) {
            this.assumptions.clear();
            for (int i = 0; i < this.incSoft.size(); i++) {
                if (!this.incSoft.get(i)) {
                    this.assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                }
            }
            final Tristate res = searchSATSolver(this.solver, satHandler(), this.assumptions);
            satSolverFinished();
            if (res == UNDEF) {
                return MaxSATResult.UNDEF;
            } else if (res == FALSE) {
                this.nbCores++;
                assert this.solver.conflict().size() > 0;
                final int coreCost = computeCostCore(this.solver.conflict());
                this.lbCost += coreCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.println(String.format("c LB : %d CS : %d W : %d", this.lbCost, this.solver.conflict().size(), coreCost));
                }
                if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                this.relaxCore(this.solver.conflict(), coreCost);
                this.incrementalBuildWeightSolver(this.weightStrategy);
            } else {
                this.nbSatisfiable++;
                if (this.nbCurrentSoft == nSoft()) {
                    assert this.incComputeCostModel(this.solver.model()) == this.lbCost;
                    if (this.lbCost == this.ubCost && this.verbosity != Verbosity.NONE) {
                        this.output.println("c LB = UB");
                    }
                    if (this.lbCost < this.ubCost) {
                        this.ubCost = this.lbCost;
                        saveModel(this.solver.model());
                        if (this.verbosity != Verbosity.NONE) {
                            this.output.println("o " + this.lbCost);
                        }
                    }
                    return MaxSATResult.OPTIMUM;
                } else {
                    updateCurrentWeight(this.weightStrategy);
                    final int cost = this.incComputeCostModel(this.solver.model());
                    if (cost < this.ubCost) {
                        this.ubCost = cost;
                        saveModel(this.solver.model());
                        if (this.verbosity != Verbosity.NONE) {
                            this.output.println("o " + this.ubCost);
                        }
                    }
                    if (this.lbCost == this.ubCost) {
                        if (this.verbosity != Verbosity.NONE) {
                            this.output.println("c LB = UB");
                        }
                        return MaxSATResult.OPTIMUM;
                    } else if (!foundUpperBound(this.ubCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                    this.incrementalBuildWeightSolver(this.weightStrategy);
                }
            }
        }
    }

    protected int incComputeCostModel(final LNGBooleanVector currentModel) {
        assert currentModel.size() != 0;
        int currentCost = 0;
        for (int i = 0; i < nSoft(); i++) {
            boolean unsatisfied = true;
            for (int j = 0; j < this.softClauses.get(i).clause().size(); j++) {
                if (this.incSoft.get(i)) {
                    unsatisfied = false;
                    continue;
                }
                assert var(this.softClauses.get(i).clause().get(j)) < currentModel.size();
                if ((sign(this.softClauses.get(i).clause().get(j)) && !currentModel.get(var(this.softClauses.get(i).clause().get(j)))) ||
                        (!sign(this.softClauses.get(i).clause().get(j)) && currentModel.get(var(this.softClauses.get(i).clause().get(j))))) {
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

    @Override
    protected MaxSATResult normalSearch() {
        final Tristate unsatResult = unsatSearch();
        if (unsatResult == UNDEF) {
            return MaxSATResult.UNDEF;
        } else if (unsatResult == FALSE) {
            return MaxSATResult.UNSATISFIABLE;
        }
        initAssumptions(this.assumptions);
        this.solver = rebuildSolver();
        this.incSoft.growTo(nSoft(), false);
        while (true) {
            this.assumptions.clear();
            for (int i = 0; i < this.incSoft.size(); i++) {
                if (!this.incSoft.get(i)) {
                    this.assumptions.push(not(this.softClauses.get(i).assumptionVar()));
                }
            }
            final Tristate res = searchSATSolver(this.solver, satHandler(), this.assumptions);
            satSolverFinished();
            if (res == UNDEF) {
                return MaxSATResult.UNDEF;
            } else if (res == FALSE) {
                this.nbCores++;
                assert this.solver.conflict().size() > 0;
                final int coreCost = computeCostCore(this.solver.conflict());
                this.lbCost += coreCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.println(String.format("c LB : %d CS : %d W : %d", this.lbCost, this.solver.conflict().size(), coreCost));
                }
                if (this.lbCost == this.ubCost) {
                    if (this.verbosity != Verbosity.NONE) {
                        this.output.println("c LB = UB");
                    }
                    return MaxSATResult.OPTIMUM;
                }
                if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                this.relaxCore(this.solver.conflict(), coreCost);
            } else {
                this.nbSatisfiable++;
                this.ubCost = this.incComputeCostModel(this.solver.model());
                assert this.lbCost == this.ubCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.println("o " + this.lbCost);
                }
                saveModel(this.solver.model());
                return MaxSATResult.OPTIMUM;
            }
        }
    }
}
