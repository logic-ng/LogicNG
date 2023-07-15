// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.handlers.Handler.aborted;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.WeightStrategy;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.sat.MiniSatStyleSolver;
import org.logicng.util.Pair;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Weighted Boolean Optimization solver.
 * @version 2.1.0
 * @since 1.0
 */
public class WBO extends MaxSAT {

    protected final PrintStream output;
    protected MiniSatStyleSolver solver;
    protected int nbCurrentSoft;
    protected WeightStrategy weightStrategy;
    protected SortedMap<Integer, Integer> coreMapping;
    protected LNGIntVector assumptions;
    protected boolean symmetryStrategy;
    protected LNGIntVector indexSoftCore;
    protected LNGVector<LNGIntVector> softMapping;
    protected LNGVector<LNGIntVector> relaxationMapping;
    protected Set<Pair<Integer, Integer>> duplicatedSymmetryClauses;
    protected int symmetryBreakingLimit;

    /**
     * Constructs a new solver with default values.
     */
    public WBO() {
        this(MaxSATConfig.builder().build());
    }

    /**
     * Constructs a new solver with a given configuration.
     * @param config the configuration
     */
    public WBO(final MaxSATConfig config) {
        super(config);
        this.solver = null;
        this.verbosity = config.verbosity;
        this.nbCurrentSoft = 0;
        this.weightStrategy = config.weightStrategy;
        this.symmetryStrategy = config.symmetry;
        this.symmetryBreakingLimit = config.limit;
        this.coreMapping = new TreeMap<>();
        this.assumptions = new LNGIntVector();
        this.indexSoftCore = new LNGIntVector();
        this.softMapping = new LNGVector<>();
        this.relaxationMapping = new LNGVector<>();
        this.duplicatedSymmetryClauses = new HashSet<>();
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
            this.initSymmetry();
        }
        if (this.problemType == ProblemType.UNWEIGHTED || this.weightStrategy == WeightStrategy.NONE) {
            return this.normalSearch();
        } else if (this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY) {
            return this.weightSearch();
        }
        throw new IllegalArgumentException("Unknown problem type.");
    }

    protected MiniSatStyleSolver rebuildWeightSolver(final WeightStrategy strategy) {
        assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
        final MiniSatStyleSolver s = newSATSolver();
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }
        if (this.symmetryStrategy) {
            this.symmetryBreaking();
        }
        LNGIntVector clause = new LNGIntVector();
        this.nbCurrentSoft = 0;
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() >= this.currentWeight) {
                this.nbCurrentSoft++;
                clause.clear();
                clause = new LNGIntVector(this.softClauses.get(i).clause());
                for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                    clause.push(this.softClauses.get(i).relaxationVars().get(j));
                }
                clause.push(this.softClauses.get(i).assumptionVar());

                s.addClause(clause, null);
            }
        }
        return s;
    }

    MiniSatStyleSolver rebuildSolver() {
        assert this.weightStrategy == WeightStrategy.NONE;
        final MiniSatStyleSolver s = newSATSolver();
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }
        if (this.symmetryStrategy) {
            this.symmetryBreaking();
        }
        LNGIntVector clause;
        for (int i = 0; i < nSoft(); i++) {
            clause = new LNGIntVector(this.softClauses.get(i).clause());
            for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                clause.push(this.softClauses.get(i).relaxationVars().get(j));
            }
            clause.push(this.softClauses.get(i).assumptionVar());
            s.addClause(clause, null);
        }
        return s;
    }

    protected MiniSatStyleSolver rebuildHardSolver() {
        final MiniSatStyleSolver s = newSATSolver();
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }
        return s;
    }

    void updateCurrentWeight(final WeightStrategy strategy) {
        assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
        if (strategy == WeightStrategy.NORMAL) {
            this.currentWeight = this.findNextWeight(this.currentWeight);
        } else if (strategy == WeightStrategy.DIVERSIFY) {
            this.currentWeight = this.findNextWeightDiversity(this.currentWeight);
        }
    }

    protected int findNextWeight(final int weight) {
        int nextWeight = 1;
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() > nextWeight && this.softClauses.get(i).weight() < weight) {
                nextWeight = this.softClauses.get(i).weight();
            }
        }
        return nextWeight;
    }

    protected int findNextWeightDiversity(final int weight) {
        assert this.weightStrategy == WeightStrategy.DIVERSIFY;
        assert this.nbSatisfiable > 0;
        int nextWeight = weight;
        int nbClauses;
        final SortedSet<Integer> nbWeights = new TreeSet<>();
        final double alpha = 1.25;
        boolean findNext = false;
        while (true) {
            if (this.nbSatisfiable > 1 || findNext) {
                nextWeight = this.findNextWeight(nextWeight);
            }
            nbClauses = 0;
            nbWeights.clear();
            for (int i = 0; i < nSoft(); i++) {
                if (this.softClauses.get(i).weight() >= nextWeight) {
                    nbClauses++;
                    nbWeights.add(this.softClauses.get(i).weight());
                }
            }
            if ((double) nbClauses / nbWeights.size() > alpha || nbClauses == nSoft()) {
                break;
            }
            if (this.nbSatisfiable == 1 && !findNext) {
                findNext = true;
            }
        }
        return nextWeight;
    }

    protected void encodeEO(final LNGIntVector lits) {
        assert lits.size() != 0;
        final LNGIntVector clause = new LNGIntVector();
        if (lits.size() == 1) {
            clause.push(lits.get(0));
            addHardClause(clause);
        } else {
            final LNGIntVector auxVariables = new LNGIntVector();
            for (int i = 0; i < lits.size() - 1; i++) {
                auxVariables.push(newLiteral(false));
            }
            for (int i = 0; i < lits.size(); i++) {
                if (i == 0) {
                    clause.clear();
                    clause.push(lits.get(i));
                    clause.push(not(auxVariables.get(i)));
                    addHardClause(clause);
                    clause.clear();
                    clause.push(not(lits.get(i)));
                    clause.push(auxVariables.get(i));
                    addHardClause(clause);
                } else if (i == lits.size() - 1) {
                    clause.clear();
                    clause.push(lits.get(i));
                    clause.push(auxVariables.get(i - 1));
                    addHardClause(clause);
                    clause.clear();
                    clause.push(not(lits.get(i)));
                    clause.push(not(auxVariables.get(i - 1)));
                    addHardClause(clause);
                } else {
                    clause.clear();
                    clause.push(not(auxVariables.get(i - 1)));
                    clause.push(auxVariables.get(i));
                    addHardClause(clause);
                    clause.clear();
                    clause.push(lits.get(i));
                    clause.push(not(auxVariables.get(i)));
                    clause.push(auxVariables.get(i - 1));
                    addHardClause(clause);
                    clause.clear();
                    clause.push(not(lits.get(i)));
                    clause.push(auxVariables.get(i));
                    addHardClause(clause);
                    clause.clear();
                    clause.push(not(lits.get(i)));
                    clause.push(not(auxVariables.get(i - 1)));
                    addHardClause(clause);
                }
            }
        }
    }

    protected void relaxCore(final LNGIntVector conflict, final int weightCore, final LNGIntVector assumps) {
        assert conflict.size() > 0;
        assert weightCore > 0;
        final LNGIntVector lits = new LNGIntVector();
        for (int i = 0; i < conflict.size(); i++) {
            final int indexSoft = this.coreMapping.get(conflict.get(i));

            if (this.softClauses.get(indexSoft).weight() == weightCore) {
                final int p = newLiteral(false);
                this.softClauses.get(indexSoft).relaxationVars().push(p);
                lits.push(p);
                if (this.symmetryStrategy) {
                    this.symmetryLog(indexSoft);
                }
            } else {
                assert this.softClauses.get(indexSoft).weight() - weightCore > 0;
                this.softClauses.get(indexSoft).setWeight(this.softClauses.get(indexSoft).weight() - weightCore);
                final LNGIntVector clause = new LNGIntVector(this.softClauses.get(indexSoft).clause());
                final LNGIntVector vars = new LNGIntVector(this.softClauses.get(indexSoft).relaxationVars());
                final int p = newLiteral(false);
                vars.push(p);
                lits.push(p);
                addSoftClause(weightCore, clause, vars);
                final int l = newLiteral(false);
                this.softClauses.get(nSoft() - 1).setAssumptionVar(l);
                this.coreMapping.put(l, nSoft() - 1);  // Map the new soft clause to its assumption literal.
                assumps.push(not(l)); // Update the assumption vector.
                if (this.symmetryStrategy) {
                    this.symmetryLog(nSoft() - 1);
                }
            }
        }
        this.encodeEO(lits);
        this.sumSizeCores += conflict.size();
    }

    int computeCostCore(final LNGIntVector conflict) {
        assert conflict.size() != 0;
        if (this.problemType == ProblemType.UNWEIGHTED) {
            return 1;
        }
        int coreCost = Integer.MAX_VALUE;
        for (int i = 0; i < conflict.size(); i++) {
            final int indexSoft = this.coreMapping.get(conflict.get(i));
            if (this.softClauses.get(indexSoft).weight() < coreCost) {
                coreCost = this.softClauses.get(indexSoft).weight();
            }
        }
        return coreCost;
    }

    void initSymmetry() {
        for (int i = 0; i < nSoft(); i++) {
            this.softMapping.push(new LNGIntVector());
            this.relaxationMapping.push(new LNGIntVector());
        }
    }

    void symmetryLog(final int p) {
        if (this.nbSymmetryClauses < this.symmetryBreakingLimit) {
            while (this.softMapping.size() <= p) {
                this.softMapping.push(new LNGIntVector());
                this.relaxationMapping.push(new LNGIntVector());
            }
            this.softMapping.get(p).push(this.nbCores);
            this.relaxationMapping.get(p).push(this.softClauses.get(p).relaxationVars().back());
            if (this.softMapping.get(p).size() > 1) {
                this.indexSoftCore.push(p);
            }
        }
    }

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
                                addHardClause(clause);
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

    Tristate unsatSearch() {
        assert this.assumptions.size() == 0;
        this.solver = this.rebuildHardSolver();
        final SATHandler satHandler = satHandler();
        final Tristate res = searchSATSolver(this.solver, satHandler, this.assumptions);
        if (aborted(satHandler)) {
            return UNDEF;
        } else if (res == FALSE) {
            this.nbCores++;
        } else if (res == TRUE) {
            this.nbSatisfiable++;
            final int cost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
            assert cost <= this.ubCost;
            this.ubCost = cost;
            saveModel(this.solver.model());
            if (this.verbosity != Verbosity.NONE) {
                this.output.println("o " + this.ubCost);
            }
        }
        this.solver = null;
        return res;
    }

    protected MaxSATResult weightSearch() {
        assert this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY;
        final Tristate unsatResult = this.unsatSearch();
        if (unsatResult == UNDEF) {
            return MaxSATResult.UNDEF;
        } else if (unsatResult == FALSE) {
            return MaxSATResult.UNSATISFIABLE;
        }
        this.initAssumptions(this.assumptions);
        this.updateCurrentWeight(this.weightStrategy);
        this.solver = this.rebuildWeightSolver(this.weightStrategy);
        while (true) {
            final SATHandler satHandler = satHandler();
            final Tristate res = searchSATSolver(this.solver, satHandler, this.assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == FALSE) {
                this.nbCores++;
                assert this.solver.conflict().size() > 0;
                final int coreCost = this.computeCostCore(this.solver.conflict());
                this.lbCost += coreCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.printf("c LB : %d CS : %d W : %d%n", this.lbCost, this.solver.conflict().size(), coreCost);
                }
                if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                this.relaxCore(this.solver.conflict(), coreCost, this.assumptions);
                this.solver = this.rebuildWeightSolver(this.weightStrategy);
            } else {
                this.nbSatisfiable++;
                if (this.nbCurrentSoft == nSoft()) {
                    assert computeCostModel(this.solver.model(), Integer.MAX_VALUE) == this.lbCost;
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
                    this.updateCurrentWeight(this.weightStrategy);
                    final int cost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
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
                    this.solver = this.rebuildWeightSolver(this.weightStrategy);
                }
            }
        }
    }

    protected MaxSATResult normalSearch() {
        final Tristate unsatResult = this.unsatSearch();
        if (unsatResult == UNDEF) {
            return MaxSATResult.UNDEF;
        } else if (unsatResult == FALSE) {
            return MaxSATResult.UNSATISFIABLE;
        }
        this.initAssumptions(this.assumptions);
        this.solver = this.rebuildSolver();
        while (true) {
            final SATHandler satHandler = satHandler();
            final Tristate res = searchSATSolver(this.solver, satHandler, this.assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == FALSE) {
                this.nbCores++;
                assert this.solver.conflict().size() > 0;
                final int coreCost = this.computeCostCore(this.solver.conflict());
                this.lbCost += coreCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.printf("c LB : %d CS : %d W : %d%n", this.lbCost, this.solver.conflict().size(), coreCost);
                }
                if (this.lbCost == this.ubCost) {
                    if (this.verbosity != Verbosity.NONE) {
                        this.output.println("c LB = UB");
                    }
                    return MaxSATResult.OPTIMUM;
                } else if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                this.relaxCore(this.solver.conflict(), coreCost, this.assumptions);
                this.solver = this.rebuildSolver();
            } else {
                this.nbSatisfiable++;
                this.ubCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
                assert this.lbCost == this.ubCost;
                if (this.verbosity != Verbosity.NONE) {
                    this.output.println("o " + this.lbCost);
                }
                saveModel(this.solver.model());
                return MaxSATResult.OPTIMUM;
            }
        }
    }

    void initAssumptions(final LNGIntVector assumps) {
        for (int i = 0; i < this.nbSoft; i++) {
            final int l = newLiteral(false);
            this.softClauses.get(i).setAssumptionVar(l);
            this.coreMapping.put(l, i);
            assumps.push(not(l));
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
