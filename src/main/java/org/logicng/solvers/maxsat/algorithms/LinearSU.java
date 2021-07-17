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

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.datastructures.MSSoftClause;
import org.logicng.solvers.maxsat.encodings.Encoder;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.io.PrintStream;

/**
 * Linear search solver with Boolean Multilevel Optimization (BMO)
 * @version 2.0.0
 * @since 1.0
 */
public class LinearSU extends MaxSAT {

    protected final Encoder encoder;
    protected final boolean bmoMode;  // Enables BMO mode.
    protected final LNGIntVector objFunction; // Literals to be used in the constraint that excludes models.
    protected final LNGIntVector coeffs; // Coefficients of the literals that are used in the constraint that excludes models.
    protected final PrintStream output;
    protected MiniSatStyleSolver solver;
    protected boolean isBmo; // Stores if the formula is BMO or not.

    /**
     * Constructs a new solver with default values.
     */
    public LinearSU() {
        this(MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).build());
    }

    /**
     * Constructs a new solver with a given configuration.
     * @param config the configuration
     */
    public LinearSU(final MaxSATConfig config) {
        super(config);
        this.solver = null;
        this.encoder = new Encoder(config.cardinalityEncoding);
        this.encoder.setPBEncoding(config.pbEncoding);
        this.verbosity = config.verbosity;
        this.bmoMode = config.bmo;
        this.isBmo = false;
        this.objFunction = new LNGIntVector();
        this.coeffs = new LNGIntVector();
        this.output = config.output;
    }

    @Override
    public MaxSATResult search() {
        this.nbInitialVariables = nVars();
        if (this.currentWeight == 1) {
            this.problemType = ProblemType.UNWEIGHTED;
        } else {
            this.isBmo = isBMO(true);
        }
        if (this.problemType == ProblemType.WEIGHTED) {
            if (this.bmoMode && this.isBmo) {
                return this.bmoSearch();
            } else {
                return this.normalSearch();
            }
        } else {
            return this.normalSearch();
        }
    }

    protected MaxSATResult bmoSearch() {
        assert this.orderWeights.size() > 0;
        Tristate res;
        this.initRelaxation();
        int currentWeight = this.orderWeights.get(0);
        final int minWeight = this.orderWeights.get(this.orderWeights.size() - 1);
        int posWeight = 0;
        final LNGVector<LNGIntVector> functions = new LNGVector<>();
        final LNGIntVector weights = new LNGIntVector();
        this.solver = this.rebuildBMO(functions, weights, currentWeight);
        int localCost = 0;
        this.ubCost = 0;
        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            }
            if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final int newCost = computeCostModel(this.solver.model(), currentWeight);
                if (currentWeight == minWeight) {
                    saveModel(this.solver.model());
                    if (this.verbosity != Verbosity.NONE) {
                        this.output.println("o " + (newCost + this.lbCost));
                    }
                    this.ubCost = newCost + this.lbCost;
                    if (newCost > 0 && !foundUpperBound(this.ubCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                } else if (this.verbosity != Verbosity.NONE) {
                    this.output.println(String.format("c BMO-UB : %d (Function %d/%d)", newCost, posWeight + 1, this.orderWeights.size()));
                }
                if (newCost == 0 && currentWeight == minWeight) {
                    return MaxSATResult.OPTIMUM;
                } else {
                    if (newCost == 0) {
                        functions.push(new LNGIntVector(this.objFunction));
                        localCost = newCost;
                        weights.push(localCost / currentWeight);
                        posWeight++;
                        currentWeight = this.orderWeights.get(posWeight);
                        localCost = 0;
                        this.solver = this.rebuildBMO(functions, weights, currentWeight);
                        if (this.verbosity != Verbosity.NONE) {
                            this.output.println("c LB : " + this.lbCost);
                        }
                    } else {
                        if (localCost == 0) {
                            this.encoder.encodeCardinality(this.solver, this.objFunction, newCost / currentWeight - 1);
                        } else {
                            this.encoder.updateCardinality(this.solver, newCost / currentWeight - 1);
                        }
                        localCost = newCost;
                    }
                }
            } else {
                this.nbCores++;
                if (currentWeight == minWeight) {
                    if (this.model.size() == 0) {
                        assert this.nbSatisfiable == 0;
                        return MaxSATResult.UNSATISFIABLE;
                    } else {
                        return MaxSATResult.OPTIMUM;
                    }
                } else {
                    functions.push(new LNGIntVector(this.objFunction));
                    weights.push(localCost / currentWeight);
                    this.lbCost += localCost;
                    posWeight++;
                    currentWeight = this.orderWeights.get(posWeight);
                    localCost = 0;
                    if (!foundLowerBound(this.lbCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                    this.solver = this.rebuildBMO(functions, weights, currentWeight);
                    if (this.verbosity != Verbosity.NONE) {
                        this.output.println("c LB : " + this.lbCost);
                    }
                }
            }
        }
    }

    protected MaxSATResult normalSearch() {
        Tristate res;
        this.initRelaxation();
        this.solver = this.rebuildSolver(1);
        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
                saveModel(this.solver.model());
                if (this.verbosity != Verbosity.NONE) {
                    this.output.println("o " + newCost);
                }
                if (newCost == 0) {
                    this.ubCost = newCost;
                    return MaxSATResult.OPTIMUM;
                } else {
                    if (this.problemType == ProblemType.WEIGHTED) {
                        if (!this.encoder.hasPBEncoding()) {
                            this.encoder.encodePB(this.solver, this.objFunction, this.coeffs, newCost - 1);
                        } else {
                            this.encoder.updatePB(this.solver, newCost - 1);
                        }
                    } else {
                        if (!this.encoder.hasCardEncoding()) {
                            this.encoder.encodeCardinality(this.solver, this.objFunction, newCost - 1);
                        } else {
                            this.encoder.updateCardinality(this.solver, newCost - 1);
                        }
                    }
                    this.ubCost = newCost;
                    if (!foundUpperBound(this.ubCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                }
            } else {
                this.nbCores++;
                if (this.model.size() == 0) {
                    assert this.nbSatisfiable == 0;
                    return MaxSATResult.UNSATISFIABLE;
                } else {
                    return MaxSATResult.OPTIMUM;
                }
            }
        }
    }

    /**
     * Rebuilds a SAT solver with the current MaxSAT formula.
     * @param minWeight the minimal weight
     * @return the rebuilt solver
     */
    protected MiniSatStyleSolver rebuildSolver(final int minWeight) {
        final LNGBooleanVector seen = new LNGBooleanVector(nVars());
        seen.growTo(nVars(), false);
        final MiniSatStyleSolver s = newSATSolver();
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() < minWeight) {
                continue;
            }
            final LNGIntVector clause = new LNGIntVector(this.softClauses.get(i).clause());
            for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                clause.push(this.softClauses.get(i).relaxationVars().get(j));
            }
            s.addClause(clause, null);
        }
        return s;
    }

    /**
     * Rebuilds a SAT solver with the current MaxSAT formula
     * @param functions     the functions
     * @param rhs           the right hand side
     * @param currentWeight the current weight
     * @return the rebuilt solver
     */
    protected MiniSatStyleSolver rebuildBMO(final LNGVector<LNGIntVector> functions, final LNGIntVector rhs, final int currentWeight) {
        assert functions.size() == rhs.size();
        final MiniSatStyleSolver s = this.rebuildSolver(currentWeight);
        this.objFunction.clear();
        this.coeffs.clear();
        for (int i = 0; i < nSoft(); i++) {
            if (this.softClauses.get(i).weight() == currentWeight) {
                this.objFunction.push(this.softClauses.get(i).relaxationVars().get(0));
                this.coeffs.push(this.softClauses.get(i).weight());
            }
        }
        for (int i = 0; i < functions.size(); i++) {
            this.encoder.encodeCardinality(s, functions.get(i), rhs.get(i));
        }
        return s;
    }

    /**
     * Initializes the relaxation variables by adding a fresh variable to the 'relaxationVars' of each soft clause.
     */
    protected void initRelaxation() {
        for (final MSSoftClause softClause : this.softClauses) {
            final int l = newLiteral(false);
            softClause.relaxationVars().push(l);
            this.objFunction.push(l);
            this.coeffs.push(softClause.weight());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
