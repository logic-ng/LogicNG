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
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.maxsat.encodings.Encoder;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.io.PrintStream;

/**
 * Linear search solver.
 * @version 2.0.0
 * @since 1.0
 */
public class LinearUS extends MaxSAT {

    protected final Encoder encoder;
    protected final MaxSATConfig.IncrementalStrategy incrementalStrategy;
    protected final LNGIntVector objFunction;
    protected final PrintStream output;
    protected MiniSatStyleSolver solver;

    /**
     * Constructs a new solver with default values.
     */
    public LinearUS() {
        this(MaxSATConfig.builder().build());
    }

    /**
     * Constructs a new solver with a given configuration.
     * @param config the configuration
     */
    public LinearUS(final MaxSATConfig config) {
        super(config);
        this.solver = null;
        this.verbosity = config.verbosity;
        this.incrementalStrategy = config.incrementalStrategy;
        this.encoder = new Encoder(config.cardinalityEncoding);
        this.objFunction = new LNGIntVector();
        this.output = config.output;
    }

    @Override
    public MaxSATResult search() {
        if (this.problemType == ProblemType.WEIGHTED) {
            throw new IllegalStateException("Error: Currently LinearUS does not support weighted MaxSAT instances.");
        }
        switch (this.incrementalStrategy) {
            case NONE:
                return this.none();
            case ITERATIVE:
                if (this.encoder.cardEncoding() != MaxSATConfig.CardinalityEncoding.TOTALIZER) {
                    throw new IllegalStateException("Error: Currently iterative encoding in LinearUS only supports the Totalizer encoding.");
                }
                return this.iterative();
            default:
                throw new IllegalArgumentException("Unknown incremental strategy: " + this.incrementalStrategy);
        }
    }

    protected MaxSATResult none() {
        this.nbInitialVariables = nVars();
        Tristate res;
        this.initRelaxation();
        this.solver = this.rebuildSolver();
        final LNGIntVector assumptions = new LNGIntVector();
        this.encoder.setIncremental(MaxSATConfig.IncrementalStrategy.NONE);
        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler, assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
                saveModel(this.solver.model());
                if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                    this.output.println("o " + newCost);
                }
                this.ubCost = newCost;
                if (this.nbSatisfiable == 1) {
                    if (!foundUpperBound(this.ubCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                    if (this.encoder.cardEncoding() == MaxSATConfig.CardinalityEncoding.MTOTALIZER) {
                        this.encoder.setModulo((int) Math.ceil(Math.sqrt(this.ubCost + 1.0)));
                    }
                    this.encoder.encodeCardinality(this.solver, this.objFunction, 0);
                } else {
                    return MaxSATResult.OPTIMUM;
                }
            } else {
                this.lbCost++;
                if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                    this.output.println("c LB : " + this.lbCost);
                }
                if (this.nbSatisfiable == 0) {
                    return MaxSATResult.UNSATISFIABLE;
                } else if (this.lbCost == this.ubCost) {
                    if (this.nbSatisfiable > 0) {
                        if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                            this.output.println("c LB = UB");
                        }
                        return MaxSATResult.OPTIMUM;
                    } else {
                        return MaxSATResult.UNSATISFIABLE;
                    }
                } else if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                this.solver = this.rebuildSolver();
                this.encoder.encodeCardinality(this.solver, this.objFunction, this.lbCost);
            }
        }
    }

    protected MaxSATResult iterative() {
        assert this.encoder.cardEncoding() == MaxSATConfig.CardinalityEncoding.TOTALIZER;
        this.nbInitialVariables = nVars();
        Tristate res;
        this.initRelaxation();
        this.solver = this.rebuildSolver();
        final LNGIntVector assumptions = new LNGIntVector();
        this.encoder.setIncremental(MaxSATConfig.IncrementalStrategy.ITERATIVE);
        while (true) {
            final SATHandler satHandler = satHandler();
            res = searchSATSolver(this.solver, satHandler, assumptions);
            if (aborted(satHandler)) {
                return MaxSATResult.UNDEF;
            } else if (res == Tristate.TRUE) {
                this.nbSatisfiable++;
                final int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
                saveModel(this.solver.model());
                if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                    this.output.println("o " + newCost);
                }
                this.ubCost = newCost;
                if (this.nbSatisfiable == 1) {
                    if (!foundUpperBound(this.ubCost, null)) {
                        return MaxSATResult.UNDEF;
                    }
                    for (int i = 0; i < this.objFunction.size(); i++) {
                        assumptions.push(not(this.objFunction.get(i)));
                    }
                } else {
                    assert this.lbCost == this.ubCost;
                    return MaxSATResult.OPTIMUM;
                }
            } else {
                this.nbCores++;
                this.lbCost++;
                if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                    this.output.println("c LB : " + this.lbCost);
                }
                if (this.nbSatisfiable == 0) {
                    return MaxSATResult.UNSATISFIABLE;
                }
                if (this.lbCost == this.ubCost) {
                    if (this.nbSatisfiable > 0) {
                        if (this.verbosity != MaxSATConfig.Verbosity.NONE) {
                            this.output.println("c LB = UB");
                        }
                        return MaxSATResult.OPTIMUM;
                    } else {
                        return MaxSATResult.UNSATISFIABLE;
                    }
                }
                if (!foundLowerBound(this.lbCost, null)) {
                    return MaxSATResult.UNDEF;
                }
                if (!this.encoder.hasCardEncoding()) {
                    this.encoder.buildCardinality(this.solver, this.objFunction, this.lbCost);
                }
                final LNGIntVector join = new LNGIntVector();
                this.encoder.incUpdateCardinality(this.solver, join, this.objFunction, this.lbCost, assumptions);
            }
        }
    }

    protected MiniSatStyleSolver rebuildSolver() {
        final MiniSatStyleSolver s = newSATSolver();
        for (int i = 0; i < nVars(); i++) {
            newSATVariable(s);
        }
        for (int i = 0; i < nHard(); i++) {
            s.addClause(this.hardClauses.get(i).clause(), null);
        }
        LNGIntVector clause;
        for (int i = 0; i < nSoft(); i++) {
            clause = new LNGIntVector(this.softClauses.get(i).clause());
            for (int j = 0; j < this.softClauses.get(i).relaxationVars().size(); j++) {
                clause.push(this.softClauses.get(i).relaxationVars().get(j));
            }
            s.addClause(clause, null);
        }
        return s;
    }

    protected void initRelaxation() {
        for (int i = 0; i < this.nbSoft; i++) {
            final int l = newLiteral(false);
            this.softClauses.get(i).relaxationVars().push(l);
            this.softClauses.get(i).setAssumptionVar(l);
            this.objFunction.push(l);
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
