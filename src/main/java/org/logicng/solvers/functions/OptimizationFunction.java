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

package org.logicng.solvers.functions;

import org.logicng.cardinalityconstraints.CCIncrementalData;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * A solver function for computing a model for the formula on the solver
 * which has a global minimum or maximum of satisfied literals. If the formula
 * is UNSAT, {@code null} will be returned.
 * <p>
 * Optimization functions are instantiated via their builder {@link #builder()}.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class OptimizationFunction implements SolverFunction<Assignment> {

    private static final String SEL_PREFIX = "@SEL_OPT_";

    private final Collection<? extends Literal> literals;
    private final SortedSet<Variable> resultModelVariables;
    private final boolean maximize;

    private OptimizationFunction(final Collection<? extends Literal> literals, final Collection<Variable> additionalVariables, final boolean maximize) {
        this.literals = literals;
        this.resultModelVariables = new TreeSet<>(additionalVariables);
        for (final Literal lit : literals) {
            this.resultModelVariables.add(lit.variable());
        }
        this.maximize = maximize;
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new OptimizationFunction.Builder();
    }

    /**
     * Returns a optimization function which maximizes the given set of literals.
     * @param literals the literals to maximize
     * @return the solver function
     */
    public static OptimizationFunction maximize(final Collection<? extends Literal> literals) {
        return new Builder().literals(literals).maximize().build();
    }

    /**
     * Returns a optimization function which minimizes the given set of literals.
     * @param literals the literals to minimize
     * @return the solver function
     */
    public static OptimizationFunction minimize(final Collection<? extends Literal> literals) {
        return new Builder().literals(literals).minimize().build();
    }

    @Override
    public Assignment apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        SolverState initialState = null;
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            initialState = solver.saveState();
        }
        final Assignment model = maximize(solver);
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            solver.loadState(initialState);
        }
        return model;
    }

    private Assignment maximize(final MiniSat solver) {
        final FormulaFactory f = solver.factory();
        LNGBooleanVector internalModel;
        final Map<Variable, Literal> selectorMap = new TreeMap<>();
        for (final Literal lit : this.literals) {
            final Variable selVar = f.variable(SEL_PREFIX + selectorMap.size());
            selectorMap.put(selVar, lit);
        }
        final Set<Variable> selectors = selectorMap.keySet();
        if (this.maximize) {
            selectorMap.forEach((selVar, lit) -> solver.add(f.or(selVar.negate(), lit)));
            selectorMap.forEach((selVar, lit) -> solver.add(f.or(lit.negate(), selVar)));
        } else {
            selectorMap.forEach((selVar, lit) -> solver.add(f.or(selVar.negate(), lit.negate())));
            selectorMap.forEach((selVar, lit) -> solver.add(f.or(lit, selVar)));
        }
        if (solver.sat() != Tristate.TRUE) {
            return null;
        }
        internalModel = solver.underlyingSolver().model();
        Assignment currentModel = solver.model(selectors);
        int currentBound = currentModel.positiveVariables().size();
        if (currentBound == 0) {
            solver.add(f.cc(CType.GE, 1, selectors));
            if (solver.sat() == Tristate.FALSE) {
                return mkResultModel(solver, internalModel);
            } else {
                internalModel = solver.underlyingSolver().model();
                currentModel = solver.model(selectors);
                currentBound = currentModel.positiveVariables().size();
            }
        } else if (currentBound == selectors.size()) {
            return mkResultModel(solver, internalModel);
        }
        final Formula cc = f.cc(CType.GE, currentBound + 1, selectors);
        assert cc instanceof CardinalityConstraint;
        final CCIncrementalData incrementalData = solver.addIncrementalCC((CardinalityConstraint) cc);
        while (solver.sat() == Tristate.TRUE) {
            internalModel = solver.underlyingSolver().model();
            currentModel = solver.model(selectors);
            currentBound = currentModel.positiveVariables().size();
            if (currentBound == selectors.size()) {
                return mkResultModel(solver, internalModel);
            }
            incrementalData.newLowerBoundForSolver(currentBound + 1);
        }
        return mkResultModel(solver, internalModel);
    }

    private Assignment mkResultModel(final MiniSat solver, final LNGBooleanVector internalModel) {
        final LNGIntVector relevantIndices = new LNGIntVector(this.resultModelVariables.size());
        for (final Variable var : this.resultModelVariables) {
            relevantIndices.push(solver.underlyingSolver().idxForName(var.name()));
        }
        return solver.createAssignment(internalModel, relevantIndices);
    }

    /**
     * The builder for an optimization function.
     */
    public static class Builder {
        private Collection<? extends Literal> literals;
        private Collection<Variable> additionalVariables = new TreeSet<>();
        private boolean maximize = true;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the set of literals that should be optimized s.t. the number of satisfied literals is maximized or minimized.
         * @param literals the set of literals
         * @return the current builder
         */
        public Builder literals(final Collection<? extends Literal> literals) {
            this.literals = literals;
            return this;
        }

        /**
         * Sets the set of literals that should be optimized s.t. the number of satisfied literals is maximized or minimized.
         * @param literals the set of literals
         * @return the current builder
         */
        public Builder literals(final Literal... literals) {
            this.literals = Arrays.asList(literals);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in the resulting model.
         * @param variables the additional variables for the resulting model
         * @return the current builder
         */
        public Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = variables;
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Variable... variables) {
            this.additionalVariables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets the optimization goal to minimize.
         * @return the current builder
         */
        public Builder minimize() {
            this.maximize = false;
            return this;
        }

        /**
         * Sets the optimization goal to maximize.
         * @return the current builder
         */
        public Builder maximize() {
            this.maximize = true;
            return this;
        }

        /**
         * Builds the optimization function with the current builder's configuration.
         * @return the optimization function
         */
        public OptimizationFunction build() {
            return new OptimizationFunction(this.literals, this.additionalVariables, this.maximize);
        }
    }
}
