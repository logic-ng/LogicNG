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

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.handlers.Handler.start;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link #builder()}.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class ModelEnumerationFunction implements SolverFunction<List<Assignment>> {

    private final ModelEnumerationHandler handler;
    private final Collection<Variable> variables;
    private final Collection<Variable> additionalVariables;

    private ModelEnumerationFunction(final ModelEnumerationHandler handler, final Collection<Variable> variables,
                                     final Collection<Variable> additionalVariables) {
        this.handler = handler;
        this.variables = variables;
        this.additionalVariables = additionalVariables;
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public List<Assignment> apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        start(this.handler);
        final List<Assignment> models = new ArrayList<>();
        SolverState stateBeforeEnumeration = null;
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            stateBeforeEnumeration = solver.saveState();
        }
        boolean proceed = true;
        final LNGIntVector relevantIndices;
        if (this.variables == null) {
            if (!solver.getConfig().isAuxiliaryVariablesInModels()) {
                relevantIndices = new LNGIntVector();
                for (final Map.Entry<String, Integer> entry : solver.underlyingSolver().getName2idx().entrySet()) {
                    if (solver.isRelevantVariable(entry.getKey())) {
                        relevantIndices.push(entry.getValue());
                    }
                }
            } else {
                relevantIndices = null;
            }
        } else {
            relevantIndices = new LNGIntVector(this.variables.size());
            for (final Variable var : this.variables) {
                relevantIndices.push(solver.underlyingSolver().idxForName(var.name()));
            }
        }
        LNGIntVector relevantAllIndices = null;
        final SortedSet<Variable> uniqueAdditionalVariables = new TreeSet<>(this.additionalVariables == null ? Collections.emptyList() : this.additionalVariables);
        if (this.variables != null) {
            uniqueAdditionalVariables.removeAll(this.variables);
        }
        if (relevantIndices != null) {
            if (uniqueAdditionalVariables.isEmpty()) {
                relevantAllIndices = relevantIndices;
            } else {
                relevantAllIndices = new LNGIntVector(relevantIndices.size() + uniqueAdditionalVariables.size());
                for (int i = 0; i < relevantIndices.size(); ++i) {
                    relevantAllIndices.push(relevantIndices.get(i));
                }
                for (final Variable var : uniqueAdditionalVariables) {
                    relevantAllIndices.push(solver.underlyingSolver().idxForName(var.name()));
                }
            }
        }
        while (proceed && modelEnumerationSATCall(solver, this.handler)) {
            final LNGBooleanVector modelFromSolver = solver.underlyingSolver().model();
            final Assignment model = solver.createAssignment(modelFromSolver, relevantAllIndices);
            models.add(model);
            proceed = this.handler == null || this.handler.foundModel(model);
            if (model.size() > 0) {
                final LNGIntVector blockingClause = generateBlockingClause(modelFromSolver, relevantIndices);
                solver.underlyingSolver().addClause(blockingClause, null);
                resultSetter.accept(UNDEF);
            } else {
                break;
            }
        }
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            solver.loadState(stateBeforeEnumeration);
        }
        return models;
    }

    private boolean modelEnumerationSATCall(final MiniSat solver, final ModelEnumerationHandler handler) {
        if (handler == null) {
            return solver.sat((SATHandler) null) == TRUE;
        }
        final Tristate tristate = solver.sat(handler.satHandler());
        return !handler.aborted() && tristate == TRUE;
    }

    /**
     * Generates a blocking clause from a given model and a set of relevant variables.
     * @param modelFromSolver the current model for which the blocking clause should be generated
     * @param relevantVars    the indices of the relevant variables.  If {@code null} all variables are relevant.
     * @return the blocking clause for the given model and relevant variables
     */
    private LNGIntVector generateBlockingClause(final LNGBooleanVector modelFromSolver, final LNGIntVector relevantVars) {
        final LNGIntVector blockingClause;
        if (relevantVars != null) {
            blockingClause = new LNGIntVector(relevantVars.size());
            for (int i = 0; i < relevantVars.size(); i++) {
                final int varIndex = relevantVars.get(i);
                if (varIndex != -1) {
                    final boolean varAssignment = modelFromSolver.get(varIndex);
                    blockingClause.push(varAssignment ? (varIndex * 2) ^ 1 : varIndex * 2);
                }
            }
        } else {
            blockingClause = new LNGIntVector(modelFromSolver.size());
            for (int i = 0; i < modelFromSolver.size(); i++) {
                final boolean varAssignment = modelFromSolver.get(i);
                blockingClause.push(varAssignment ? (i * 2) ^ 1 : i * 2);
            }
        }
        return blockingClause;
    }

    /**
     * The builder for a model enumeration function.
     */
    public static class Builder {
        private ModelEnumerationHandler handler;
        private Collection<Variable> variables;
        private Collection<Variable> additionalVariables;

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the model enumeration handler for this function
         * @param handler the handler
         * @return the current builder
         */
        public Builder handler(final ModelEnumerationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model.
         * @param variables the additional variables for each model
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
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public ModelEnumerationFunction build() {
            return new ModelEnumerationFunction(this.handler, this.variables, this.additionalVariables);
        }
    }
}
