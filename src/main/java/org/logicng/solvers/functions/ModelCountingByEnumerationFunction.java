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

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class ModelCountingByEnumerationFunction extends AbstractModelEnumerationFunction<Long> {


    ModelCountingByEnumerationFunction(final AdvancedModelEnumerationHandler handler, final Collection<Variable> variables,
                                       final Collection<Variable> additionalVariables, final boolean fastEvaluable,
                                       final SplitVariableProvider splitVariableProvider, final int maxNumberOfModels) {
        super(handler, variables, additionalVariables, fastEvaluable, splitVariableProvider, maxNumberOfModels);
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    EnumerationCollector<Long> newCollector() {
        return new ModelCountCollector();
    }

    /**
     * The builder for a model enumeration function.
     */
    public static class Builder {
        protected AdvancedModelEnumerationHandler handler;
        protected Collection<Variable> variables;
        protected Collection<Variable> additionalVariables;
        protected boolean fastEvaluable = false;
        protected SplitVariableProvider splitVariableProvider = null;
        protected int maxNumberOfModels = 1000;

        Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the model enumeration handler for this function
         * @param handler the handler
         * @return the current builder
         */
        public Builder handler(final AdvancedModelEnumerationHandler handler) {
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
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = variables;
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Variable... variables) {
            this.additionalVariables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets the flag whether the created assignment should be {@link Assignment#fastEvaluable() fast evaluable} assignments.
         * @param fastEvaluable {@code true} if the created assignment should be fast evaluable, otherwise {@code false}
         * @return the builder
         */
        public Builder fastEvaluable(final boolean fastEvaluable) {
            this.fastEvaluable = fastEvaluable;
            return this;
        }

        /**
         * Sets the split variable provider. If no split variable provider is given, enumeration is performed without splits. Else the enumeration is
         * performed with the split variables provided by the {@link SplitVariableProvider}.
         * @param splitVariableProvider the given split variable provider
         * @return the builder
         */
        public Builder splitVariableProvider(final SplitVariableProvider splitVariableProvider) {
            this.splitVariableProvider = splitVariableProvider;
            return this;
        }

        public Builder maxNumberOfModels(final int maxNumberOfModels) {
            this.maxNumberOfModels = maxNumberOfModels;
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public ModelCountingByEnumerationFunction build() {
            return new ModelCountingByEnumerationFunction(this.handler, this.variables, this.additionalVariables, this.fastEvaluable,
                    this.splitVariableProvider, this.maxNumberOfModels);
        }
    }

    static class ModelEnumerationCollector implements EnumerationCollector<List<Model>> {
        private final List<Model> committedModels = new ArrayList<>();
        private final List<Model> uncommittedModels = new ArrayList<>();

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final Collection<Variable> additionalVarsNotOnSolver, final AdvancedModelEnumerationHandler handler) {
            final Model model = solver.createModel(modelFromSolver, relevantAllIndices);
            this.uncommittedModels.add(model);
            return handler == null || handler.foundModel();
        }

        @Override
        public boolean commit(final AdvancedModelEnumerationHandler handler) {
            this.committedModels.addAll(this.uncommittedModels);
            this.uncommittedModels.clear();
            return handler == null || handler.commit();
        }

        @Override
        public boolean rollback(final AdvancedModelEnumerationHandler handler) {
            this.uncommittedModels.clear();
            return handler == null || handler.rollback();
        }

        @Override
        public List<Model> rollbackAndReturnModels(final MiniSat solver, final AdvancedModelEnumerationHandler handler) {
            final List<Model> modelsToReturn = new ArrayList<>(this.uncommittedModels);
            rollback(handler);
            return modelsToReturn;
        }

        @Override
        public List<Model> getResult() {
            return this.committedModels;
        }
    }

    static class ModelCountCollector implements EnumerationCollector<Long> {
        private long committedCount = 0;
        private final List<LNGBooleanVector> uncommittedModels = new ArrayList<>(100);
        private final List<LNGIntVector> uncommittedIndices = new ArrayList<>(100);

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final Collection<Variable> additionalVarsNotOnSolver, final AdvancedModelEnumerationHandler handler) {
            this.uncommittedModels.add(modelFromSolver);
            this.uncommittedIndices.add(relevantAllIndices);
            return handler == null || handler.foundModel();
        }

        @Override
        public boolean commit(final AdvancedModelEnumerationHandler handler) {
            this.committedCount += this.uncommittedModels.size();
            return clearUncommitted(handler);
        }

        @Override
        public boolean rollback(final AdvancedModelEnumerationHandler handler) {
            return clearUncommitted(handler);
        }

        @Override
        public List<Model> rollbackAndReturnModels(final MiniSat solver, final AdvancedModelEnumerationHandler handler) {
            final List<Model> modelsToReturn = new ArrayList<>(this.uncommittedModels.size());
            for (int i = 0; i < this.uncommittedModels.size(); i++) {
                modelsToReturn.add(solver.createModel(this.uncommittedModels.get(i), this.uncommittedIndices.get(i)));
            }
            rollback(handler);
            return modelsToReturn;
        }

        @Override
        public Long getResult() {
            return this.committedCount;
        }

        private boolean clearUncommitted(final AdvancedModelEnumerationHandler handler) {
            this.uncommittedModels.clear();
            this.uncommittedIndices.clear();
            return handler == null || handler.commit();
        }
    }
}
