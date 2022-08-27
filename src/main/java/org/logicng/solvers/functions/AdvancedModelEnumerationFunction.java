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

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class AdvancedModelEnumerationFunction extends AbstractModelEnumerationFunction<List<Model>> {


    AdvancedModelEnumerationFunction(final AdvancedModelEnumerationHandler handler, final Collection<Variable> variables,
                                     final Collection<Variable> additionalVariables, final boolean fastEvaluable,
                                     final SplitVariableProvider splitVariableProvider, final int maxNumberOfModels) {
        super(handler, variables, additionalVariables, fastEvaluable, splitVariableProvider, maxNumberOfModels);
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    EnumerationCollector<List<Model>> newCollector(final SortedSet<Variable> dontCareVariables, final SortedSet<Variable> additionalVariablesNotKnownBySolver) {
        return new ModelEnumerationCollector(dontCareVariables, additionalVariablesNotKnownBySolver);
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
            this.variables = asList(variables);
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
            this.additionalVariables = asList(variables);
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
        public AdvancedModelEnumerationFunction build() {
            return new AdvancedModelEnumerationFunction(this.handler, this.variables, this.additionalVariables, this.fastEvaluable,
                    this.splitVariableProvider, this.maxNumberOfModels);
        }
    }

    static class ModelEnumerationCollector implements EnumerationCollector<List<Model>> {
        private final List<Model> committedModels = new ArrayList<>();
        private final List<Model> uncommittedModels = new ArrayList<>();
        private final List<List<Literal>> baseModels;
        private final SortedSet<Variable> additionalVariablesNotKnownBySolver;

        public ModelEnumerationCollector(final SortedSet<Variable> dontCareVariables, final SortedSet<Variable> additionalVariablesNotKnownBySolver) {
            this.baseModels = getCartesianProduct(dontCareVariables);
            this.additionalVariablesNotKnownBySolver = additionalVariablesNotKnownBySolver;
        }

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final AdvancedModelEnumerationHandler handler) {
            final Model model = solver.createModel(modelFromSolver, relevantAllIndices);
            final List<Literal> modelLiterals = new ArrayList<>(this.additionalVariablesNotKnownBySolver);
            modelLiterals.addAll(model.getLiterals());
            final List<Model> allModels = new ArrayList<>(this.baseModels.size());
            for (final List<Literal> baseModel : this.baseModels) {
                final List<Literal> completeModel = new ArrayList<>(baseModel.size() + modelLiterals.size());
                completeModel.addAll(baseModel);
                completeModel.addAll(modelLiterals);
                allModels.add(new Model(completeModel));
            }
            this.uncommittedModels.addAll(allModels);
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

        static List<List<Literal>> getCartesianProduct(final Collection<Variable> dontCares) {
            if (dontCares.isEmpty()) {
                return singletonList(Collections.emptyList());
            }
            final List<List<Literal>> modelsToFactorOut = dontCares.stream().map(v -> asList(v, v.negate())).collect(Collectors.toList());
            List<List<Literal>> currentResult = singletonList(Collections.emptyList());
            for (final List<Literal> newModels : modelsToFactorOut) {
                final List<List<Literal>> newResult = new ArrayList<>();
                for (final Literal newModel : newModels) {
                    for (final List<Literal> existingModel : currentResult) {
                        final List<Literal> extendedModel = new ArrayList<>(existingModel);
                        extendedModel.add(newModel);
                        newResult.add(extendedModel);
                    }
                }
                currentResult = newResult;
            }
            return currentResult;
        }
    }
}
