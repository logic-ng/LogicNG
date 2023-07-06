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
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Model;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.modelenumeration.AbstractModelEnumerationFunction;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.EnumerationCollector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder}.
 * @version 2.5.0
 * @since 2.5.0
 */
public class AdvancedModelEnumerationFunction extends AbstractModelEnumerationFunction<List<Model>> {

    AdvancedModelEnumerationFunction(final SortedSet<Variable> variables, final SortedSet<Variable> additionalVariables,
                                     final AdvancedModelEnumerationConfig config) {
        super(variables, additionalVariables, configuration(variables, config));
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    protected EnumerationCollector<List<Model>> newCollector(final FormulaFactory f, final SortedSet<Variable> knownVariables,
                                                             final SortedSet<Variable> dontCareVariablesNotOnSolver, final SortedSet<Variable> additionalVariablesNotOnSolver) {
        return new ModelEnumerationCollector(dontCareVariablesNotOnSolver, additionalVariablesNotOnSolver);
    }

    /**
     * The builder for a model enumeration function.
     */
    public static class Builder {
        private SortedSet<Variable> variables;
        private SortedSet<Variable> additionalVariables;
        private AdvancedModelEnumerationConfig configuration;

        Builder() {
            // Initialize only via factory
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Collection<Variable> variables) {
            this.variables = new TreeSet<>(variables);
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        public Builder variables(final Variable... variables) {
            this.variables = new TreeSet<>(asList(variables));
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = new TreeSet<>(variables);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        public Builder additionalVariables(final Variable... variables) {
            this.additionalVariables = new TreeSet<>(asList(variables));
            return this;
        }

        /**
         * Sets the configuration for the model enumeration split algorithm.
         * @param configuration the configuration
         * @return the current builder
         */
        public Builder configuration(final AdvancedModelEnumerationConfig configuration) {
            this.configuration = configuration;
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public AdvancedModelEnumerationFunction build() {
            return new AdvancedModelEnumerationFunction(this.variables, this.additionalVariables, this.configuration);
        }
    }

    static class ModelEnumerationCollector implements EnumerationCollector<List<Model>> {
        private final List<Model> committedModels = new ArrayList<>();
        private final List<List<Literal>> uncommittedModels = new ArrayList<>();
        private final List<List<Literal>> baseModels;
        private final SortedSet<Variable> additionalVariablesNotOnSolver;

        public ModelEnumerationCollector(final SortedSet<Variable> dontCareVariablesNotOnSolver, final SortedSet<Variable> additionalVariablesNotOnSolver) {
            this.baseModels = getCartesianProduct(dontCareVariablesNotOnSolver);
            this.additionalVariablesNotOnSolver = additionalVariablesNotOnSolver;
        }

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final AdvancedModelEnumerationHandler handler) {
            if (handler == null || handler.foundModels(this.baseModels.size())) {
                final Model model = solver.createModel(modelFromSolver, relevantAllIndices);
                final List<Literal> modelLiterals = new ArrayList<>(this.additionalVariablesNotOnSolver);
                modelLiterals.addAll(model.getLiterals());
                this.uncommittedModels.add(modelLiterals);
                return true;
            } else {
                return false;
            }
        }

        @Override
        public boolean commit(final AdvancedModelEnumerationHandler handler) {
            this.committedModels.addAll(expandUncommittedModels());
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
            final List<Model> modelsToReturn = this.uncommittedModels.stream().map(Model::new).collect(Collectors.toList());
            rollback(handler);
            return modelsToReturn;
        }

        @Override
        public List<Model> getResult() {
            return this.committedModels;
        }

        private List<Model> expandUncommittedModels() {
            final List<Model> expanded = new ArrayList<>(this.baseModels.size());
            for (final List<Literal> baseModel : this.baseModels) {
                for (final List<Literal> uncommittedModel : this.uncommittedModels) {
                    final List<Literal> completeModel = new ArrayList<>(baseModel.size() + uncommittedModel.size());
                    completeModel.addAll(baseModel);
                    completeModel.addAll(uncommittedModel);
                    expanded.add(new Model(completeModel));
                }
            }
            return expanded;
        }

        /**
         * Returns the Cartesian product for the given variables, i.e. all combinations of literals are generated
         * with each variable occurring positively and negatively.
         * @param variables the variables, must not be {@code null}
         * @return the Cartesian product
         */
        static List<List<Literal>> getCartesianProduct(final SortedSet<Variable> variables) {
            List<List<Literal>> result = singletonList(emptyList());
            for (final Variable var : variables) {
                final List<List<Literal>> extended = new ArrayList<>(result.size() * 2);
                for (final List<Literal> literals : result) {
                    extended.add(extendedByLiteral(literals, var));
                    extended.add(extendedByLiteral(literals, var.negate()));
                }
                result = extended;
            }
            return result;
        }

        private static List<Literal> extendedByLiteral(final List<Literal> literals, final Literal lit) {
            final ArrayList<Literal> extended = new ArrayList<>(literals);
            extended.add(lit);
            return extended;
        }
    }
}
