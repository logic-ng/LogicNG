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

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Model;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.modelenumeration.AbstractModelEnumerationFunction;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.EnumerationCollector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A solver function for enumerating models on the solver and storing the result in a BDD.
 * If used with a subset of the original formula's variables this performs an existential
 * quantifier elimination (or projection) of the original formula into a BDD.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class ModelEnumerationToBddFunction extends AbstractModelEnumerationFunction<BDD> {

    ModelEnumerationToBddFunction(final SortedSet<Variable> variables, final AdvancedModelEnumerationConfig config) {
        super(variables, Collections.emptySortedSet(), configuration(variables, config));
    }

    /**
     * Returns the builder for this function.
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    protected EnumerationCollector<BDD> newCollector(final FormulaFactory f, final SortedSet<Variable> knownVariables, final SortedSet<Variable> dontCareVariablesNotOnSolver,
                                                     final SortedSet<Variable> additionalVariablesNotOnSolver) {
        return new BddModelEnumerationCollector(f, this.variables, knownVariables, dontCareVariablesNotOnSolver.size());
    }

    /**
     * The builder for a BDD model enumeration function.
     */
    public static class Builder {
        private SortedSet<Variable> variables;
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
        public ModelEnumerationToBddFunction build() {
            return new ModelEnumerationToBddFunction(this.variables, this.configuration);
        }
    }

    static class BddModelEnumerationCollector implements EnumerationCollector<BDD> {
        private final BDDKernel kernel;
        private BDD committedModels;
        private final List<Model> uncommittedModels = new ArrayList<>();
        private final int dontCareFactor;

        public BddModelEnumerationCollector(final FormulaFactory f, final SortedSet<Variable> variables, final SortedSet<Variable> knownVariables,
                                            final int numberDontCareVariablesNotOnSolver) {
            final List<Variable> sortedVariables = variables != null
                    ? new ArrayList<>(variables)
                    : new ArrayList<>(knownVariables);
            final int numVars = sortedVariables.size();
            this.kernel = new BDDKernel(f, sortedVariables, numVars * 30, numVars * 50);
            this.committedModels = BDDFactory.build(f.falsum(), this.kernel);
            this.dontCareFactor = (int) Math.pow(2, numberDontCareVariablesNotOnSolver);
        }

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final AdvancedModelEnumerationHandler handler) {
            if (handler == null || handler.foundModels(this.dontCareFactor)) {
                final Model model = solver.createModel(modelFromSolver, relevantAllIndices);
                this.uncommittedModels.add(model);
                return true;
            } else {
                return false;
            }
        }

        @Override
        public boolean commit(final AdvancedModelEnumerationHandler handler) {
            for (final Model uncommittedModel : this.uncommittedModels) {
                this.committedModels = this.committedModels.or(model2Bdd(uncommittedModel));
            }
            this.uncommittedModels.clear();
            return handler == null || handler.commit();
        }

        private BDD model2Bdd(final Model model) {
            BDD bdd = BDDFactory.build(this.kernel.factory().verum(), this.kernel);
            for (final Literal literal : model.getLiterals()) {
                bdd = bdd.and(BDDFactory.build(literal, this.kernel));
            }
            return bdd;
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
        public BDD getResult() {
            return this.committedModels;
        }
    }
}
