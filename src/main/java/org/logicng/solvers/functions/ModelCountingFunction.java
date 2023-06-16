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
import org.logicng.datastructures.Model;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.modelenumeration.AbstractModelEnumerationFunction;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.EnumerationCollector;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class ModelCountingFunction extends AbstractModelEnumerationFunction<BigInteger> {

    ModelCountingFunction(final SortedSet<Variable> variables, final AdvancedModelEnumerationConfig config) {
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
    protected EnumerationCollector<BigInteger> newCollector(final FormulaFactory f, final SortedSet<Variable> knownVariables, final SortedSet<Variable> dontCareVariablesNotOnSolver,
                                                            final SortedSet<Variable> additionalVariablesNotOnSolver) {
        return new ModelCountCollector(dontCareVariablesNotOnSolver.size());
    }

    /**
     * The builder for a model enumeration function.
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
            this.variables = new TreeSet<>(Arrays.asList(variables));
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
        public ModelCountingFunction build() {
            return new ModelCountingFunction(this.variables, this.configuration);
        }
    }

    static class ModelCountCollector implements EnumerationCollector<BigInteger> {
        private BigInteger committedCount = BigInteger.ZERO;
        private final List<LNGBooleanVector> uncommittedModels = new ArrayList<>(100);
        private final List<LNGIntVector> uncommittedIndices = new ArrayList<>(100);
        private final BigInteger dontCareFactor;

        public ModelCountCollector(final int numberDontCareVariablesNotOnSolver) {
            this.dontCareFactor = BigInteger.valueOf(2).pow(numberDontCareVariablesNotOnSolver);
        }

        @Override
        public boolean addModel(final LNGBooleanVector modelFromSolver, final MiniSat solver, final LNGIntVector relevantAllIndices,
                                final AdvancedModelEnumerationHandler handler) {
            this.uncommittedModels.add(modelFromSolver);
            this.uncommittedIndices.add(relevantAllIndices);
            return handler == null || handler.foundModels(this.dontCareFactor.intValue());
        }

        @Override
        public boolean commit(final AdvancedModelEnumerationHandler handler) {
            this.committedCount = this.committedCount.add(BigInteger.valueOf(this.uncommittedModels.size()).multiply(this.dontCareFactor));
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
        public BigInteger getResult() {
            return this.committedCount;
        }

        private boolean clearUncommitted(final AdvancedModelEnumerationHandler handler) {
            this.uncommittedModels.clear();
            this.uncommittedIndices.clear();
            return handler == null || handler.commit();
        }
    }
}
