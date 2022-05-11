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

import static org.logicng.handlers.Handler.start;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.splitVariableProvider.LeastCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.SplitVariableProvider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link Builder()}.
 * @version 2.3.0
 * @since 2.3.0
 */
public final class AdvancedModelEnumerationFunction extends ModelEnumerationFunction {

    private final boolean computeWithComponents;
    private static final int minNumberOfVars = 15;

    public AdvancedModelEnumerationFunction(final boolean computeWithComponents, final SplitVariableProvider splitVariableProvider,
                                            final ModelEnumerationHandler handler, final Collection<Variable> variables,
                                            final Collection<Variable> additionalVariables, final boolean fastEvaluable) {
        super(handler, variables, additionalVariables, fastEvaluable, splitVariableProvider);
        this.computeWithComponents = computeWithComponents;
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
        if (!this.computeWithComponents || solver.knownVariables().size() < minNumberOfVars) {
            return ModelEnumerationFunction.builder().handler(this.handler)
                    .splitVariableProvider(this.splitVariableProvider).variables(this.variables).additionalVariables(this.additionalVariables)
                    .fastEvaluable(this.fastEvaluable).build()
                    .apply(solver, resultSetter);
        }
        final Set<Formula> formulasOnSolver = solver.execute(FormulaOnSolverFunction.get());
        if (formulasOnSolver.isEmpty()) {
            return Collections.emptyList();
        }
        final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromFormulas(formulasOnSolver);
        final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
        final List<List<Formula>> components = ConnectedComponentsComputation.splitFormulasByComponent(formulasOnSolver, ccs);
        final List<List<Assignment>> modelsForAllComponents = new ArrayList<>();
        final SortedSet<Variable> leftOverVars = getVarsForEnumeration(solver.knownVariables());
        for (final List<Formula> component : components) {
            final SortedSet<Variable> varsInThisComponent = getVarsInThisComponent(solver.knownVariables(), component);
            leftOverVars.removeAll(varsInThisComponent);
            final List<Assignment> models = splitModelEnumeration(solver, resultSetter, component, varsInThisComponent, this.additionalVariables);
            if (!models.isEmpty()) {
                modelsForAllComponents.add(models);
            }
        }
        if (!leftOverVars.isEmpty()) {
            modelsForAllComponents.add(enumerate(solver, resultSetter, leftOverVars, this.additionalVariables));
        }
        return modelsForAllComponents.isEmpty() ? Collections.emptyList() : getCartesianProduct(modelsForAllComponents);
    }

    private SortedSet<Variable> getVarsInThisComponent(final Collection<Variable> knownVariables, final Collection<Formula> component) {
        final SortedSet<Variable> varsComponent = new TreeSet<>();
        component.forEach(x -> varsComponent.addAll(x.variables()));
        return getVarsForEnumeration(knownVariables).stream().filter(varsComponent::contains).collect(Collectors.toCollection(TreeSet::new));
    }

    private List<Assignment> getCartesianProduct(final List<List<Assignment>> allModelsList) {
        List<List<Literal>> currentResult = Collections.singletonList(Collections.emptyList());
        for (final List<Assignment> newAssignments : allModelsList) {
            final List<List<Literal>> newResult = new ArrayList<>();
            for (final Assignment newAssignment : newAssignments) {
                final SortedSet<Literal> newLiterals = newAssignment.literals();
                for (final List<Literal> existingAssignment : currentResult) {
                    final List<Literal> extendedAssignment = new ArrayList<>(existingAssignment);
                    extendedAssignment.addAll(newLiterals);
                    newResult.add(extendedAssignment);
                }
            }
            currentResult = newResult;
        }
        return currentResult.stream().map(Assignment::new).collect(Collectors.toList());
    }

    private static <T> void product(final List<List<T>> result, final List<T> existingTupleToComplete, final List<List<T>> valuesToUse) {
        for (final T value : valuesToUse.get(0)) {
            final List<T> newExisting = new ArrayList<>(existingTupleToComplete);
            newExisting.add(value);
            if (valuesToUse.size() == 1) {
                result.add(newExisting);
            } else {
                final List<List<T>> newValues = new ArrayList<>();
                for (int i = 1; i < valuesToUse.size(); i++) {
                    newValues.add(valuesToUse.get(i));
                }
                product(result, newExisting, newValues);
            }
        }
    }

    /**
     * The builder for an advanced model enumeration function.
     */
    public static class Builder extends ModelEnumerationFunction.Builder {
        private boolean computeWithComponents = false;
        private SplitVariableProvider splitVariableProvider = new LeastCommonVariableProvider(new FormulaFactory());

        private Builder() {
            // Initialize only via factory
        }

        /**
         * Indicates whether the computation should be performed by splitting into components.
         * @param computeWithComponents the flag for whether the computation should be performed with components.
         * @return the builder
         */
        public Builder computeWithComponents(final boolean computeWithComponents) {
            this.computeWithComponents = computeWithComponents;
            return this;
        }

        /**
         * Sets the split variable provider for the split.
         * @param splitVariableProvider the split variable provider
         * @return the builder
         */
        @Override
        public Builder splitVariableProvider(final SplitVariableProvider splitVariableProvider) {
            this.splitVariableProvider = splitVariableProvider;
            return this;
        }

        /**
         * Sets the model enumeration handler for this function
         * @param handler the handler
         * @return the current builder
         */
        @Override
        public Builder handler(final ModelEnumerationHandler handler) {
            this.handler = handler;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        @Override
        public Builder variables(final Collection<Variable> variables) {
            this.variables = variables;
            return this;
        }

        /**
         * Sets the set of variables over which the model enumeration should iterate.
         * @param variables the set of variables
         * @return the current builder
         */
        @Override
        public Builder variables(final Variable... variables) {
            this.variables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        @Override
        public Builder additionalVariables(final Collection<Variable> variables) {
            this.additionalVariables = variables;
            return this;
        }

        /**
         * Sets an additional set of variables which should occur in every model. Only set this field if 'variables' is non-empty.
         * @param variables the additional variables for each model
         * @return the current builder
         */
        @Override
        public Builder additionalVariables(final Variable... variables) {
            this.additionalVariables = Arrays.asList(variables);
            return this;
        }

        /**
         * Sets the flag whether the created assignment should be {@link Assignment#fastEvaluable() fast evaluable} assignments.
         * @param fastEvaluable {@code true} if the created assignment should be fast evaluable, otherwise {@code false}
         * @return the builder
         */
        @Override
        public Builder fastEvaluable(final boolean fastEvaluable) {
            this.fastEvaluable = fastEvaluable;
            return this;
        }


        /**
         * Builds the advanced model enumeration function with the current builder's configuration.
         * @return the advanced model enumeration function
         */
        @Override
        public AdvancedModelEnumerationFunction build() {
            return new AdvancedModelEnumerationFunction(this.computeWithComponents, this.splitVariableProvider, this.handler, this.variables,
                    this.additionalVariables, this.fastEvaluable);
        }
    }
}
