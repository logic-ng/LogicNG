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

import static org.logicng.formulas.FormulaFactory.CC_PREFIX;
import static org.logicng.formulas.FormulaFactory.CNF_PREFIX;
import static org.logicng.formulas.FormulaFactory.PB_PREFIX;
import static org.logicng.handlers.Handler.start;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.splitVariables.LeastCommonVariables;
import org.logicng.solvers.functions.splitVariables.SplitVariableProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link #builder()}.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class AdvancedModelEnumerationFunction implements SolverFunction<List<Assignment>> {

    private final ModelEnumerationHandler handler;
    private final boolean computeWithComponents;
    private final SplitVariableProvider splitVariableProvider;

    public AdvancedModelEnumerationFunction(final ModelEnumerationHandler handler, final boolean computeWithComponents,
                                            final SplitVariableProvider splitVariableProvider) {
        this.handler = handler;
        this.computeWithComponents = computeWithComponents;
        this.splitVariableProvider = splitVariableProvider;
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
        final Set<Formula> formulasOnSolver = solver.execute(FormulaOnSolverFunction.get());
        if (formulasOnSolver.isEmpty()) {
            return Collections.emptyList();
        }

        final ModelEnumerationFunction modelEnumerationFunction = ModelEnumerationFunction.builder().splitVariableProvider(this.splitVariableProvider)
                .build();
        if (!this.computeWithComponents || solver.knownVariables().size() < 15) {
            return modelEnumerationFunction.apply(solver, resultSetter);
        } else {
            final SolverState initialState = solver.saveState();
            final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromFormulas(formulasOnSolver);
            final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
            final List<List<Formula>> components = ConnectedComponentsComputation.splitFormulasByComponent(formulasOnSolver, ccs);
            final List<List<Assignment>> modelsForAllComponents = new ArrayList<>();
            final SortedSet<Variable> leftOverVars = solver.knownVariables();
            leftOverVars.removeIf(x -> !isNotHelpVar(x));
            // if (components.size() == 1 && components.get(0).size() == 1 && !components.get(0).get(0).holds(new ContingencyPredicate(solver.factory()))) {
            //     return Collections.emptyList();
            // }
            for (final List<Formula> component : components) {
                final SortedSet<Variable> varsInThisComponent = getVarsInThisComponent(solver.knownVariables(), component);
                leftOverVars.removeAll(varsInThisComponent);
                final List<Assignment> models = modelEnumerationFunction.enumerate(solver, resultSetter, varsInThisComponent);
                if (!models.isEmpty()) {
                    modelsForAllComponents.add(models);
                }
            }
            if (!leftOverVars.isEmpty()) {
                modelsForAllComponents.add(modelEnumerationFunction.enumerate(solver, resultSetter, leftOverVars));
            }
            return modelsForAllComponents.isEmpty() ? Collections.emptyList() : getCartesianProduct(modelsForAllComponents);
        }
    }

    private boolean isNotHelpVar(final Variable var) {
        return !var.name().startsWith(CC_PREFIX) && !var.name().startsWith(PB_PREFIX) && !var.name().startsWith(CNF_PREFIX);
    }

    private SortedSet<Variable> getVarsInThisComponent(final Collection<Variable> variables, final Collection<Formula> component) {
        final SortedSet<Variable> varsInThisComponent = new TreeSet<>();
        final SortedSet<Variable> varsCom = new TreeSet<>();
        for (final Formula formula : component) {
            varsCom.addAll(formula.variables());
        }
        for (final Variable var : variables) {
            if (isNotHelpVar(var) && varsCom.contains(var)) {
                varsInThisComponent.add(var);
            }
        }
        return varsInThisComponent;
    }

    private List<Assignment> getCartesianProduct(final List<List<Assignment>> allModelsList) {
        if (allModelsList.size() == 1) {
            return allModelsList.get(0);
        }
        final List<Assignment> allJoinedAssignments = new ArrayList<>();
        final List<List<Assignment>> product = CartesianProduct.product(allModelsList);
        for (final List<Assignment> assignmentList : product) {
            final Assignment assignment = new Assignment();
            for (final Assignment assignment1 : assignmentList) {
                for (final Literal literal : assignment1.literals()) {
                    assignment.addLiteral(literal);
                }
            }
            allJoinedAssignments.add(assignment);
        }
        return allJoinedAssignments;
    }

    /**
     * The builder for a model enumeration function.
     */
    public static class Builder {
        private ModelEnumerationHandler handler;
        private boolean computeWithComponents = false;
        private SplitVariableProvider splitVariableProvider = new LeastCommonVariables();

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
        public Builder SplitVariableProvider(final SplitVariableProvider splitVariableProvider) {
            this.splitVariableProvider = splitVariableProvider;
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public AdvancedModelEnumerationFunction build() {
            return new AdvancedModelEnumerationFunction(this.handler, this.computeWithComponents, this.splitVariableProvider);
        }
    }

    private static class CartesianProduct {
        public static <T> List<List<T>> product(final List<List<T>> lists) {
            final List<List<T>> product = new ArrayList<>();
            product(product, new ArrayList<>(), lists);
            return product;
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
    }
}
