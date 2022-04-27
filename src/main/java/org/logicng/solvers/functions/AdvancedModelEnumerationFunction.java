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
import static org.logicng.formulas.FormulaFactory.CC_PREFIX;
import static org.logicng.formulas.FormulaFactory.CNF_PREFIX;
import static org.logicng.formulas.FormulaFactory.PB_PREFIX;
import static org.logicng.handlers.Handler.start;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.functions.VariableProfileFunction;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.predicates.satisfiability.ContingencyPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * <p>
 * Model enumeration functions are instantiated via their builder {@link #builder()}.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class AdvancedModelEnumerationFunction implements SolverFunction<List<Assignment>> {

    private final ModelEnumerationHandler handler;
    private final Collection<Variable> variables;
    private final Collection<Variable> additionalVariables;
    private final boolean fastEvaluable;
    private final boolean enumerateWithSplit;
    private final boolean computeWithComponents;
    private final int minNumberOfVarsForSplit;
    private final SPLIT_CRITERION splitCriterion;
    private final int lowerBoundSplit;
    private final int upperBoundSplit;
    private static final double HUNDRED = 100;


    private AdvancedModelEnumerationFunction(final ModelEnumerationHandler handler, final Collection<Variable> variables,
                                             final Collection<Variable> additionalVariables, final boolean fastEvaluable, final boolean computeWithComponents,
                                             final boolean enumerateWithSplit, final int minNumberOfVarsForSplit, final SPLIT_CRITERION splitCriterion,
                                             final int lowerBoundSplit, final int upperBoundSplit) {
        this.handler = handler;
        this.variables = variables;
        this.additionalVariables = additionalVariables;
        this.fastEvaluable = fastEvaluable;
        this.computeWithComponents = computeWithComponents;
        this.enumerateWithSplit = enumerateWithSplit;
        this.minNumberOfVarsForSplit = minNumberOfVarsForSplit;
        this.splitCriterion = splitCriterion;
        this.lowerBoundSplit = lowerBoundSplit;
        this.upperBoundSplit = upperBoundSplit;
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
        if (!this.computeWithComponents) {
            return this.enumerateWithSplit ? enumerateWithSplit(solver, resultSetter, formulasOnSolver, solver.knownVariables()) :
                    enumerate(solver, resultSetter, this.variables);
        } else {
            final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromFormulas(formulasOnSolver);
            final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
            final List<List<Formula>> components = ConnectedComponentsComputation.splitFormulasByComponent(formulasOnSolver, ccs);
            final List<List<Assignment>> modelsForAllComponents = new ArrayList<>();
            final SortedSet<Variable> leftOverVars = solver.knownVariables();
            leftOverVars.removeIf(x -> !isNotHelpVar(x));
            if (components.size() == 1 && components.get(0).size() == 1 && !components.get(0).get(0).holds(new ContingencyPredicate(solver.factory()))) {
                return Collections.emptyList();
            }
            for (final List<Formula> component : components) {
                final SortedSet<Variable> varsInThisComponent = getVarsInThisComponent(solver.knownVariables(), component);
                leftOverVars.removeAll(varsInThisComponent);
                final List<Assignment> models = this.enumerateWithSplit ?
                        enumerateWithSplit(solver, resultSetter, component, varsInThisComponent) :
                        enumerate(solver, resultSetter, varsInThisComponent);
                if (!models.isEmpty()) {
                    modelsForAllComponents.add(models);
                }
            }
            if (!leftOverVars.isEmpty()) {
                modelsForAllComponents.add(enumerate(solver, resultSetter, leftOverVars));
            }
            return modelsForAllComponents.isEmpty() ? Collections.emptyList() : getCartesianProduct(modelsForAllComponents);
        }
    }

    private List<Assignment> enumerateWithSplit(final MiniSat solver, final Consumer<Tristate> resultSetter, final Collection<Formula> formulas,
                                                final Collection<Variable> varsInThisComponent) {
        if (varsInThisComponent.size() < this.minNumberOfVarsForSplit) {
            return enumerate(solver, resultSetter, varsInThisComponent);
        } else {
            final SolverState initialState = solver.saveState();
            final SortedSet<Variable> splitVars = computeSplitVars(varsInThisComponent, formulas);
            final List<Assignment> splitAssignments = enumerate(solver, resultSetter, splitVars);
            final List<Assignment> modelsForThisComponent = new ArrayList<>();
            for (final Assignment splitAssignment : splitAssignments) {
                solver.add(splitAssignment.formula(solver.factory()));
                modelsForThisComponent.addAll(enumerate(solver, resultSetter, varsInThisComponent));
                solver.loadState(initialState);
            }
            return modelsForThisComponent;
        }
    }

    private SortedSet<Variable> computeSplitVars(final Collection<Variable> varsInThisComponent, final Collection<Formula> component) {
        final SortedSet<Variable> splitVars = new TreeSet<>();
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = getOccurrence2Vars(component);
        final int minNumberOfSplitVars = (int) Math.ceil(this.lowerBoundSplit * varsInThisComponent.size() / HUNDRED);
        final int maxNumberOfSplitVars = (int) Math.floor(this.upperBoundSplit * varsInThisComponent.size() / HUNDRED);
        switch (this.splitCriterion) {
            case RANDOM:
                final List<Variable> vars = new ArrayList<>(varsInThisComponent);
                int counterR = 0;
                while (splitVars.size() < minNumberOfSplitVars) {
                    splitVars.add(vars.get(counterR));
                    counterR++;
                }
                return splitVars;
            case LEAST_COMMON_VARS:
                int counterL = occurrence2Vars.entrySet().stream().findFirst().get().getKey();
                while (splitVars.size() < minNumberOfSplitVars) {
                    if (occurrence2Vars.containsKey(counterL)) {
                        splitVars.addAll(occurrence2Vars.get(counterL));
                    }
                    counterL++;
                }
                return splitVars.size() <= maxNumberOfSplitVars ? splitVars : removeVarsFromSplitVars(splitVars, maxNumberOfSplitVars);
            case MOST_COMMON_VARS:
                int counterM = occurrence2Vars.keySet().stream().mapToInt(i -> i).max().getAsInt();
                while (splitVars.size() < minNumberOfSplitVars) {
                    if (occurrence2Vars.containsKey(counterM)) {
                        splitVars.addAll(occurrence2Vars.get(counterM));
                    }
                    counterM--;
                }
                return splitVars.size() <= maxNumberOfSplitVars ? splitVars : removeVarsFromSplitVars(splitVars, maxNumberOfSplitVars);
            default:
                throw new IllegalArgumentException("Unknown split criterion");
        }
    }

    private Map<Integer, SortedSet<Variable>> getOccurrence2Vars(final Collection<Formula> component) {
        final Formula componentFormulas = component.stream().findAny().get().factory().and(component);
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = new TreeMap<>();
        final Map<Variable, Integer> variableIntegerMap = componentFormulas.apply(new VariableProfileFunction()).entrySet().stream()
                .filter(x -> isNotHelpVar(x.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        for (final Map.Entry<Variable, Integer> entry : variableIntegerMap.entrySet()) {
            occurrence2Vars.computeIfAbsent(entry.getValue(), x -> new TreeSet<>()).add(entry.getKey());
        }
        return occurrence2Vars;
    }

    private boolean isNotHelpVar(final Variable var) {
        return !var.name().startsWith(CC_PREFIX) && !var.name().startsWith(PB_PREFIX) && !var.name().startsWith(CNF_PREFIX);
    }

    private SortedSet<Variable> removeVarsFromSplitVars(final SortedSet<Variable> splitVars, final int maxNumerOfSplitVars) {
        final SortedSet<Variable> updatedSplitVars = new TreeSet<>(splitVars);
        for (final Variable splitVar : splitVars) {
            updatedSplitVars.remove(splitVar);
            if (updatedSplitVars.size() <= maxNumerOfSplitVars) {
                break;
            }
        }
        return updatedSplitVars;
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

    private List<Assignment> enumerate(final MiniSat solver, final Consumer<Tristate> resultSetter, final Collection<Variable> variables) {
        final List<Assignment> models = new ArrayList<>();
        SolverState stateBeforeEnumeration = null;
        if (solver.getStyle() == MiniSat.SolverStyle.MINISAT && solver.isIncremental()) {
            stateBeforeEnumeration = solver.saveState();
        }
        boolean proceed = true;
        final LNGIntVector relevantIndices;
        if (variables == null) {
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
            relevantIndices = new LNGIntVector(variables.size());
            for (final Variable var : variables) {
                relevantIndices.push(solver.underlyingSolver().idxForName(var.name()));
            }
        }
        LNGIntVector relevantAllIndices = null;
        final SortedSet<Variable> uniqueAdditionalVariables =
                new TreeSet<>(this.additionalVariables == null ? Collections.emptyList() : this.additionalVariables);
        if (variables != null) {
            uniqueAdditionalVariables.removeAll(variables);
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
            final Assignment model = solver.createAssignment(modelFromSolver, relevantAllIndices, this.fastEvaluable);
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
        private boolean fastEvaluable = false;
        private boolean enumerateWithSplit = false;
        private boolean computeWithComponents = false;
        private int minNumberOfVarsForSplit = 10;
        private SPLIT_CRITERION splitCriterion = SPLIT_CRITERION.RANDOM;
        private int lowerBoundSplit = 50;
        private int upperBoundSplit = 65;

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
         * Sets the flag whether the created assignment should be {@link Assignment#fastEvaluable() fast evaluable} assignments.
         * @param fastEvaluable {@code true} if the created assignment should be fast evaluable, otherwise {@code false}
         * @return the builder
         */
        public Builder fastEvaluable(final boolean fastEvaluable) {
            this.fastEvaluable = fastEvaluable;
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
         * Sets the flag whether the model enumeration should be performed with splits.
         * @param enumerateWithSplit {@code true} if the model enumeration should be performed with split, otherwise {@code false}
         * @return the builder
         */
        public Builder enumerateWithSplit(final boolean enumerateWithSplit) {
            this.enumerateWithSplit = enumerateWithSplit;
            return this;
        }

        /**
         * Sets the minimum number of variables for which the model enumeration should be performed with splits.
         * @param minNumberOfVarsForSplit {@code true} if the computation should be performed with split, otherwise {@code false}
         * @return the builder
         */
        public Builder minNumberOfVarsForSplit(final int minNumberOfVarsForSplit) {
            this.minNumberOfVarsForSplit = minNumberOfVarsForSplit;
            return this;
        }

        /**
         * Sets the split criterium for the split.
         * @param splitCriterion the split criterium
         * @return the builder
         */
        public Builder splitCriterion(final SPLIT_CRITERION splitCriterion) {
            this.splitCriterion = splitCriterion;
            return this;
        }

        /**
         * Sets the lower bound for the number of variables after which should be split.
         * @param lowerBoundSplit the lower bound for the split
         * @return the builder
         */
        public Builder lowerBoundSplit(final int lowerBoundSplit) {
            this.lowerBoundSplit = lowerBoundSplit;
            return this;
        }

        /**
         * Sets the upper bound for the number of variables after which should be split.
         * @param upperBoundSplit the upper bound for the split
         * @return the builder
         */
        public Builder upperBoundSplit(final int upperBoundSplit) {
            this.upperBoundSplit = upperBoundSplit;
            return this;
        }

        /**
         * Builds the model enumeration function with the current builder's configuration.
         * @return the model enumeration function
         */
        public AdvancedModelEnumerationFunction build() {
            return new AdvancedModelEnumerationFunction(this.handler, this.variables, this.additionalVariables, this.fastEvaluable, this.computeWithComponents,
                    this.enumerateWithSplit, this.minNumberOfVarsForSplit, splitCriterion, lowerBoundSplit, upperBoundSplit);
        }
    }

    enum SPLIT_CRITERION {
        MOST_COMMON_VARS,
        LEAST_COMMON_VARS,
        RANDOM
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
