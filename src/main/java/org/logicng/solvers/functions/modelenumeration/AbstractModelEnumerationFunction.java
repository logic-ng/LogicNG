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

package org.logicng.solvers.functions.modelenumeration;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.formulas.FormulaFactory.CC_PREFIX;
import static org.logicng.formulas.FormulaFactory.CNF_PREFIX;
import static org.logicng.formulas.FormulaFactory.PB_PREFIX;
import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;
import static org.logicng.solvers.functions.modelenumeration.ModelEnumerationCommon.generateBlockingClause;
import static org.logicng.solvers.functions.modelenumeration.ModelEnumerationCommon.relevantAllIndicesFromSolver;
import static org.logicng.solvers.functions.modelenumeration.ModelEnumerationCommon.relevantIndicesFromSolver;
import static org.logicng.util.CollectionHelper.difference;
import static org.logicng.util.CollectionHelper.nullSafe;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.configurations.ConfigurationType;
import org.logicng.datastructures.Model;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedModelEnumerationHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.SolverFunction;

import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * @param <RESULT> The result type of the model enumeration function.  Can be e.g. a model count, a list of models, or a BDD.
 * @version 2.5.0
 * @since 2.5.0
 */
public abstract class AbstractModelEnumerationFunction<RESULT> implements SolverFunction<RESULT> {

    private static final AdvancedModelEnumerationStrategy NO_SPLIT_VARS_STRATEGY =
            new DefaultAdvancedModelEnumerationStrategy((solver, vars) -> new TreeSet<>(), Integer.MAX_VALUE);

    protected final SortedSet<Variable> variables;
    protected final SortedSet<Variable> additionalVariables;
    protected final AdvancedModelEnumerationHandler handler;
    protected final AdvancedModelEnumerationStrategy strategy;

    protected AbstractModelEnumerationFunction(final SortedSet<Variable> variables, final SortedSet<Variable> additionalVariables,
                                               final AdvancedModelEnumerationConfig configuration) {
        this.variables = variables;
        this.additionalVariables = additionalVariables;
        this.handler = configuration.handler;
        this.strategy = configuration.strategy == null ? NO_SPLIT_VARS_STRATEGY : configuration.strategy;
    }

    protected abstract EnumerationCollector<RESULT> newCollector(final FormulaFactory f, final SortedSet<Variable> knownVariables, final SortedSet<Variable> dontCareVariablesNotOnSolver,
                                                                 SortedSet<Variable> additionalVariablesNotOnSolver);

    @Override
    public RESULT apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        if (!solver.canSaveLoadState()) {
            throw new IllegalArgumentException("Recursive model enumeration function can only be applied to solvers with load/save state capability.");
        }
        start(this.handler);
        final SortedSet<Variable> knownVariables = solver.knownVariables();
        final SortedSet<Variable> additionalVarsNotOnSolver = difference(this.additionalVariables, knownVariables, TreeSet::new);
        final SortedSet<Variable> dontCareVariablesNotOnSolver = difference(this.variables, knownVariables, TreeSet::new);
        final EnumerationCollector<RESULT> collector = newCollector(solver.factory(), knownVariables, dontCareVariablesNotOnSolver, additionalVarsNotOnSolver);
        final SortedSet<Variable> enumerationVars = getVarsForEnumeration(knownVariables);
        final SortedSet<Variable> initialSplitVars = nullSafe(() -> this.strategy.splitVarsForRecursionDepth(enumerationVars, solver, 0), TreeSet::new);
        enumerateRecursive(collector, solver, new Model(), resultSetter, enumerationVars, initialSplitVars, 0);
        return collector.getResult();
    }

    private void enumerateRecursive(final EnumerationCollector<RESULT> collector, final MiniSat solver, final Model splitAssignment,
                                    final Consumer<Tristate> resultSetter, final SortedSet<Variable> enumerationVars,
                                    final SortedSet<Variable> splitVars, final int recursionDepth) {
        final int maxNumberOfModelsForEnumeration = this.strategy.maxNumberOfModelsForEnumeration(recursionDepth);
        final SolverState state = solver.saveState();
        solver.add(splitAssignment.formula(solver.factory()));
        final boolean enumerationFinished = enumerate(collector, solver, resultSetter, enumerationVars, this.additionalVariables, maxNumberOfModelsForEnumeration, this.handler);
        if (!enumerationFinished) {
            if (!collector.rollback(this.handler)) {
                solver.loadState(state);
                return;
            }
            SortedSet<Variable> newSplitVars = new TreeSet<>(splitVars);
            final int maxNumberOfModelsForSplitAssignments = this.strategy.maxNumberOfModelsForSplitAssignments(recursionDepth);
            while (!enumerate(collector, solver, resultSetter, newSplitVars, null, maxNumberOfModelsForSplitAssignments, this.handler)) {
                if (!collector.rollback(this.handler)) {
                    solver.loadState(state);
                    return;
                }
                newSplitVars = this.strategy.reduceSplitVars(newSplitVars, recursionDepth);
            }
            if (aborted(this.handler)) {
                collector.rollback(this.handler);
                return;
            }

            final List<Model> newSplitAssignments = collector.rollbackAndReturnModels(solver, this.handler);
            final SortedSet<Variable> recursiveSplitVars = this.strategy.splitVarsForRecursionDepth(difference(enumerationVars, newSplitVars, TreeSet::new), solver, recursionDepth + 1);
            for (final Model newSplitAssignment : newSplitAssignments) {
                enumerateRecursive(collector, solver, newSplitAssignment, resultSetter, enumerationVars, recursiveSplitVars, recursionDepth + 1);
                if (!collector.commit(this.handler)) {
                    solver.loadState(state);
                    return;
                }
            }
        } else {
            if (!collector.commit(this.handler)) {
                solver.loadState(state);
                return;
            }
        }
        solver.loadState(state);
    }

    protected static <R> boolean enumerate(final EnumerationCollector<R> collector, final MiniSat solver, final Consumer<Tristate> resultSetter,
                                           final SortedSet<Variable> variables, final SortedSet<Variable> additionalVariables, final int maxModels,
                                           final AdvancedModelEnumerationHandler handler) {
        final SolverState stateBeforeEnumeration = solver.saveState();
        final LNGIntVector relevantIndices = relevantIndicesFromSolver(variables, solver);
        final LNGIntVector relevantAllIndices = relevantAllIndicesFromSolver(variables, additionalVariables, relevantIndices, solver);

        int foundModels = 0;
        boolean proceed = true;
        while (proceed && modelEnumerationSATCall(solver, handler)) {
            final LNGBooleanVector modelFromSolver = solver.underlyingSolver().model();
            if (++foundModels >= maxModels) {
                solver.loadState(stateBeforeEnumeration);
                return false;
            }
            proceed = collector.addModel(modelFromSolver, solver, relevantAllIndices, handler);
            if (modelFromSolver.size() > 0) {
                final LNGIntVector blockingClause = generateBlockingClause(modelFromSolver, relevantIndices);
                solver.underlyingSolver().addClause(blockingClause, null);
                resultSetter.accept(UNDEF);
            } else {
                break;
            }
        }
        solver.loadState(stateBeforeEnumeration);
        return true;
    }

    protected SortedSet<Variable> getVarsForEnumeration(final SortedSet<Variable> knownVariables) {
        final SortedSet<Variable> relevantVars = knownVariables.stream()
                .filter(AbstractModelEnumerationFunction::isNotAuxiliaryVariable)
                .collect(Collectors.toCollection(TreeSet::new));
        return this.variables == null ? relevantVars : relevantVars.stream().filter(this.variables::contains).collect(Collectors.toCollection(TreeSet::new));
    }

    private static boolean isNotAuxiliaryVariable(final Variable var) {
        return !var.name().startsWith(CC_PREFIX) && !var.name().startsWith(PB_PREFIX) && !var.name().startsWith(CNF_PREFIX);
    }

    private static boolean modelEnumerationSATCall(final MiniSat solver, final AdvancedModelEnumerationHandler handler) {
        final SATHandler satHandler = handler == null ? null : handler.satHandler();
        final boolean sat = solver.sat(satHandler) == TRUE;
        return !aborted(handler) && sat;
    }

    protected static FormulaFactory factory(final SortedSet<Variable> variables) {
        return variables == null || variables.isEmpty() ? new FormulaFactory() : variables.first().factory();
    }

    protected static AdvancedModelEnumerationConfig configuration(final SortedSet<Variable> variables, final AdvancedModelEnumerationConfig config) {
        return config == null ? (AdvancedModelEnumerationConfig) factory(variables).configurationFor(ConfigurationType.ADVANCED_MODEL_ENUMERATION) : config;
    }
}
