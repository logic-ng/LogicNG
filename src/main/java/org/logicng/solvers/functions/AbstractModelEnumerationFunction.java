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
import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;

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
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;
import org.logicng.util.CollectionHelper;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * A solver function for enumerating models on the solver.
 * @param <R> The result type of the model enumeration function.  Can be e.g. a model count, a list of models, or a BDD.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class AbstractModelEnumerationFunction<R> implements SolverFunction<R> {

    protected final Collection<Variable> variables;
    protected final Collection<Variable> additionalVariables;
    protected final AdvancedModelEnumerationHandler handler;
    protected final SplitVariableProvider splitVariableProvider;
    protected final int maxNumberOfModels;

    AbstractModelEnumerationFunction(final Collection<Variable> variables, final Collection<Variable> additionalVariables,
                                     final AdvancedModelEnumerationConfig configuration) {
        this.variables = variables;
        this.additionalVariables = additionalVariables;
        this.handler = configuration.handler;
        this.splitVariableProvider = configuration.splitVariableProvider;
        this.maxNumberOfModels = configuration.maxNumberOfModels;
    }

    abstract EnumerationCollector<R> newCollector(final SortedSet<Variable> dontCareVariables, SortedSet<Variable> additionalVariablesNotKnownBySolver);

    @Override
    public R apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        if (!solver.canSaveLoadState()) {
            throw new IllegalArgumentException("Recursive model enumeration function can only be applied to solvers with load/save state capability.");
        }
        start(this.handler);
        final SortedSet<Variable> additionalVarsNotOnSolver = CollectionHelper.difference(this.additionalVariables, solver.knownVariables(), TreeSet::new);
        final SortedSet<Variable> dontCareVariables = CollectionHelper.difference(this.variables, solver.knownVariables(), TreeSet::new);
        final EnumerationCollector<R> collector = newCollector(dontCareVariables, additionalVarsNotOnSolver);
        if (this.splitVariableProvider == null) {
            enumerate(collector, solver, resultSetter, this.variables, this.additionalVariables, Integer.MAX_VALUE, this.handler);
            collector.commit(this.handler);
            return collector.getResult();
        }
        final SortedSet<Variable> relevantVars = getVarsForEnumeration(solver.knownVariables());
        splitModelEnumeration(collector, solver, resultSetter, relevantVars, this.additionalVariables);
        return collector.getResult();
    }

    protected void splitModelEnumeration(final EnumerationCollector<R> collector, final MiniSat solver, final Consumer<Tristate> resultSetter,
                                         final SortedSet<Variable> relevantVars, final Collection<Variable> additionalVariables) {
        final SortedSet<Variable> initialSplitVars = this.splitVariableProvider.getSplitVars(solver, relevantVars);
        enumerateRecursive(collector, solver, new Model(), resultSetter, relevantVars, initialSplitVars, additionalVariables);
    }

    private void enumerateRecursive(final EnumerationCollector<R> collector, final MiniSat solver, final Model splitAssignment,
                                    final Consumer<Tristate> resultSetter, final Collection<Variable> enumerationVars, final Collection<Variable> nextSplitVars,
                                    final Collection<Variable> additionalVars) {
        final SolverState state = solver.saveState();
        solver.add(splitAssignment.formula(solver.factory()));
        final boolean enumerationFinished = enumerate(collector, solver, resultSetter, enumerationVars, additionalVars, this.maxNumberOfModels, this.handler);
        if (!enumerationFinished) {
            if (!collector.rollback(this.handler)) {
                solver.loadState(state);
                return;
            }
            SortedSet<Variable> newSplitVars = new TreeSet<>(nextSplitVars);
            while (!enumerate(collector, solver, resultSetter, newSplitVars, null, this.maxNumberOfModels, this.handler)) {
                if (!collector.rollback(this.handler)) {
                    solver.loadState(state);
                    return;
                }
                newSplitVars = updateSplitVars(newSplitVars);
            }
            if (aborted(this.handler)) {
                collector.rollback(this.handler);
                return;
            }

            final List<Model> newSplitAssignments = collector.rollbackAndReturnModels(solver, this.handler);
            final SortedSet<Variable> recursiveSplitVars = updateSplitVars(CollectionHelper.difference(enumerationVars, nextSplitVars, TreeSet::new));
            for (final Model newSplitAssignment : newSplitAssignments) {
                enumerateRecursive(collector, solver, newSplitAssignment, resultSetter, enumerationVars, recursiveSplitVars, additionalVars);
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

    private static SortedSet<Variable> updateSplitVars(final SortedSet<Variable> splitVars) {
        return splitVars.stream().limit(splitVars.size() / 2).collect(Collectors.toCollection(TreeSet::new));
    }

    protected static <R> boolean enumerate(final EnumerationCollector<R> collector, final MiniSat solver, final Consumer<Tristate> resultSetter,
                                           final Collection<Variable> variables, final Collection<Variable> additionalVariables, final int maxModels,
                                           final AdvancedModelEnumerationHandler handler) {
        start(handler);
        final SolverState stateBeforeEnumeration = solver.saveState();
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
                new TreeSet<>(additionalVariables == null ? Collections.emptyList() : additionalVariables);
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
        int foundModels = 0;
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

    protected SortedSet<Variable> getVarsForEnumeration(final Collection<Variable> knownVariables) {
        final SortedSet<Variable> relevantVars = knownVariables.stream()
                .filter(AbstractModelEnumerationFunction::isNotAuxiliaryVariable)
                .collect(Collectors.toCollection(TreeSet::new));
        return this.variables == null ? relevantVars : relevantVars.stream().filter(this.variables::contains).collect(Collectors.toCollection(TreeSet::new));
    }

    private static boolean isNotAuxiliaryVariable(final Variable var) {
        return !var.name().startsWith(CC_PREFIX) && !var.name().startsWith(PB_PREFIX) && !var.name().startsWith(CNF_PREFIX);
    }

    private static boolean modelEnumerationSATCall(final MiniSat solver, final AdvancedModelEnumerationHandler handler) {
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
    private static LNGIntVector generateBlockingClause(final LNGBooleanVector modelFromSolver, final LNGIntVector relevantVars) {
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

    protected static FormulaFactory factory(final Collection<Variable> variables) {
        return variables.isEmpty() ? new FormulaFactory() : variables.iterator().next().factory();
    }

    protected static AdvancedModelEnumerationConfig configuration(final Collection<Variable> variables, final AdvancedModelEnumerationConfig config) {
        return config == null ? (AdvancedModelEnumerationConfig) factory(variables).configurationFor(ConfigurationType.ADVANCED_MODEL_ENUMERATION) : config;
    }
}
