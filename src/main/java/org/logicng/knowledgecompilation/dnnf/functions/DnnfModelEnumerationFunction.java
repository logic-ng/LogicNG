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

package org.logicng.knowledgecompilation.dnnf.functions;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * A DNNF function which counts models.
 * @version 2.5.0
 * @since 2.5.0
 */
public final class DnnfModelEnumerationFunction implements DnnfFunction<List<Assignment>> {

    private final SortedSet<Variable> variables;

    public DnnfModelEnumerationFunction(final Collection<Variable> variables) {
        this.variables = variables == null ? null : new TreeSet<>(variables);
    }

    @Override
    public List<Assignment> apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        final Formula projectedFormula = this.variables == null ? formula : new DnnfProjectionFunction(this.variables).apply(originalVariables, formula).formula();
        final Set<Set<Literal>> partialModels = applyRec(projectedFormula);
        final SortedSet<Variable> globalDontCares = originalVariables.stream()
                .filter(v -> !formula.variables().contains(v) && (this.variables == null || this.variables.contains(v)))
                .collect(Collectors.toCollection(TreeSet::new));
        final List<Variable> invalidVars = this.variables == null
                ? emptyList()
                : this.variables.stream().filter(v -> !originalVariables.contains(v)).collect(Collectors.toList());
        final Set<Set<Literal>> partialDontCareModels = getCartesianProduct(globalDontCares);
        final Set<Set<Literal>> partialModelsWithGlobalDontCares = combineDisjointModels(Arrays.asList(partialModels, partialDontCareModels));
        final Set<Set<Literal>> result = expandModelsWithMissingVars(partialModelsWithGlobalDontCares, this.variables);
        addInvalidVars(result, invalidVars);
        final List<Assignment> resultAssignments = new ArrayList<>();
        for (final Set<Literal> model : result) {
            resultAssignments.add(new Assignment(model));
        }
        return resultAssignments;
    }

    private Set<Set<Literal>> applyRec(final Formula formula) {
        switch (formula.type()) {
            case FALSE:
                return new HashSet<>();
            case LITERAL:
            case TRUE:
                return singleton(formula.literals());
            case AND:
                final List<Set<Set<Literal>>> opResults = new ArrayList<>();
                for (final Formula op : formula) {
                    opResults.add(applyRec(op));
                }
                return combineDisjointModels(opResults);
            case OR:
                final Set<Set<Literal>> allModels = new HashSet<>();
                for (final Formula op : formula) {
                    allModels.addAll(applyRec(op));
                }
                return allModels;
            default:
                throw new IllegalArgumentException("Unexpected formula type: " + formula.type());
        }
    }

    private Set<Set<Literal>> expandModelsWithMissingVars(final Set<Set<Literal>> partialModels, final SortedSet<Variable> allVariables) {
        final Set<Set<Literal>> result = new HashSet<>();
        for (final Set<Literal> partialModel : partialModels) {
            final Set<Variable> missingVariables = new HashSet<>(allVariables);
            for (final Literal lit : partialModel) {
                missingVariables.remove(lit.variable());
            }
            if (missingVariables.isEmpty()) {
                result.add(partialModel);
            } else {
                result.addAll(combineDisjointModels(Arrays.asList(singleton(partialModel), getCartesianProduct(missingVariables))));
            }
        }
        return result;
    }

    private static void addInvalidVars(final Set<Set<Literal>> models, final List<Variable> invalidVars) {
        final List<Literal> negated = invalidVars.stream().map(Variable::negate).collect(Collectors.toList());
        for (final Set<Literal> model : models) {
            model.addAll(negated);
        }
    }

    /**
     * Combines a list of a model-sets (a model-set is a set of models).
     * The variables of the model-sets in the list must be disjoint
     * (i.e. a variable in a model-set must not occur in any other
     * model-set, but it will usually, but not necessarily, occur
     * in all models of the model-set).
     * <p>
     * Basically the models of each model-set are combined with every
     * other model of the other set. Since the variables are disjoint,
     * the models can just be combined into one model for each combination.
     * @param modelLists the list of sets of models
     * @return the combined model-set list
     */
    private static Set<Set<Literal>> combineDisjointModels(final List<Set<Set<Literal>>> modelLists) {
        Set<Set<Literal>> currentModels = modelLists.get(0);
        for (int i = 1; i < modelLists.size(); i++) {
            final Set<Set<Literal>> additionalModels = modelLists.get(i);
            final Set<Set<Literal>> newModels = new HashSet<>();
            for (final Set<Literal> currentModel : currentModels) {
                for (final Set<Literal> additionalModel : additionalModels) {
                    newModels.add(setAdd(currentModel, additionalModel));
                }
            }
            currentModels = newModels;
        }
        return currentModels;
    }

    /**
     * Returns the Cartesian product for the given variables, i.e. all
     * combinations of literals are generated with each variable occurring
     * positively and negatively.
     * @param variables the variables, must not be {@code null}
     * @return the Cartesian product
     */
    private static Set<Set<Literal>> getCartesianProduct(final Collection<Variable> variables) {
        Set<Set<Literal>> result = singleton(emptySet());
        for (final Variable var : variables) {
            final Set<Set<Literal>> extended = new HashSet<>(result.size() * 2);
            for (final Set<Literal> literals : result) {
                extended.add(extendedByLiteral(literals, var));
                extended.add(extendedByLiteral(literals, var.negate()));
            }
            result = extended;
        }
        return result;
    }

    private static Set<Literal> extendedByLiteral(final Set<Literal> literals, final Literal lit) {
        final Set<Literal> extended = new HashSet<>(literals);
        extended.add(lit);
        return extended;
    }

    private static Set<Literal> setAdd(final Set<Literal> first, final Set<Literal> second) {
        final Set<Literal> result = new HashSet<>();
        result.addAll(first);
        result.addAll(second);
        return result;
    }
}
