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
import static java.util.Collections.singleton;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.util.CollectionHelper;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * A DNNF function for (projected) model enumeration.
 * <p>
 * This function does not require a deterministic DNNF.
 * <p>
 * The enumeration requires a set of variables which the formula is projected
 * to. Variables which are not known by the DNNF will be added negatively to
 * all models.
 * <p>
 * Note that an enumeration over {@code n} variables can have up to {@code 2^n}
 * models, so the number of variables must be chosen carefully.
 * @version 2.5.0
 * @since 2.5.0
 */
public final class DnnfModelEnumerationFunction implements DnnfFunction<List<Assignment>> {

    private final SortedSet<Variable> variables;

    /**
     * Creates a new DNNF model enumeration function over the given set of
     * variables.
     * @param variables the variables, must not be {@code null}
     */
    public DnnfModelEnumerationFunction(final Collection<Variable> variables) {
        this.variables = new TreeSet<>(variables);
    }

    @Override
    public List<Assignment> apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        final Dnnf projectedDnnf = this.variables.equals(originalVariables)
                ? new Dnnf(this.variables, formula)
                : new DnnfProjectionFunction(this.variables).apply(originalVariables, formula);
        final Formula projectedFormula = projectedDnnf.formula();
        final SortedSet<Variable> projectedVariables = projectedDnnf.getOriginalVariables();
        final Pair<List<Variable>, Map<Variable, Integer>> mapping = createVarMapping(projectedVariables);
        final List<Variable> intToVar = mapping.first();
        final Map<Variable, Integer> varToInt = mapping.second();

        final Set<BitSet> partialModels = computePartialModels(projectedFormula, varToInt);

        final SortedSet<Variable> globalDontCares = projectedVariables.stream()
                .filter(v -> !projectedFormula.variables().contains(v))
                .collect(Collectors.toCollection(TreeSet::new));
        final Set<BitSet> partialDontCareModels = getCartesianProduct(globalDontCares.stream().map(varToInt::get).collect(Collectors.toList()), projectedVariables.size());
        final Set<BitSet> partialModelsWithGlobalDontCares = combineDisjointModels(Arrays.asList(partialModels, partialDontCareModels));

        final Set<BitSet> expandedModels = expandModelsWithMissingVars(partialModelsWithGlobalDontCares, projectedVariables.size());

        final SortedSet<Variable> invalidVars = CollectionHelper.difference(this.variables, originalVariables, TreeSet::new);
        return translateBitSetsToAssignments(expandedModels, intToVar, invalidVars);
    }

    private static Set<BitSet> computePartialModels(final Formula formula, final Map<Variable, Integer> varToInt) {
        switch (formula.type()) {
            case FALSE:
                return new HashSet<>();
            case TRUE:
                return singleton(new BitSet(varToInt.size() * 2));
            case LITERAL:
                final BitSet newBitSet = new BitSet(varToInt.size() * 2);
                for (final Literal lit : formula.literals()) {
                    newBitSet.set(2 * varToInt.get(lit.variable()) + (lit.phase() ? 0 : 1));
                }
                return singleton(newBitSet);
            case AND:
                final List<Set<BitSet>> opResults = new ArrayList<>();
                for (final Formula op : formula) {
                    opResults.add(computePartialModels(op, varToInt));
                }
                return combineDisjointModels(opResults);
            case OR:
                final Set<BitSet> allModels = new HashSet<>();
                for (final Formula op : formula) {
                    allModels.addAll(computePartialModels(op, varToInt));
                }
                return allModels;
            default:
                throw new IllegalArgumentException("Unexpected formula type: " + formula.type());
        }
    }

    private static Set<BitSet> expandModelsWithMissingVars(final Set<BitSet> partialModels, final int numVars) {
        final Set<BitSet> result = new HashSet<>();
        for (final BitSet partialModel : partialModels) {
            final List<Integer> missingVariables = findMissingVars(partialModel, numVars);
            if (missingVariables.isEmpty()) {
                result.add(partialModel);
            } else {
                result.addAll(combineDisjointModels(Arrays.asList(singleton(partialModel), getCartesianProduct(missingVariables, numVars))));
            }
        }
        return result;
    }

    private static List<Integer> findMissingVars(final BitSet partialModel, final int numVars) {
        final int cardinality = partialModel.cardinality();
        if (cardinality == numVars) {
            return emptyList();
        }
        final List<Integer> missing = new ArrayList<>(numVars - cardinality);
        for (int i = 0; i < numVars; i++) {
            if (!partialModel.get(2 * i) && !partialModel.get(2 * i + 1)) {
                missing.add(i);
            }
        }
        return missing;
    }

    private static List<Assignment> translateBitSetsToAssignments(final Set<BitSet> models, final List<Variable> intToVar, final SortedSet<Variable> invalidVars) {
        final List<Assignment> result = new ArrayList<>(models.size());
        for (final BitSet model : models) {
            final Assignment assignment = new Assignment();
            for (int i = model.nextSetBit(0); i >= 0; i = model.nextSetBit(i + 1)) {
                final Variable variable = intToVar.get(i / 2);
                assignment.addLiteral(i % 2 == 0 ? variable : variable.negate());
            }
            for (final Variable invalidVar : invalidVars) {
                assignment.addLiteral(invalidVar.negate());
            }
            result.add(assignment);
        }
        return result;
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
    private static Set<BitSet> combineDisjointModels(final List<Set<BitSet>> modelLists) {
        Set<BitSet> currentModels = modelLists.get(0);
        for (int i = 1; i < modelLists.size(); i++) {
            final Set<BitSet> additionalModels = modelLists.get(i);
            final Set<BitSet> newModels = new HashSet<>();
            for (final BitSet currentModel : currentModels) {
                for (final BitSet additionalModel : additionalModels) {
                    newModels.add(combineBitSets(currentModel, additionalModel));
                }
            }
            currentModels = newModels;
        }
        return currentModels;
    }

    private static BitSet combineBitSets(final BitSet currentModel, final BitSet additionalModel) {
        final BitSet copiedBitSet = (BitSet) currentModel.clone();
        copiedBitSet.or(additionalModel);
        return copiedBitSet;
    }

    /**
     * Returns the Cartesian product for the given variables, i.e. all
     * combinations of literals are generated with each variable occurring
     * positively and negatively.
     * @param variables the variables, must not be {@code null}
     * @return the Cartesian product
     */
    private static Set<BitSet> getCartesianProduct(final Collection<Integer> variables, final int numVars) {
        Set<BitSet> result = singleton(new BitSet(numVars * 2));
        for (final int var : variables) {
            final Set<BitSet> extended = new HashSet<>(result.size() * 2);
            for (final BitSet current : result) {
                extended.add(extendedByLiteral(current, 2 * var));
                extended.add(extendedByLiteral(current, 2 * var + 1));
            }
            result = extended;
        }
        return result;
    }

    private static BitSet extendedByLiteral(final BitSet current, final int lit) {
        final BitSet extended = (BitSet) current.clone();
        extended.set(lit);
        return extended;
    }

    private static Pair<List<Variable>, Map<Variable, Integer>> createVarMapping(final SortedSet<Variable> originalVariables) {
        final List<Variable> intToVariable = new ArrayList<>(originalVariables);
        final Map<Variable, Integer> variableToInt = new HashMap<>();
        for (int i = 0; i < intToVariable.size(); i++) {
            variableToInt.put(intToVariable.get(i), i);
        }
        return new Pair<>(intToVariable, variableToInt);
    }
}
