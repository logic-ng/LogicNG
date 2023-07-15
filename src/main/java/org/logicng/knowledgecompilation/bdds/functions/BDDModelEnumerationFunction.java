// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Performs model enumeration on a BDD. The models are returned as a list of {@link Assignment assignments}.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class BDDModelEnumerationFunction implements BDDFunction<List<Assignment>> {

    private final Collection<Variable> variables;

    /**
     * Constructs a new model enumeration function. The models are projected to a given set of variables.
     * @param variables the variables to which models are projected
     */
    public BDDModelEnumerationFunction(final Collection<Variable> variables) {
        this.variables = variables;
    }

    @Override
    public List<Assignment> apply(final BDD bdd) {
        final Set<Assignment> res = new HashSet<>();
        final BDDKernel kernel = bdd.underlyingKernel();
        final List<byte[]> models = new BDDOperations(kernel).allSat(bdd.index());
        final SortedSet<Integer> temp;
        if (this.variables == null) {
            temp = new TreeSet<>(kernel.var2idx().values());
        } else {
            temp = new TreeSet<>();
            for (final Map.Entry<Variable, Integer> e : kernel.var2idx().entrySet()) {
                if (this.variables.contains(e.getKey())) {
                    temp.add(e.getValue());
                }
            }
        }
        final int[] relevantIndices = new int[temp.size()];
        int count = 0;
        for (final Integer i : temp) {
            relevantIndices[count++] = i;
        }
        for (final byte[] model : models) {
            final List<Assignment> allAssignments = new ArrayList<>();
            generateAllModels(kernel, allAssignments, model, relevantIndices, 0);
            res.addAll(allAssignments);
        }
        return new ArrayList<>(res);
    }

    private void generateAllModels(final BDDKernel kernel, final List<Assignment> assignments, final byte[] model, final int[] relevantIndices,
                                   final int position) {
        if (position == relevantIndices.length) {
            final Assignment assignment = new Assignment();
            for (final int i : relevantIndices) {
                assignment.addLiteral(model[i] == 0 ? kernel.getVariableForIndex(i).negate() : kernel.getVariableForIndex(i));
            }
            assignments.add(assignment);
        } else if (model[relevantIndices[position]] != -1) {
            generateAllModels(kernel, assignments, model, relevantIndices, position + 1);
        } else {
            model[relevantIndices[position]] = 0;
            generateAllModels(kernel, assignments, model, relevantIndices, position + 1);
            model[relevantIndices[position]] = 1;
            generateAllModels(kernel, assignments, model, relevantIndices, position + 1);
            model[relevantIndices[position]] = -1;
        }
    }
}
