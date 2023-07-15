// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.orderings;

import static org.logicng.knowledgecompilation.bdds.orderings.MinToMaxOrdering.sortProfileByOccurrence;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.functions.VariableProfileFunction;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * A BDD variable ordering sorting the variables from maximal to minimal occurrence
 * in the input formula.  If two variables have the same number of occurrences, their
 * ordering according to their DFS ordering will be considered.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class MaxToMinOrdering implements VariableOrderingProvider {

    private final VariableProfileFunction profileFunction = VariableProfileFunction.get();
    private final DFSOrdering dfsOrdering = new DFSOrdering();

    @Override
    public List<Variable> getOrder(final Formula formula) {
        final Map<Variable, Integer> profile = formula.apply(this.profileFunction);
        final List<Variable> dfs = this.dfsOrdering.getOrder(formula);

        final Comparator<Map.Entry<Variable, Integer>> comparator = (o1, o2) -> {
            final int occComp = o1.getValue().compareTo(o2.getValue());
            if (occComp != 0) {
                return occComp;
            }
            final int index1 = dfs.indexOf(o1.getKey());
            final int index2 = dfs.indexOf(o2.getKey());
            return index1 - index2;
        };
        final Map<Variable, Integer> sortedProfile = sortProfileByOccurrence(profile, comparator);
        final List<Variable> order = new ArrayList<>(sortedProfile.size());
        for (final Map.Entry<Variable, Integer> entry : sortedProfile.entrySet()) {
            order.add(entry.getKey());
        }
        return order;
    }
}
