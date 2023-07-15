// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * A superclass for subsumptions (CNF or DNF).
 * @version 2.0.0
 * @since 1.5.0
 */
public abstract class Subsumption {

    /**
     * Generates a UBTree from the formulas operands (clauses in CNF, minterms in DNF)
     * where all subsumed operands are already deleted.
     * @param formula the formula (must be an n-ary operator and CNF or DNF)
     * @return the UBTree with the operands and deleted subsumed operands
     */
    protected static UBTree<Literal> generateSubsumedUBTree(final Formula formula) {
        final SortedMap<Integer, List<SortedSet<Literal>>> mapping = new TreeMap<>();
        for (final Formula term : formula) {
            mapping.computeIfAbsent(term.literals().size(), k -> new ArrayList<>()).add(term.literals());
        }
        final UBTree<Literal> ubTree = new UBTree<>();
        for (final Map.Entry<Integer, List<SortedSet<Literal>>> entry : mapping.entrySet()) {
            for (final SortedSet<Literal> set : entry.getValue()) {
                if (ubTree.firstSubset(set) == null) {
                    ubTree.addSet(set);
                }
            }
        }
        return ubTree;
    }
}
