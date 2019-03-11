package org.logicng.transformations;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * A superclass for subsumptions (CNF or DNF).
 * @version 1.5.0
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
            List<SortedSet<Literal>> terms = mapping.get(term.literals().size());
            if (terms == null) {
                terms = new LinkedList<>();
                mapping.put(term.literals().size(), terms);
            }
            terms.add(term.literals());
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
