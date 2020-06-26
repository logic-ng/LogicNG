package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.DNNFSATSolver;

import java.util.BitSet;
import java.util.List;
import java.util.SortedSet;

/**
 * Super class for a Decomposition Tree (DTree) for the DNNF Compiler
 * This is either a {@link DTreeNode} or a {@link DTreeLeaf}.
 * @version 2.0.0
 * @since 2.0.0
 */
public abstract class DTree {

    int[] staticVariables;
    BitSet staticVarSet;
    int[] staticSeparator;

    /**
     * Initializes the DTree.
     * @param solver a specializes DNNF SAT solver
     */
    public abstract void initialize(final DNNFSATSolver solver);

    /**
     * Returns the size of the DTree.
     * @return the size of the DTree
     */
    public abstract int size();

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    int[] staticVarSetArray() {
        return this.staticVariables;
    }

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    public BitSet staticVarSet() {
        return this.staticVarSet;
    }

    /**
     * Returns all variables of this DTree.
     * <p>
     * Since this set of variables can be cached, this is a constant time operation.
     * @return all variables of this DTree
     */
    abstract SortedSet<Variable> staticVariableSet();

    /**
     * Computes the dynamic variable set of this DTree.  "Dynamic" means that subsumed clauses are ignored during the computation.
     * The dynamic variable set includes all variables which:
     * - are contained in the given set (interestingVars)
     * - are not yet assigned and
     * - occur in clauses that are currently not subsumed
     * @param vars the bitset in which the variables should be stored
     */
    abstract void dynamicVarSet(final BitSet vars);

    /**
     * The dynamic separator of this DTree.  "Dynamic" means that subsumed clauses are ignored during the separator computation.
     * @return The dynamic separator of this DTree
     */
    public abstract BitSet dynamicSeparator();

    /**
     * The ids clauses in this DTree.
     * @return The clause ids
     */
    abstract int[] staticClauseIds();

    /**
     * Sets the cache key according to this tree.
     * @param key               the key to set
     * @param numberOfVariables the number of variables
     */
    public abstract void cacheKey(final BitSet key, final int numberOfVariables);

    /**
     * Counts the number of unsubsumed occurrences for each variable in occurrences.
     * <p>
     * The parameter occurrences should be modified by the method accordingly.
     * @param occurrences The current number of occurrences for each variable which should be modified accordingly
     */
    public abstract void countUnsubsumedOccurrences(final int[] occurrences);

    /**
     * Returns the depth of this tree.
     * @return the depth of this tree
     */
    public abstract int depth();

    /**
     * Returns the widest separator of this tree.
     * @return the widest separator
     */
    public abstract int widestSeparator();

    /**
     * Returns all leafs of this tree.
     * @return all leafs of this tree
     */
    abstract List<DTreeLeaf> leafs();
}
