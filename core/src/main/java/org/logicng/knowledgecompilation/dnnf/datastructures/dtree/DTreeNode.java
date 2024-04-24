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

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.DnnfSatSolver;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.BitSet;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * An internal node in a DTree.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DTreeNode extends DTree {

    protected final DTree left;
    protected final DTree right;
    protected final int size;

    protected DnnfSatSolver solver;

    protected final SortedSet<Variable> staticVariableSet;
    protected final BitSet staticSeparatorBitSet;
    protected final int[] staticClauseIds;
    protected final int depth;
    protected int widestSeparator;

    protected final DTreeLeaf[] leafs; // all leafs
    protected final DTreeLeaf[] leftLeafs;
    protected final DTreeLeaf[] rightLeafs;

    protected int[] clauseContents; // content of all clauses under this node, e.g. clause {1,3} with id 0, {2,6,8} with id 1, {4,6} with id 6 --> [1,3,-1,2,6,-2,4,6,-7]
    protected int[] leftClauseContents;
    protected int[] rightClauseContents;

    protected BitSet localLeftVarSet;
    protected BitSet localRightVarSet;

    /**
     * Constructs a new DTree node with the given left and right DTree.
     * @param left  the left DTree
     * @param right the right DTree
     */
    public DTreeNode(final DTree left, final DTree right) {
        this.left = left;
        this.right = right;
        this.size = left.size() + right.size();

        final List<DTreeLeaf> ll = left.leafs();
        excludeUnitLeafs(ll);
        this.leftLeafs = ll.toArray(new DTreeLeaf[0]);
        final List<DTreeLeaf> rl = right.leafs();
        excludeUnitLeafs(rl);
        this.rightLeafs = rl.toArray(new DTreeLeaf[0]);
        this.leafs = new DTreeLeaf[this.leftLeafs.length + this.rightLeafs.length];
        System.arraycopy(this.leftLeafs, 0, this.leafs, 0, this.leftLeafs.length);
        System.arraycopy(this.rightLeafs, 0, this.leafs, this.leftLeafs.length, this.rightLeafs.length);

        this.staticVariableSet = new TreeSet<>(left.staticVariableSet());
        this.staticVariableSet.addAll(right.staticVariableSet());
        this.staticSeparatorBitSet = new BitSet();
        final int[] leftClauseIds = left.staticClauseIds();
        final int[] rightClauseIds = right.staticClauseIds();
        this.staticClauseIds = new int[leftClauseIds.length + rightClauseIds.length];
        System.arraycopy(leftClauseIds, 0, this.staticClauseIds, 0, leftClauseIds.length);
        System.arraycopy(rightClauseIds, 0, this.staticClauseIds, leftClauseIds.length, rightClauseIds.length);
        this.depth = 1 + Math.max(left.depth(), right.depth());
    }

    /**
     * Returns the left DTree
     * @return the left DTree
     */
    public DTree left() {
        return this.left;
    }

    /**
     * Returns the right DTree
     * @return the right DTree
     */
    public DTree right() {
        return this.right;
    }

    @Override
    public void initialize(final DnnfSatSolver solver) {
        this.solver = solver;
        this.left.initialize(solver);
        this.right.initialize(solver);
        this.staticVarSet = this.left.staticVarSet();
        this.staticVarSet.or(this.right.staticVarSet());
        this.staticVariables = toArray(this.staticVarSet);
        this.staticSeparator = sortedIntersect(this.left.staticVarSetArray(), this.right.staticVarSetArray());
        for (final int i : this.staticSeparator) {
            this.staticSeparatorBitSet.set(i);
        }
        this.widestSeparator = Math.max(this.staticSeparator.length, Math.max(this.left.widestSeparator(), this.right.widestSeparator()));
        this.localLeftVarSet = new BitSet(this.staticVariables[this.staticVariables.length - 1]);
        this.localRightVarSet = new BitSet(this.staticVariables[this.staticVariables.length - 1]);

        final LNGIntVector lClauseContents = new LNGIntVector();
        for (final DTreeLeaf leaf : this.leftLeafs) {
            for (final int i : leaf.literals()) {
                lClauseContents.push(i);
            }
            lClauseContents.push(-leaf.getId() - 1);
        }
        this.leftClauseContents = lClauseContents.toArray();
        final LNGIntVector rClauseContents = new LNGIntVector();
        for (final DTreeLeaf leaf : this.rightLeafs) {
            for (final int i : leaf.literals()) {
                rClauseContents.push(i);
            }
            rClauseContents.push(-leaf.getId() - 1);
        }
        this.rightClauseContents = rClauseContents.toArray();
        this.clauseContents = new int[this.leftClauseContents.length + this.rightClauseContents.length];
        System.arraycopy(this.leftClauseContents, 0, this.clauseContents, 0, this.leftClauseContents.length);
        System.arraycopy(this.rightClauseContents, 0, this.clauseContents, this.leftClauseContents.length, this.rightClauseContents.length);
    }

    @Override
    public int size() {
        return this.size;
    }

    @Override
    public SortedSet<Variable> staticVariableSet() {
        return this.staticVariableSet;
    }

    @Override
    public BitSet dynamicSeparator() {
        this.localLeftVarSet.clear();
        this.localRightVarSet.clear();
        varSet(this.leftClauseContents, this.localLeftVarSet);
        varSet(this.rightClauseContents, this.localRightVarSet);
        this.localLeftVarSet.and(this.localRightVarSet);
        return this.localLeftVarSet;
    }

    protected void varSet(final int[] clausesContents, final BitSet localVarSet) {
        int i = 0;
        while (i < clausesContents.length) {
            int j = i;
            boolean subsumed = false;
            while (clausesContents[j] >= 0) {
                if (!subsumed && this.solver.valueOf(clausesContents[j]) == Tristate.TRUE) {
                    subsumed = true;
                }
                j++;
            }
            if (!subsumed) {
                for (int n = i; n < j; n++) {
                    if (this.solver.valueOf(clausesContents[n]) == Tristate.UNDEF) {
                        localVarSet.set(MiniSatStyleSolver.var(clausesContents[n]));
                    }
                }
            }
            i = j + 1;
        }
    }

    @Override
    public int[] staticClauseIds() {
        return this.staticClauseIds;
    }

    /**
     * Sets the cache key according to this tree.
     * @param key               the key to set
     * @param numberOfVariables the number of variables
     */
    public void cacheKey(final BitSet key, final int numberOfVariables) {
        int i = 0;
        while (i < this.clauseContents.length) {
            int j = i;
            boolean subsumed = false;
            while (this.clauseContents[j] >= 0) {
                if (!subsumed && this.solver.valueOf(this.clauseContents[j]) == Tristate.TRUE) {
                    subsumed = true;
                }
                j++;
            }
            if (!subsumed) {
                key.set(-this.clauseContents[j] + 1 + numberOfVariables);
                for (int n = i; n < j; n++) {
                    if (this.solver.valueOf(this.clauseContents[n]) == Tristate.UNDEF) {
                        key.set(MiniSatStyleSolver.var(this.clauseContents[n]));
                    }
                }
            }
            i = j + 1;
        }
    }

    @Override
    public void countUnsubsumedOccurrences(final int[] occurrences) {
        for (final DTreeLeaf leaf : this.leafs) {
            leaf.countUnsubsumedOccurrences(occurrences);
        }
    }

    @Override
    public int depth() {
        return this.depth;
    }

    @Override
    public int widestSeparator() {
        return this.widestSeparator;
    }

    @Override
    public List<DTreeLeaf> leafs() {
        final List<DTreeLeaf> result = this.left.leafs();
        result.addAll(this.right.leafs());
        return result;
    }

    @Override
    public String toString() {
        return String.format("DTreeNode: [%s, %s]", this.left, this.right);
    }

    protected void excludeUnitLeafs(final List<DTreeLeaf> leafs) {
        leafs.removeIf(dTreeLeaf -> dTreeLeaf.clauseSize() == 1);
    }

    static int[] toArray(final BitSet bits) {
        final int[] result = new int[bits.cardinality()];
        int n = 0;
        for (int i = bits.nextSetBit(0); i != -1; i = bits.nextSetBit(i + 1)) {
            result[n++] = i;
        }
        return result;
    }

    static int[] sortedIntersect(final int[] left, final int[] right) {
        final SortedSet<Integer> l = new TreeSet<>();
        final SortedSet<Integer> intersection = new TreeSet<>();
        for (final int i : left) {
            l.add(i);
        }
        for (final int i : right) {
            if (l.contains(i)) {
                intersection.add(i);
            }
        }
        final int[] result = new int[intersection.size()];
        int i = 0;
        for (final Integer elem : intersection) {
            result[i++] = elem;
        }
        return result;
    }
}
