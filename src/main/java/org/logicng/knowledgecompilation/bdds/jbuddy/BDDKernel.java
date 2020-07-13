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

/*========================================================================
           Copyright (C) 1996-2002 by Jorn Lind-Nielsen
                        All rights reserved

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, reproduce, prepare derivative
works, distribute, and display this software and its documentation
for any purpose, provided that (1) the above copyright notice and
the following two paragraphs appear in all copies of the source code
and (2) redistributions, including without limitation binaries,
reproduce these notices in the supporting documentation. Substantial
modifications to this software may be copyrighted by their authors
and need not follow the licensing terms described here, provided
that the new terms are clearly indicated in all files where they apply.

IN NO EVENT SHALL JORN LIND-NIELSEN, OR DISTRIBUTORS OF THIS
SOFTWARE BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL,
INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS
SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE AUTHORS OR ANY OF THE
ABOVE PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

JORN LIND-NIELSEN SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
========================================================================*/

package org.logicng.knowledgecompilation.bdds.jbuddy;

import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDTree.addRange;

import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.BDDHandler;
import org.logicng.util.Pair;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * The jBuddy kernel.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDKernel {

    public static final int BDD_ABORT = -1;
    public static final int BDD_TRUE = 1;
    public static final int BDD_FALSE = 0;

    protected static final int MAXVAR = 0x1FFFFF;
    protected static final int MAXREF = 0x3FF;
    protected static final int MARKON = 0x200000;
    protected static final int MARKOFF = 0x1FFFFF;
    protected static final int MARKHIDE = 0x1FFFFF;

    protected byte[] allunsatProfile;

    protected enum Operand {
        AND(0, new int[]{0, 0, 0, 1}),
        OR(2, new int[]{0, 1, 1, 1}),
        IMP(5, new int[]{1, 1, 0, 1}),
        EQUIV(6, new int[]{1, 0, 0, 1}),
        NOT(10, new int[]{1, 1, 0, 0});

        protected final int v;
        protected final int[] tt;

        Operand(final int value, final int[] truthTable) {
            this.v = value;
            this.tt = truthTable;
        }
    }

    protected static final int CACHEID_RESTRICT = 0x1;
    protected static final int CACHEID_SATCOU = 0x2;
    protected static final int CACHEID_PATHCOU_ONE = 0x4;
    protected static final int CACHEID_PATHCOU_ZERO = 0x8;
    protected static final int CACHEID_FORALL = 0x1;

    protected final FormulaFactory f;
    protected final SortedMap<Variable, Integer> var2idx;
    protected final SortedMap<Integer, Variable> idx2var;

    protected int[] nodes; // All of the bdd nodes
    protected int[] vars; // Set of defined BDD variables
    protected final int minfreenodes; // Minimal % of nodes that has to be left after a garbage collection
    protected int gbcollectnum; // Number of garbage collections
    protected final int cachesize; // Size of the operator caches
    protected int nodesize; // Number of allocated nodes
    protected final int maxnodeincrease; // Max. # of nodes used to inc. table
    protected int freepos; // First free node
    protected int freenum; // Number of free nodes
    protected long produced; // Number of new nodes ever produced
    protected int varnum; // Number of defined BDD variables
    protected int[] refstack; // Internal node reference stack
    protected int refstacktop; // Internal node reference stack top
    protected int[] level2var; // Level -> variable table
    protected int[] var2level; // Variable -> level table

    protected int[] quantvarset; // Current variable set for quant.
    protected int quantvarsetID; // Current id used in quantvarset
    protected int quantlast; // Current last variable to be quant.
    protected int supportID; // Current ID (true value) for support
    protected int supportMax; // Max. used level in support calc.
    protected int[] supportSet; // The found support set

    protected BDDCache applycache; // Cache for apply results
    protected BDDCache itecache; // Cache for ITE results
    protected BDDCache quantcache; // Cache for exist/forall results
    protected BDDCache appexcache; // Cache for appex/appall results
    protected BDDCache replacecache; // Cache for replace results
    protected BDDCache misccache; // Cache for other results

    //    protected final BDDPairs pairs;
    protected int usednodes_nextreorder;

    /**
     * Constructor for the BDD kernel.
     * @param f         the formula factory to use
     * @param numVars   the number of variables
     * @param nodeSize  the initial number of nodes in the nodetable
     * @param cacheSize the fixed size of the internal caches
     */
    public BDDKernel(final FormulaFactory f, final int numVars, final int nodeSize, final int cacheSize) {
        this.f = f;
        this.var2idx = new TreeMap<>();
        this.idx2var = new TreeMap<>();
        this.nodesize = BDDPrime.primeGTE(Math.max(nodeSize, 3));
        this.nodes = new int[this.nodesize * 6];
        this.minfreenodes = 20;
        for (int n = 0; n < this.nodesize; n++) {
            setRefcou(n, 0);
            setLow(n, -1);
            setHash(n, 0);
            setLevel(n, 0);
            setNext(n, n + 1);
        }
        setNext(this.nodesize - 1, 0);
        setRefcou(0, MAXREF);
        setRefcou(1, MAXREF);
        setLow(0, 0);
        setHigh(0, 0);
        setLow(1, 1);
        setHigh(1, 1);
        initOperators(Math.max(cacheSize, 3));
        this.freepos = 2;
        this.freenum = this.nodesize - 2;
        this.varnum = 0;
        this.gbcollectnum = 0;
        this.cachesize = cacheSize;
        this.usednodes_nextreorder = this.nodesize;
        this.maxnodeincrease = 50000;
        //        this.pairs = new BDDPairs();
        bdd_reorder_init();
        setNumberOfVars(numVars);
    }

    /**
     * Constructor for the BDD kernel.
     * @param f         the formula factory to use
     * @param ordering  the variable ordering
     * @param nodeSize  the initial number of nodes in the nodetable
     * @param cacheSize the fixed size of the internal caches
     */
    public BDDKernel(final FormulaFactory f, final List<Variable> ordering, final int nodeSize, final int cacheSize) {
        this(f, ordering.size(), nodeSize, cacheSize);
        for (final Variable var : ordering) {
            getOrAddVarIndex(var);
        }
    }

    /**
     * Sets the number of variables to use. It may be called more than one time, but only
     * to increase the number of variables.
     * @param num the number of variables to use
     */
    protected void setNumberOfVars(final int num) {
        if (num < 0 || num > MAXVAR) {
            throw new IllegalArgumentException("Invalid variable number: " + num);
        }
        final int oldbddvarnum = this.varnum;
        bdd_disable_reorder();
        this.vars = new int[num * 2];
        this.level2var = new int[num + 1];
        this.var2level = new int[num + 1];
        this.refstack = new int[num * 2 + 4];
        this.refstacktop = 0;
        while (this.varnum < num) {
            this.vars[this.varnum * 2] = pushRef(makeNode(this.varnum, 0, 1));
            this.vars[this.varnum * 2 + 1] = makeNode(this.varnum, 1, 0);
            popref(1);
            setRefcou(this.vars[this.varnum * 2], MAXREF);
            setRefcou(this.vars[this.varnum * 2 + 1], MAXREF);
            this.level2var[this.varnum] = this.varnum;
            this.var2level[this.varnum] = this.varnum;
            this.varnum++;
        }
        setLevel(0, num);
        setLevel(1, num);
        this.level2var[num] = num;
        this.var2level[num] = num;
        //        this.pairs.resize(oldbddvarnum, this.varnum, this);
        varResize();
        bdd_enable_reorder();
    }

    /**
     * Returns the index for the given variable.
     * <p>
     * If the variable hasn't been seen before, the next free variable index is assigned to it.
     * If no free variables are left, an illegal argument exception is thrown.
     * @param variable the variable
     * @return the index for the variable
     * @throws IllegalArgumentException if the variable does not yet exist in the kernel and there are no free variable indizes left
     */
    public int getOrAddVarIndex(final Variable variable) {
        Integer index = this.var2idx.get(variable);
        if (index == null) {
            if (this.var2idx.size() >= this.varnum) {
                throw new IllegalArgumentException("No free variables left! You did not set the number of variables high enough.");
            } else {
                index = this.var2idx.size();
                this.var2idx.put(variable, index);
                this.idx2var.put(index, variable);
            }
        }
        return index;
    }

    /**
     * Returns the formula factory.
     * @return the formula factory
     */
    public FormulaFactory factory() {
        return this.f;
    }

    /**
     * Returns the mapping from variables to indizes.
     * @return the mapping from variables to indizes
     */
    public SortedMap<Variable, Integer> var2idx() {
        return this.var2idx;
    }

    /**
     * Returns the mapping from indizes to variables.
     * @return the mapping from indizes to variables
     */
    public SortedMap<Integer, Variable> idx2var() {
        return this.idx2var;
    }

    /**
     * Returns the current ordering of the variables.
     * @return the current variable ordering
     */
    public int[] getCurrentVarOrder() {
        return Arrays.copyOf(this.level2var, this.level2var.length - 1); // last var is always 0
    }

    /**
     * Returns a BDD representing the i-th variable (one node with the children true and false).
     * @param i the index i
     * @return the BDD representing the i-th variable
     * @throws IllegalArgumentException if the index is not within the range of variables
     */
    public int ithVar(final int i) {
        if (i < 0 || i >= this.varnum) {
            throw new IllegalArgumentException("Illegal variable number: " + i);
        }
        return this.vars[i * 2];
    }

    /**
     * Returns a BDD representing the negation of the i-th variable (one node with the children true and false).
     * @param i the index i
     * @return the BDD representing the negated i-th variable
     * @throws IllegalArgumentException if the index is not within the range of variables
     */
    public int nithVar(final int i) {
        if (i < 0 || i >= this.varnum) {
            throw new IllegalArgumentException("Illegal variable number: " + i);
        }
        return this.vars[i * 2 + 1];
    }

    /**
     * Returns the variable index labeling the given root node.
     * @param root the root node of the BDD
     * @return the variable index
     */
    public int bddVar(final int root) {
        if (root < 2) {
            throw new IllegalArgumentException("Illegal node number: " + root);
        }
        return this.level2var[level(root)];
    }

    /**
     * Returns the false branch of the given root node.
     * @param root the root node of the BDD
     * @return the false branch
     */
    public int bddLow(final int root) {
        if (root < 2) {
            throw new IllegalArgumentException("Illegal node number: " + root);
        }
        return low(root);
    }

    /**
     * Returns the true branch of the given root node.
     * @param root the root node of the BDD
     * @return the true branch
     */
    public int bddHigh(final int root) {
        if (root < 2) {
            throw new IllegalArgumentException("Illegal node number: " + root);
        }
        return (high(root));
    }

    /**
     * Returns the conjunction of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the conjunction of the two BDDs
     */
    public int and(final int l, final int r) {
        return apply(l, r, Operand.AND);
    }

    /**
     * Returns the disjunction of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the disjunction of the two BDDs
     */
    public int or(final int l, final int r) {
        return apply(l, r, Operand.OR);
    }

    /**
     * Returns the implication of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the implication of the two BDDs
     */
    public int implication(final int l, final int r) {
        return apply(l, r, Operand.IMP);
    }

    /**
     * Returns the equivalence of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the equivalence of the two BDDs
     */
    public int equivalence(final int l, final int r) {
        return apply(l, r, Operand.EQUIV);
    }

    protected int doWithPotentialReordering(final BddOperation operation) {
        try {
            initRef();
            return operation.perform();
        } catch (final BddReorderRequest reorderRequest) {
            bdd_checkreorder();
            initRef();
            bdd_disable_reorder();
            try {
                return operation.perform();
            } catch (final BddReorderRequest e) {
                throw new IllegalStateException("Must not happen");
            } finally {
                bdd_enable_reorder();
            }
        }
    }

    protected int apply(final int l, final int r, final Operand op) {
        return doWithPotentialReordering(() -> applyRec(l, r, op));
    }

    protected int applyRec(final int l, final int r, final Operand op) throws BddReorderRequest {
        final int res;
        switch (op) {
            case AND:
                if (l == r) {
                    return l;
                }
                if (isZero(l) || isZero(r)) {
                    return 0;
                }
                if (isOne(l)) {
                    return r;
                }
                if (isOne(r)) {
                    return l;
                }
                break;
            case OR:
                if (l == r) {
                    return l;
                }
                if (isOne(l) || isOne(r)) {
                    return 1;
                }
                if (isZero(l)) {
                    return r;
                }
                if (isZero(r)) {
                    return l;
                }
                break;
            case IMP:
                if (isZero(l)) {
                    return 1;
                }
                if (isOne(l)) {
                    return r;
                }
                if (isOne(r)) {
                    return 1;
                }
                break;
        }
        if (isConst(l) && isConst(r)) {
            res = op.tt[l << 1 | r];
        } else {
            final BDDCacheEntry entry = this.applycache.lookup(triple(l, r, op.v));
            if (entry.a == l && entry.b == r && entry.c == op.v) {
                return entry.res;
            }
            if (level(l) == level(r)) {
                pushRef(applyRec(low(l), low(r), op));
                pushRef(applyRec(high(l), high(r), op));
                res = makeNode(level(l), readRef(2), readRef(1));
            } else if (level(l) < level(r)) {
                pushRef(applyRec(low(l), r, op));
                pushRef(applyRec(high(l), r, op));
                res = makeNode(level(l), readRef(2), readRef(1));
            } else {
                pushRef(applyRec(l, low(r), op));
                pushRef(applyRec(l, high(r), op));
                res = makeNode(level(r), readRef(2), readRef(1));
            }
            popref(2);
            entry.a = l;
            entry.b = r;
            entry.c = op.v;
            entry.res = res;
        }
        return res;
    }

    /**
     * Returns the negation of a BDD.
     * @param r the BDD
     * @return the negation of the BDD
     */
    public int not(final int r) {
        return doWithPotentialReordering(() -> notRec(r));
    }

    protected int notRec(final int r) throws BddReorderRequest {
        if (isZero(r)) {
            return BDDKernel.BDD_TRUE;
        }
        if (isOne(r)) {
            return BDDKernel.BDD_FALSE;
        }
        final BDDCacheEntry entry = this.applycache.lookup(r);
        if (entry.a == r && entry.c == Operand.NOT.v) {
            return entry.res;
        }
        pushRef(notRec(low(r)));
        pushRef(notRec(high(r)));
        final int res = makeNode(level(r), readRef(2), readRef(1));
        popref(2);
        entry.a = r;
        entry.c = Operand.NOT.v;
        entry.res = res;
        return res;
    }

    /**
     * Adds a reference for a given node.  Reference counting is done on externally referenced nodes only and the count for
     * a specific node {@code r} can and must be increased using this function to avoid loosing the node in the next
     * garbage collection.  If a BDD handler is given, the handler's {@link BDDHandler#newRefAdded()} method is called.
     * If the generation gets aborted due to the handler, the method will return {@link BDDKernel#BDD_ABORT} as result. If
     * {@code null} is passed as handler, the generation will continue without interruption.
     * @param root    the node
     * @param handler the BDD handler
     * @return return the node
     * @throws IllegalArgumentException if the root node was invalid
     */
    public int addRef(final int root, final BDDHandler handler) {
        if (handler != null && !handler.newRefAdded()) {
            return BDD_ABORT;
        }
        if (root < 2) {
            return root;
        }
        if (root >= this.nodesize) {
            throw new IllegalArgumentException("Not a valid BDD root node: " + root);
        }
        if (low(root) == -1) {
            throw new IllegalArgumentException("Not a valid BDD root node: " + root);
        }
        incRef(root);
        return root;
    }

    /**
     * Deletes a reference for a given node.
     * @param root the node
     * @throws IllegalArgumentException if the root node was invalid
     */
    public void delRef(final int root) {
        if (root < 2) {
            return;
        }
        if (root >= this.nodesize) {
            throw new IllegalStateException("Cannot dereference a variable > varnum");
        }
        if (this.low(root) == -1) {
            throw new IllegalStateException("Cannot dereference variable -1");
        }
        if (!this.hasref(root)) {
            throw new IllegalStateException("Cannot dereference a variable which has no reference");
        }
        this.decRef(root);
    }

    protected void decRef(final int n) {
        if (refcou(n) != MAXREF && refcou(n) > 0) {
            setRefcou(n, refcou(n) - 1);
        }
    }

    protected void incRef(final int n) {
        if (refcou(n) < MAXREF) {
            setRefcou(n, refcou(n) + 1);
        }
    }

    /**
     * Returns all nodes for a given root node in their internal representation.  The internal representation is stored
     * in an array: {@code [node number, variable, low, high]}
     * @param r the BDD root node
     * @return all Nodes in their internal representation
     */
    public List<int[]> allNodes(final int r) {
        final List<int[]> result = new ArrayList<>();
        if (r < 2) {
            return result;
        }
        mark(r);
        for (int n = 0; n < this.nodesize; n++) {
            if ((level(n) & MARKON) != 0) {
                setLevel(n, level(n) & MARKOFF);
                result.add(new int[]{n, this.level2var[level(n)], low(n), high(n)});
            }
        }
        return result;
    }

    protected int makeNode(final int level, final int low, final int high) throws BddReorderRequest {
        if (low == high) {
            return low;
        }
        int hash = nodehash(level, low, high);
        int res = hash(hash);
        while (res != 0) {
            if (level(res) == level && low(res) == low && high(res) == high) {
                return res;
            }
            res = next(res);
        }
        if (this.freepos == 0) {
            gbc();
            if ((this.nodesize - this.freenum) >= this.usednodes_nextreorder && bdd_reorder_ready()) {
                throw new BddReorderRequest();
            }
            if ((this.freenum * 100) / this.nodesize <= this.minfreenodes) {
                nodeResize(true);
                hash = nodehash(level, low, high);
            }
            if (this.freepos == 0) {
                throw new IllegalStateException("Cannot allocate more space for more nodes.");
            }
        }
        res = this.freepos;
        this.freepos = next(this.freepos);
        this.freenum--;
        this.produced++;
        setLevel(res, level);
        setLow(res, low);
        setHigh(res, high);
        setNext(res, hash(hash));
        setHash(hash, res);
        return res;
    }

    protected void unmark(final int i) {
        if (i < 2) {
            return;
        }
        if (!marked(i) || low(i) == -1) {
            return;
        }
        unmarkNode(i);
        unmark(low(i));
        unmark(high(i));
    }

    protected int markCount(final int i) {
        if (i < 2) {
            return 0;
        }
        if (marked(i) || low(i) == -1) {
            return 0;
        }
        setMark(i);
        int count = 1;
        count += markCount(low(i));
        count += markCount(high(i));
        return count;
    }

    protected void gbc() {
        for (int r = 0; r < this.refstacktop; r++) {
            mark(this.refstack[r]);
        }
        for (int n = 0; n < this.nodesize; n++) {
            if (refcou(n) > 0) {
                mark(n);
            }
            setHash(n, 0);
        }
        this.freepos = 0;
        this.freenum = 0;
        for (int n = this.nodesize - 1; n >= 2; n--) {
            if ((level(n) & MARKON) != 0 && low(n) != -1) {
                setLevel(n, level(n) & MARKOFF);
                final int hash = nodehash(level(n), low(n), high(n));
                setNext(n, hash(hash));
                setHash(hash, n);
            } else {
                setLow(n, -1);
                setNext(n, this.freepos);
                this.freepos = n;
                this.freenum++;
            }
        }
        resetCaches();
        this.gbcollectnum++;
    }

    protected void gbcRehash() {
        this.freepos = 0;
        this.freenum = 0;
        for (int n = this.nodesize - 1; n >= 2; n--) {
            if (low(n) != -1) {
                final int hash = nodehash(level(n), low(n), high(n));
                setNext(n, hash(hash));
                setHash(hash, n);
            } else {
                setNext(n, this.freepos);
                this.freepos = n;
                this.freenum++;
            }
        }
    }

    protected void mark(final int i) {
        if (i < 2) {
            return;
        }
        if ((level(i) & MARKON) != 0 || low(i) == -1) {
            return;
        }
        setLevel(i, level(i) | MARKON);
        mark(low(i));
        mark(high(i));
    }

    protected void nodeResize(final boolean doRehash) {
        final int oldsize = this.nodesize;
        int n;
        this.nodesize = this.nodesize << 1;
        if (this.nodesize > oldsize + this.maxnodeincrease) {
            this.nodesize = oldsize + this.maxnodeincrease;
        }
        this.nodesize = BDDPrime.primeLTE(this.nodesize);
        final int[] newnodes = new int[this.nodesize * 6];
        System.arraycopy(this.nodes, 0, newnodes, 0, this.nodes.length);
        this.nodes = newnodes;
        if (doRehash) {
            for (n = 0; n < oldsize; n++) {
                setHash(n, 0);
            }
        }
        for (n = oldsize; n < this.nodesize; n++) {
            setRefcou(n, 0);
            setHash(n, 0);
            setLevel(n, 0);
            setLow(n, -1);
            setNext(n, n + 1);
        }
        setNext(this.nodesize - 1, this.freepos);
        this.freepos = oldsize;
        this.freenum += this.nodesize - oldsize;
        if (doRehash) {
            gbcRehash();
        }
    }

    void bdd_checkreorder() {
        bdd_reorder_auto();
        /* Do not reorder before twice as many nodes have been used */
        this.usednodes_nextreorder = 2 * (this.nodesize - this.freenum);
        /* And if very little was gained this time (< 20%) then wait until
         * even more nodes (upto twice as many again) have been used */
        if (bdd_reorder_gain() < 20) {
            this.usednodes_nextreorder += (this.usednodes_nextreorder * (20 - bdd_reorder_gain())) / 20;
        }
    }

    protected int refcou(final int node) {
        return this.nodes[6 * node];
    }

    protected int level(final int node) {
        return this.nodes[6 * node + 1];
    }

    protected int low(final int node) {
        return this.nodes[6 * node + 2];
    }

    protected int high(final int node) {
        return this.nodes[6 * node + 3];
    }

    protected int hash(final int node) {
        return this.nodes[6 * node + 4];
    }

    protected int next(final int node) {
        return this.nodes[6 * node + 5];
    }

    protected void setRefcou(final int node, final int refcou) {
        this.nodes[6 * node] = refcou;
    }

    protected void setLevel(final int node, final int level) {
        this.nodes[6 * node + 1] = level;
    }

    protected void setLow(final int node, final int low) {
        this.nodes[6 * node + 2] = low;
    }

    protected void setHigh(final int node, final int high) {
        this.nodes[6 * node + 3] = high;
    }

    protected void setHash(final int node, final int hash) {
        this.nodes[6 * node + 4] = hash;
    }

    protected void setNext(final int node, final int next) {
        this.nodes[6 * node + 5] = next;
    }

    protected void initRef() {
        this.refstacktop = 0;
    }

    protected int pushRef(final int n) {
        this.refstack[this.refstacktop++] = n;
        return n;
    }

    protected int readRef(final int n) {
        return this.refstack[this.refstacktop - n];
    }

    protected void popref(final int n) {
        this.refstacktop -= n;
    }

    protected boolean hasref(final int n) {
        return refcou(n) > 0;
    }

    protected boolean isConst(final int n) {
        return n < 2;
    }

    protected boolean isOne(final int n) {
        return n == 1;
    }

    protected boolean isZero(final int n) {
        return n == 0;
    }

    protected boolean marked(final int n) {
        return (level(n) & MARKON) != 0;
    }

    protected void setMark(final int n) {
        setLevel(n, level(n) | MARKON);
    }

    protected void unmarkNode(final int n) {
        setLevel(n, level(n) & MARKOFF);
    }

    protected int nodehash(final int lvl, final int l, final int h) {
        return Math.abs(triple(lvl, l, h) % this.nodesize);
    }

    protected int nodehash_reorder(final int var, final int l, final int h) {
        return Math.abs(pair(l, h) % this.levels[var].size) + this.levels[var].start;
    }

    protected int pair(final int a, final int b) {
        return (a + b) * (a + b + 1) / 2 + a;
    }

    protected int triple(final int a, final int b, final int c) {
        return pair(c, pair(a, b));
    }

    protected void initOperators(final int cachesize) {
        this.applycache = new BDDCache(cachesize);
        this.itecache = new BDDCache(cachesize);
        this.quantcache = new BDDCache(cachesize);
        this.appexcache = new BDDCache(cachesize);
        this.replacecache = new BDDCache(cachesize);
        this.misccache = new BDDCache(cachesize);
        this.quantvarsetID = 0;
        this.quantvarset = null;
        this.supportSet = null;
    }

    protected void resetCaches() {
        this.applycache.reset();
        this.itecache.reset();
        this.quantcache.reset();
        this.appexcache.reset();
        this.replacecache.reset();
        this.misccache.reset();
    }

    protected void varResize() {
        this.quantvarset = new int[this.varnum];
        this.quantvarsetID = 0;
    }

    /**
     * Restricts the variables in the BDD {@code r} to constants true or false.  The restriction is submitted in the BDD
     * {@code var}.
     * @param r   the BDD to be restricted
     * @param var the variable mapping as a BDD
     * @return the restricted BDD
     */
    public int restrict(final int r, final int var) {
        if (var < 2) {
            return r;
        }
        varset2svartable(var);
        return doWithPotentialReordering(() -> restrictRec(r, (var << 3) | CACHEID_RESTRICT));
    }

    protected int restrictRec(final int r, final int miscid) throws BddReorderRequest {
        final int res;
        if (isConst(r) || level(r) > this.quantlast) {
            return r;
        }
        final BDDCacheEntry entry = this.misccache.lookup(pair(r, miscid));
        if (entry.a == r && entry.c == miscid) {
            return entry.res;
        }
        if (insvarset(level(r))) {
            if (this.quantvarset[level(r)] > 0) {
                res = restrictRec(high(r), miscid);
            } else {
                res = restrictRec(low(r), miscid);
            }
        } else {
            pushRef(restrictRec(low(r), miscid));
            pushRef(restrictRec(high(r), miscid));
            res = makeNode(level(r), readRef(2), readRef(1));
            popref(2);
        }
        entry.a = r;
        entry.c = miscid;
        entry.res = res;
        return res;
    }

    /**
     * Existential quantifier elimination for the variables in {@code var}.
     * @param r   the BDD root node
     * @param var the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public int exists(final int r, final int var) {
        if (var < 2) {
            return r;
        }
        varset2vartable(var);
        return doWithPotentialReordering(() -> quantRec(r, Operand.OR, var << 3));
    }

    /**
     * Universal quantifier elimination for the variables in {@code var}.
     * @param r   the BDD root node
     * @param var the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public int forAll(final int r, final int var) {
        final int res;
        if (var < 2) {
            return r;
        }
        varset2vartable(var);
        return doWithPotentialReordering(() -> quantRec(r, Operand.AND, (var << 3) | CACHEID_FORALL));
    }

    protected int quantRec(final int r, final Operand op, final int quantid) throws BddReorderRequest {
        final int res;
        if (r < 2 || level(r) > this.quantlast) {
            return r;
        }
        final BDDCacheEntry entry = this.quantcache.lookup(r);
        if (entry.a == r && entry.c == quantid) {
            return entry.res;
        }
        pushRef(quantRec(low(r), op, quantid));
        pushRef(quantRec(high(r), op, quantid));
        if (invarset(level(r))) {
            res = applyRec(readRef(2), readRef(1), op);
        } else {
            res = makeNode(level(r), readRef(2), readRef(1));
        }
        popref(2);
        entry.a = r;
        entry.c = quantid;
        entry.res = res;
        return res;
    }

    /**
     * Finds one satisfying variable assignment and returns it as BDD.
     * @param r the BDD root node
     * @return the satisfying variable assignment of the BDD as a BDD itself
     */
    public int satOne(final int r) {
        if (r < 2) {
            return r;
        }
        bdd_disable_reorder();
        initRef();
        final int res = satOneRec(r);
        bdd_enable_reorder();
        return res;
    }

    protected int satOneRec(final int r) throws BddReorderRequest {
        if (isConst(r)) {
            return r;
        }
        if (isZero(low(r))) {
            final int res = satOneRec(high(r));
            return pushRef(makeNode(level(r), BDDKernel.BDD_FALSE, res));
        } else {
            final int res = satOneRec(low(r));
            return pushRef(makeNode(level(r), res, BDDKernel.BDD_FALSE));
        }
    }

    /**
     * Returns an arbitrary model for a given BDD or {@code null} which contains at least the given variables.  If a variable
     * is a don't care variable, it will be assigned with the given default value.
     * @param r   the BDD root node
     * @param var the set of variable which has to be contained in the model as a BDD
     * @param pol the default value for don't care variables as a BDD
     * @return an arbitrary model of this BDD
     */
    public int satOneSet(final int r, final int var, final int pol) {
        if (isZero(r)) {
            return r;
        }
        if (!isConst(pol)) {
            throw new IllegalArgumentException("polarity for satOneSet must be a constant");
        }
        bdd_disable_reorder();
        initRef();
        final int res = satOneSetRec(r, var, pol);
        bdd_enable_reorder();
        return res;
    }

    protected int satOneSetRec(final int r, final int var, final int satPolarity) throws BddReorderRequest {
        if (isConst(r) && isConst(var)) {
            return r;
        }
        if (level(r) < level(var)) {
            if (isZero(low(r))) {
                final int res = satOneSetRec(high(r), var, satPolarity);
                return pushRef(makeNode(level(r), BDDKernel.BDD_FALSE, res));
            } else {
                final int res = satOneSetRec(low(r), var, satPolarity);
                return pushRef(makeNode(level(r), res, BDDKernel.BDD_FALSE));
            }
        } else if (level(var) < level(r)) {
            final int res = satOneSetRec(r, high(var), satPolarity);
            if (satPolarity == BDDKernel.BDD_TRUE) {
                return pushRef(makeNode(level(var), BDDKernel.BDD_FALSE, res));
            } else {
                return pushRef(makeNode(level(var), res, BDDKernel.BDD_FALSE));
            }
        } else {
            if (isZero(low(r))) {
                final int res = satOneSetRec(high(r), high(var), satPolarity);
                return pushRef(makeNode(level(r), BDDKernel.BDD_FALSE, res));
            } else {
                final int res = satOneSetRec(low(r), high(var), satPolarity);
                return pushRef(makeNode(level(r), res, BDDKernel.BDD_FALSE));
            }
        }
    }

    /**
     * Returns a full model in all variables for the given BDD.
     * @param r the BDD root node
     * @return a full model of this BDD
     */
    public int fullSatOne(final int r) {
        if (r == 0) {
            return 0;
        }
        bdd_disable_reorder();
        initRef();
        int res = fullSatOneRec(r);
        for (int v = level(r) - 1; v >= 0; v--) {
            res = pushRef(makeNode(v, res, 0));
        }
        bdd_enable_reorder();
        return res;
    }

    protected int fullSatOneRec(final int r) throws BddReorderRequest {
        if (r < 2) {
            return r;
        }
        if (low(r) != 0) {
            int res = fullSatOneRec(low(r));
            for (int v = level(low(r)) - 1; v > level(r); v--) {
                res = pushRef(makeNode(v, res, 0));
            }
            return pushRef(makeNode(level(r), res, 0));
        } else {
            int res = fullSatOneRec(high(r));
            for (int v = level(high(r)) - 1; v > level(r); v--) {
                res = pushRef(makeNode(v, res, 0));
            }
            return pushRef(makeNode(level(r), 0, res));
        }
    }

    /**
     * Returns all models for a given BDD.
     * @param r the BDD root node
     * @return all models for the BDD
     */
    public List<byte[]> allSat(final int r) {
        final byte[] allsatProfile = new byte[this.varnum];
        for (int v = level(r) - 1; v >= 0; --v) {
            allsatProfile[this.level2var[v]] = -1;
        }
        initRef();
        final List<byte[]> allSat = new ArrayList<>();
        allSatRec(r, allSat, allsatProfile);
        return allSat;
    }

    protected void allSatRec(final int r, final List<byte[]> models, final byte[] allsatProfile) {
        if (isOne(r)) {
            models.add(Arrays.copyOf(allsatProfile, allsatProfile.length));
            return;
        }
        if (isZero(r)) {
            return;
        }
        if (!isZero(low(r))) {
            allsatProfile[this.level2var[level(r)]] = 0;
            for (int v = level(low(r)) - 1; v > level(r); --v) {
                allsatProfile[this.level2var[v]] = -1;
            }
            allSatRec(low(r), models, allsatProfile);
        }
        if (!isZero(high(r))) {
            allsatProfile[this.level2var[level(r)]] = 1;
            for (int v = level(high(r)) - 1; v > level(r); --v) {
                allsatProfile[this.level2var[v]] = -1;
            }
            allSatRec(high(r), models, allsatProfile);
        }
    }

    /**
     * Returns the model count for the given BDD.
     * @param r the BDD root node
     * @return the model count for the BDD
     */
    public BigInteger satCount(final int r) {
        final BigInteger size = BigInteger.valueOf(2).pow(level(r));
        return satCountRec(r, CACHEID_SATCOU).multiply(size);
    }

    protected BigInteger satCountRec(final int root, final int miscid) {
        if (root < 2) {
            return BigInteger.valueOf(root);
        }
        final BDDCacheEntry entry = this.misccache.lookup(root);
        if (entry.a == root && entry.c == miscid) {
            return entry.bdres;
        }
        BigInteger size = BigInteger.ZERO;
        BigInteger s = BigInteger.ONE;
        s = s.multiply(BigInteger.valueOf(2).pow(level(low(root)) - level(root) - 1));
        size = size.add(s.multiply(satCountRec(low(root), miscid)));
        s = BigInteger.ONE;
        s = s.multiply(BigInteger.valueOf(2).pow(level(high(root)) - level(root) - 1));
        size = size.add(s.multiply(satCountRec(high(root), miscid)));
        entry.a = root;
        entry.c = miscid;
        entry.bdres = size;
        return size;
    }

    /**
     * Returns the number of paths to the terminal node 'one'.
     * @param r the BDD root node
     * @return the number of paths to the terminal node 'one'
     */
    public BigInteger pathCountOne(final int r) {
        return pathCountRecOne(r, CACHEID_PATHCOU_ONE);
    }

    protected BigInteger pathCountRecOne(final int r, final int miscid) {
        final BigInteger size;
        if (isZero(r)) {
            return BigInteger.ZERO;
        }
        if (isOne(r)) {
            return BigInteger.ONE;
        }
        final BDDCacheEntry entry = this.misccache.lookup(r);
        if (entry.a == r && entry.c == miscid) {
            return entry.bdres;
        }
        size = pathCountRecOne(low(r), miscid).add(pathCountRecOne(high(r), miscid));
        entry.a = r;
        entry.c = miscid;
        entry.bdres = size;
        return size;
    }

    /**
     * Returns the number of paths to the terminal node 'zero'.
     * @param r the BDD root node
     * @return the number of paths to the terminal node 'zero'
     */
    public BigInteger pathCountZero(final int r) {
        return pathCountRecZero(r, CACHEID_PATHCOU_ZERO);
    }

    protected BigInteger pathCountRecZero(final int r, final int miscid) {
        final BigInteger size;
        if (isZero(r)) {
            return BigInteger.ONE;
        }
        if (isOne(r)) {
            return BigInteger.ZERO;
        }
        final BDDCacheEntry entry = this.misccache.lookup(r);
        if (entry.a == r && entry.c == miscid) {
            return entry.bdres;
        }
        size = pathCountRecZero(low(r), miscid).add(pathCountRecZero(high(r), miscid));
        entry.a = r;
        entry.c = miscid;
        entry.bdres = size;
        return size;
    }

    /**
     * Returns all unsatisfiable assignments for a given BDD.
     * @param r the BDD root node
     * @return all unsatisfiable assignments for the BDD
     */
    public List<byte[]> allUnsat(final int r) {
        this.allunsatProfile = new byte[this.varnum];
        for (int v = level(r) - 1; v >= 0; --v) {
            this.allunsatProfile[this.level2var[v]] = -1;
        }
        initRef();
        final List<byte[]> allUnsat = new ArrayList<>();
        allUnsatRec(r, allUnsat);
        return allUnsat;
    }

    protected void allUnsatRec(final int r, final List<byte[]> models) {
        if (isZero(r)) {
            models.add(Arrays.copyOf(this.allunsatProfile, this.allunsatProfile.length));
            return;
        }
        if (isOne(r)) {
            return;
        }
        if (!isOne(low(r))) {
            this.allunsatProfile[this.level2var[level(r)]] = 0;
            for (int v = level(low(r)) - 1; v > level(r); --v) {
                this.allunsatProfile[this.level2var[v]] = -1;
            }
            allUnsatRec(low(r), models);
        }
        if (!isOne(high(r))) {
            this.allunsatProfile[this.level2var[level(r)]] = 1;
            for (int v = level(high(r)) - 1; v > level(r); --v) {
                this.allunsatProfile[this.level2var[v]] = -1;
            }
            allUnsatRec(high(r), models);
        }
    }

    /**
     * Returns all the variables that a given BDD depends on.
     * @param r the BDD root node
     * @return all the variables that the BDD depends on
     */
    public int support(final int r) {
        final int supportSize = 0;
        int res = 1;
        if (r < 2) {
            return BDDKernel.BDD_FALSE;
        }
        if (supportSize < this.varnum) {
            this.supportSet = new int[this.varnum];
            this.supportID = 0;
        }
        if (this.supportID == 0x0FFFFFFF) {
            this.supportID = 0;
        }
        ++this.supportID;
        final int supportMin = level(r);
        this.supportMax = supportMin;
        supportRec(r, this.supportSet);
        unmark(r);

        bdd_disable_reorder();
        for (int n = this.supportMax; n >= supportMin; --n) {
            if (this.supportSet[n] == this.supportID) {
                addRef(res, null);
                final int tmp = makeNode(n, 0, res);
                delRef(res);
                res = tmp;
            }
        }
        bdd_enable_reorder();
        return res;
    }

    protected void supportRec(final int r, final int[] support) {
        if (r < 2) {
            return;
        }
        if ((level(r) & BDDKernel.MARKON) != 0 || low(r) == -1) {
            return;
        }
        support[level(r)] = this.supportID;
        if (level(r) > this.supportMax) {
            this.supportMax = level(r);
        }
        setLevel(r, level(r) | BDDKernel.MARKON);
        supportRec(low(r), support);
        supportRec(high(r), support);
    }

    /**
     * Returns the number of nodes for a given BDD.
     * @param r the BDD root node
     * @return the number of nodes for the BDD
     */
    public int nodeCount(final int r) {
        final int count = markCount(r);
        unmark(r);
        return count;
    }

    /**
     * Returns how often each variable occurs in the given BDD.
     * @param r the BDD root node
     * @return how often each variable occurs in the BDD
     */
    public int[] varProfile(final int r) {
        final int[] varprofile = new int[this.varnum];
        varProfileRec(r, varprofile);
        unmark(r);
        return varprofile;
    }

    protected void varProfileRec(final int r, final int[] varprofile) {
        if (r < 2) {
            return;
        }
        if ((level(r) & BDDKernel.MARKON) != 0) {
            return;
        }
        varprofile[this.level2var[level(r)]]++;
        setLevel(r, level(r) | BDDKernel.MARKON);
        varProfileRec(low(r), varprofile);
        varProfileRec(high(r), varprofile);
    }

    protected void varset2svartable(final int r) {
        if (r < 2) {
            throw new IllegalArgumentException("Illegal variable: " + r);
        }
        this.quantvarsetID++;
        if (this.quantvarsetID == Integer.MAX_VALUE / 2) {
            this.quantvarset = new int[this.varnum];
            this.quantvarsetID = 1;
        }
        for (int n = r; !isConst(n); ) {
            if (isZero(low(n))) {
                this.quantvarset[level(n)] = this.quantvarsetID;
                n = high(n);
            } else {
                this.quantvarset[level(n)] = -this.quantvarsetID;
                n = low(n);
            }
            this.quantlast = level(n);
        }
    }

    protected void varset2vartable(final int r) {
        if (r < 2) {
            throw new IllegalArgumentException("Illegal variable: " + r);
        }
        this.quantvarsetID++;
        if (this.quantvarsetID == Integer.MAX_VALUE) {
            this.quantvarset = new int[this.varnum];
            this.quantvarsetID = 1;
        }
        for (int n = r; n > 1; n = high(n)) {
            this.quantvarset[level(n)] = this.quantvarsetID;
            this.quantlast = level(n);
        }
    }

    protected boolean insvarset(final int a) {
        return Math.abs(this.quantvarset[a]) == this.quantvarsetID;
    }

    protected boolean invarset(final int a) {
        return this.quantvarset[a] == this.quantvarsetID;
    }

    /**
     * Returns the statistics for this BDD Kernel.
     * @return the statistics
     */
    public BDDStatistics statistics() {
        final BDDStatistics statistics = new BDDStatistics();
        statistics.produced = this.produced;
        statistics.nodesize = this.nodesize;
        statistics.freenum = this.freenum;
        statistics.varnum = this.varnum;
        statistics.cachesize = this.cachesize;
        statistics.gbcollectnum = this.gbcollectnum;
        return statistics;
    }

    /**
     * A class for BDD statistics.
     */
    public static final class BDDStatistics {
        protected long produced;
        protected int nodesize;
        protected int freenum;
        protected int varnum;
        protected int cachesize;
        protected int gbcollectnum;

        /**
         * Returns the number of produced nodes.
         * @return the number of produced nodes
         */
        public long produced() {
            return this.produced;
        }

        /**
         * Returns the number of allocated nodes.
         * @return the number of allocated nodes
         */
        public int nodesize() {
            return this.nodesize;
        }

        /**
         * Returns the number of free nodes.
         * @return the number of free nodes
         */
        public int freenum() {
            return this.freenum;
        }

        /**
         * Returns the number of variables.
         * @return the number of variables
         */
        public int varnum() {
            return this.varnum;
        }

        /**
         * Returns the cache size.
         * @return the cache size
         */
        public int cachesize() {
            return this.cachesize;
        }

        /**
         * Returns the number of garbage collections.
         * @return the number of garbage collections
         */
        public int gbcollectnum() {
            return this.gbcollectnum;
        }

        /**
         * Returns the number of used nodes.
         * @return the number of used nodes
         */
        public int usedNodes() {
            return this.nodesize - this.freenum;
        }

        @Override
        public String toString() {
            return "BDDStatistics{" +
                    "produced nodes=" + this.produced +
                    ", allocated nodes=" + this.nodesize +
                    ", free nodes=" + this.freenum +
                    ", variables=" + this.varnum +
                    ", cache size=" + this.cachesize +
                    ", garbage collections=" + this.gbcollectnum +
                    '}';
        }
    }

    //////////// REORDERING ////////////

    /* IMPORTANT:
     * The semantics of the "level" field in the BddNode struct changes during
     * variable reordering in order to make a fast variable swap possible when
     * two variables are independent. Instead of refering to the level of the node
     * it refers to the *variable* !!!
     */

    /* Change macros to reflect the above idea */
    protected int VAR(final int n) {
        return level(n);
    }

    //    protected int VARp(final BddNode p) {
    //        return p.level;
    //    }

    /*=== Reordering algorithms ============================================*/

    /* Current auto reord. method and number of automatic reorderings left */
    protected BDDReordering bddreordermethod;
    protected int bddreordertimes;

    /* Flag for disabling reordering temporarily */
    protected boolean reorderdisabled;

    /* Store for the variable relationships */
    protected BDDTree vartree;
    protected int blockid;

    /* Store for the ref.cou. of the external roots */
    protected int[] extroots;
    protected int extrootsize;

    protected LevelData[] levels; /* Indexed by variable! */

    /* Interaction matrix */
    protected InteractionMatrix iactmtx;

    /* Number of live nodes before and after a reordering session */
    protected int usednum_before;
    protected int usednum_after;

    /* Flag telling us when a node table resize is done */
    protected boolean resizedInMakenode;

    protected void bdd_reorder_init() {
        this.reorderdisabled = false;
        this.vartree = null;
        bdd_clrvarblocks();
        bdd_autoreorder_times(BDDReordering.BDD_REORDER_NONE, 0);
        this.usednum_before = this.usednum_after = 0;
        this.blockid = 0;
    }

    protected int reorder_nodenum() {
        return this.nodesize - this.freenum;
    }

    /**
     * Reorders the levels in this kernel using the given reordering method.
     * Only blocks of variables will be reordered. See the documentation of
     * {@link #addVariableBlock} to learn more about such variable blocks.
     * Without the definition of any block, nothing will be reordered.
     * <p>
     * If the reordering should be performed without any restrictions,
     * {@link #addVariableBlockAll()} can be called before this method.
     * @param method the method to be used for the reordering
     */
    public void bdd_reorder(final BDDReordering method) {
        final BDDTree top;
        final BDDReordering savemethod = this.bddreordermethod;
        final int savetimes = this.bddreordertimes;
        this.bddreordermethod = method;
        this.bddreordertimes = 1;
        top = new BDDTree(-1);
        if (reorder_init() < 0) {
            return;
        }
        this.usednum_before = this.nodesize - this.freenum;
        top.setFirst(0);
        top.setLast(this.varnum - 1);
        top.setFixed(false);
        top.setNext(null);
        top.setNextlevel(this.vartree);

        reorder_block(top, method);
        this.vartree = top.getNextlevel();
        this.usednum_after = this.nodesize - this.freenum;
        reorder_done();
        this.bddreordermethod = savemethod;
        this.bddreordertimes = savetimes;
    }

    protected BDDTree reorder_block(final BDDTree t, final BDDReordering method) {
        BDDTree thisTree;
        if (t == null) {
            return null;
        }
        if (!t.isFixed() && t.getNextlevel() != null) {
            switch (method) {
                case BDD_REORDER_WIN2:
                    t.setNextlevel(reorder_win2(t.getNextlevel()));
                    break;
                case BDD_REORDER_WIN2ITE:
                    t.setNextlevel(reorder_win2ite(t.getNextlevel()));
                    break;
                case BDD_REORDER_SIFT:
                    t.setNextlevel(reorder_sift(t.getNextlevel()));
                    break;
                case BDD_REORDER_SIFTITE:
                    t.setNextlevel(reorder_siftite(t.getNextlevel()));
                    break;
                case BDD_REORDER_WIN3:
                    t.setNextlevel(reorder_win3(t.getNextlevel()));
                    break;
                case BDD_REORDER_WIN3ITE:
                    t.setNextlevel(reorder_win3ite(t.getNextlevel()));
                    break;
                case BDD_REORDER_RANDOM:
                    t.setNextlevel(reorder_random(t.getNextlevel()));
                    break;
            }
        }
        for (thisTree = t.getNextlevel(); thisTree != null; thisTree = thisTree.getNext()) {
            reorder_block(thisTree, method);
        }
        if (t.getSeq() != null) {
            // qsort(t->seq, t->last-t->first+1, sizeof(int), varseqCmp);
            t.setSeq(Arrays.stream(t.getSeq()).limit(t.getLast() - t.getFirst() + 1).boxed()
                    .sorted(this::varseqCmp)
                    .mapToInt(i -> i)
                    .toArray());
        }
        return t;
    }

    protected int varseqCmp(final Integer aa, final Integer bb) {
        final int a = this.var2level[aa];
        final int b = this.var2level[bb];
        return Integer.compare(a, b);
    }

    protected void reorder_done() {
        for (int n = 0; n < this.extrootsize; n++) {
            setMark(this.extroots[n]);
        }
        for (int n = 2; n < this.nodesize; n++) {
            if (marked(n)) {
                unmark(n);
            } else {
                setRefcou(n, 0);
            }
            /* This is where we go from .var to .level again! - Do NOT use the LEVEL macro here. */
            setLevel(n, this.var2level[level(n)]);
        }
        gbc();
    }

    protected BDDTree reorder_win2(final BDDTree t) {
        BDDTree thisTree = t;
        BDDTree first = t;
        if (t == null) {
            return t;
        }
        while (thisTree.getNext() != null) {
            final int best = reorder_nodenum();
            blockdown(thisTree);
            if (best < reorder_nodenum()) {
                blockdown(thisTree.getPrev());
                thisTree = thisTree.getNext();
            } else if (first == thisTree) {
                first = thisTree.getPrev();
            }
        }
        return first;
    }

    protected BDDTree reorder_win2ite(final BDDTree t) {
        BDDTree thisTree = t;
        BDDTree first = t;
        if (t == null) {
            return t;
        }
        int lastsize;

        do {
            lastsize = reorder_nodenum();
            thisTree = t;
            while (thisTree.getNext() != null) {
                final int best = reorder_nodenum();

                blockdown(thisTree);

                if (best < reorder_nodenum()) {
                    blockdown(thisTree.getPrev());
                    thisTree = thisTree.getNext();
                } else if (first == thisTree) {
                    first = thisTree.getPrev();
                }
            }
        }
        while (reorder_nodenum() != lastsize);

        return first;
    }

    protected BDDTree reorder_win3(final BDDTree t) {
        BDDTree thisTree = t;
        BDDTree first = t;

        if (t == null) {
            return t;
        }

        while (thisTree.getNext() != null) {
            final Pair<BDDTree, BDDTree> swapResult = reorder_swapwin3(thisTree);
            thisTree = swapResult.first();
            first = swapResult.second() != null ? swapResult.second() : first;
        }

        return first;
    }

    protected BDDTree reorder_win3ite(final BDDTree t) {
        BDDTree thisTree = t;
        BDDTree first = t;
        int lastsize;

        if (t == null) {
            return t;
        }

        do {
            lastsize = reorder_nodenum();
            thisTree = first;

            while (thisTree.getNext() != null && thisTree.getNext().getNext() != null) {
                final Pair<BDDTree, BDDTree> swapResult = reorder_swapwin3(thisTree);
                thisTree = swapResult.first();
                first = swapResult.second() != null ? swapResult.second() : first;
            }
        }
        while (reorder_nodenum() != lastsize);
        return first;
    }

    protected Pair<BDDTree, BDDTree> reorder_swapwin3(BDDTree thisTree) {
        BDDTree first = null;
        final boolean setfirst = thisTree.getPrev() == null;
        BDDTree next = thisTree;
        int best = reorder_nodenum();

        if (thisTree.getNext().getNext() == null) /* Only two blocks left -> win2 swap */ {
            blockdown(thisTree);

            if (best < reorder_nodenum()) {
                blockdown(thisTree.getPrev());
                next = thisTree.getNext();
            } else {
                next = thisTree;
                if (setfirst) {
                    first = thisTree.getPrev();
                }
            }
        } else /* Real win3 swap */ {
            int pos = 0;
            blockdown(thisTree);  /* B A* C (4) */
            pos++;
            if (best > reorder_nodenum()) {
                pos = 0;
                best = reorder_nodenum();
            }

            blockdown(thisTree);  /* B C A* (3) */
            pos++;
            if (best > reorder_nodenum()) {
                pos = 0;
                best = reorder_nodenum();
            }

            thisTree = thisTree.getPrev().getPrev();
            blockdown(thisTree);  /* C B* A (2) */
            pos++;
            if (best > reorder_nodenum()) {
                pos = 0;
                best = reorder_nodenum();
            }

            blockdown(thisTree);  /* C A B* (1) */
            pos++;
            if (best > reorder_nodenum()) {
                pos = 0;
                best = reorder_nodenum();
            }

            thisTree = thisTree.getPrev().getPrev();
            blockdown(thisTree);  /* A C* B (0)*/
            pos++;
            if (best > reorder_nodenum()) {
                pos = 0;
                best = reorder_nodenum();
            }

            if (pos >= 1)  /* A C B -> C A* B */ {
                thisTree = thisTree.getPrev();
                blockdown(thisTree);
                next = thisTree;
                if (setfirst) {
                    first = thisTree.getPrev();
                }
            }

            if (pos >= 2)  /* C A B -> C B A* */ {
                blockdown(thisTree);
                next = thisTree.getPrev();
                if (setfirst) {
                    first = thisTree.getPrev().getPrev();
                }
            }

            if (pos >= 3)  /* C B A -> B C* A */ {
                thisTree = thisTree.getPrev().getPrev();
                blockdown(thisTree);
                next = thisTree;
                if (setfirst) {
                    first = thisTree.getPrev();
                }
            }

            if (pos >= 4)  /* B C A -> B A C* */ {
                blockdown(thisTree);
                next = thisTree.getPrev();
                if (setfirst) {
                    first = thisTree.getPrev().getPrev();
                }
            }

            if (pos >= 5)  /* B A C -> A B* C */ {
                thisTree = thisTree.getPrev().getPrev();
                blockdown(thisTree);
                next = thisTree;
                if (setfirst) {
                    first = thisTree.getPrev();
                }
            }
        }
        return new Pair<>(next, first);
    }

    /**
     * Do sifting iteratively until no more improvement can be found
     */
    protected BDDTree reorder_siftite(final BDDTree t) {
        BDDTree first = t;
        int lastsize;

        if (t == null) {
            return t;
        }

        do {
            lastsize = reorder_nodenum();
            first = reorder_sift(first);
        }
        while (reorder_nodenum() != lastsize);

        return first;
    }

    /**
     * Find sifting sequence based on the number of nodes at each level
     */
    protected BDDTree reorder_sift(BDDTree t) {
        BDDTree thisTree;
        final BDDTree[] seq;
        final BDDSizePair[] p;
        int n, num;

        for (thisTree = t, num = 0; thisTree != null; thisTree = thisTree.getNext()) {
            thisTree.setPos(num++);
        }

        p = new BDDSizePair[num];
        for (int i = 0; i < p.length; i++) {
            p[i] = new BDDSizePair();
        }
        seq = new BDDTree[num];

        for (thisTree = t, n = 0; thisTree != null; thisTree = thisTree.getNext(), n++) {
            int v;

            /* Accumulate number of nodes for each block */
            p[n].val = 0;
            for (v = thisTree.getFirst(); v <= thisTree.getLast(); v++) {
                p[n].val = p[n].val - this.levels[v].nodenum;
            }

            p[n].block = thisTree;
        }

        /* Sort according to the number of nodes at each level */
        Arrays.sort(p, 0, num, this::siftTestCmp);

        /* Create sequence */
        for (n = 0; n < num; n++) {
            seq[n] = p[n].block;
        }

        /* Do the sifting on this sequence */
        t = reorder_sift_seq(t, seq, num);

        return t;
    }

    /**
     * Go through all blocks in a specific sequence and find best
     * position for each of them
     */
    protected BDDTree reorder_sift_seq(final BDDTree t, final BDDTree[] seq, final int num) {
        BDDTree thisTree;
        int n;

        if (t == null) {
            return t;
        }

        for (n = 0; n < num; n++) {
            reorder_sift_bestpos(seq[n], num / 2);
        }

        /* Find first block */
        for (thisTree = t; thisTree.getPrev() != null; thisTree = thisTree.getPrev()) {
            /* nil */
        }

        return thisTree;
    }

    /**
     * Move a specific block up and down in the order and place at last in
     * the best position
     */
    protected void reorder_sift_bestpos(final BDDTree blk, final int middlePos) {
        int best = reorder_nodenum();
        int maxAllowed = best / 5 + best;
        int bestpos = 0;
        boolean dirIsUp = true;
        int n;

        /* Determine initial direction */
        if (blk.getPos() > middlePos) {
            dirIsUp = false;
        }

        /* Move block back and forth */
        for (n = 0; n < 2; n++) {
            boolean first = true;

            if (dirIsUp) {
                while (blk.getPrev() != null &&
                        (reorder_nodenum() <= maxAllowed || first)) {
                    first = false;
                    blockdown(blk.getPrev());
                    bestpos--;

                    if (reorder_nodenum() < best) {
                        best = reorder_nodenum();
                        bestpos = 0;
                        maxAllowed = best / 5 + best;
                    }
                }
            } else {
                while (blk.getNext() != null &&
                        (reorder_nodenum() <= maxAllowed || first)) {
                    first = false;
                    blockdown(blk);
                    bestpos++;

                    if (reorder_nodenum() < best) {
                        best = reorder_nodenum();
                        bestpos = 0;
                        maxAllowed = best / 5 + best;
                    }
                }
            }
            dirIsUp = !dirIsUp;
        }

        /* Move to best pos */
        while (bestpos < 0) {
            blockdown(blk);
            bestpos++;
        }
        while (bestpos > 0) {
            blockdown(blk.getPrev());
            bestpos--;
        }
    }

    protected int siftTestCmp(final BDDSizePair a, final BDDSizePair b) {
        return Integer.compare(a.val, b.val);
    }

    /**
     * === Random reordering (mostly for debugging and test ) =============
     */
    protected BDDTree reorder_random(final BDDTree t) {
        BDDTree thisTree;
        final BDDTree[] seq;
        int n, num = 0;

        if (t == null) {
            return t;
        }

        for (thisTree = t; thisTree != null; thisTree = thisTree.getNext()) {
            num++;
        }
        seq = new BDDTree[num];
        for (thisTree = t, num = 0; thisTree != null; thisTree = thisTree.getNext()) {
            seq[num++] = thisTree;
        }

        final Random random = new Random(42);
        for (n = 0; n < 4 * num; n++) {
            final int blk = random.nextInt(num);
            if (seq[blk].getNext() != null) {
                blockdown(seq[blk]);
            }
        }

        /* Find first block */
        for (thisTree = t; thisTree.getPrev() != null; thisTree = thisTree.getPrev()) {
            /* nil */
        }

        return thisTree;
    }

    /**
     * Swaps adjacent blocks
     * @param left the left BDD tree
     */
    protected void blockdown(final BDDTree left) {
        final BDDTree right = left.getNext();
        int n;
        final int leftsize = left.getLast() - left.getFirst();
        final int rightsize = right.getLast() - right.getFirst();
        final int leftstart = this.var2level[left.getSeq()[0]];
        final int[] lseq = left.getSeq();
        final int[] rseq = right.getSeq();

        /* Move left past right */
        while (this.var2level[lseq[0]] < this.var2level[rseq[rightsize]]) {
            for (n = 0; n < leftsize; n++) {
                if (this.var2level[lseq[n]] + 1 != this.var2level[lseq[n + 1]] && this.var2level[lseq[n]] < this.var2level[rseq[rightsize]]) {
                    reorder_vardown(lseq[n]);
                }
            }

            if (this.var2level[lseq[leftsize]] < this.var2level[rseq[rightsize]]) {
                reorder_vardown(lseq[leftsize]);
            }
        }

        /* Move right to where left started */
        while (this.var2level[rseq[0]] > leftstart) {
            for (n = rightsize; n > 0; n--) {
                if (this.var2level[rseq[n]] - 1 != this.var2level[rseq[n - 1]] && this.var2level[rseq[n]] > leftstart) {
                    reorder_varup(rseq[n]);
                }
            }

            if (this.var2level[rseq[0]] > leftstart) {
                reorder_varup(rseq[0]);
            }
        }

        /* Swap left and right data in the order */
        left.setNext(right.getNext());
        right.setPrev(left.getPrev());
        left.setPrev(right);
        right.setNext(left);

        if (right.getPrev() != null) {
            right.getPrev().setNext(right);
        }
        if (left.getNext() != null) {
            left.getNext().setPrev(left);
        }
        n = left.getPos();
        left.setPos(right.getPos());
        right.setPos(n);
    }

    protected void reorder_varup(final int var) {
        if (var < 0 || var >= this.varnum) {
            throw new IllegalStateException("Illegal variable in reordering");
        }
        if (this.var2level[var] != 0) {
            reorder_vardown(this.level2var[this.var2level[var] - 1]);
        }
    }

    protected void reorder_vardown(final int var) {
        int n;
        final int level;
        if (var < 0 || var >= this.varnum) {
            throw new IllegalStateException("Illegal variable in reordering");
        }
        level = this.var2level[var];
        if (level >= this.varnum - 1) {
            return;
        }
        this.resizedInMakenode = false;

        if (this.iactmtx.depends(var, this.level2var[level + 1]) > 0) {
            final int toBeProcessed = reorder_downSimple(var);
            reorder_swap(toBeProcessed, var);
            reorder_localGbc(var);
        }

        /* Swap the var<->level tables */
        n = this.level2var[level];
        this.level2var[level] = this.level2var[level + 1];
        this.level2var[level + 1] = n;
        n = this.var2level[var];
        this.var2level[var] = this.var2level[this.level2var[level]];
        this.var2level[this.level2var[level]] = n;
        /* Update all rename pairs */
        //        this.pairs.vardown(level);

        if (this.resizedInMakenode) {
            reorder_rehashAll();
        }
    }

    protected int reorder_downSimple(final int var0) {
        int toBeProcessed = 0;
        final int var1 = this.level2var[this.var2level[var0] + 1];
        final int vl0 = this.levels[var0].start;
        final int size0 = this.levels[var0].size;
        int n;

        this.levels[var0].nodenum = 0;

        for (n = 0; n < size0; n++) {
            int r;
            r = hash(n + vl0);
            setHash(n + vl0, 0);
            while (r != 0) {
                final int next = next(r);
                if (VAR(low(r)) != var1 && VAR(high(r)) != var1) {
                    /* Node does not depend on next var, let it stay in the chain */
                    setNext(r, hash(n + vl0));
                    setHash(n + vl0, r);
                    this.levels[var0].nodenum++;
                } else {
                    /* Node depends on next var - save it for later procesing */
                    setNext(r, toBeProcessed);
                    toBeProcessed = r;
                }
                r = next;
            }
        }
        return toBeProcessed;
    }

    protected void reorder_swap(int toBeProcessed, final int var0) {
        final int var1 = this.level2var[this.var2level[var0] + 1];
        while (toBeProcessed > 0) {
            final int next = next(toBeProcessed);
            int f0 = low(toBeProcessed);
            int f1 = high(toBeProcessed);
            final int f00;
            final int f01;
            final int f10;
            final int f11;
            final int hash;

            /* Find the cofactors for the new nodes */
            if (VAR(f0) == var1) {
                f00 = low(f0);
                f01 = high(f0);
            } else {
                f00 = f01 = f0;
            }
            if (VAR(f1) == var1) {
                f10 = low(f1);
                f11 = high(f1);
            } else {
                f10 = f11 = f1;
            }

            /* Note: makenode does refcou. */
            f0 = reorder_makenode(var0, f00, f10);
            f1 = reorder_makenode(var0, f01, f11);
            //            assert node == this.nodes[toBeProcessed];
            //            node = this.nodes[toBeProcessed];  /* Might change in makenode [SHi: why? I don't think so] */

            /* We know that the refcou of the grandchilds of this node
             * is greater than one (these are f00...f11), so there is
             * no need to do a recursive refcou decrease. It is also
             * possible for the LOWp(node)/high nodes to come alive again,
             * so deref. of the childs is delayed until the local GBC. */

            decRef(low(toBeProcessed));
            decRef(high(toBeProcessed));

            /* Update in-place */
            setLevel(toBeProcessed, var1);
            setLow(toBeProcessed, f0);
            setHigh(toBeProcessed, f1);
            this.levels[var1].nodenum++;
            /* Rehash the node since it got new childs */
            hash = nodehash_reorder(VAR(toBeProcessed), low(toBeProcessed), high(toBeProcessed));
            setNext(toBeProcessed, hash(hash));
            setHash(hash, toBeProcessed);
            toBeProcessed = next;
        }
    }

    protected int reorder_makenode(final int var, final int low, final int high) {
        final int hash;
        int res;

        /* Note: We know that low,high has a refcou greater than zero, so there is no need to add reference *recursively* */
        /* check whether childs are equal */
        if (low == high) {
            incRef(low);
            return low;
        }

        /* Try to find an existing node of this kind */
        hash = nodehash_reorder(var, low, high);
        res = hash(hash);

        while (res != 0) {
            if (low(res) == low && high(res) == high) {
                incRef(res);
                return res;
            }
            res = next(res);
        }
        /* No existing node -> build one */
        /* Any free nodes to use ? */
        if (this.freepos == 0) {
            /* Try to allocate more nodes - call noderesize without
             * enabling rehashing.
             * Note: if ever rehashing is allowed here, then remember to
             * update local variable "hash" */
            nodeResize(false);
            this.resizedInMakenode = true;
            assert this.freepos > 0;
        }

        /* Build new node */
        res = this.freepos;
        this.freepos = next(this.freepos);
        this.levels[var].nodenum++;
        this.produced++;
        this.freenum--;

        setLevel(res, var);
        setLow(res, low);
        setHigh(res, high);

        /* Insert node in hash chain */
        setNext(res, hash(hash));
        setHash(hash, res);

        /* Make sure it is reference counted */
        setRefcou(res, 1);
        incRef(low(res));
        incRef(high(res));
        return res;
    }

    protected void reorder_localGbc(final int var0) {
        final int var1 = this.level2var[this.var2level[var0] + 1];
        final int vl1 = this.levels[var1].start;
        final int size1 = this.levels[var1].size;
        int n;

        for (n = 0; n < size1; n++) {
            final int hash = n + vl1;
            int r = hash(hash);
            setHash(hash, 0);
            while (r > 0) {
                final int next = next(r);

                if (refcou(r) > 0) {
                    setNext(r, hash(hash));
                    setHash(hash, r);
                } else {
                    decRef(low(r));
                    decRef(high(r));
                    setLow(r, -1);
                    setNext(r, this.freepos);
                    this.freepos = r;
                    this.levels[var1].nodenum--;
                    this.freenum++;
                }
                r = next;
            }
        }
    }

    protected void reorder_rehashAll() {
        int n;
        reorder_setLevellookup();
        this.freepos = 0;
        for (n = this.nodesize - 1; n >= 0; n--) {
            setHash(n, 0);
        }
        for (n = this.nodesize - 1; n >= 2; n--) {
            if (refcou(n) > 0) {
                final int hash = nodehash_reorder(VAR(n), low(n), high(n));
                setNext(n, hash(hash));
                setHash(hash, n);
            } else {
                setNext(n, this.freepos);
                this.freepos = n;
            }
        }
    }

    protected void reorder_setLevellookup() {
        int n;
        for (n = 0; n < this.varnum; n++) {
            this.levels[n].maxsize = this.nodesize / this.varnum;
            this.levels[n].start = n * this.levels[n].maxsize;
            this.levels[n].size = this.levels[n].maxsize;
            if (this.levels[n].size >= 4) {
                this.levels[n].size = BDDPrime.primeLTE(this.levels[n].size);
            }
        }
    }

    protected void bdd_clrvarblocks() {
        this.vartree = null;
        this.blockid = 0;
    }

    /**
     * Activates or deactivates the automatic reordering during the construction of a BDD.
     * <p>
     * Automatic reordering can be deactivated by passing {@link BDDReordering#BDD_REORDER_NONE}
     * for the {@code method} parameter, otherwise the reordering is activated with the
     * given method. The reordering is executed at most {@code num} times.
     * @param method the method to be used for reordering
     * @param num    the maximum number of reorders to be performed
     */
    public void bdd_autoreorder_times(final BDDReordering method, final int num) {
        this.bddreordermethod = method;
        this.bddreordertimes = num;
    }

    protected void bdd_disable_reorder() {
        this.reorderdisabled = true;
    }

    protected void bdd_enable_reorder() {
        this.reorderdisabled = false;
    }

    protected boolean bdd_reorder_ready() {
        return this.bddreordermethod != BDDReordering.BDD_REORDER_NONE && this.vartree != null && this.bddreordertimes != 0 && !this.reorderdisabled;
    }

    protected void bdd_reorder_auto() {
        if (!bdd_reorder_ready()) {
            return;
        }
        bdd_reorder(this.bddreordermethod);
        this.bddreordertimes--;
    }

    protected int reorder_init() {
        this.levels = new LevelData[this.varnum];
        for (int n = 0; n < this.varnum; n++) {
            this.levels[n] = new LevelData();
            this.levels[n].start = -1;
            this.levels[n].size = 0;
            this.levels[n].nodenum = 0;
        }
        /* First mark and recursive refcou. all roots and childs. Also do some
         * setup here for both setLevellookup and reorder_gbc */
        if (mark_roots() < 0) {
            return -1;
        }
        /* Initialize the hash tables */
        reorder_setLevellookup();
        /* Garbage collect and rehash to new scheme */
        reorder_gbc();
        return 0;
    }

    protected int mark_roots() {
        final int[] dep = new int[this.varnum];
        this.extrootsize = 0;
        for (int n = 2; n < this.nodesize; n++) {
            /* This is where we go from .level to .var! - Do NOT use the LEVEL macro here. */
            setLevel(n, this.level2var[level(n)]);
            if (refcou(n) > 0) {
                this.extrootsize++;
                setMark(n);
            }
        }
        this.extroots = new int[this.extrootsize];
        this.iactmtx = new InteractionMatrix(this.varnum);
        this.extrootsize = 0;
        for (int n = 2; n < this.nodesize; n++) {
            if (marked(n)) {
                unmarkNode(n);
                this.extroots[this.extrootsize++] = n;
                dep[VAR(n)] = 1;
                this.levels[VAR(n)].nodenum++;
                addref_rec(low(n), dep);
                addref_rec(high(n), dep);
                addDependencies(dep);
            }
            /* Make sure the hash field is empty. This saves a loop in the initial GBC */
            setHash(n, 0);
        }
        setHash(0, 0);
        setHash(1, 0);
        return 0;
    }

    protected void reorder_gbc() {
        this.freepos = 0;
        this.freenum = 0;
        /* No need to zero all hash fields - this is done in mark_roots */
        for (int n = this.nodesize - 1; n >= 2; n--) {
            if (refcou(n) > 0) {
                final int hash = nodehash_reorder(VAR(n), low(n), high(n));
                setNext(n, hash(hash));
                setHash(hash, n);
            } else {
                setLow(n, -1);
                setNext(n, this.freepos);
                this.freepos = n;
                this.freenum++;
            }
        }
    }

    protected void addref_rec(final int r, final int[] dep) {
        if (r < 2) {
            return;
        }
        if (refcou(r) == 0) {
            this.freenum--;

            /* Detect variable dependencies for the interaction matrix */
            dep[VAR(r) & MARKHIDE] = 1;

            /* Make sure the nodenum field is updated. Used in the initial GBC */
            this.levels[VAR(r) & MARKHIDE].nodenum++;

            addref_rec(low(r), dep);
            addref_rec(high(r), dep);
        } else {
            int n;

            /* Update (from previously found) variable dependencies
             * for the interaction matrix */
            for (n = 0; n < this.varnum; n++) {
                dep[n] |= this.iactmtx.depends(VAR(r) & MARKHIDE, n);
            }
        }
        incRef(r);
    }

    protected void addDependencies(final int[] dep) {
        for (int n = 0; n < this.varnum; n++) {
            for (int m = n; m < this.varnum; m++) {
                if (dep[n] > 0 && dep[m] > 0) {
                    this.iactmtx.set(n, m);
                    this.iactmtx.set(m, n);
                }
            }
        }
    }

    /**
     * Adds a variable block starting at variable {@code first} and ending in variable
     * {@code last} (both inclusive).
     * <p>
     * <b>Variable blocks</b> are used in the {@link #bdd_reorder BDD reordering}
     * or in the automatic reordering during the construction of the BDD (configured by
     * {@link #bdd_autoreorder_times}). Variable blocks can be nested, i.e. one block can
     * contain an arbitrary number of ("child") blocks. Furthermore, a variable block can also
     * be a single variable.
     * <p>
     * During reordering, the child blocks of a parent block can be reordered, but they are kept
     * together. So no other block can be moved in between the child blocks. Furthermore
     * variables in a block which are not in a child block will be left untouched.
     * <p>
     * Example: Lets assume we have a BDD with the variable ordering {@code v1, v2, v3, v4, v5, v6, v7}.
     * We create the following blocks:
     * <ul>
     *     <li>{@code A}  reaching from {@code v1} to {@code v5}</li>
     *     <li>{@code B}  reaching from {@code v6} to {@code v7}</li>
     *     <li>{@code A1} reaching from {@code v1} to {@code v2}</li>
     *     <li>{@code A2} reaching from {@code v3} to {@code v3}</li>
     *     <li>{@code A3} reaching from {@code v4} to {@code v5}</li>
     * </ul>
     * This means that the variables of {@code A} and {@code B} can never be mixed up in the order.
     * So during reordering the variables {@code v6} and {@code v7} can either be moved to the
     * front (before {@code A}) or remain at their position.
     * Furthermore for example {@code v1} and {@code v2} will always stay together and neither
     * {@code v3} nor any other variable can be moved in between them. On the other hand, the blocks
     * {@code A1}, {@code A2}, and {@code A3} can be swapped arbitrarily.
     * <p>
     * These are valid result of a reordering based on the above blocks:
     * <ul>
     *     <li>{@code v3, v1, v2, v4, v5, v6, v7}</li>
     *     <li>{@code v6, v7, v4, v5, v3, v1, v2}</li>
     *     <li>{@code v6, v7, v1, v2, v3, v4, v5}</li>
     * </ul>
     * These however would be <b>illegal</b>:
     * <ul>
     *     <li>{@code v2, v1, v3, v4, v5, v6, v7} (variables in a block which are not in a child block will not be reordered)</li>
     *     <li>{@code v1, v3, v2, v4, v5, v6, v7} (variables of different block will not be mixed up)</li>
     * </ul>
     * <p>
     * If a block is <b>fixed</b> (the example above assumed always blocks which are not fixed), its
     * immediate child blocks will remain in their order. E.g. if block {@code A} was fixed, the blocks
     * {@code A1}, {@code A2}, and {@code A3} would not be allowed to be swapped.
     * Let's assume block {@code A} to be fixed an that we have two other unfixed blocks:
     * <ul>
     *     <li>{@code A11} reaching from {@code v1} to {@code v1}</li>
     *     <li>{@code A12} reaching from {@code v2} to {@code v2}</li>
     * </ul>
     * These are examples for <b>legal</b> reorderings:
     * <ul>
     *     <li>{@code v2, v1, v3, v4, v5, v6, v7} (block {@code A} is fixed, but "grandchildren" are still allowed to be reordered</li>
     *     <li>{@code v6, v7, v2, v1, v3, v4, v5}</li>
     * </ul>
     * These are examples for <b>illegal</b> reorderings:
     * <ul>
     *     <li>{@code v3, v2, v1, v4, v5, v6, v7} (block {@code A} is fixed, so it's child blocks must be be reordered</li>
     *     <li>{@code v1, v2, v4, v5, v3, v6, v7}</li>
     * </ul>
     * <p>
     * Each block (including all nested blocks) must be defined by a separate call to this method. The blocks
     * may be added in an arbitrary order, so it is not required to add them top-down or buttom-up.
     * However, the blocks <b>must not intersect</b>, except of one block containing the other. Furthermore
     * both the {@code first} and the {@code last} variable must be known by this kernel and the level {@code first}
     * must be lower than the level of {@code last}.
     * @param first the variable at which the block starts (inclusive)
     * @param last  the variable at which the block ends (inclusive)
     * @param fixed whether the block should be fixed or not
     */
    public void addVariableBlock(final int first, final int last, final boolean fixed) {
        if (first < 0 || first >= this.varnum || last < 0 || last >= this.varnum) {
            throw new IllegalArgumentException("invalid var range from " + first + " to " + last);
        }
        final BDDTree t = addRange(this.vartree, first, last, fixed, this.blockid, this.level2var);
        if (t == null) {
            throw new IllegalStateException("Could not add range to tree");
        }
        this.vartree = t;
        this.blockid++;
    }

    /**
     * Adds a variable block for all the variables which can be reached from the given bdd.
     * For details, see the documentation of {@link #addVariableBlock(int, int, boolean)}.
     * @param bdd   the bdd
     * @param fixed whether the block should be fixed or not
     */
    public void addVariableBlock(final int bdd, final boolean fixed) {
        int first;
        int last;
        final int[] v = scanset(bdd);
        if (v.length < 1) {
            throw new IllegalStateException("Constant BDD in variable block");
        }
        first = last = v[0];
        for (final int n : v) {
            if (n < first) {
                first = n;
            }
            if (n > last) {
                last = n;
            }
        }
        final BDDTree t = addRange(this.vartree, first, last, fixed, this.blockid, this.level2var);
        if (t == null) {
            throw new IllegalStateException("Could not add range to tree");
        }
        this.vartree = t;
        this.blockid++;
    }

    /**
     * Adds a single variable block or all variables known by this kernel.
     */
    public void addVariableBlockAll() {
        for (int n = 0; n < this.varnum; n++) {
            addVariableBlock(n, n, false);
        }
    }

    protected int[] scanset(final int r) {
        if (r < 0 || (r) >= this.nodesize || r >= 2 && low(r) == -1) {
            throw new IllegalArgumentException("Invalid BDD " + r + " as input");
        }
        if (r < 2) {
            return new int[0];
        }
        int num = 0;
        for (int n = r; n > 1; n = high(n)) {
            num++;
        }
        final int[] varset = new int[num];
        num = 0;
        for (int n = r; n > 1; n = high(n)) {
            varset[num++] = this.level2var[level(n)];
        }
        return varset;
    }

    protected int bdd_reorder_gain() {
        if (this.usednum_before == 0) {
            return 0;
        }
        return (100 * (this.usednum_before - this.usednum_after)) / this.usednum_before;
    }

    /* Level data */
    protected static class LevelData {
        int start;    /* Start of this sub-table (entry in "bddnodes") */
        int size;     /* Size of this sub-table */
        int maxsize;  /* Max. allowed size of sub-table */
        int nodenum;  /* Number of nodes in this level */
    }

    protected static class BDDSizePair {
        protected int val;
        protected BDDTree block;
    }

    protected static class InteractionMatrix {
        protected final int[][] rows;

        InteractionMatrix(final int size) {
            this.rows = new int[size][];
            for (int n = 0; n < size; n++) {
                this.rows[n] = new int[size / 8 + 1];
            }
        }

        void set(final int a, final int b) {
            this.rows[a][b / 8] |= 1 << (b % 8);
        }

        int depends(final int a, final int b) {
            return this.rows[a][b / 8] & (1 << (b % 8));
        }
    }

    /////////////////// Reordering SwapVar /////////////////////////////////

    /**
     * Swaps the variables {@code v1} and {@code v2}. This affects all BDDs
     * created by this kernel.
     * @param v1 the first variable to swap
     * @param v2 the second variable to swap
     */
    public void bdd_swapvar(int v1, int v2) {
        int l1, l2;
        /* Do not swap when variable-blocks are used */
        if (this.vartree != null) {
            throw new IllegalStateException("Swapping variables is not allowed with variable blocks");
        }

        /* Don't bother swapping x with x */
        if (v1 == v2) {
            return;
        }

        /* Make sure the variable exists */
        if (v1 < 0 || v1 >= this.varnum) {
            throw new IllegalArgumentException("Unknown variable number: " + v1);
        }

        if (v2 < 0 || v2 >= this.varnum) {
            throw new IllegalArgumentException("Unknown variable number: " + v2);
        }

        l1 = this.var2level[v1];
        l2 = this.var2level[v2];

        /* Make sure v1 is before v2 */
        if (l1 > l2) {
            final int tmp = v1;
            v1 = v2;
            v2 = tmp;
            l1 = this.var2level[v1];
            l2 = this.var2level[v2];
        }

        reorder_init();
        /* Move v1 to v2's position */
        while (this.var2level[v1] < l2) {
            reorder_vardown(v1);
        }

        /* Move v2 to v1's position */
        while (this.var2level[v2] > l1) {
            reorder_varup(v2);
        }
        reorder_done();
    }

    /////////////////// Verification /////////////////////////////////

    /**
     * Debug method for verifying the consistency of the BDD at index {@code root}.
     * @param root the root of the BDD
     * @return whether the BDD is valid or not
     */
    public boolean verify(final int root) {
        final int varnum = this.level2var.length - 1;
        for (int i = 0; i < varnum * 2 + 2; i++) {
            if (refcou(i) != BDDKernel.MAXREF) {
                System.out.println("Constant or Variable without MAXREF count: " + i);
                return false;
            }
            if (i == 0 && (low(i) != 0 || high(i) != 0 || level(i) != varnum)) {
                System.out.println("Illegal FALSE node");
                return false;
            }
            if (i == 1 && (low(i) != 1 || high(i) != 1 || level(i) != varnum)) {
                System.out.println("Illegal TRUE node");
                return false;
            }
            if (i > 1 && i % 2 == 0) {
                if (low(i) != 0) {
                    System.out.println("VAR Low wrong");
                    return false;
                } else if (high(i) != 1) {
                    System.out.println("VAR High wrong");
                    return false;
                }
            }
            if (i > 1 && i % 2 == 1) {
                if (low(i) != 1) {
                    System.out.println("VAR Low wrong");
                    return false;
                } else if (high(i) != 0) {
                    System.out.println("VAR High wrong");
                    return false;
                }
            }
            if (i > 1 && level(i) >= varnum) { //this.level2var[node.level] != i / 2 - 1) {
                System.out.println("VAR Level wrong");
                return false;
            }
        }
        if (root >= 0) {
            for (int i = varnum * 2 + 2; i < this.nodesize; i++) {
                if (refcou(i) > 1) {
                    System.out.println("Refcou > 1");
                    return false;
                } else if (refcou(i) == 1 && i != root) {
                    System.out.println("Wrong refcou");
                    return false;
                } else if (refcou(i) == 0 && i == root) {
                    System.out.println("Entry point not marked");
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Debug method for verifying the consistency of the BDD at index {@code root}.
     * @param root the root of the BDD
     * @return whether the BDD is valid or not
     */
    public long verifyTree(final int root) {
        return verifyTreeRec(root, new long[this.nodes.length]);
    }

    protected long verifyTreeRec(final int root, final long[] cache) {
        if (cache[root] > 0) {
            return cache[root];
        }
        final int low = low(root);
        final int high = high(root);
        final int nodeLevel;
        final int lowLevel;
        final int highLevel;

        nodeLevel = level(root);
        lowLevel = level(low);
        highLevel = level(high);

        if (root == 0 || root == 1) {
            cache[root] = 1;
            return 1;
        }
        if (nodeLevel > lowLevel && nodeLevel > highLevel) {
            System.out.println(root + " inconsistent!");
            return -1;
        }
        final long lowRec = verifyTreeRec(low, cache);
        final long highRec = verifyTreeRec(high, cache);
        final long result = lowRec < 0 || highRec < 0 ? -1 : lowRec + highRec;
        if (result >= 0) {
            cache[root] = result;
        }
        return result;
    }

    protected boolean verifyLevelData() {
        for (int level = 0; level < this.levels.length; level++) {
            final LevelData data = this.levels[level];
            for (int i = data.start; i < data.start + data.size; i++) {
                int r = hash(i);
                while (r != 0) {
                    if (level(r) != level) {
                        System.out.println("Wrong level!");
                        return false;
                    }
                    r = next(r);
                }
            }
        }
        return true;
    }

    protected void nodesOutput() {
        System.out.println("------------------------------------------");
        for (int i = 0; i < this.nodes.length; i++) {
            //            System.out.println(this.nodes[i].info(i));
        }
        System.out.println("------------------------------------------");
    }

    protected void hashOutput() {
        System.out.println("------------------------------------------");
        for (int i = 0; i < this.nodes.length; i++) {
            System.out.println(String.format("%2d: Hash = %2d, Next = %2d", i, hash(i), next(i)));
        }
        System.out.println("------------------------------------------");
    }

    /**
     * Replaces the calls in Buddy for setjmp and longjmp.
     */
    protected static class BddReorderRequest extends RuntimeException {
    }

    @FunctionalInterface
    protected interface BddOperation {
        int perform() throws BddReorderRequest;
    }
}
