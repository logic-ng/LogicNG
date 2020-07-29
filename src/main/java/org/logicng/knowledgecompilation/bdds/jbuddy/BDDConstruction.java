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

package org.logicng.knowledgecompilation.bdds.jbuddy;

import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.CACHEID_FORALL;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.CACHEID_RESTRICT;

/**
 * This class provides abstractions for the construction of BDDs.
 * @version 2.0.0
 * @since 2.0.0
 */
public class BDDConstruction {

    private final BDDKernel k;

    /**
     * Constructs a new object with the given kernel.
     * @param k the kernel
     */
    public BDDConstruction(final BDDKernel k) {
        this.k = k;
    }

    /**
     * Returns a BDD representing the i-th variable (one node with the children true and false).
     * @param i the index i
     * @return the BDD representing the i-th variable
     * @throws IllegalArgumentException if the index is not within the range of variables
     */
    public int ithVar(final int i) {
        if (i < 0 || i >= this.k.varnum) {
            throw new IllegalArgumentException("Illegal variable number: " + i);
        }
        return this.k.vars[i * 2];
    }

    /**
     * Returns a BDD representing the negation of the i-th variable (one node with the children true and false).
     * @param i the index i
     * @return the BDD representing the negated i-th variable
     * @throws IllegalArgumentException if the index is not within the range of variables
     */
    public int nithVar(final int i) {
        if (i < 0 || i >= this.k.varnum) {
            throw new IllegalArgumentException("Illegal variable number: " + i);
        }
        return this.k.vars[i * 2 + 1];
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
        return this.k.level2var[this.k.level(root)];
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
        return this.k.low(root);
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
        return this.k.high(root);
    }

    /**
     * Returns the conjunction of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the conjunction of the two BDDs
     */
    public int and(final int l, final int r) {
        return this.k.apply(l, r, BDDKernel.Operand.AND);
    }

    /**
     * Returns the disjunction of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the disjunction of the two BDDs
     */
    public int or(final int l, final int r) {
        return this.k.apply(l, r, BDDKernel.Operand.OR);
    }

    /**
     * Returns the implication of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the implication of the two BDDs
     */
    public int implication(final int l, final int r) {
        return this.k.apply(l, r, BDDKernel.Operand.IMP);
    }

    /**
     * Returns the equivalence of two BDDs.
     * @param l the first BDD
     * @param r the second BDD
     * @return the equivalence of the two BDDs
     */
    public int equivalence(final int l, final int r) {
        return this.k.apply(l, r, BDDKernel.Operand.EQUIV);
    }

    /**
     * Returns the negation of a BDD.
     * @param r the BDD
     * @return the negation of the BDD
     */
    public int not(final int r) {
        return this.k.doWithPotentialReordering(() -> notRec(r));
    }

    protected int notRec(final int r) throws BDDKernel.BddReorderRequest {
        if (this.k.isZero(r)) {
            return BDDKernel.BDD_TRUE;
        }
        if (this.k.isOne(r)) {
            return BDDKernel.BDD_FALSE;
        }
        final BDDCacheEntry entry = this.k.applycache.lookup(r);
        if (entry.a == r && entry.c == BDDKernel.Operand.NOT.v) {
            return entry.res;
        }
        this.k.pushRef(notRec(this.k.low(r)));
        this.k.pushRef(notRec(this.k.high(r)));
        final int res = this.k.makeNode(this.k.level(r), this.k.readRef(2), this.k.readRef(1));
        this.k.popref(2);
        entry.a = r;
        entry.c = BDDKernel.Operand.NOT.v;
        entry.res = res;
        return res;
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
        return this.k.doWithPotentialReordering(() -> restrictRec(r, (var << 3) | CACHEID_RESTRICT));
    }

    protected int restrictRec(final int r, final int miscid) throws BDDKernel.BddReorderRequest {
        final int res;
        if (this.k.isConst(r) || this.k.level(r) > this.k.quantlast) {
            return r;
        }
        final BDDCacheEntry entry = this.k.misccache.lookup(this.k.pair(r, miscid));
        if (entry.a == r && entry.c == miscid) {
            return entry.res;
        }
        if (insvarset(this.k.level(r))) {
            if (this.k.quantvarset[this.k.level(r)] > 0) {
                res = restrictRec(this.k.high(r), miscid);
            } else {
                res = restrictRec(this.k.low(r), miscid);
            }
        } else {
            this.k.pushRef(restrictRec(this.k.low(r), miscid));
            this.k.pushRef(restrictRec(this.k.high(r), miscid));
            res = this.k.makeNode(this.k.level(r), this.k.readRef(2), this.k.readRef(1));
            this.k.popref(2);
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
        return this.k.doWithPotentialReordering(() -> quantRec(r, BDDKernel.Operand.OR, var << 3));
    }

    /**
     * Universal quantifier elimination for the variables in {@code var}.
     * @param r   the BDD root node
     * @param var the variables to eliminate
     * @return the BDD with the eliminated variables
     */
    public int forAll(final int r, final int var) {
        if (var < 2) {
            return r;
        }
        varset2vartable(var);
        return this.k.doWithPotentialReordering(() -> quantRec(r, BDDKernel.Operand.AND, (var << 3) | CACHEID_FORALL));
    }

    protected int quantRec(final int r, final BDDKernel.Operand op, final int quantid) throws BDDKernel.BddReorderRequest {
        final int res;
        if (r < 2 || this.k.level(r) > this.k.quantlast) {
            return r;
        }
        final BDDCacheEntry entry = this.k.quantcache.lookup(r);
        if (entry.a == r && entry.c == quantid) {
            return entry.res;
        }
        this.k.pushRef(quantRec(this.k.low(r), op, quantid));
        this.k.pushRef(quantRec(this.k.high(r), op, quantid));
        if (invarset(this.k.level(r))) {
            res = this.k.applyRec(this.k.readRef(2), this.k.readRef(1), op);
        } else {
            res = this.k.makeNode(this.k.level(r), this.k.readRef(2), this.k.readRef(1));
        }
        this.k.popref(2);
        entry.a = r;
        entry.c = quantid;
        entry.res = res;
        return res;
    }

    protected void varset2svartable(final int r) {
        if (r < 2) {
            throw new IllegalArgumentException("Illegal variable: " + r);
        }
        this.k.quantvarsetID++;
        if (this.k.quantvarsetID == Integer.MAX_VALUE / 2) {
            this.k.quantvarset = new int[this.k.varnum];
            this.k.quantvarsetID = 1;
        }
        for (int n = r; !this.k.isConst(n); ) {
            if (this.k.isZero(this.k.low(n))) {
                this.k.quantvarset[this.k.level(n)] = this.k.quantvarsetID;
                n = this.k.high(n);
            } else {
                this.k.quantvarset[this.k.level(n)] = -this.k.quantvarsetID;
                n = this.k.low(n);
            }
            this.k.quantlast = this.k.level(n);
        }
    }

    protected void varset2vartable(final int r) {
        if (r < 2) {
            throw new IllegalArgumentException("Illegal variable: " + r);
        }
        this.k.quantvarsetID++;
        if (this.k.quantvarsetID == Integer.MAX_VALUE) {
            this.k.quantvarset = new int[this.k.varnum];
            this.k.quantvarsetID = 1;
        }
        for (int n = r; n > 1; n = this.k.high(n)) {
            this.k.quantvarset[this.k.level(n)] = this.k.quantvarsetID;
            this.k.quantlast = this.k.level(n);
        }
    }

    protected boolean insvarset(final int a) {
        return Math.abs(this.k.quantvarset[a]) == this.k.quantvarsetID;
    }

    protected boolean invarset(final int a) {
        return this.k.quantvarset[a] == this.k.quantvarsetID;
    }
}
