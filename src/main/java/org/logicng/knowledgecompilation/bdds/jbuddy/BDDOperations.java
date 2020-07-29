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

import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.CACHEID_PATHCOU_ONE;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.CACHEID_PATHCOU_ZERO;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.CACHEID_SATCOU;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.MARKOFF;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.MARKON;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A collection of operations on a BDD kernel.
 * @version 2.0.0
 * @since 2.0.0
 */
public class BDDOperations {
    protected final BDDKernel k;

    protected byte[] allunsatProfile;
    protected int supportID; // Current ID (true value) for support
    protected int supportMax; // Max. used level in support calc.
    protected int[] supportSet; // The found support set

    /**
     * Constructs a new object which will perform operations on the given kernel.
     * @param k the kernel
     */
    public BDDOperations(final BDDKernel k) {
        this.k = k;
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
        this.k.reordering.disableReorder();
        this.k.initRef();
        final int res = satOneRec(r);
        this.k.reordering.enableReorder();
        return res;
    }

    protected int satOneRec(final int r) throws BDDKernel.BddReorderRequest {
        if (this.k.isConst(r)) {
            return r;
        }
        if (this.k.isZero(this.k.low(r))) {
            final int res = satOneRec(this.k.high(r));
            return this.k.pushRef(this.k.makeNode(this.k.level(r), BDDKernel.BDD_FALSE, res));
        } else {
            final int res = satOneRec(this.k.low(r));
            return this.k.pushRef(this.k.makeNode(this.k.level(r), res, BDDKernel.BDD_FALSE));
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
        if (this.k.isZero(r)) {
            return r;
        }
        if (!this.k.isConst(pol)) {
            throw new IllegalArgumentException("polarity for satOneSet must be a constant");
        }
        this.k.reordering.disableReorder();
        this.k.initRef();
        final int res = satOneSetRec(r, var, pol);
        this.k.reordering.enableReorder();
        return res;
    }

    protected int satOneSetRec(final int r, final int var, final int satPolarity) throws BDDKernel.BddReorderRequest {
        if (this.k.isConst(r) && this.k.isConst(var)) {
            return r;
        }
        if (this.k.level(r) < this.k.level(var)) {
            if (this.k.isZero(this.k.low(r))) {
                final int res = satOneSetRec(this.k.high(r), var, satPolarity);
                return this.k.pushRef(this.k.makeNode(this.k.level(r), BDDKernel.BDD_FALSE, res));
            } else {
                final int res = satOneSetRec(this.k.low(r), var, satPolarity);
                return this.k.pushRef(this.k.makeNode(this.k.level(r), res, BDDKernel.BDD_FALSE));
            }
        } else if (this.k.level(var) < this.k.level(r)) {
            final int res = satOneSetRec(r, this.k.high(var), satPolarity);
            if (satPolarity == BDDKernel.BDD_TRUE) {
                return this.k.pushRef(this.k.makeNode(this.k.level(var), BDDKernel.BDD_FALSE, res));
            } else {
                return this.k.pushRef(this.k.makeNode(this.k.level(var), res, BDDKernel.BDD_FALSE));
            }
        } else {
            if (this.k.isZero(this.k.low(r))) {
                final int res = satOneSetRec(this.k.high(r), this.k.high(var), satPolarity);
                return this.k.pushRef(this.k.makeNode(this.k.level(r), BDDKernel.BDD_FALSE, res));
            } else {
                final int res = satOneSetRec(this.k.low(r), this.k.high(var), satPolarity);
                return this.k.pushRef(this.k.makeNode(this.k.level(r), res, BDDKernel.BDD_FALSE));
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
        this.k.reordering.disableReorder();
        this.k.initRef();
        int res = fullSatOneRec(r);
        for (int v = this.k.level(r) - 1; v >= 0; v--) {
            res = this.k.pushRef(this.k.makeNode(v, res, 0));
        }
        this.k.reordering.enableReorder();
        return res;
    }

    protected int fullSatOneRec(final int r) throws BDDKernel.BddReorderRequest {
        if (r < 2) {
            return r;
        }
        if (this.k.low(r) != 0) {
            int res = fullSatOneRec(this.k.low(r));
            for (int v = this.k.level(this.k.low(r)) - 1; v > this.k.level(r); v--) {
                res = this.k.pushRef(this.k.makeNode(v, res, 0));
            }
            return this.k.pushRef(this.k.makeNode(this.k.level(r), res, 0));
        } else {
            int res = fullSatOneRec(this.k.high(r));
            for (int v = this.k.level(this.k.high(r)) - 1; v > this.k.level(r); v--) {
                res = this.k.pushRef(this.k.makeNode(v, res, 0));
            }
            return this.k.pushRef(this.k.makeNode(this.k.level(r), 0, res));
        }
    }

    /**
     * Returns all models for a given BDD.
     * @param r the BDD root node
     * @return all models for the BDD
     */
    public List<byte[]> allSat(final int r) {
        final byte[] allsatProfile = new byte[this.k.varnum];
        for (int v = this.k.level(r) - 1; v >= 0; --v) {
            allsatProfile[this.k.level2var[v]] = -1;
        }
        this.k.initRef();
        final List<byte[]> allSat = new ArrayList<>();
        allSatRec(r, allSat, allsatProfile);
        return allSat;
    }

    protected void allSatRec(final int r, final List<byte[]> models, final byte[] allsatProfile) {
        if (this.k.isOne(r)) {
            models.add(Arrays.copyOf(allsatProfile, allsatProfile.length));
            return;
        }
        if (this.k.isZero(r)) {
            return;
        }
        if (!this.k.isZero(this.k.low(r))) {
            allsatProfile[this.k.level2var[this.k.level(r)]] = 0;
            for (int v = this.k.level(this.k.low(r)) - 1; v > this.k.level(r); --v) {
                allsatProfile[this.k.level2var[v]] = -1;
            }
            allSatRec(this.k.low(r), models, allsatProfile);
        }
        if (!this.k.isZero(this.k.high(r))) {
            allsatProfile[this.k.level2var[this.k.level(r)]] = 1;
            for (int v = this.k.level(this.k.high(r)) - 1; v > this.k.level(r); --v) {
                allsatProfile[this.k.level2var[v]] = -1;
            }
            allSatRec(this.k.high(r), models, allsatProfile);
        }
    }

    /**
     * Returns the model count for the given BDD.
     * @param r the BDD root node
     * @return the model count for the BDD
     */
    public BigInteger satCount(final int r) {
        final BigInteger size = BigInteger.valueOf(2).pow(this.k.level(r));
        return satCountRec(r, CACHEID_SATCOU).multiply(size);
    }

    protected BigInteger satCountRec(final int root, final int miscid) {
        if (root < 2) {
            return BigInteger.valueOf(root);
        }
        final BDDCacheEntry entry = this.k.misccache.lookup(root);
        if (entry.a == root && entry.c == miscid) {
            return entry.bdres;
        }
        BigInteger size = BigInteger.ZERO;
        BigInteger s = BigInteger.ONE;
        s = s.multiply(BigInteger.valueOf(2).pow(this.k.level(this.k.low(root)) - this.k.level(root) - 1));
        size = size.add(s.multiply(satCountRec(this.k.low(root), miscid)));
        s = BigInteger.ONE;
        s = s.multiply(BigInteger.valueOf(2).pow(this.k.level(this.k.high(root)) - this.k.level(root) - 1));
        size = size.add(s.multiply(satCountRec(this.k.high(root), miscid)));
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
        if (this.k.isZero(r)) {
            return BigInteger.ZERO;
        }
        if (this.k.isOne(r)) {
            return BigInteger.ONE;
        }
        final BDDCacheEntry entry = this.k.misccache.lookup(r);
        if (entry.a == r && entry.c == miscid) {
            return entry.bdres;
        }
        size = pathCountRecOne(this.k.low(r), miscid).add(pathCountRecOne(this.k.high(r), miscid));
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
        if (this.k.isZero(r)) {
            return BigInteger.ONE;
        }
        if (this.k.isOne(r)) {
            return BigInteger.ZERO;
        }
        final BDDCacheEntry entry = this.k.misccache.lookup(r);
        if (entry.a == r && entry.c == miscid) {
            return entry.bdres;
        }
        size = pathCountRecZero(this.k.low(r), miscid).add(pathCountRecZero(this.k.high(r), miscid));
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
        this.allunsatProfile = new byte[this.k.varnum];
        for (int v = this.k.level(r) - 1; v >= 0; --v) {
            this.allunsatProfile[this.k.level2var[v]] = -1;
        }
        this.k.initRef();
        final List<byte[]> allUnsat = new ArrayList<>();
        allUnsatRec(r, allUnsat);
        return allUnsat;
    }

    protected void allUnsatRec(final int r, final List<byte[]> models) {
        if (this.k.isZero(r)) {
            models.add(Arrays.copyOf(this.allunsatProfile, this.allunsatProfile.length));
            return;
        }
        if (this.k.isOne(r)) {
            return;
        }
        if (!this.k.isOne(this.k.low(r))) {
            this.allunsatProfile[this.k.level2var[this.k.level(r)]] = 0;
            for (int v = this.k.level(this.k.low(r)) - 1; v > this.k.level(r); --v) {
                this.allunsatProfile[this.k.level2var[v]] = -1;
            }
            allUnsatRec(this.k.low(r), models);
        }
        if (!this.k.isOne(this.k.high(r))) {
            this.allunsatProfile[this.k.level2var[this.k.level(r)]] = 1;
            for (int v = this.k.level(this.k.high(r)) - 1; v > this.k.level(r); --v) {
                this.allunsatProfile[this.k.level2var[v]] = -1;
            }
            allUnsatRec(this.k.high(r), models);
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
        if (supportSize < this.k.varnum) {
            this.supportSet = new int[this.k.varnum];
            this.supportID = 0;
        }
        if (this.supportID == 0x0FFFFFFF) {
            this.supportID = 0;
        }
        ++this.supportID;
        final int supportMin = this.k.level(r);
        this.supportMax = supportMin;
        supportRec(r, this.supportSet);
        this.k.unmark(r);

        this.k.reordering.disableReorder();
        for (int n = this.supportMax; n >= supportMin; --n) {
            if (this.supportSet[n] == this.supportID) {
                this.k.addRef(res, null);
                final int tmp = this.k.makeNode(n, 0, res);
                this.k.delRef(res);
                res = tmp;
            }
        }
        this.k.reordering.enableReorder();
        return res;
    }

    protected void supportRec(final int r, final int[] support) {
        if (r < 2) {
            return;
        }
        if ((this.k.level(r) & MARKON) != 0 || this.k.low(r) == -1) {
            return;
        }
        support[this.k.level(r)] = this.supportID;
        if (this.k.level(r) > this.supportMax) {
            this.supportMax = this.k.level(r);
        }
        this.k.setLevel(r, this.k.level(r) | MARKON);
        supportRec(this.k.low(r), support);
        supportRec(this.k.high(r), support);
    }

    /**
     * Returns the number of nodes for a given BDD.
     * @param r the BDD root node
     * @return the number of nodes for the BDD
     */
    public int nodeCount(final int r) {
        final int count = this.k.markCount(r);
        this.k.unmark(r);
        return count;
    }

    /**
     * Returns how often each variable occurs in the given BDD.
     * @param r the BDD root node
     * @return how often each variable occurs in the BDD
     */
    public int[] varProfile(final int r) {
        final int[] varprofile = new int[this.k.varnum];
        this.varProfileRec(r, varprofile);
        this.k.unmark(r);
        return varprofile;
    }

    protected void varProfileRec(final int r, final int[] varprofile) {
        if (r < 2) {
            return;
        }
        if ((this.k.level(r) & BDDKernel.MARKON) != 0) {
            return;
        }
        varprofile[this.k.level2var[this.k.level(r)]]++;
        this.k.setLevel(r, this.k.level(r) | BDDKernel.MARKON);
        varProfileRec(this.k.low(r), varprofile);
        varProfileRec(this.k.high(r), varprofile);
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
        this.k.mark(r);
        for (int n = 0; n < this.k.nodesize; n++) {
            if ((this.k.level(n) & MARKON) != 0) {
                this.k.setLevel(n, this.k.level(n) & MARKOFF);
                result.add(new int[]{n, this.k.level2var[this.k.level(n)], this.k.low(n), this.k.high(n)});
            }
        }
        return result;
    }
}
