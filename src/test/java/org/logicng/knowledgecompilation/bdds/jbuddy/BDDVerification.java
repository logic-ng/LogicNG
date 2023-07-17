// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.jbuddy;

public class BDDVerification {
    private final BDDKernel k;

    public BDDVerification(final BDDKernel k) {
        this.k = k;
    }

    /////////////////// Verification /////////////////////////////////

    /**
     * Debug method for verifying the consistency of the BDD at index
     * {@code root}.
     * @param root the root of the BDD
     * @return whether the BDD is valid or not
     */
    public boolean verify(final int root) {
        final int varnum = this.k.level2var.length - 1;
        for (int i = 0; i < varnum * 2 + 2; i++) {
            if (this.k.refcou(i) != BDDKernel.MAXREF) {
                System.out.println("Constant or Variable without MAXREF count: " + i);
                return false;
            }
            if (i == 0 && (this.k.low(i) != 0 || this.k.high(i) != 0 || this.k.level(i) != varnum)) {
                System.out.println("Illegal FALSE node");
                return false;
            }
            if (i == 1 && (this.k.low(i) != 1 || this.k.high(i) != 1 || this.k.level(i) != varnum)) {
                System.out.println("Illegal TRUE node");
                return false;
            }
            if (i > 1 && i % 2 == 0) {
                if (this.k.low(i) != 0) {
                    System.out.println("VAR Low wrong");
                    return false;
                } else if (this.k.high(i) != 1) {
                    System.out.println("VAR High wrong");
                    return false;
                }
            }
            if (i > 1 && i % 2 == 1) {
                if (this.k.low(i) != 1) {
                    System.out.println("VAR Low wrong");
                    return false;
                } else if (this.k.high(i) != 0) {
                    System.out.println("VAR High wrong");
                    return false;
                }
            }
            if (i > 1 && this.k.level(i) >= varnum) { // this.level2var[node.level]
                                                      // != i / 2 - 1) {
                System.out.println("VAR Level wrong");
                return false;
            }
        }
        if (root >= 0) {
            for (int i = varnum * 2 + 2; i < this.k.nodesize; i++) {
                if (this.k.refcou(i) > 1) {
                    System.out.println("Refcou > 1");
                    return false;
                } else if (this.k.refcou(i) == 1 && i != root) {
                    System.out.println("Wrong refcou");
                    return false;
                } else if (this.k.refcou(i) == 0 && i == root) {
                    System.out.println("Entry point not marked");
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Debug method for verifying the consistency of the BDD at index
     * {@code root}.
     * @param root the root of the BDD
     * @return whether the BDD is valid or not
     */
    public long verifyTree(final int root) {
        return verifyTreeRec(root, new long[this.k.nodes.length]);
    }

    protected long verifyTreeRec(final int root, final long[] cache) {
        if (cache[root] > 0) {
            return cache[root];
        }
        final int low = this.k.low(root);
        final int high = this.k.high(root);
        final int nodeLevel;
        final int lowLevel;
        final int highLevel;

        nodeLevel = this.k.level(root);
        lowLevel = this.k.level(low);
        highLevel = this.k.level(high);

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
        for (int level = 0; level < this.k.reordering.levels.length; level++) {
            final BDDReordering.LevelData data = this.k.reordering.levels[level];
            for (int i = data.start; i < data.start + data.size; i++) {
                int r = this.k.hash(i);
                while (r != 0) {
                    if (this.k.level(r) != level) {
                        System.out.println("Wrong level!");
                        return false;
                    }
                    r = this.k.next(r);
                }
            }
        }
        return true;
    }

    protected void hashOutput() {
        System.out.println("------------------------------------------");
        for (int i = 0; i < this.k.nodes.length; i++) {
            System.out.printf("%2d: Hash = %2d, Next = %2d%n", i, this.k.hash(i), this.k.next(i));
        }
        System.out.println("------------------------------------------");
    }
}
