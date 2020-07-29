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

/**
 * A BDD tree used to represent nested variables blocks or variable reorderings.
 * @version 2.0.0
 * @since 2.0.0
 */
public class BDDTree {
    protected int first;
    protected int last;  /* First and last variable in this block */
    protected int pos;          /* Sifting position */
    protected int[] seq;        /* Sequence of first...last in the current order */
    protected boolean fixed;       /* Are the sub-blocks fixed or may they be reordered */
    protected final int id;           /* A sequential id number given by addblock */
    protected BDDTree next;
    protected BDDTree prev;
    protected BDDTree nextlevel;

    /**
     * Constructs a new BDD tree with the given id.
     * @param id the id
     */
    public BDDTree(final int id) {
        this.id = id;
        this.first = -1;
        this.last = -1;
        this.fixed = true;
        this.next = null;
        this.prev = null;
        this.nextlevel = null;
        this.seq = null;
    }

    /**
     * Adds a new range in the tree.
     * @param tree      the tree in which the range should be added
     * @param first     the start of the range
     * @param last      the end of the range
     * @param fixed     whether the range should be fixed or not
     * @param id        the id of the tree
     * @param level2var the level to variable mapping
     * @return the (possibly changed) BDD tree
     */
    public static BDDTree addRange(final BDDTree tree, final int first, final int last, final boolean fixed, final int id, final int[] level2var) {
        return addRangeRec(tree, null, first, last, fixed, id, level2var);
    }

    /**
     * Returns the first variable of this block.
     * @return the first variable of this block
     */
    public int getFirst() {
        return this.first;
    }

    /**
     * Sets the first variable of this block.
     * @param first the first variable of this block
     */
    public void setFirst(final int first) {
        this.first = first;
    }

    /**
     * Returns the last variable of this block.
     * @return the last variable of this block
     */
    public int getLast() {
        return this.last;
    }

    /**
     * Sets the last variable of this block.
     * @param last the last variable of this block
     */
    public void setLast(final int last) {
        this.last = last;
    }

    /**
     * Returns the sifting position.
     * @return the sifting position
     */
    public int getPos() {
        return this.pos;
    }

    /**
     * Sets the sifting position.
     * @param pos the sifting position
     */
    public void setPos(final int pos) {
        this.pos = pos;
    }

    /**
     * Returns the sequence of variables between {@code first} and {@code last} in the current order.
     * @return the sequence of variables
     */
    public int[] getSeq() {
        return this.seq;
    }

    /**
     * Sets the sequence of variables between {@code first} and {@code last} in the current order.
     * @param seq the sequence of variables
     */
    public void setSeq(final int[] seq) {
        this.seq = seq;
    }

    /**
     * Returns whether this block is fixed or not.
     * @return whether this block is fixed or not
     */
    public boolean isFixed() {
        return this.fixed;
    }

    /**
     * Sets whether this block is fixed or not.
     * @param fixed whether this block is fixed or not
     */
    public void setFixed(final boolean fixed) {
        this.fixed = fixed;
    }

    /**
     * Returns the id of this block.
     * @return the id of this block
     */
    public int getId() {
        return this.id;
    }

    /**
     * Returns the next tree or {@code null} if no such tree exists.
     * @return the next tree
     */
    public BDDTree getNext() {
        return this.next;
    }

    /**
     * Sets the next tree.
     * @param next the next tree
     */
    public void setNext(final BDDTree next) {
        this.next = next;
    }

    /**
     * Returns the previous tree or {@code null} if no such tree exists.
     * @return the previous tree
     */
    public BDDTree getPrev() {
        return this.prev;
    }

    /**
     * Sets the previous tree.
     * @param prev the previous tree
     */
    public void setPrev(final BDDTree prev) {
        this.prev = prev;
    }

    /**
     * Returns the tree of the next level or {@code null} if no such tree exists.
     * @return the tree of the next level
     */
    public BDDTree getNextlevel() {
        return this.nextlevel;
    }

    /**
     * Sets the tree of the next level.
     * @param nextlevel the tree of the next level
     */
    public void setNextlevel(final BDDTree nextlevel) {
        this.nextlevel = nextlevel;
    }

    /**
     * Adds a new range in the tree.
     * @param t         the tree in which the range should be added
     * @param prev      the predecessor if t is {@code null}
     * @param first     the start of the range
     * @param last      the end of the range
     * @param fixed     whether the range should be fixed or not
     * @param id        the id of the tree
     * @param level2var the level to variable mapping
     * @return the (possibly changed) BDD tree
     */
    public static BDDTree addRangeRec(BDDTree t, final BDDTree prev, final int first, final int last, final boolean fixed, final int id, final int[] level2var) {
        if (first < 0 || last < 0 || last < first) {
            return null;
        }

        /* Empty tree -> build one */
        if (t == null) {
            t = new BDDTree(id);
            t.first = first;
            t.fixed = fixed;
            t.seq = new int[last - first + 1];
            t.last = last;
            t.updateSeq(level2var);
            t.prev = prev;
            return t;
        }

        /* Check for identity */
        if (first == t.first && last == t.last) {
            return t;
        }

        /* Before this section -> insert */
        if (last < t.first) {
            final BDDTree tnew = new BDDTree(id);
            tnew.first = first;
            tnew.last = last;
            tnew.fixed = fixed;
            tnew.seq = new int[last - first + 1];
            tnew.updateSeq(level2var);
            tnew.next = t;
            tnew.prev = t.prev;
            t.prev = tnew;
            return tnew;
        }

        /* After this this section -> go to next */
        if (first > t.last) {
            final BDDTree next = addRangeRec(t.next, t, first, last, fixed, id, level2var);
            if (next != null) {
                t.next = next;
            }
            return t;
        }

        /* Inside this section -> insert in next level */
        if (first >= t.first && last <= t.last) {
            final BDDTree nextlevel = addRangeRec(t.nextlevel, null, first, last, fixed, id, level2var);
            if (nextlevel != null) {
                t.nextlevel = nextlevel;
            }
            return t;
        }

        /* Covering this section -> insert above this level */
        if (first <= t.first) {
            final BDDTree tnew;
            BDDTree thisTree = t;

            while (true) {
                /* Partial cover ->error */
                if (last >= thisTree.first && last < thisTree.last) {
                    return null;
                }
                if (thisTree.next == null || last < thisTree.next.first) {
                    tnew = new BDDTree(id);
                    tnew.first = first;
                    tnew.last = last;
                    tnew.fixed = fixed;
                    tnew.seq = new int[last - first + 1];
                    tnew.updateSeq(level2var);
                    tnew.nextlevel = t;
                    tnew.next = thisTree.next;
                    tnew.prev = t.prev;
                    if (thisTree.next != null) {
                        thisTree.next.prev = tnew;
                    }
                    thisTree.next = null;
                    t.prev = null;
                    return tnew;
                }
                thisTree = thisTree.next;
            }
        }
        // partial cover
        return null;
    }

    protected void updateSeq(final int[] bddvar2level) {
        int n;
        int low = this.first;

        for (n = this.first; n <= this.last; n++) {
            if (bddvar2level[n] < bddvar2level[low]) {
                low = n;
            }
        }

        for (n = this.first; n <= this.last; n++) {
            this.seq[bddvar2level[n] - bddvar2level[low]] = n;
        }
    }
}
