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
//  Copyright 2015-2018 Christoph Zengler                                //
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

package org.logicng.bdds.jbuddy;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * The jBuddy kernel.
 * @version 1.4
 * @since 1.4
 */
public final class BDDKernel {

  public static final int BDD_TRUE = 1;
  public static final int BDD_FALSE = 0;

  private static final int MAXVAR = 0x1FFFFF;
  private static final int MAXREF = 0x3FF;
  private static final int MARKON = 0x200000;
  private static final int MARKOFF = 0x1FFFFF;

  private byte[] allunsatProfile;

  private enum Operand {
    AND(0, new int[]{0, 0, 0, 1}),
    OR(2, new int[]{0, 1, 1, 1}),
    IMP(5, new int[]{1, 1, 0, 1}),
    EQUIV(6, new int[]{1, 0, 0, 1}),
    NOT(10, new int[]{1, 1, 0, 0});

    private final int v;
    private final int[] tt;

    Operand(final int value, final int[] truthTable) {
      this.v = value;
      this.tt = truthTable;
    }
  }

  private static final int CACHEID_RESTRICT = 0x1;
  private static final int CACHEID_SATCOU = 0x2;
  private static final int CACHEID_PATHCOU = 0x4;
  private static final int CACHEID_FORALL = 0x1;

  private BddNode[] nodes; // All of the bdd nodes
  private int[] vars; // Set of defined BDD variables
  private final int minfreenodes; // Mininimal % of nodes that has to be left after a garbage collection
  private int gbcollectnum; // Number of garbage collections
  private final int cachesize; // Size of the operator caches
  private int nodesize; // Number of allocated nodes
  private final int maxnodeincrease; // Max. # of nodes used to inc. table
  private int freepos; // First free node
  private int freenum; // Number of free nodes
  private long produced; // Number of new nodes ever produced
  private int varnum; // Number of defined BDD variables
  private int[] refstack; // Internal node reference stack
  private int refstacktop; // Internal node reference stack top
  private int[] var2level; // Variable -> level table
  private int[] level2var; // Level -> variable table
  private boolean resized; // Flag indicating a resize of the nodetable

  private int[] quantvarset; // Current variable set for quant.
  private int quantvarsetID; // Current id used in quantvarset
  private int quantlast; // Current last variable to be quant.
  private int supportID; // Current ID (true value) for support
  private int supportMax; // Max. used level in support calc.
  private int[] supportSet; // The found support set

  private BDDCache applycache; // Cache for apply results
  private BDDCache itecache; // Cache for ITE results
  private BDDCache quantcache; // Cache for exist/forall results
  private BDDCache appexcache; // Cache for appex/appall results
  private BDDCache replacecache; // Cache for replace results
  private BDDCache misccache; // Cache for other results
  private int cacheratio;
  //  private byte[] allsatProfile; // Variable profile for allSat()
  //  private byte[] allunsatProfile; // Variable profile for allSat()

  /**
   * Constructor for the BDD kernel.
   * @param initialSize the initial number of nodes in the nodetable
   * @param cs          the fixed size of the internal caches
   */
  public BDDKernel(final int initialSize, final int cs) {
    this.nodesize = BDDPrime.primeGTE(initialSize);
    this.nodes = new BddNode[this.nodesize];
    this.resized = false;
    this.minfreenodes = 20;
    for (int n = 0; n < this.nodesize; n++) {
      this.nodes[n] = new BddNode();
      this.nodes[n].refcou = 0;
      this.nodes[n].low = -1;
      this.nodes[n].hash = 0;
      this.nodes[n].level = 0;
      this.nodes[n].next = n + 1;
    }
    this.nodes[this.nodesize - 1].next = 0;
    this.nodes[0].refcou = MAXREF;
    this.nodes[1].refcou = MAXREF;
    this.nodes[0].low = 0;
    this.nodes[0].high = 0;
    this.nodes[1].low = 1;
    this.nodes[1].high = 1;
    initOperators(cs);
    this.freepos = 2;
    this.freenum = this.nodesize - 2;
    this.varnum = 0;
    this.gbcollectnum = 0;
    this.cachesize = cs;
    this.maxnodeincrease = 50000;
    //    bdd_fdd_init();
  }

  /**
   * Sets the number of variables to use. It may be called more than one time, but only
   * to increase the number of variables.
   * @param num the number of variables to use
   */
  public void setNumberOfVars(final int num) {
    if (num < 1 || num > MAXVAR)
      throw new IllegalArgumentException("Invalid variable number: " + num);
    if (num < this.varnum)
      throw new IllegalArgumentException("Cannot decrease variable number");
    if (num == this.varnum)
      return;
    if (this.vars == null) {
      this.vars = new int[num * 2];
      this.level2var = new int[num + 1];
      this.var2level = new int[num + 1];
    } else {
      int[] temp = new int[num * 2];
      System.arraycopy(this.vars, 0, temp, 0, this.vars.length);
      this.vars = temp;
      temp = new int[num + 1];
      System.arraycopy(this.level2var, 0, temp, 0, this.level2var.length);
      this.level2var = temp;
      temp = new int[num + 1];
      System.arraycopy(this.var2level, 0, temp, 0, this.var2level.length);
      this.var2level = temp;
    }
    this.refstack = new int[num * 2 + 4];
    this.refstacktop = 0;
    while (this.varnum < num) {
      this.vars[this.varnum * 2] = pushRef(makeNode(this.varnum, 0, 1));
      this.vars[this.varnum * 2 + 1] = makeNode(this.varnum, 1, 0);
      popref(1);
      this.nodes[this.vars[this.varnum * 2]].refcou = MAXREF;
      this.nodes[this.vars[this.varnum * 2 + 1]].refcou = MAXREF;
      this.level2var[this.varnum] = this.varnum;
      this.var2level[this.varnum] = this.varnum;
      this.varnum++;
    }
    this.nodes[0].level = num;
    this.nodes[1].level = num;
    this.var2level[num] = num;
    this.level2var[num] = num;
    varResize();
  }

  /**
   * Increases the number of variables to use by the given number.
   * @param num the number of new variables
   * @throws IllegalArgumentException if the number of new variables is not legel (negative or too big)
   */
  public void extendVarNum(final int num) {
    if (num < 0 || num > 0x3FFFFFFF)
      throw new IllegalArgumentException("illegal number of variables to extend");
    setNumberOfVars(this.varnum + num);
  }

  /**
   * Returns the number of variables.
   * @return the number of variables
   */
  public int numberOfVars() {
    return this.varnum;
  }

  /**
   * Returns the number of nodes in the nodetable that are currently in use. Note that dead nodes that have not been
   * reclaimed yet by a garbage collection are counted as active.
   * @return the number of active nodes
   */
  public int numberOfNodes() {
    return this.nodesize - this.freenum;
  }

  /**
   * Returns the number of nodes currently allocated. This includes both dead and active nodes.
   * @return the number of nodes currently allocated
   */
  public int numberOfAllocs() {
    return this.nodesize;
  }

  /**
   * Returns a BDD representing the i-th variable (one node with the childs true and false).
   * @param i the index i
   * @return the BDD representing the i-th variable
   * @throws IllegalArgumentException if the index is not within the range of variables
   */
  public int ithVar(final int i) {
    if (i < 0 || i >= this.varnum)
      throw new IllegalArgumentException("Illegal variable number: " + i);
    return this.vars[i * 2];
  }

  /**
   * Returns a BDD representing the negation of the i-th variable (one node with the childs true and false).
   * @param i the index i
   * @return the BDD representing the negated i-th variable
   * @throws IllegalArgumentException if the index is not within the range of variables
   */
  public int nithVar(final int i) {
    if (i < 0 || i >= this.varnum)
      throw new IllegalArgumentException("Illegal variable number: " + i);
    return this.vars[i * 2 + 1];
  }

  /**
   * Returns the variable index labeling the given root node.
   * @param root the root node of the BDD
   * @return the variable index
   */
  public int bddVar(final int root) {
    if (root < 2)
      throw new IllegalArgumentException("Illegal node number: " + root);
    return (this.level2var[level(root)]);
  }

  /**
   * Returns the false branch of the given root node.
   * @param root the root node of the BDD
   * @return the false branch
   */
  public int bddLow(final int root) {
    if (root < 2)
      throw new IllegalArgumentException("Illegal node number: " + root);
    return (low(root));
  }

  /**
   * Returns the true branch of the given root node.
   * @param root the root node of the BDD
   * @return the true branch
   */
  public int bddHigh(final int root) {
    if (root < 2)
      throw new IllegalArgumentException("Illegal node number: " + root);
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

  private int apply(final int l, final int r, final Operand op) {
    final int res;
    initRef();
    res = applyRec(l, r, op);
    checkResize();
    return res;
  }

  private int applyRec(final int l, final int r, final Operand op) {
    final int res;
    switch (op) {
      case AND:
        if (l == r)
          return l;
        if (isZero(l) || isZero(r))
          return 0;
        if (isOne(l))
          return r;
        if (isOne(r))
          return l;
        break;
      case OR:
        if (l == r)
          return l;
        if (isOne(l) || isOne(r))
          return 1;
        if (isZero(l))
          return r;
        if (isZero(r))
          return l;
        break;
      case IMP:
        if (isZero(l))
          return 1;
        if (isOne(l))
          return r;
        if (isOne(r))
          return 1;
        break;
    }
    if (isConst(l) && isConst(r))
      res = op.tt[l << 1 | r];
    else {
      final BDDCacheEntry entry = this.applycache.lookup(triple(l, r, op.v));
      if (entry.a == l && entry.b == r && entry.c == op.v)
        return entry.res;
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
    final int res;
    initRef();
    res = notRec(r);
    checkResize();
    return res;
  }

  private int notRec(final int r) {
    if (isZero(r))
      return BDDKernel.BDD_TRUE;
    if (isOne(r))
      return BDDKernel.BDD_FALSE;
    final BDDCacheEntry entry = this.applycache.lookup(r);
    if (entry.a == r && entry.c == Operand.NOT.v)
      return entry.res;
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
   * garbage collection.
   * @param root the node
   * @return return the node
   * @throws IllegalArgumentException if the root node was invalid
   */
  public int addRef(final int root) {
    if (root < 2)
      return root;
    if (root >= this.nodesize)
      throw new IllegalArgumentException("Not a valid BDD root node: " + root);
    if (low(root) == -1)
      throw new IllegalArgumentException("Not a valid BDD root node: " + root);
    incRef(root);
    return root;
  }

  /**
   * Deletes a reference for a given node.
   * @param root the node
   * @throws IllegalArgumentException if the root node was invalid
   */
  private void delRef(final int root) {
    if (root < 2)
      return;
    if (root >= this.nodesize)
      throw new IllegalStateException("Cannot dereference a variable > varnum");
    if (this.low(root) == -1)
      throw new IllegalStateException("Cannot dereference variable -1");
    if (!this.hasref(root))
      throw new IllegalStateException("Cannot dereference a variable which has no reference");
    this.decRef(root);
  }

  private void decRef(final int n) {
    if (this.nodes[n].refcou != MAXREF && this.nodes[n].refcou > 0)
      this.nodes[n].refcou--;
  }

  private void incRef(final int n) {
    if (this.nodes[n].refcou < MAXREF)
      this.nodes[n].refcou++;
  }

  /**
   * Returns a BDD representing a variable set.  The BDD variable set is represented as the conjunction of all the
   * variables in their positive form and may just as well be made that way by the user. The user should keep a
   * reference to the returned BDD instead of building it every time the set is needed.
   * @param varset the set of variable indices
   * @return the BDD representing the variable set
   */
  public int makeSet(final int[] varset) {
    int res = 1;
    for (int v = varset.length - 1; v >= 0; v--) {
      addRef(res);
      final int tmp = and(res, ithVar(varset[v]));
      delRef(res);
      res = tmp;
    }
    return res;
  }


  /**
   * Prints the node table for a given BDD root node.
   * @param r the BDD root node
   */
  public void printTable(final int r) {
    System.out.println(String.format("ROOT: %d", r));
    if (r < 2)
      return;
    mark(r);
    for (int n = 0; n < this.nodesize; n++) {
      if ((level(n) & MARKON) != 0) {
        final BddNode node = this.nodes[n];
        node.level &= MARKOFF;
        System.out.print(String.format("[%5d] ", n));
        System.out.print(String.format("%3d", this.level2var[node.level]));
        System.out.print(String.format(": %3d", node.low));
        System.out.println(String.format(" %3d", node.high));
      }
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
    if (r < 2)
      return result;
    mark(r);
    for (int n = 0; n < this.nodesize; n++) {
      if ((level(n) & MARKON) != 0) {
        final BddNode node = this.nodes[n];
        node.level &= MARKOFF;
        result.add(new int[]{n, this.level2var[node.level], node.low, node.high});
      }
    }
    return result;
  }

  // TODO comment

  private int makeNode(final int level, final int low, final int high) {
    final BddNode node;
    if (low == high)
      return low;
    int hash = nodehash(level, low, high);
    int res = this.nodes[hash].hash;
    while (res != 0) {
      if (level(res) == level && low(res) == low && high(res) == high)
        return res;
      res = this.nodes[res].next;
    }
    if (this.freepos == 0) {
      gbc();
      if ((this.freenum * 100) / this.nodesize <= this.minfreenodes) {
        nodeResize();
        hash = nodehash(level, low, high);
      }
      if (this.freepos == 0)
        throw new IllegalStateException("Cannot allocate more space for more nodes.");
    }
    res = this.freepos;
    this.freepos = this.nodes[this.freepos].next;
    this.freenum--;
    this.produced++;
    node = this.nodes[res];
    node.level = level;
    node.low = low;
    node.high = high;
    node.next = this.nodes[hash].hash;
    this.nodes[hash].hash = res;
    return res;
  }

  private void unmark(final int i) {
    if (i < 2)
      return;
    final BddNode node = this.nodes[i];
    if (!markedNode(node) || node.low == -1)
      return;
    unmarkNode(node);
    unmark(node.low);
    unmark(node.high);
  }

  private int markCount(final int i) {
    if (i < 2)
      return 0;
    final BddNode node = this.nodes[i];
    if (markedNode(node) || node.low == -1)
      return 0;
    setMarkNode(node);
    int count = 1;
    count += markCount(node.low);
    count += markCount(node.high);
    return count;
  }

  private void gbc() {
    int r;
    BDDGBCStat s = new BDDGBCStat();
    s.nodes = this.nodesize;
    s.freenodes = this.freenum;
    s.num = this.gbcollectnum;
    for (r = 0; r < this.refstacktop; r++)
      mark(this.refstack[r]);
    for (int n = 0; n < this.nodesize; n++) {
      if (this.nodes[n].refcou > 0)
        mark(n);
      this.nodes[n].hash = 0;
    }
    this.freepos = 0;
    this.freenum = 0;
    for (int n = this.nodesize - 1; n >= 2; n--) {
      final BddNode node = this.nodes[n];
      if ((node.level & MARKON) != 0 && node.low != -1) {
        node.level &= MARKOFF;
        final int hash = nodehash(node.level, node.low, node.high);
        node.next = this.nodes[hash].hash;
        this.nodes[hash].hash = n;
      } else {
        node.low = -1;
        node.next = this.freepos;
        this.freepos = n;
        this.freenum++;
      }
    }
    resetCaches();
    this.gbcollectnum++;
    s = new BDDGBCStat();
    s.nodes = this.nodesize;
    s.freenodes = this.freenum;
    s.num = this.gbcollectnum;
  }

  private void gbcRehash() {
    this.freepos = 0;
    this.freenum = 0;
    for (int n = this.nodesize - 1; n >= 2; n--) {
      final BddNode node = this.nodes[n];
      if (node.low != -1) {
        final int hash = nodehash(node.level, node.low, node.high);
        node.next = this.nodes[hash].hash;
        this.nodes[hash].hash = n;
      } else {
        node.next = this.freepos;
        this.freepos = n;
        this.freenum++;
      }
    }
  }

  private void mark(final int i) {
    if (i < 2)
      return;
    if ((level(i) & MARKON) != 0 || low(i) == -1)
      return;
    this.nodes[i].level |= MARKON;
    mark(low(i));
    mark(high(i));
  }

  private void nodeResize() {
    final int oldsize = this.nodesize;
    int n;
    this.nodesize = this.nodesize << 1;
    if (this.nodesize > oldsize + this.maxnodeincrease)
      this.nodesize = oldsize + this.maxnodeincrease;
    this.nodesize = BDDPrime.primeLTE(this.nodesize);
    final BddNode[] newnodes = new BddNode[this.nodesize];
    System.arraycopy(this.nodes, 0, newnodes, 0, this.nodes.length);
    for (int i = oldsize; i < newnodes.length; i++)
      newnodes[i] = new BddNode();
    this.nodes = newnodes;
    for (n = 0; n < oldsize; n++)
      this.nodes[n].hash = 0;
    for (n = oldsize; n < this.nodesize; n++) {
      final BddNode nn = this.nodes[n];
      nn.refcou = 0;
      nn.hash = 0;
      nn.level = 0;
      nn.low = -1;
      nn.next = n + 1;
    }
    this.nodes[this.nodesize - 1].next = this.freepos;
    this.freepos = oldsize;
    this.freenum += this.nodesize - oldsize;
    gbcRehash();
    this.resized = true;
  }

  private void initRef() {
    this.refstacktop = 0;
  }

  private int pushRef(final int a) {
    this.refstack[this.refstacktop++] = a;
    return a;
  }

  private int readRef(final int a) {
    return this.refstack[this.refstacktop - a];
  }

  private void popref(final int a) {
    this.refstacktop -= a;
  }

  private boolean hasref(final int n) {
    return this.nodes[n].refcou > 0;
  }

  private boolean isConst(final int a) {
    return a < 2;
  }

  private boolean isOne(final int a) {
    return a == 1;
  }

  private boolean isZero(final int a) {
    return a == 0;
  }

  private int level(final int a) {
    return this.nodes[a].level;
  }

  private int low(final int a) {
    return this.nodes[a].low;
  }

  private int high(final int a) {
    return this.nodes[a].high;
  }

  private void setMarkNode(final BddNode p) {
    p.level |= MARKON;
  }

  private void unmarkNode(final BddNode p) {
    p.level &= MARKOFF;
  }

  private boolean markedNode(final BddNode p) {
    return (p.level & MARKON) != 0;
  }

  private int nodehash(final int lvl, final int l, final int h) {
    return Math.abs(triple(lvl, l, h) % this.nodesize);
  }

  private int pair(final int a, final int b) {
    return (a + b) * (a + b + 1) / 2 + a;
  }

  private int triple(final int a, final int b, final int c) {
    return pair(c, pair(a, b));
  }

  private void initOperators(final int cachesize) {
    this.applycache = new BDDCache(cachesize);
    this.itecache = new BDDCache(cachesize);
    this.quantcache = new BDDCache(cachesize);
    this.appexcache = new BDDCache(cachesize);
    this.replacecache = new BDDCache(cachesize);
    this.misccache = new BDDCache(cachesize);
    this.quantvarsetID = 0;
    this.quantvarset = null;
    this.cacheratio = 0;
    this.supportSet = null;
  }

  private void resetCaches() {
    this.applycache.reset();
    this.itecache.reset();
    this.quantcache.reset();
    this.appexcache.reset();
    this.replacecache.reset();
    this.misccache.reset();
  }

  private void varResize() {
    this.quantvarset = new int[this.varnum];
    this.quantvarsetID = 0;
  }

  int setCacheRatio(final int r) {
    final int old = this.cacheratio;
    if (r <= 0)
      throw new IllegalArgumentException("Invalid cache ratio: " + r);
    if (this.nodesize == 0)
      return old;
    this.cacheratio = r;
    operatorsNodeResize();
    return old;
  }

  /**
   * Restricts the variables in the BDD {@code r} to constants true or false.  The restriction is submitted in the BDD
   * {@code var}.
   * @param r   the BDD to be restricted
   * @param var the variable mapping as a BDD
   * @return the restricted BDD
   */
  public int restrict(final int r, final int var) {
    final int res;
    if (var < 2)
      return r;
    varset2svartable(var);
    initRef();
    res = restrictRec(r, (var << 3) | CACHEID_RESTRICT);
    checkResize();
    return res;
  }

  private int restrictRec(final int r, final int miscid) {
    final int res;
    if (isConst(r) || level(r) > this.quantlast)
      return r;
    final BDDCacheEntry entry = this.misccache.lookup(pair(r, miscid));
    if (entry.a == r && entry.c == miscid)
      return entry.res;
    if (insvarset(level(r))) {
      if (this.quantvarset[level(r)] > 0)
        res = restrictRec(high(r), miscid);
      else
        res = restrictRec(low(r), miscid);
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
    final int res;
    if (var < 2)
      return r;
    varset2vartable(var);
    initRef();
    res = quantRec(r, Operand.OR, var << 3);
    checkResize();
    return res;
  }

  /**
   * Universal quantifier elimination for the variables in {@code var}.
   * @param r   the BDD root node
   * @param var the variables to eliminate
   * @return the BDD with the eliminated variables
   */
  public int forAll(final int r, final int var) {
    final int res;
    if (var < 2)
      return r;
    varset2vartable(var);
    initRef();
    res = quantRec(r, Operand.AND, (var << 3) | CACHEID_FORALL);
    checkResize();
    return res;
  }

  private int quantRec(final int r, final Operand op, final int quantid) {
    final int res;
    if (r < 2 || level(r) > this.quantlast)
      return r;
    final BDDCacheEntry entry = this.quantcache.lookup(r);
    if (entry.a == r && entry.c == quantid)
      return entry.res;
    pushRef(quantRec(low(r), op, quantid));
    pushRef(quantRec(high(r), op, quantid));
    if (invarset(level(r)))
      res = applyRec(readRef(2), readRef(1), op);
    else
      res = makeNode(level(r), readRef(2), readRef(1));
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
    if (r < 2)
      return r;
    initRef();
    final int res = satOneRec(r);
    checkResize();
    return res;
  }

  private int satOneRec(final int r) {
    if (isConst(r))
      return r;
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
    if (isZero(r))
      return r;
    if (!isConst(pol))
      throw new IllegalArgumentException("polarity for satOneSet must be a constant");
    initRef();
    final int res = satOneSetRec(r, var, pol);
    checkResize();
    return res;
  }

  private int satOneSetRec(final int r, final int var, final int satPolarity) {
    if (isConst(r) && isConst(var))
      return r;
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
      if (satPolarity == BDDKernel.BDD_TRUE)
        return pushRef(makeNode(level(var), BDDKernel.BDD_FALSE, res));
      else
        return pushRef(makeNode(level(var), res, BDDKernel.BDD_FALSE));
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
    if (r == 0)
      return 0;
    initRef();
    int res = fullSatOneRec(r);
    for (int v = level(r) - 1; v >= 0; v--)
      res = pushRef(makeNode(v, res, 0));
    checkResize();
    return res;
  }

  private int fullSatOneRec(final int r) {
    if (r < 2)
      return r;
    if (low(r) != 0) {
      int res = fullSatOneRec(low(r));
      for (int v = level(low(r)) - 1; v > level(r); v--)
        res = pushRef(makeNode(v, res, 0));
      return pushRef(makeNode(level(r), res, 0));
    } else {
      int res = fullSatOneRec(high(r));
      for (int v = level(high(r)) - 1; v > level(r); v--)
        res = pushRef(makeNode(v, res, 0));
      return pushRef(makeNode(level(r), 0, res));
    }
  }

  public List<byte[]> allSat(final int r) {
    final byte[] allsatProfile = new byte[this.varnum];
    for (int v = level(r) - 1; v >= 0; --v)
      allsatProfile[this.level2var[v]] = -1;
    initRef();
    final List<byte[]> allSat = new LinkedList<>();
    allSatRec(r, allSat, allsatProfile);
    return allSat;
  }

  private void allSatRec(final int r, final List<byte[]> models, final byte[] allsatProfile) {
    if (isOne(r)) {
      models.add(Arrays.copyOf(allsatProfile, allsatProfile.length));
      return;
    }
    if (isZero(r))
      return;
    if (!isZero(low(r))) {
      allsatProfile[this.level2var[level(r)]] = 0;
      for (int v = level(low(r)) - 1; v > level(r); --v)
        allsatProfile[this.level2var[v]] = -1;
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

  public BigDecimal satCount(final int r) {
    final BigDecimal size = new BigDecimal(2).pow(level(r));
    return satCountRec(r, CACHEID_SATCOU).multiply(size);
  }


  BigDecimal satCountSet(final int r, final int varset) {
    int unused = this.varnum;
    if (isConst(varset) || isZero(r))
      return BigDecimal.ZERO;
    for (int n = varset; !isConst(n); n = high(n))
      unused--;
    return satCount(r).divide(new BigDecimal(2).pow(unused));
  }

  private BigDecimal satCountRec(final int root, final int miscid) {
    if (root < 2)
      return new BigDecimal(root);
    final BDDCacheEntry entry = this.misccache.lookup(root);
    if (entry.a == root && entry.c == miscid)
      return entry.bdres;
    final BDDKernel.BddNode node = this.nodes[root];
    BigDecimal size = BigDecimal.ZERO;
    BigDecimal s = BigDecimal.ONE;
    s = s.multiply(new BigDecimal(2).pow(level(node.low) - node.level - 1));
    size = size.add(s.multiply(satCountRec(node.low, miscid)));
    s = BigDecimal.ONE;
    s = s.multiply(new BigDecimal(2).pow(level(node.high) - node.level - 1));
    size = size.add(s.multiply(satCountRec(node.high, miscid)));
    entry.a = root;
    entry.c = miscid;
    entry.bdres = size;
    return size;
  }

  public BigDecimal pathCountOne(final int r) {
    return pathCountRecOne(r, CACHEID_PATHCOU);
  }

  private BigDecimal pathCountRecOne(final int r, final int miscid) {
    final BigDecimal size;
    if (isZero(r))
      return BigDecimal.ZERO;
    if (isOne(r))
      return BigDecimal.ONE;
    final BDDCacheEntry entry = this.misccache.lookup(r);
    if (entry.a == r && entry.c == miscid)
      return entry.bdres;
    size = pathCountRecOne(low(r), miscid).add(pathCountRecOne(high(r), miscid));
    entry.a = r;
    entry.c = miscid;
    entry.bdres = size;
    return size;
  }

  public BigDecimal pathCountZero(final int r) {
    return pathCountRecZero(r, CACHEID_PATHCOU);
  }

  private BigDecimal pathCountRecZero(final int r, final int miscid) {
    final BigDecimal size;
    if (isZero(r))
      return BigDecimal.ONE;
    if (isOne(r))
      return BigDecimal.ZERO;
    final BDDCacheEntry entry = this.misccache.lookup(r);
    if (entry.a == r && entry.c == miscid)
      return entry.bdres;
    size = pathCountRecZero(low(r), miscid).add(pathCountRecZero(high(r), miscid));
    entry.a = r;
    entry.c = miscid;
    entry.bdres = size;
    return size;
  }


  public List<byte[]> allUnsat(final int r) {
    this.allunsatProfile = new byte[this.varnum];
    for (int v = level(r) - 1; v >= 0; --v)
      this.allunsatProfile[this.level2var[v]] = -1;
    initRef();
    final List<byte[]> allUnsat = new LinkedList<>();
    allUnsatRec(r, allUnsat);
    return allUnsat;
  }

  private void allUnsatRec(final int r, final List<byte[]> models) {
    if (isZero(r)) {
      models.add(Arrays.copyOf(this.allunsatProfile, this.allunsatProfile.length));
      return;
    }
    if (isOne(r))
      return;
    if (!isOne(low(r))) {
      this.allunsatProfile[this.level2var[level(r)]] = 0;
      for (int v = level(low(r)) - 1; v > level(r); --v)
        this.allunsatProfile[this.level2var[v]] = -1;
      allUnsatRec(low(r), models);
    }
    if (!isOne(high(r))) {
      this.allunsatProfile[this.level2var[level(r)]] = 1;
      for (int v = level(high(r)) - 1; v > level(r); --v)
        this.allunsatProfile[this.level2var[v]] = -1;
      allUnsatRec(high(r), models);
    }
  }

  int support(final int r) {
    final int supportSize = 0;
    int res = 1;
    if (r < 2)
      return BDDKernel.BDD_FALSE;
    if (supportSize < this.varnum) {
      this.supportSet = new int[this.varnum];
      this.supportID = 0;
    }
    if (this.supportID == 0x0FFFFFFF)
      this.supportID = 0;
    ++this.supportID;
    final int supportMin = level(r);
    this.supportMax = supportMin;
    supportRec(r, this.supportSet);
    unmark(r);

    for (int n = this.supportMax; n >= supportMin; --n)
      if (this.supportSet[n] == this.supportID) {
        addRef(res);
        final int tmp = makeNode(n, 0, res);
        delRef(res);
        res = tmp;
      }
    return res;
  }

  private void supportRec(final int r, final int[] support) {
    if (r < 2)
      return;
    final BDDKernel.BddNode node = this.nodes[r];
    if ((node.level & BDDKernel.MARKON) != 0 || node.low == -1)
      return;
    support[node.level] = this.supportID;
    if (node.level > this.supportMax)
      this.supportMax = node.level;
    node.level |= BDDKernel.MARKON;
    supportRec(node.low, support);
    supportRec(node.high, support);
  }

  private void checkResize() {
    if (this.resized)
      operatorsNodeResize();
    this.resized = false;
  }

  private void operatorsNodeResize() {
    if (this.cacheratio > 0) {
      final int newcachesize = this.nodesize / this.cacheratio;
      this.applycache.resize(newcachesize);
      this.itecache.resize(newcachesize);
      this.quantcache.resize(newcachesize);
      this.appexcache.resize(newcachesize);
      this.replacecache.resize(newcachesize);
      this.misccache.resize(newcachesize);
    }
  }

  int nodeCount(final int r) {
    final int count = markCount(r);
    unmark(r);
    return count;
  }

  int bdd_anodecount(final int[] r, final int num) {
    int count = 0;
    for (int n = 0; n < num; n++)
      count += markCount(r[n]);
    for (int n = 0; n < num; n++)
      unmark(r[n]);
    return count;
  }

  int[] varProfile(final int r) {
    final int[] varprofile = new int[this.varnum];
    varProfileRec(r, varprofile);
    unmark(r);
    return varprofile;
  }

  private void varProfileRec(final int r, final int[] varprofile) {
    if (r < 2)
      return;
    final BDDKernel.BddNode node = this.nodes[r];
    if ((node.level & BDDKernel.MARKON) != 0)
      return;
    varprofile[this.level2var[node.level]]++;
    node.level |= BDDKernel.MARKON;
    varProfileRec(node.low, varprofile);
    varProfileRec(node.high, varprofile);
  }

  private void varset2svartable(final int r) {
    if (r < 2)
      throw new IllegalArgumentException("Illegal variable: " + r);
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

  private void varset2vartable(final int r) {
    if (r < 2)
      throw new IllegalArgumentException("Illegal variable: " + r);
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

  private boolean insvarset(final int a) {
    return Math.abs(this.quantvarset[a]) == this.quantvarsetID;
  }

  private boolean invarset(final int a) {
    return this.quantvarset[a] == this.quantvarsetID;
  }

  /**
   * Prints the state of the kernel
   */
  public void printStats() {
    System.out.println("Internel state");
    System.out.println("==============");
    System.out.println(String.format("Produced nodes:     %d", this.produced));
    System.out.println(String.format("Allocated nodes:    %d", this.nodesize));
    System.out.println(String.format("Free nodes          %d", this.freenum));
    System.out.println(String.format("Variables           %d", this.varnum));
    System.out.println(String.format("Cache size          %d", this.cachesize));
    System.out.println(String.format("Garbage collections %d", this.gbcollectnum));
  }

  private static final class BddNode {
    int refcou = 10;
    int level = 22;
    int low;
    int high;
    int hash;
    int next;
  }

  private static final class BDDGBCStat {
    int nodes;
    int freenodes;
    int num;
  }
}
