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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.bdds.simple;

import org.logicng.bdds.BDD;
import org.logicng.bdds.BDDFactory;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.Variable;
import org.logicng.util.Pair;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A BDD factory with complementary edges.
 * <p/>
 * (c.f. Harrison: Handbook of Practical Logic and Automated Reasoning)
 * @version 1.2
 * @since 1.2
 */
public class BDDFactoryComplementaryEdges extends BDDFactory {

  private List<Variable> ord;
  private SortedMap<Integer, BDDNode> unique;
  private Map<BDDNode, Integer> uback;
  private Map<Set<Integer>, Integer> computeTable;
  private int index;

  /**
   * Constructor.
   * @param f   the formula factory
   * @param ord the variable ordering.
   */
  public BDDFactoryComplementaryEdges(final FormulaFactory f, final List<Variable> ord) {
    super(f);
    this.ord = ord;
    this.unique = new TreeMap<>();
    this.uback = new HashMap<>();
    this.computeTable = new HashMap<>();
    this.index = 2;
  }

  /**
   * Constructor.
   * @param f the formula factory
   */
  public BDDFactoryComplementaryEdges(final FormulaFactory f) {
    this(f, new LinkedList<Variable>());
  }

  @Override
  public BDD build(final Formula f) {
    return new BDD(buildRec(f));
  }

  @Override
  public BigDecimal modelCount(BDD bdd) {
    //TODO implement
    return BigDecimal.ZERO;
  }

  @Override
  public BigDecimal modelCount(BDD bdd, int unimportantVars) {
    //TODO implement
    return null;
  }

  @Override
  public List<Assignment> enumerateAllModels(BDD bdd, Collection<Literal> literals) {
    //TODO implement
    return new LinkedList<>();
  }

  @Override
  public Formula cnf(BDD bdd) {
    //TODO implement
    return null;
  }

  /**
   * Recursive build procedure for a BDD.
   * @param f the formula
   * @return the BDD index
   */
  private int buildRec(final Formula f) {
    switch (f.type()) {
      case FALSE:
        return -1;
      case TRUE:
        return 1;
      case LITERAL:
        final Literal lit = (Literal) f;
        return lit.phase() ? mkNode(lit.variable(), 1, -1) : -mkNode(lit.variable(), 1, -1);
      case NOT:
        return -buildRec(((Not) f).operand());
      case IMPL:
        final Implication impl = (Implication) f;
        return bddImplication(buildRec(impl.left()), buildRec(impl.right()));
      case EQUIV:
        final Equivalence equiv = (Equivalence) f;
        return bddEquiv(buildRec(equiv.left()), buildRec(equiv.right()));
      case AND:
      case OR:
        final Iterator<Formula> it = f.iterator();
        int res = buildRec(it.next());
        while (it.hasNext())
          res = f.type() == FType.AND
                  ? this.bddAnd(res, buildRec(it.next()))
                  : this.bddOr(res, buildRec(it.next()));
        return res;
      default:
        throw new IllegalArgumentException("Unsupported operator for BDD generation: " + f.type());
    }
  }

  /**
   * Get a Formula representation of the BDD indexed by m
   * @param m the index
   * @param f the formula factory
   * @return the Formula representation of the BDD rooted at m
   */
  public Formula toFormula(int m, final FormulaFactory f) {
    if (m == 1)
      return f.verum();
    else if (m < 0)
      return f.not(toFormula(-m, f)).nnf();
    else {
      final BDDNode node = expandNode(m);
      return f.or(f.and(node.v, toFormula(node.left, f)), f.and(node.v.negate(), toFormula(node.right, f))).nnf();
    }
  }

  @Override
  public boolean isTautology(final BDD m) {
    return m.index() == 1;
  }

  @Override
  public boolean isContradiction(final BDD m) {
    return m.index() == -1;
  }

  @Override
  public void setVariableOrder(final Variable... varOrder) {
    //TODO implement
  }

  /**
   * Get the corresponding BDD node to an index
   * @param n the index
   * @return the corresponding node
   */
  private BDDNode expandNode(int n) {
    if (n >= 0) {
      final BDDNode result = unique.get(n);
      if (result != null)
        return result;
      return new BDDNode(null, 1, 1);
    } else {
      final BDDNode result = unique.get(-n);
      if (result != null)
        return result.complement();
      return new BDDNode(null, 1, 1);
    }
  }

  /**
   * Add a new node to the BDD.
   * If the node is not already present, add a new one.  In both cases return the index of the new node.
   * @param node the new node to add
   * @return the index of the new node
   */

  private int lookupUnique(final BDDNode node) {
    final Integer result = uback.get(node);
    if (result != null)
      return result;
    unique.put(index, node);
    uback.put(node, index);
    index++;
    return index - 1;
  }

  /**
   * Extract the order of a BDD.
   * @param v1 the first variable for comparison
   * @param v2 the second variable for comparison
   * @return `true` if v1 < v2 wrt. the current ordering, `false` else
   */
  private boolean order(Variable v1, Variable v2) {
    return v2 == null && v1 != null || ord.indexOf(v1) < ord.indexOf(v2);
  }

  /**
   * Make a new BDD node and return its index.
   * @param v     the variable at this node
   * @param left  the index of the left child
   * @param right the index of the right child
   * @return the index of the new node
   */
  private int mkNode(final Variable v, int left, int right) {
    if (!ord.contains(v))
      this.ord.add(v);
    if (left == right)
      return left;
    else if (left >= 0)
      return lookupUnique(new BDDNode(v, left, right));
    else
      return -lookupUnique(new BDDNode(v, -left, -right));
  }

  /**
   * Compute the conjunction of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 & m2 (index of root node)
   */
  private int bddAnd(int m1, int m2) {
    if (m1 == -1 || m2 == -1)
      return -1;
    else if (m1 == 1)
      return m2;
    else if (m2 == 1)
      return m1;
    final HashSet<Integer> key = new HashSet<>(Arrays.asList(m1, m2));
    Integer result = computeTable.get(key);
    if (result != null)
      return result;
    final BDDNode node1 = expandNode(m1);
    final BDDNode node2 = expandNode(m2);
    Variable p;
    int lnew;
    int rnew;
    if (node1.v == node2.v) {
      p = node1.v;
      lnew = bddAnd(node1.left, node2.left);
      rnew = bddAnd(node1.right, node2.right);
    } else if (order(node1.v, node2.v)) {
      p = node1.v;
      lnew = bddAnd(node1.left, m2);
      rnew = bddAnd(node1.right, m2);
    } else {
      p = node2.v;
      lnew = bddAnd(m1, node2.left);
      rnew = bddAnd(m1, node2.right);
    }
    final int newNode = mkNode(p, lnew, rnew);
    computeTable.put(key, newNode);
    return newNode;
  }

  /**
   * Compute the disjunction of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 | m2 (index of root node)
   */
  private int bddOr(int m1, int m2) {
    return -bddAnd(-m1, -m2);
  }

  /**
   * Compute the implication of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 => m2 (index of root node)
   */
  private int bddImplication(int m1, int m2) {
    return bddOr(-m1, m2);
  }

  /**
   * Compute the equivalence of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 <=> m2 (index of root node)
   */
  private int bddEquiv(int m1, int m2) {
    return bddOr(bddAnd(m1, m2), bddAnd(-m1, -m2));
  }

  /*
   * Existential quantifier elimination.
   * @param m1 the index
   * @param vs the set of variables to eliminate (existentially quantified)
   */
  public int bddExists(int m1, final Set<Literal> vs) {
    if (m1 == 1 || m1 == -1)
      return m1;
    final BDDNode node = expandNode(m1);
    if (vs.contains(node.v))
      return bddOr(bddExists(node.left, vs), bddExists(node.right, vs));
    else
      return mkNode(node.v, bddExists(node.left, vs), bddExists(node.right, vs));
  }

  /*
   * Universal quantifier elimination.
   * @param m1 the index
   * @param vs the set of variables to eliminate (universally quantified)
   */
  public int bddForAll(int m1, final Set<Literal> vs) {
    if (m1 == 1 || m1 == -1)
      return m1;
    final BDDNode node = expandNode(m1);
    if (vs.contains(node.v))
      return bddAnd(bddForAll(node.left, vs), bddForAll(node.right, vs));
    else
      return mkNode(node.v, bddForAll(node.left, vs), bddForAll(node.right, vs));
  }

  public void printDebugInfo() {
    System.out.println("Unique Table\n------------------\n");
    for (final Map.Entry<Integer, BDDNode> e : this.unique.entrySet())
      System.out.println(String.format("Node %d: %s", e.getKey(), e.getValue()));
  }

  /**
   * A printer for the Graphviz dot graph drawing utility
   * @param bdd root of the BDD to output
   */
  public String printDot(final BDD bdd) {
    final StringBuilder sb = new StringBuilder();
    Map<Pair<Integer, Integer>, Boolean> edgeSeen = new HashMap<>();
    sb.append("digraph {\n");
    sb.append("0[style = invisible];\n");
    sb.append("1[shape = box];\n");
    final int m = bdd.index();
    printEdge(0, Math.abs(m), m < 0, true, edgeSeen, sb);
    visitnode(Math.abs(m), edgeSeen, sb);
    sb.append("}\n");
    return sb.toString();
  }

  private void printEdge(int a, int b, boolean comp, boolean left,
                         final Map<Pair<Integer, Integer>, Boolean> edgeSeen, final StringBuilder sb) {
    final Pair<Integer, Integer> key = new Pair<>(a, b);
    if (!edgeSeen.containsKey(key)) {
      edgeSeen.put(key, true);
      final String es = left ? "solid" : "dotted";
      if (comp)
        sb.append(String.format("  %d -> %d [arrowtail=dot,arrowhead=normal,style=%s];%n", a, b, es));
      else
        sb.append(String.format("  %d -> %d [style=%s];%n", a, b, es));
    }
  }

  private void visitnode(int a, final Map<Pair<Integer, Integer>, Boolean> edgeSeen, final StringBuilder sb) {
    if (a > 1) {
      final BDDNode node = expandNode(a);
      sb.append(String.format("%n  %d [label=%s];%n", a, node.v.name()));
      printEdge(a, Math.abs(node.left), node.left < 0, true, edgeSeen, sb);
      printEdge(a, Math.abs(node.right), node.right < 0, false, edgeSeen, sb);
      visitnode(Math.abs(node.left), edgeSeen, sb);
      visitnode(Math.abs(node.right), edgeSeen, sb);
    }
  }

  @Override
  public String toString() {
    return String.format("<BDD with %d nodes>", index);
  }

  /**
   * Inner class representing a single BDD node
   * <p/>
   * Corresponds to Harrison's BDD((unique,uback,index),ord)
   */
  private static class BDDNode {
    private Variable v;
    private int left;
    private int right;

    /**
     * Constructor
     * @param v     the variable at this node
     * @param left  the index of the left child
     * @param right the index of the right child
     */
    private BDDNode(final Variable v, int left, int right) {
      this.v = v;
      this.left = left;
      this.right = right;
    }

    /**
     * Get the complement of this node (i.e. negated left and right children)
     * @return the complemented node
     */
    private BDDNode complement() {
      return new BDDNode(v, -left, -right);
    }

    @Override
    public int hashCode() {
      return Objects.hash(this.v.name(), this.left, this.right);
    }

    @Override
    public boolean equals(final Object other) {
      if (this == other)
        return true;
      if (other instanceof BDDNode) {
        final BDDNode o = (BDDNode) other;
        return Objects.equals(this.v.name(), o.v.name())
                && this.left == o.left
                && this.right == o.right;
      }
      return false;
    }

    @Override
    public String toString() {
      return String.format("[%s | left: %d , right: %d]", v, left, right);
    }
  }
}
