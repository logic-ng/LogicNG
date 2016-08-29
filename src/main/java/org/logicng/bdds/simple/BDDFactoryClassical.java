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
import org.logicng.bdds.BDDNode;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A simple BBD factory.
 * @version 1.2
 * @since 1.2
 */
public final class BDDFactoryClassical extends BDDFactory {

  private final NodeTable nodes;
  private final NodeCache cache;
  private final ApplyCache applyCacheAnd;
  private final ApplyCache applyCacheOr;
  private final ApplyCache applyCacheImplication;
  private final ApplyCache applyCacheEquivalence;
  private final SortedMap<Integer, Variable> idx2var;
  private final int numVars;
  private ApplyCache currentApplyCache;

  /**
   * Constructor.
   * @param f the formula factory
   */
  public BDDFactoryClassical(final FormulaFactory f, final Collection<Variable> variables) {
    super(f);
    this.numVars = variables.size();
    this.nodes = new NodeTable(numVars);
    this.applyCacheAnd = new ApplyCache();
    this.applyCacheOr = new ApplyCache();
    this.applyCacheImplication = new ApplyCache();
    this.applyCacheEquivalence = new ApplyCache();
    this.cache = new NodeCache();
    this.idx2var = new TreeMap<>();
    for (final Variable var : variables) {
      final int idx = idx2var.size();
      idx2var.put(idx, var);
    }
  }

  /**
   * Returns the BDD for a given formula.
   * @param formula the formula
   * @return the root BDD node
   */
  public BDDNode bddForFormula(final Formula formula) {
    final int root = this.build(formula, 0);
    final SortedMap<Integer, BDDNode> usedNodes = new TreeMap<>();
    return (nodes.bddNodeForTableEntry(root, this.idx2var, usedNodes, formula.factory()));
  }

  @Override
  public BDD build(final Formula formula) {
    return new BDD(this.build(formula, 0));
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
   * Builds a BDD for a given formula (with the apply method).
   * @param formula the formula
   * @return the top node of the BDD
   */
  public int buildWithApply(final Formula formula) {
    final Formula nnf = formula.nnf();
    switch (nnf.type()) {
      case FALSE:
        return 0;
      case TRUE:
        return 1;
      case LITERAL:
        return build(formula, 0);
      case AND:
      case OR:
        final Iterator<Formula> it = nnf.iterator();
        int res = buildWithApply(it.next());
        while (it.hasNext())
          res = this.apply(nnf.type(), res, buildWithApply(it.next()));
        return res;
      default:
        throw new UnsupportedOperationException("Invlalid operator in NNF: " + nnf.type());
    }
  }

  @Override
  public boolean isTautology(final BDD m) {
    return m.index() == 1;
  }

  @Override
  public boolean isContradiction(final BDD m) {
    return m.index() == 0;
  }

  @Override
  public void setVariableOrder(final Variable... varOrder) {
    //TODO implement
  }

  /**
   * Applies a given operator to two BDD root nodes.
   * @param op    the operator
   * @param root1 the first root node index
   * @param root2 the second root node index
   * @return the resulting root node
   */
  private int apply(final FType op, int root1, int root2) {
    switch (op) {
      case AND:
        currentApplyCache = this.applyCacheAnd;
        break;
      case OR:
        currentApplyCache = this.applyCacheOr;
        break;
      case IMPL:
        currentApplyCache = this.applyCacheImplication;
        break;
      case EQUIV:
        currentApplyCache = this.applyCacheEquivalence;
        break;
      default:
        throw new UnsupportedOperationException("Operation " + op + " not supported for BDD apply.");
    }
    return this.applyRec(op, root1, root2);
  }

  /**
   * Recursive build method for a BDD.
   * @param formula the formula
   * @param i       the current variable index
   * @return the top node of the BDD
   */
  private int build(final Formula formula, int i) {
    if (i >= this.numVars)
      return (formula.type() == FType.FALSE ? 0 : 1);
    final int low = build(formula.restrict(new Assignment(idx2var.get(i).negate())), i + 1);
    final int high = build(formula.restrict(new Assignment(idx2var.get(i))), i + 1);
    return makeNode(i, low, high);
  }

  /**
   * Creates a new node and returns the resulting node index.
   * @param var  the variable index
   * @param low  the low index
   * @param high the high index
   */
  private int makeNode(int var, int low, int high) {
    if (low == high)
      return low;
    final int lookup = this.cache.lookup(var, low, high);
    if (lookup > -1)
      return lookup;
    final int nodeIndex = nodes.add(var, low, high);
    this.cache.insert(var, low, high, nodeIndex);
    return nodeIndex;
  }

  /**
   * Applies a given operator to two BDD root nodes recursively.
   * @param op    the operator
   * @param root1 the first root node index
   * @param root2 the second root node index
   * @return the resulting root node
   */
  private int applyRec(final FType op, int root1, int root2) {
    final int lookup = currentApplyCache.lookup(root1, root2);
    if (lookup > -1)
      return lookup;
    int result;
    if (root1 < 2 && root2 < 2)
      result = computeOperator(op, root1, root2);
    else if (nodes.var(root1) == nodes.var(root2))
      result = makeNode(nodes.var(root1), applyRec(op, nodes.low(root1), nodes.low(root2)),
              applyRec(op, nodes.high(root1), nodes.high(root2)));
    else if (nodes.var(root1) < nodes.var(root2))
      result = makeNode(nodes.var(root1), applyRec(op, nodes.low(root1), root2), applyRec(op, nodes.high(root1), root2));
    else
      result = makeNode(nodes.var(root2), applyRec(op, root1, nodes.low(root2)), applyRec(op, root1, nodes.high(root2)));
    currentApplyCache.insert(root1, root2, result);
    return result;
  }

  /**
   * Computes a given operator for two terminal nodes.
   * @param op    the operator
   * @param root1 the first terminal node index
   * @param root2 the second terminal node index
   * @return the resulting terminal node
   */
  private int computeOperator(final FType op, int root1, int root2) {
    switch (op) {
      case AND:
        return root1 == 1 && root2 == 1 ? 1 : 0;
      case OR:
        return root1 == 1 || root2 == 1 ? 1 : 0;
      case IMPL:
        return root1 == 0 || root2 == 1 ? 1 : 0;
      case EQUIV:
        return root1 == root2 ? 1 : 0;
      default:
        throw new UnsupportedOperationException("Operation " + op + " not supported for BDD apply.");
    }
  }

  public void printStatistics() {
    System.out.println("#vars:  " + this.numVars);
    System.out.println("#nodes: " + this.nodes.size());
    System.out.println();
    System.out.println(this.nodes);
  }

}
