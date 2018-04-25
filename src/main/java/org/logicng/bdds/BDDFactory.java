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

package org.logicng.bdds;

import org.logicng.bdds.datastructures.BDD;
import org.logicng.bdds.datastructures.BDDConstant;
import org.logicng.bdds.datastructures.BDDInnerNode;
import org.logicng.bdds.datastructures.BDDNode;
import org.logicng.bdds.jbuddy.BDDKernel;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.Variable;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import static org.logicng.formulas.FType.AND;

/**
 * The factory for the jBuddy implementation.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDFactory {

  private final BDDKernel kernel;

  private final FormulaFactory f;
  private final SortedMap<Variable, Integer> var2idx;
  private final SortedMap<Integer, Variable> idx2var;


  /**
   * Constructs a new jBuddy BDD factory.
   * @param numNodes  the initial number of nodes in the BDD
   * @param cacheSize the cache size
   * @param f         the formula factory
   */
  public BDDFactory(final int numNodes, final int cacheSize, final FormulaFactory f) {
    this.f = f;
    this.kernel = new BDDKernel(numNodes, cacheSize);
    this.var2idx = new TreeMap<>();
    this.idx2var = new TreeMap<>();
  }

  /**
   * Builds a BDD for a given formula.  BDDs support all Boolean formula types but not pseudo-Boolean constraints.
   * The reason is that before converting a formula to a BDD one must specify the number of variables.  In case of
   * pseudo-Boolean constraints this number depends on the translation of the constraint.  Therefore the caller first
   * has to transform any pseudo-Boolean constraints in their respective CNF representation before converting them
   * to a BDD.
   * @param formula the formula
   * @return the top node of the BDD
   */
  public BDD build(final Formula formula) {
    return new BDD(buildRec(formula), this);
  }

  /**
   * Recursive build procedure for the BDD.
   * @param formula the formula
   * @return the BDD index
   */
  private int buildRec(final Formula formula) {
    switch (formula.type()) {
      case FALSE:
        return BDDKernel.BDD_FALSE;
      case TRUE:
        return BDDKernel.BDD_TRUE;
      case LITERAL:
        final Literal lit = (Literal) formula;
        Integer idx = this.var2idx.get(lit.variable());
        if (idx == null) {
          idx = this.var2idx.size();
          this.var2idx.put(lit.variable(), idx);
          this.idx2var.put(idx, lit.variable());
        }
        return lit.phase() ? this.kernel.ithVar(idx) : this.kernel.nithVar(idx);
      case NOT:
        final Not not = (Not) formula;
        return this.kernel.addRef(this.kernel.not(buildRec(not.operand())));
      case IMPL:
        final Implication impl = (Implication) formula;
        return this.kernel.addRef(this.kernel.implication(buildRec(impl.left()), buildRec(impl.right())));
      case EQUIV:
        final Equivalence equiv = (Equivalence) formula;
        return this.kernel.addRef(this.kernel.equivalence(buildRec(equiv.left()), buildRec(equiv.right())));
      case AND:
      case OR:
        final Iterator<Formula> it = formula.iterator();
        int res = buildRec(it.next());
        while (it.hasNext())
          res = formula.type() == AND
                  ? this.kernel.addRef(this.kernel.and(res, buildRec(it.next())))
                  : this.kernel.addRef(this.kernel.or(res, buildRec(it.next())));
        return res;
      case PBC:
        return buildRec(formula.nnf());
      default:
        throw new IllegalArgumentException("Unsupported operator for BDD generation: " + formula.type());
    }
  }

  /**
   * Sets the variable order for this BDD.  This method must be called BEFORE the BDD is generated.
   * @param varOrder the variable order
   */
  public void setVariableOrder(final List<Variable> varOrder) {
    this.setVariableOrder(varOrder.toArray(new Variable[0]));
  }

  /**
   * Sets the variable order for this BDD.  This method must be called BEFORE the BDD is generated.
   * @param varOrder the variable order
   */
  public void setVariableOrder(final LNGVector<Variable> varOrder) {
    this.setVariableOrder(varOrder.toArray());
  }

  /**
   * Sets the variable order for this factory manually.  In this case the number of variables has not to be set manually.
   * @param varOrder the variable order.
   */
  public void setVariableOrder(final Variable... varOrder) {
    this.kernel.setNumberOfVars(varOrder.length);
    for (final Variable lit : varOrder) {
      final int idx = this.var2idx.size();
      this.var2idx.put(lit.variable(), idx);
      this.idx2var.put(idx, lit.variable());
    }
  }

  /**
   * Sets the number of variables for this factory.
   * @param num the number of variables
   */
  public void setNumberOfVars(final int num) {
    this.kernel.setNumberOfVars(num);
  }

  /**
   * Returns whether BDD m is a tautology.
   * @param m the BDD to test for the tautology test (index of root node)
   * @return {@code true} if m is a tautology, {@code false} else
   */
  public boolean isTautology(final BDD m) {
    return m.index() == 1;
  }

  /**
   * Returns whether BDD m is a contradiction.
   * @param m the BDD to test for the contradiction test (index of root node)
   * @return {@code true} if m is a contradiction, {@code false} else
   */
  public boolean isContradiction(final BDD m) {
    return m.index() == 0;
  }

  /**
   * Returns the model count of a given BDD.
   * @param bdd the BDD
   * @return the model count
   */
  public BigDecimal modelCount(final BDD bdd) {
    return this.kernel.satCount(bdd.index());
  }

  /**
   * Returns the model count of a given BDD with a given number of unimportant variables.
   * @param bdd             the BDD
   * @param unimportantVars the number of unimportant variables
   * @return the model count
   */
  public BigDecimal modelCount(final BDD bdd, final int unimportantVars) {
    return modelCount(bdd).divide(BigDecimal.valueOf((int) Math.pow(2, unimportantVars)));
  }

  /**
   * Enumerates all models of a given BDD.
   * @param bdd the BDD
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final BDD bdd) {
    return enumerateAllModels(bdd, null);
  }

  /**
   * Enumerates all models of a given BDD wrt. a given set of variables.
   * @param bdd       the BDD
   * @param variables the variables
   * @return the list of all models
   */
  public List<Assignment> enumerateAllModels(final BDD bdd, final Collection<Variable> variables) {
    final Set<Assignment> res = new HashSet<>();
    final List<byte[]> models = this.kernel.allSat(bdd.index());
    final SortedSet<Integer> temp;
    if (variables == null)
      temp = new TreeSet<>(this.var2idx.values());
    else {
      temp = new TreeSet<>();
      for (final Map.Entry<Variable, Integer> e : this.var2idx.entrySet())
        if (variables.contains(e.getKey()))
          temp.add(e.getValue());
    }
    final int[] relevantIndices = new int[temp.size()];
    int count = 0;
    for (final Integer i : temp)
      relevantIndices[count++] = i;
    for (final byte[] model : models) {
      final List<Assignment> allAssignments = new LinkedList<>();
      generateAllModels(allAssignments, model, relevantIndices, 0);
      res.addAll(allAssignments);
    }
    return new ArrayList<>(res);
  }

  private void generateAllModels(final List<Assignment> assignments, final byte[] model, final int[] relevantIndices, final int position) {
    if (position == relevantIndices.length) {
      final Assignment assignment = new Assignment();
      for (final int i : relevantIndices)
        if (model[i] == 0)
          assignment.addLiteral(this.idx2var.get(i).negate());
        else
          assignment.addLiteral(this.idx2var.get(i));
      assignments.add(assignment);
    } else if (model[relevantIndices[position]] != -1)
      generateAllModels(assignments, model, relevantIndices, position + 1);
    else {
      model[relevantIndices[position]] = 0;
      generateAllModels(assignments, model, relevantIndices, position + 1);
      model[relevantIndices[position]] = 1;
      generateAllModels(assignments, model, relevantIndices, position + 1);
      model[relevantIndices[position]] = -1;
    }
  }

  /**
   * Returns a CNF formula for a given BDD.
   * @param bdd the node
   * @return the CNF for the formula represented by the BDD
   */
  public Formula cnf(final BDD bdd) {
    final List<byte[]> unsatPaths = this.kernel.allUnsat(bdd.index());
    final List<Formula> clauses = new LinkedList<>();
    List<Formula> literals;
    for (final byte[] path : unsatPaths) {
      literals = new LinkedList<>();
      for (int i = 0; i < path.length; i++)
        if (path[i] == 0)
          literals.add(this.idx2var.get(i));
        else if (path[i] == 1)
          literals.add(this.idx2var.get(i).negate());
      clauses.add(this.f.or(literals));
    }
    return this.f.and(clauses);
  }

  /**
   * Returns the number of clauses for the CNF formula of the given BDD.
   * @param bdd the node
   * @return the number of clauses for the CNF formula of the given BDD
   */
  public BigDecimal numberOfClausesCNF(final BDD bdd) {
    return this.kernel.pathCountZero(bdd.index());
  }

  /**
   * Returns a DNF formula for a given BDD.
   * @param bdd the BDD
   * @return the DNF for the formula represented by the BDD
   */
  public Formula dnf(final BDD bdd) {
    final List<Formula> ops = new LinkedList<>();
    for (final Assignment ass : this.enumerateAllModels(bdd))
      ops.add(ass.formula(this.f));
    return ops.isEmpty() ? this.f.falsum() : this.f.or(ops);
  }

  /**
   * Returns an arbitrary model for a given BDD or {@code null} if there is none.
   * @param bdd the BDD
   * @return an arbitrary model of the BDD
   */
  public Assignment model(final BDD bdd) {
    final int modelBDD = this.kernel.satOne(bdd.index());
    return createAssignment(modelBDD);
  }

  /**
   * Returns an arbitrary model of a given BDD or {@code null} which contains at least the given variables.  If a variable
   * is a don't care variable, it will be assigned with the given default value.
   * @param bdd          the BDD
   * @param vars         the set of variable which has to be contained in the model
   * @param defaultValue the default value for don't care variables
   * @return an arbitrary model of this BDD
   */
  public Assignment model(final BDD bdd, final Collection<Variable> vars, final boolean defaultValue) {
    final int varBDD = build(this.f.and(vars)).index();
    final int pol = defaultValue ? BDDKernel.BDD_TRUE : BDDKernel.BDD_FALSE;
    final int modelBDD = this.kernel.satOneSet(bdd.index(), varBDD, pol);
    return createAssignment(modelBDD);
  }

  /**
   * Returns a full model in all variables for the given BDD.
   * @param bdd the BDD
   * @return a full model of this BDD
   */
  public Assignment fullModel(final BDD bdd) {
    final int modelBDD = this.kernel.fullSatOne(bdd.index());
    return createAssignment(modelBDD);
  }

  /**
   * Returns all the variables that a given BDD depends on.
   * @param bdd the BDD
   * @return all the variables that the BDD depends on
   */
  public SortedSet<Variable> support(final BDD bdd) {
    final int supportBDD = this.kernel.support(bdd.index());
    final Assignment assignment = createAssignment(supportBDD);
    assert assignment == null || assignment.negativeLiterals().isEmpty();
    return assignment == null ? new TreeSet<Variable>() : new TreeSet<>(assignment.positiveLiterals());
  }

  /**
   * Creates an assignment from a BDD.
   * @param modelBDD the BDD
   * @return the assignment
   * @throws IllegalStateException if the BDD does not represent a unique model
   */
  private Assignment createAssignment(final int modelBDD) {
    if (modelBDD == BDDKernel.BDD_FALSE)
      return null;
    if (modelBDD == BDDKernel.BDD_TRUE)
      return new Assignment();
    final List<int[]> nodes = this.kernel.allNodes(modelBDD);
    final Assignment assignment = new Assignment();
    for (final int[] node : nodes) {
      final Variable variable = this.idx2var.get(node[1]);
      if (node[2] == BDDKernel.BDD_FALSE)
        assignment.addLiteral(variable);
      else if (node[3] == BDDKernel.BDD_FALSE)
        assignment.addLiteral(variable.negate());
      else
        throw new IllegalStateException("Expected that the model BDD has one unique path through the BDD.");
    }
    return assignment;
  }

  /**
   * Returns how often each variable occurs in the given BDD.
   * @param bdd the BDD
   * @return how often each variable occurs in the BDD
   */
  public SortedMap<Variable, Integer> variableProfile(final BDD bdd) {
    final int[] varProfile = this.kernel.varProfile(bdd.index());
    final SortedMap<Variable, Integer> profile = new TreeMap<>();
    for (int i = 0; i < varProfile.length; i++) {
      profile.put(this.idx2var.get(i), varProfile[i]);
    }
    return profile;
  }

  /**
   * Returns a LogicNG internal BDD data structure of a given BDD.
   * @param bdd the BDD
   * @return the BDD as LogicNG data structure
   */
  public BDDNode toLngBdd(final int bdd) {
    final BDDConstant falseNode = BDDConstant.getFalsumNode(this.f);
    final BDDConstant trueNode = BDDConstant.getVerumNode(this.f);
    if (bdd == BDDKernel.BDD_FALSE)
      return falseNode;
    if (bdd == BDDKernel.BDD_TRUE)
      return trueNode;
    final List<int[]> nodes = this.kernel.allNodes(bdd);
    final Map<Integer, BDDInnerNode> innerNodes = new HashMap<>();
    for (final int[] node : nodes) {
      final int nodenum = node[0];
      final Variable variable = this.idx2var.get(node[1]);
      final BDDNode lowNode = getInnerNode(node[2], falseNode, trueNode, innerNodes);
      final BDDNode highNode = getInnerNode(node[3], falseNode, trueNode, innerNodes);
      if (innerNodes.get(nodenum) == null)
        innerNodes.put(nodenum, new BDDInnerNode(variable, lowNode, highNode));
    }
    return innerNodes.get(bdd);
  }

  /**
   * Returns the internal nodes of a given BDD (for e.g. writing to a DOT file)
   * @param bdd the BDD
   * @return the internal nodes of the BDD
   */
  public List<InternalBDDNode> getInternalNodes(final int bdd) {
    final List<InternalBDDNode> result = new ArrayList<>();
    for (final int[] node : this.kernel.allNodes(bdd))
      result.add(new InternalBDDNode(node[0], this.idx2var.get(node[1]).name(), node[2], node[3]));
    return result;
  }

  private BDDNode getInnerNode(final int index, final BDDConstant falseNode, final BDDConstant trueNode,
                               final Map<Integer, BDDInnerNode> innerNodes) {
    if (index == BDDKernel.BDD_FALSE)
      return falseNode;
    else if (index == BDDKernel.BDD_TRUE)
      return trueNode;
    else {
      return innerNodes.get(index);
    }
  }

  /**
   * Returns the BDD Kernel of this factory.  The Kernel should only be accessed when you know, what you are doing.
   * @return the BDD Kernel
   */
  public BDDKernel underlyingKernel() {
    return this.kernel;
  }

  /**
   * Returns the formula factory for this BDD factory.
   * @return the formula factory
   */
  public FormulaFactory getF() {
    return this.f;
  }

  /**
   * A internal representation of BDD nodes.  Only for internal usage.
   */
  public static class InternalBDDNode {
    private final int nodenum;
    private final String label;
    private final int low;
    private final int high;

    /**
     * Constructs a new internal BDD node.
     * @param nodeNum the node number
     * @param label   the label
     * @param low     the low node number
     * @param high    the high node number
     */
    private InternalBDDNode(final int nodeNum, final String label, final int low, final int high) {
      this.nodenum = nodeNum;
      this.label = label;
      this.low = low;
      this.high = high;
    }

    /**
     * Returns the node number.
     * @return the node number
     */
    public int nodenum() {
      return this.nodenum;
    }

    /**
     * Returns the label.
     * @return the label
     */
    public String label() {
      return this.label;
    }

    /**
     * Returns the low node number.
     * @return the low node number
     */
    public int low() {
      return this.low;
    }

    /**
     * Returns the high node number.
     * @return the high node number
     */
    public int high() {
      return this.high;
    }
  }
}
