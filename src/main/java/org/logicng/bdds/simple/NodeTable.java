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

import org.logicng.bdds.BDDInnerNode;
import org.logicng.bdds.BDDNode;
import org.logicng.bdds.BDDTerminalNode;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Node table for the simple BDD implementation.
 * @version 1.2
 * @since 1.2
 */
final class NodeTable {

  /**
   * Mapping from node index -> [var, low, high]
   */
  private final SortedMap<Integer, Integer[]> nodes;

  /**
   * Constructor.
   * @param numVars the number of variables for this BDD.
   */
  NodeTable(int numVars) {
    this.nodes = new TreeMap<>();
    this.nodes.put(0, new Integer[]{numVars, 0, 0});
    this.nodes.put(1, new Integer[]{numVars, 0, 0});
  }

  /**
   * Adds a new node and returns its index.
   * @param var  the variable index of the node
   * @param low  the low index of the node
   * @param high the high index of the node
   * @return the index of the new node
   */
  public int add(final int var, int low, int high) {
    final int node = this.nodes.size();
    this.nodes.put(node, new Integer[]{var, low, high});
    return node;
  }

  /**
   * Returns the variable index of a given node index.
   * @param node the node index
   * @return the variable index
   */
  public int var(int node) {
    return this.nodes.get(node)[0];
  }

  /**
   * Returns the low index of a given node index.
   * @param node the node index
   * @return the low index
   */
  int low(int node) {
    return this.nodes.get(node)[1];
  }

  /**
   * Returns the high index of a given node index.
   * @param node the node index
   * @return the high index
   */
  int high(int node) {
    return this.nodes.get(node)[2];
  }

  /**
   * Returns the size of this node table.
   * @return the size of this node table
   */
  public int size() {
    return this.nodes.size();
  }

  /**
   * Returns a BDD node for a given node index.
   * @param node    the node index
   * @param idx2var the mapping from variable indices to variables
   * @param f       the formula factory
   * @return the BDD node
   */
  BDDNode bddNodeForTableEntry(int node, final SortedMap<Integer, Variable> idx2var,
                               final SortedMap<Integer, BDDNode> usedNodes, final FormulaFactory f) {
    if (node == 0)
      return BDDTerminalNode.getFalsumNode(f);
    if (node == 1)
      return BDDTerminalNode.getVerumNode(f);

    BDDNode low = usedNodes.get(low(node));
    if (low == null) {
      low = bddNodeForTableEntry(low(node), idx2var, usedNodes, f);
      usedNodes.put(low(node), low);
    }
    BDDNode high = usedNodes.get(high(node));
    if (high == null) {
      high = bddNodeForTableEntry(high(node), idx2var, usedNodes, f);
      usedNodes.put(high(node), high);
    }
    return new BDDInnerNode(idx2var.get(var(node)), low, high);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    for (final Map.Entry<Integer, Integer[]> e : this.nodes.entrySet())
      sb.append(e.getKey()).append(" | var=").append(e.getValue()[0]).append(" | low=")
              .append(e.getValue()[1]).append(" | high=").append(e.getValue()[2]).append("\n");
    return sb.toString();
  }
}
