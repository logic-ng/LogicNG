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

package org.logicng.graphs.datastructures;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * A generic node of a graph.
 * @param <T> the element type of the node
 * @version 1.3
 * @since 1.2
 */
public class Node<T> {

  private final Graph<T> graph;
  private final T content;
  private final Set<Node<T>> neighbours;

  /**
   * Constructor.
   * @param content the content of the node
   * @param graph   the graph the node will be a part of
   */
  Node(final T content, final Graph<T> graph) {
    this.content = content;
    this.graph = graph;
    this.neighbours = new LinkedHashSet<>();
  }

  /**
   * Adds the given node to the neighbours of this node. Both nodes must be in the same graph.
   * @param o the given node
   */
  void connectTo(final Node<T> o) {
    if (!this.graph.equals(o.graph))
      throw new IllegalArgumentException("Cannot connect to nodes of two different graphs.");
    if (this.equals(o)) {
      return;
    }
    neighbours.add(o);
  }

  /**
   * Removes the given node from the neighbours of this node.
   * @param o the given node
   */
  void disconnectFrom(final Node<T> o) {
    neighbours.remove(o);
  }

  /**
   * Returns the content of the node.
   * @return the content of the node
   */
  public T content() {
    return content;
  }

  /**
   * Returns the neighbours of the node.
   * @return the neighbours of the node
   */
  public Set<Node<T>> neighbours() {
    return new LinkedHashSet<>(neighbours);
  }

  /**
   * Returns the graph to which the node belongs.
   * @return the node's graph
   */
  public Graph<T> graph() {
    return graph;
  }

  @Override
  public int hashCode() {
    int result = graph.hashCode();
    result = 31 * result + (content != null ? content.hashCode() : 0);
    return result;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null || getClass() != o.getClass())
      return false;
    final Node<?> node = (Node<?>) o;
    return graph.equals(node.graph) && (content != null ? content.equals(node.content) : node.content == null);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Node{content=").append(content).append(", neighbours:");
    for (Node neighbour : neighbours)
      sb.append(neighbour.content()).append(",");
    sb.deleteCharAt(sb.length() - 1);
    sb.append("}");
    return sb.toString();
  }
}
