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

package org.logicng.graphs.algorithms;

import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * This class implements an algorithm to compute the connected components of a graph.
 * @version 1.2
 * @since 1.2
 */
public class ConnectedComponentsComputation {

  /**
   * Computes the set of connected components of a graph, where each component is represented by a set of nodes.
   * @param graph the graph
   * @param <T>   the type of the graph content
   * @return the set of sets of nodes representing the connected components
   */
  public static <T> Set<Set<Node<T>>> compute(final Graph<T> graph) {
    final Set<Set<Node<T>>> connectedComponents = new LinkedHashSet<>();
    final Set<Node<T>> unmarkedNodes = new LinkedHashSet<>(graph.nodes());

    while (!unmarkedNodes.isEmpty()) {
      Set<Node<T>> connectedComp = new LinkedHashSet<>();
      deepFirstSearch(unmarkedNodes.iterator().next(), connectedComp, unmarkedNodes);
      connectedComponents.add(connectedComp);
    }

    return connectedComponents;
  }

  private static <T> void deepFirstSearch(Node<T> v, Set<Node<T>> component, final Set<Node<T>> unmarkedNodes) {
    component.add(v);
    unmarkedNodes.remove(v);
    for (Node<T> neigh : v.neighbours())
      if (unmarkedNodes.contains(neigh))
        deepFirstSearch(neigh, component, unmarkedNodes);
  }
}
