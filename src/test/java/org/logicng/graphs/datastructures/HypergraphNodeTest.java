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

package org.logicng.graphs.datastructures;

import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link HypergraphNode}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class HypergraphNodeTest {

  @Test
  public void testSimpleMethods() {
    final Hypergraph<String> hypergraph = new Hypergraph<>();
    final HypergraphNode<String> node1 = new HypergraphNode<>(hypergraph, "A");
    final HypergraphNode<String> node2 = new HypergraphNode<>(hypergraph, "B");
    final HypergraphEdge<String> edge1 = new HypergraphEdge<>(node1, node2);
    assertThat(node1.graph()).isEqualTo(hypergraph);
    assertThat(node1.content()).isEqualTo("A");
    assertThat(node1.edges()).containsExactlyInAnyOrder(edge1);
  }


}
