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

import org.assertj.core.data.Offset;
import org.junit.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.graphs.generators.HypergraphGenerator;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link HypergraphEdge}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class HypergraphEdgeTest {

  private final Offset<Double> offset = Offset.offset(0.000001);

  @Test
  public void testSimpleMethods() {
    final Hypergraph<String> hypergraph = new Hypergraph<>();
    final HypergraphNode<String> node1 = new HypergraphNode<>(hypergraph, "A");
    final HypergraphNode<String> node2 = new HypergraphNode<>(hypergraph, "B");
    final HypergraphEdge<String> edge1 = new HypergraphEdge<>(node1, node2);
    assertThat(edge1.nodes()).containsExactlyInAnyOrder(node1, node2);
  }

  @Test
  public void testCenterOfGravity() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Hypergraph<Variable> hypergraph = HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("A | B | ~C | D")));
    final HypergraphEdge<Variable> edge = hypergraph.edges().iterator().next();
    final Map<HypergraphNode<Variable>, Integer> ordering = new HashMap<>();
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("A")), 1);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("B")), 2);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("C")), 3);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("D")), 4);
    assertThat(edge.centerOfGravity(ordering)).isCloseTo(2.5, this.offset);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("A")), 2);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("B")), 4);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("C")), 6);
    ordering.put(new HypergraphNode<>(hypergraph, f.variable("D")), 8);
    assertThat(edge.centerOfGravity(ordering)).isCloseTo(5, this.offset);
  }

  @Test(expected = IllegalStateException.class)
  public void testIllegalCenterOfGravity() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Hypergraph<Variable> hypergraph = HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("A | B | ~C | D")));
    final HypergraphEdge<Variable> edge = hypergraph.edges().iterator().next();
    edge.centerOfGravity(new HashMap<HypergraphNode<Variable>, Integer>());
  }

}
