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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Unit tests for the class {@link Graph} and the class {@link Node}.
 * @version 1.3
 * @since 1.2
 */
public class GraphTest {

  public static Graph<Long> getLongGraph(String id) throws IOException {
    Graph<Long> g = new Graph<>(id + "-Long");

    final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/graphs/graph" + id + ".txt"));

    while (reader.ready()) {
      final String line = reader.readLine().trim();
      String[] nodePair = line.split(":");
      g.connect(g.node(Long.valueOf(nodePair[0])), g.node(Long.valueOf(nodePair[1])));
    }

    return g;
  }

  @Test
  public void testLongGraph() {
    Graph<Long> g = new Graph<>("Graph with Long nodes.");

    Node<Long> a = g.node(1L);
    Node<Long> b = g.node(2L);
    Node<Long> c = g.node(3L);
    Node<Long> d = g.node(4L);
    Node<Long> d2 = g.node(4L);

    g.connect(d, d2);
    Assert.assertFalse(d.neighbours().contains(d2));

    g.connect(a, b);
    g.connect(a, c);
    g.connect(a, d);
    g.connect(b, c);

    d.connectTo(d2);
    Assert.assertFalse(d2.neighbours().contains(d));

    Assert.assertEquals(3, a.neighbours().size());
    Assert.assertTrue(c.neighbours().contains(a));
    Assert.assertTrue(c.neighbours().contains(b));

    Assert.assertEquals("Node{content=1, neighbours:2,3,4}", a.toString());
  }

  @Test
  public void testFormulaGraph() {
    FormulaFactory f = new FormulaFactory();
    Graph<Formula> g = new Graph<>("Graph with Formula nodes.");

    Variable a = f.variable("A");
    Node<Formula> an = g.node(a);
    Variable b = f.variable("B");
    Node<Formula> bn = g.node(b);

    Formula aNb = f.and(a, b);
    Node<Formula> aNbn = g.node(aNb);
    g.connect(aNbn, an);
    g.connect(aNbn, bn);

    Assert.assertEquals(2, aNbn.neighbours().size());
    Assert.assertTrue(an.neighbours().contains(aNbn));
    Assert.assertTrue(bn.neighbours().contains(aNbn));

    Assert.assertEquals(g.name(), an.graph().name());

    g.disconnect(aNbn, an);

    Assert.assertTrue(an.neighbours().isEmpty());

    Assert.assertEquals(3, g.nodes().size());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testTwoGraphs() {
    Graph<String> g1 = new Graph<>("G1");
    Graph<String> g2 = new Graph<>("G2");

    Node<String> a = g1.node("A");
    Node<String> b = g2.node("B");

    g1.disconnect(a, b);
    Assert.assertTrue(a.neighbours().isEmpty());

    g1.connect(a, b);
  }

}
