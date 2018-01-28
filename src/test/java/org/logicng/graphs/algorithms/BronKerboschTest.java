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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.GraphTest;
import org.logicng.graphs.datastructures.Node;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;

/**
 * Unit tests for the class {@link BronKerbosch}.
 * @version 1.2
 * @since 1.2
 */
public class BronKerboschTest {

  @Test
  public void graph50Test() throws IOException {
    Graph<Long> g = GraphTest.getLongGraph("50");

    BronKerbosch<Long> bkp = new BronKerbosch<>(g);
    Set<SortedSet<Node<Long>>> resultBkp = bkp.compute();

    Assert.assertEquals(910, resultBkp.size());

    for (SortedSet<Node<Long>> clique1 : resultBkp) {
      for (SortedSet<Node<Long>> clique2 : resultBkp) {
        if (clique1.size() != clique2.size()) {
          Assert.assertFalse(clique1.containsAll(clique2));
        }
      }
    }

    Node<Long> eleven = g.node(11L);
    for (Node<Long> nb : eleven.neighbours()) {
      g.disconnect(nb, eleven);
    }

    Set<SortedSet<Node<Long>>> resultEleven = bkp.compute();
    int elevenCliques = 0;
    for (SortedSet<Node<Long>> clique : resultEleven) {
      if (clique.contains(eleven)) {
        elevenCliques++;
        Assert.assertEquals(1, clique.size());
      }
    }
    Assert.assertEquals(1, elevenCliques);

    g.connect(eleven, g.node(10L));

    bkp.compute();
    int tenCliques = 0;
    for (List<Long> clique : bkp.getCliquesAsTLists()) {
      if (clique.contains(11L)) {
        tenCliques++;
        Assert.assertEquals(2, clique.size());
        Assert.assertTrue(clique.contains(10L));
      }
    }
    Assert.assertEquals(1, tenCliques);
  }
}
