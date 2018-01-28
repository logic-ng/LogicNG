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
import java.util.HashSet;
import java.util.Set;

/**
 * Unit tests for the class {@link ConnectedComponentsComputation}.
 * @version 1.2
 * @since 1.2
 */
public class ConnectedComponentsComputerTest {

  @Test
  public void graph30Test() throws IOException {
    Graph<Long> g = GraphTest.getLongGraph("30");
    for (long i = 0; i < 30; i++) {
      g.node(i);
    }

    Assert.assertEquals(30, g.nodes().size());

    Set<Set<Node<Long>>> ccs = ConnectedComponentsComputation.compute(g);
    Assert.assertEquals(7, ccs.size());
    int bigOnes = 0;
    for (Set<Node<Long>> cc : ccs) {
      if (cc.size() > 1) {
        Assert.assertTrue(bigOnes < 4);
        bigOnes++;
        Assert.assertTrue(cc.size() > 4);
      } else {
        Assert.assertEquals(1, cc.size());
      }
      int equals = 0;
      for (Set<Node<Long>> cc2 : ccs) {
        Set<Node<Long>> cut = new HashSet<>(cc2);
        cut.retainAll(cc);
        if (cut.size() == cc.size()) {
          equals++;
        } else {
          Assert.assertTrue(cut.isEmpty());
        }
      }
      Assert.assertEquals(1, equals);
    }
  }

  @Test
  public void graph60Test() throws IOException {
    Graph<Long> g = GraphTest.getLongGraph("50");
    for (long i = 0; i < 60; i++) {
      g.node(i);
    }

    Assert.assertEquals(60, g.nodes().size());

    Set<Set<Node<Long>>> ccs = ConnectedComponentsComputation.compute(g);
    Assert.assertEquals(11, ccs.size());
    boolean bigOneFound = false;
    for (Set<Node<Long>> cc : ccs) {
      if (cc.size() > 1) {
        Assert.assertFalse(bigOneFound);
        bigOneFound = true;
        Assert.assertEquals(50, cc.size());
      } else {
        Assert.assertEquals(1, cc.size());
      }
      int equals = 0;
      for (Set<Node<Long>> cc2 : ccs) {
        Set<Node<Long>> cut = new HashSet<>(cc2);
        cut.retainAll(cc);
        if (cut.size() == cc.size()) {
          equals++;
        } else {
          Assert.assertTrue(cut.isEmpty());
        }
      }
      Assert.assertEquals(1, equals);
    }
  }
}
