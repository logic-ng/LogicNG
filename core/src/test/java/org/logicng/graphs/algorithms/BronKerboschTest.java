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
//  Copyright 2015-20xx Christoph Zengler                                //
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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.GraphTest;
import org.logicng.graphs.datastructures.Node;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;

/**
 * Unit tests for the class {@link BronKerbosch}.
 * @version 2.0.0
 * @since 1.2
 */
public class BronKerboschTest {

    @Test
    public void graph50Test() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("50");

        final BronKerbosch<Long> bkp = new BronKerbosch<>(g);
        final Set<SortedSet<Node<Long>>> resultBkp = bkp.compute();

        assertThat(resultBkp.size()).isEqualTo(910);

        for (final SortedSet<Node<Long>> clique1 : resultBkp) {
            for (final SortedSet<Node<Long>> clique2 : resultBkp) {
                if (clique1.size() != clique2.size()) {
                    assertThat(clique1.containsAll(clique2)).isFalse();
                }
            }
        }

        final Node<Long> eleven = g.node(11L);
        for (final Node<Long> nb : eleven.neighbours()) {
            g.disconnect(nb, eleven);
        }

        final Set<SortedSet<Node<Long>>> resultEleven = bkp.compute();
        int elevenCliques = 0;
        for (final SortedSet<Node<Long>> clique : resultEleven) {
            if (clique.contains(eleven)) {
                elevenCliques++;
                assertThat(clique.size()).isEqualTo(1);
            }
        }
        assertThat(elevenCliques).isEqualTo(1);

        g.connect(eleven, g.node(10L));

        bkp.compute();
        int tenCliques = 0;
        for (final List<Long> clique : bkp.getCliquesAsTLists()) {
            if (clique.contains(11L)) {
                tenCliques++;
                assertThat(clique.size()).isEqualTo(2);
                assertThat(clique.contains(10L)).isTrue();
            }
        }
        assertThat(tenCliques).isEqualTo(1);
    }
}
