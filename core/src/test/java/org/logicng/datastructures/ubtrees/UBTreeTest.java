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

package org.logicng.datastructures.ubtrees;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link UBTree}.
 * @version 2.5.0
 * @since 1.5.0
 */
public class UBTreeTest {

    @Test
    public void testGenerateSubsumedUBTree() {
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        final SortedSet<String> empty = set();
        assertThat(UBTree.generateSubsumedUBTree(emptyList()).allSets()).isEmpty();
        assertThat(UBTree.generateSubsumedUBTree(singletonList(e0123)).allSets()).containsExactlyInAnyOrder(e0123);
        assertThat(UBTree.generateSubsumedUBTree(asList(e0123, e013, e012, e23)).allSets()).containsExactlyInAnyOrder(e013, e012, e23);
        assertThat(UBTree.generateSubsumedUBTree(asList(e0123, e013, e012, e23, empty)).allSets()).containsExactlyInAnyOrder(empty);
    }

    @Test
    public void testEmtpyUBTree() {
        final UBTree<String> tree = new UBTree<>();
        assertThat(tree.rootNodes()).isEmpty();
        assertThat(tree.rootSet()).isNull();
    }

    @Test
    public void testEmptySet() {
        final UBTree<String> tree = new UBTree<>();
        tree.addSet(new TreeSet<>());
        assertThat(tree.rootNodes()).isEmpty();
        assertThat(tree.rootSet()).isEmpty();
    }

    @Test
    public void testSingleSet() {
        final UBTree<String> tree = new UBTree<>();
        tree.addSet(set("A", "B", "C"));
        assertThat(tree.rootNodes()).hasSize(1);
        assertThat(tree.rootNodes().keySet()).containsExactly("A");
        assertThat(tree.rootNodes().get("A").children()).hasSize(1);
        assertThat(tree.rootNodes().get("A").isEndOfPath()).isFalse();
        assertThat(tree.rootNodes().get("A").children().keySet()).containsExactly("B");
        assertThat(tree.rootNodes().get("A").children().get("B").children()).hasSize(1);
        assertThat(tree.rootNodes().get("A").children().get("B").isEndOfPath()).isFalse();
        assertThat(tree.rootNodes().get("A").children().get("B").children().keySet()).containsExactly("C");
        assertThat(tree.rootNodes().get("A").children().get("B").children().get("C").isEndOfPath()).isTrue();
        assertThat(tree.rootNodes().get("A").children().get("B").children().get("C").set()).isEqualTo(set("A", "B", "C"));
        assertThat(tree.rootNodes().get("A").children().get("B").children().get("C").children()).isEmpty();
        assertThat(tree.rootSet()).isNull();
    }

    @Test
    public void testExampleFromPaper() {
        final UBTree<String> tree = new UBTree<>();
        tree.addSet(set("e0", "e1", "e2", "e3"));
        tree.addSet(set("e0", "e1", "e3"));
        tree.addSet(set("e0", "e1", "e2"));
        tree.addSet(set("e2", "e3"));
        assertThat(tree.rootNodes()).hasSize(2);
        assertThat(tree.rootNodes().keySet()).containsExactly("e0", "e2");
        assertThat(tree.rootSet()).isNull();

        // root nodes
        final UBNode<String> e0 = tree.rootNodes().get("e0");
        final UBNode<String> e2 = tree.rootNodes().get("e2");
        assertThat(e0.isEndOfPath()).isFalse();
        assertThat(e0.children().keySet()).containsExactly("e1");
        assertThat(e2.isEndOfPath()).isFalse();
        assertThat(e2.children().keySet()).containsExactly("e3");

        // first level
        final UBNode<String> e0e1 = e0.children().get("e1");
        final UBNode<String> e2e3 = e2.children().get("e3");
        assertThat(e0e1.isEndOfPath()).isFalse();
        assertThat(e0e1.children().keySet()).containsExactly("e2", "e3");
        assertThat(e2e3.isEndOfPath()).isTrue();
        assertThat(e2e3.set()).isEqualTo(set("e2", "e3"));
        assertThat(e2e3.children().keySet()).isEmpty();

        // second level
        final UBNode<String> e0e1e2 = e0e1.children().get("e2");
        assertThat(e0e1e2.isEndOfPath()).isTrue();
        assertThat(e0e1e2.set()).isEqualTo(set("e0", "e1", "e2"));
        assertThat(e0e1e2.children().keySet()).containsExactly("e3");
        final UBNode<String> e0e1e3 = e0e1.children().get("e3");
        assertThat(e0e1e3.isEndOfPath()).isTrue();
        assertThat(e0e1e3.set()).isEqualTo(set("e0", "e1", "e3"));
        assertThat(e0e1e3.children().keySet()).isEmpty();

        //third level
        final UBNode<String> e0e1e2e3 = e0e1e2.children().get("e3");
        assertThat(e0e1e2e3.isEndOfPath()).isTrue();
        assertThat(e0e1e2e3.set()).isEqualTo(set("e0", "e1", "e2", "e3"));
        assertThat(e0e1e2e3.children().keySet()).isEmpty();
    }

    @Test
    public void testContainsSubset() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        assertThat(tree.firstSubset(set())).isNull();
        assertThat(tree.firstSubset(set("e0"))).isNull();
        assertThat(tree.firstSubset(set("e1"))).isNull();
        assertThat(tree.firstSubset(set("e2"))).isNull();
        assertThat(tree.firstSubset(set("e3"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e1"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e2"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e3"))).isNull();
        assertThat(tree.firstSubset(set("e1", "e2"))).isNull();
        assertThat(tree.firstSubset(set("e1", "e3"))).isNull();
        assertThat(tree.firstSubset(set("e2", "e3"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e0", "e1", "e2"))).isEqualTo(e012);
        assertThat(tree.firstSubset(set("e0", "e1", "e3"))).isEqualTo(e013);
        assertThat(tree.firstSubset(set("e0", "e2", "e3"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e1", "e2", "e3"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e3"))).isIn(e0123, e013, e012, e23);
        assertThat(tree.firstSubset(set("e0", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e1", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e2", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e3", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e1", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e2", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e0", "e3", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e1", "e2", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e1", "e3", "e4"))).isNull();
        assertThat(tree.firstSubset(set("e2", "e3", "e4"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e4"))).isEqualTo(e012);
        assertThat(tree.firstSubset(set("e0", "e1", "e3", "e4"))).isEqualTo(e013);
        assertThat(tree.firstSubset(set("e0", "e2", "e3", "e4"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e1", "e2", "e3", "e4"))).isEqualTo(e23);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e3", "e4"))).isIn(e0123, e013, e012, e23);
    }

    @Test
    public void testContainsSubsetWithEmtpySet() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        final SortedSet<String> empty = set();
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        tree.addSet(empty);
        assertThat(tree.firstSubset(set())).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e2"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e2"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e2"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e2", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e2"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e2", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e2", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e3"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e2", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e2", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e2", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e2", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e2", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e1", "e2", "e3", "e4"))).isEqualTo(empty);
        assertThat(tree.firstSubset(set("e0", "e1", "e2", "e3", "e4"))).isEqualTo(empty);
    }

    @Test
    public void testAllSubsets() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        assertThat(tree.allSubsets(set())).isEmpty();
        assertThat(tree.allSubsets(set("e0"))).isEmpty();
        assertThat(tree.allSubsets(set("e1"))).isEmpty();
        assertThat(tree.allSubsets(set("e2"))).isEmpty();
        assertThat(tree.allSubsets(set("e3"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e1"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e2"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e3"))).isEmpty();
        assertThat(tree.allSubsets(set("e1", "e2"))).isEmpty();
        assertThat(tree.allSubsets(set("e1", "e3"))).isEmpty();
        assertThat(tree.allSubsets(set("e2", "e3"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2"))).containsExactlyInAnyOrder(e012);
        assertThat(tree.allSubsets(set("e0", "e1", "e3"))).containsExactlyInAnyOrder(e013);
        assertThat(tree.allSubsets(set("e0", "e2", "e3"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e1", "e2", "e3"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e3"))).containsExactlyInAnyOrder(e0123, e013, e012, e23);
        assertThat(tree.allSubsets(set("e0", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e1", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e2", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e3", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e1", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e2", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e0", "e3", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e1", "e2", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e1", "e3", "e4"))).isEmpty();
        assertThat(tree.allSubsets(set("e2", "e3", "e4"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e4"))).containsExactlyInAnyOrder(e012);
        assertThat(tree.allSubsets(set("e0", "e1", "e3", "e4"))).containsExactlyInAnyOrder(e013);
        assertThat(tree.allSubsets(set("e0", "e2", "e3", "e4"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e1", "e2", "e3", "e4"))).containsExactlyInAnyOrder(e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e3", "e4"))).containsExactlyInAnyOrder(e0123, e013, e012, e23);
    }

    @Test
    public void testAllSubsetsWithEmptySet() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        final SortedSet<String> empty = set();
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        tree.addSet(empty);
        assertThat(tree.allSubsets(set())).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e2"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e3"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e1"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e2"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e3"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1", "e2"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1", "e3"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e2", "e3"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2"))).containsExactlyInAnyOrder(empty, e012);
        assertThat(tree.allSubsets(set("e0", "e1", "e3"))).containsExactlyInAnyOrder(empty, e013);
        assertThat(tree.allSubsets(set("e0", "e2", "e3"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e1", "e2", "e3"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e3"))).containsExactlyInAnyOrder(empty, e0123, e013, e012, e23);
        assertThat(tree.allSubsets(set("e0", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e2", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e3", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e1", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e2", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e0", "e3", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1", "e2", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e1", "e3", "e4"))).containsExactlyInAnyOrder(empty);
        assertThat(tree.allSubsets(set("e2", "e3", "e4"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e4"))).containsExactlyInAnyOrder(empty, e012);
        assertThat(tree.allSubsets(set("e0", "e1", "e3", "e4"))).containsExactlyInAnyOrder(empty, e013);
        assertThat(tree.allSubsets(set("e0", "e2", "e3", "e4"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e1", "e2", "e3", "e4"))).containsExactlyInAnyOrder(empty, e23);
        assertThat(tree.allSubsets(set("e0", "e1", "e2", "e3", "e4"))).containsExactlyInAnyOrder(empty, e0123, e013, e012, e23);
    }

    @Test
    public void testAllSupersets() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        assertThat(tree.allSupersets(set())).containsExactlyInAnyOrder(e0123, e013, e012, e23);
        assertThat(tree.allSupersets(set("e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e1"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e2"))).containsExactlyInAnyOrder(e0123, e012, e23);
        assertThat(tree.allSupersets(set("e3"))).containsExactlyInAnyOrder(e0123, e013, e23);
        assertThat(tree.allSupersets(set("e0", "e1"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e0", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e0", "e3"))).containsExactlyInAnyOrder(e0123, e013);
        assertThat(tree.allSupersets(set("e1", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e1", "e3"))).containsExactlyInAnyOrder(e0123, e013);
        assertThat(tree.allSupersets(set("e2", "e3"))).containsExactlyInAnyOrder(e0123, e23);
        assertThat(tree.allSupersets(set("e0", "e1", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e0", "e2", "e3"))).containsExactlyInAnyOrder(e0123);
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e3"))).containsExactlyInAnyOrder(e0123);
        assertThat(tree.allSupersets(set("e0", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e3", "e4"))).isEmpty();
    }

    @Test
    public void testAllSupersetsWithEmptySet() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        final SortedSet<String> empty = set();
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
        tree.addSet(empty);
        assertThat(tree.allSupersets(set())).containsExactlyInAnyOrder(e0123, e013, e012, e23, empty);
        assertThat(tree.allSupersets(set("e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e1"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e2"))).containsExactlyInAnyOrder(e0123, e012, e23);
        assertThat(tree.allSupersets(set("e3"))).containsExactlyInAnyOrder(e0123, e013, e23);
        assertThat(tree.allSupersets(set("e0", "e1"))).containsExactlyInAnyOrder(e0123, e012, e013);
        assertThat(tree.allSupersets(set("e0", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e0", "e3"))).containsExactlyInAnyOrder(e0123, e013);
        assertThat(tree.allSupersets(set("e1", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e1", "e3"))).containsExactlyInAnyOrder(e0123, e013);
        assertThat(tree.allSupersets(set("e2", "e3"))).containsExactlyInAnyOrder(e0123, e23);
        assertThat(tree.allSupersets(set("e0", "e1", "e2"))).containsExactlyInAnyOrder(e0123, e012);
        assertThat(tree.allSupersets(set("e0", "e2", "e3"))).containsExactlyInAnyOrder(e0123);
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e3"))).containsExactlyInAnyOrder(e0123);
        assertThat(tree.allSupersets(set("e0", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e1", "e2", "e3", "e4"))).isEmpty();
        assertThat(tree.allSupersets(set("e0", "e1", "e2", "e3", "e4"))).isEmpty();
    }

    @Test
    public void testAllSets() {
        final UBTree<String> tree = new UBTree<>();
        final SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        final SortedSet<String> e013 = set("e0", "e1", "e3");
        final SortedSet<String> e012 = set("e0", "e1", "e2");
        final SortedSet<String> e23 = set("e2", "e3");
        final SortedSet<String> empty = set();
        tree.addSet(e0123);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123);
        tree.addSet(e013);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013);
        tree.addSet(e012);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013, e012);
        tree.addSet(e23);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013, e012, e23);
        tree.addSet(empty);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013, e012, e23, empty);
    }

    private static SortedSet<String> set(final String... elements) {
        return new TreeSet<>(asList(elements));
    }
}
