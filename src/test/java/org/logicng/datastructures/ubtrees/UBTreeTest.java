package org.logicng.datastructures.ubtrees;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link UBTree}.
 * @version 1.5.0
 * @since 1.5.0
 */
public class UBTreeTest {

    @Test
    public void testEmptySet() {
        UBTree<String> tree = new UBTree<>();
        tree.addSet(new TreeSet<String>());
        assertThat(tree.rootNodes()).isEmpty();
    }

    @Test
    public void testSingleSet() {
        UBTree<String> tree = new UBTree<>();
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
    }

    @Test
    public void testExampleFromPaper() {
        UBTree<String> tree = new UBTree<>();
        tree.addSet(set("e0", "e1", "e2", "e3"));
        tree.addSet(set("e0", "e1", "e3"));
        tree.addSet(set("e0", "e1", "e2"));
        tree.addSet(set("e2", "e3"));
        assertThat(tree.rootNodes()).hasSize(2);
        assertThat(tree.rootNodes().keySet()).containsExactly("e0", "e2");

        // root nodes
        UBNode<String> e0 = tree.rootNodes().get("e0");
        UBNode<String> e2 = tree.rootNodes().get("e2");
        assertThat(e0.isEndOfPath()).isFalse();
        assertThat(e0.children().keySet()).containsExactly("e1");
        assertThat(e2.isEndOfPath()).isFalse();
        assertThat(e2.children().keySet()).containsExactly("e3");

        // first level
        UBNode<String> e0e1 = e0.children().get("e1");
        UBNode<String> e2e3 = e2.children().get("e3");
        assertThat(e0e1.isEndOfPath()).isFalse();
        assertThat(e0e1.children().keySet()).containsExactly("e2", "e3");
        assertThat(e2e3.isEndOfPath()).isTrue();
        assertThat(e2e3.set()).isEqualTo(set("e2", "e3"));
        assertThat(e2e3.children().keySet()).isEmpty();

        // second level
        UBNode<String> e0e1e2 = e0e1.children().get("e2");
        assertThat(e0e1e2.isEndOfPath()).isTrue();
        assertThat(e0e1e2.set()).isEqualTo(set("e0", "e1", "e2"));
        assertThat(e0e1e2.children().keySet()).containsExactly("e3");
        UBNode<String> e0e1e3 = e0e1.children().get("e3");
        assertThat(e0e1e3.isEndOfPath()).isTrue();
        assertThat(e0e1e3.set()).isEqualTo(set("e0", "e1", "e3"));
        assertThat(e0e1e3.children().keySet()).isEmpty();

        //third level
        UBNode<String> e0e1e2e3 = e0e1e2.children().get("e3");
        assertThat(e0e1e2e3.isEndOfPath()).isTrue();
        assertThat(e0e1e2e3.set()).isEqualTo(set("e0", "e1", "e2", "e3"));
        assertThat(e0e1e2e3.children().keySet()).isEmpty();
    }

    @Test
    public void testContainsSubset() {
        UBTree<String> tree = new UBTree<>();
        SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        SortedSet<String> e013 = set("e0", "e1", "e3");
        SortedSet<String> e012 = set("e0", "e1", "e2");
        SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
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
    public void testAllSubsets() {
        UBTree<String> tree = new UBTree<>();
        SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        SortedSet<String> e013 = set("e0", "e1", "e3");
        SortedSet<String> e012 = set("e0", "e1", "e2");
        SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
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
    public void testAllSupersets() {
        UBTree<String> tree = new UBTree<>();
        SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        SortedSet<String> e013 = set("e0", "e1", "e3");
        SortedSet<String> e012 = set("e0", "e1", "e2");
        SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        tree.addSet(e013);
        tree.addSet(e012);
        tree.addSet(e23);
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
        UBTree<String> tree = new UBTree<>();
        SortedSet<String> e0123 = set("e0", "e1", "e2", "e3");
        SortedSet<String> e013 = set("e0", "e1", "e3");
        SortedSet<String> e012 = set("e0", "e1", "e2");
        SortedSet<String> e23 = set("e2", "e3");
        tree.addSet(e0123);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123);
        tree.addSet(e013);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013);
        tree.addSet(e012);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013, e012);
        tree.addSet(e23);
        assertThat(tree.allSets()).containsExactlyInAnyOrder(e0123, e013, e012, e23);
    }

    private SortedSet<String> set(final String... elements) {
        return new TreeSet<>(Arrays.asList(elements));
    }
}
