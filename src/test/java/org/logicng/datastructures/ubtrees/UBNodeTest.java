package org.logicng.datastructures.ubtrees;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link UBNode}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class UBNodeTest extends TestWithExampleFormulas {

    private final UBNode<Integer> node1;
    private final UBNode<String> node2;

    public UBNodeTest() {
        this.node1 = new UBNode<>(1);
        this.node2 = new UBNode<>("String");
    }

    @Test
    public void testHashCode() {
        assertThat(this.node1.hashCode()).isEqualTo(this.node1.hashCode());
        assertThat(this.node1.hashCode()).isEqualTo(new UBNode<>(1).hashCode());
    }

    @Test
    public void testEquals() {
        assertThat(this.node1.hashCode()).isEqualTo(this.node1.hashCode());
        final List<SortedSet<Literal>> primeImplicants = new ArrayList<>();
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        primeImplicants.add(new TreeSet<>(Arrays.asList(this.A, this.C)));
        final List<SortedSet<Literal>> primeImplicates = new ArrayList<>();
        primeImplicates.add(new TreeSet<>(Arrays.asList(this.A, this.NB)));
        assertThat(this.node1.equals(this.node1)).isTrue();
        assertThat(this.node1.equals(new UBNode<>(1))).isTrue();
        assertThat(this.node1.equals(this.node2)).isFalse();
        assertThat(this.node2.equals(this.node1)).isFalse();
        assertThat(this.node1.equals(null)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.node1.toString()).isEqualTo(
                "UBNode{" +
                        "element=1" +
                        ", children={}" +
                        ", set=null" +
                        '}');
    }
}
