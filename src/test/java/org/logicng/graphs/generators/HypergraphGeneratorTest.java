// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.graphs.generators;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Hypergraph;
import org.logicng.graphs.datastructures.HypergraphEdge;
import org.logicng.graphs.datastructures.HypergraphNode;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;

/**
 * Unit tests for {@link HypergraphGenerator}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class HypergraphGeneratorTest {

    @Test
    public void testCNF() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(HypergraphGenerator.fromCNF(p.parse("$false")).nodes()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(p.parse("$false")).edges()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(p.parse("$true")).nodes()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(p.parse("$true")).edges()).isEmpty();

        Hypergraph<Variable> hypergraph = HypergraphGenerator.fromCNF(p.parse("A"));
        HypergraphNode<Variable> nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        assertThat(hypergraph.nodes()).containsExactly(nodeA);
        assertThat(hypergraph.edges()).containsExactly(new HypergraphEdge<>(Collections.singletonList(nodeA)));

        hypergraph = HypergraphGenerator.fromCNF(p.parse("A | B | ~C"));
        nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        HypergraphNode<Variable> nodeB = new HypergraphNode<>(hypergraph, f.variable("B"));
        HypergraphNode<Variable> nodeC = new HypergraphNode<>(hypergraph, f.variable("C"));
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(nodeA, nodeB, nodeC);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(new HypergraphEdge<>(Arrays.asList(nodeA, nodeB, nodeC)));

        hypergraph = HypergraphGenerator.fromCNF(p.parse("(A | B | ~C) & (B | ~D) & (C | ~E) & (~B | ~D | E) & X & ~Y"));
        nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        nodeB = new HypergraphNode<>(hypergraph, f.variable("B"));
        nodeC = new HypergraphNode<>(hypergraph, f.variable("C"));
        final HypergraphNode<Variable> nodeD = new HypergraphNode<>(hypergraph, f.variable("D"));
        final HypergraphNode<Variable> nodeE = new HypergraphNode<>(hypergraph, f.variable("E"));
        final HypergraphNode<Variable> nodeX = new HypergraphNode<>(hypergraph, f.variable("X"));
        final HypergraphNode<Variable> nodeY = new HypergraphNode<>(hypergraph, f.variable("Y"));
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(nodeA, nodeB, nodeC, nodeD, nodeE, nodeX, nodeY);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(
                new HypergraphEdge<>(Arrays.asList(nodeA, nodeB, nodeC)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD)),
                new HypergraphEdge<>(Arrays.asList(nodeC, nodeE)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD, nodeE)),
                new HypergraphEdge<>(Collections.singletonList(nodeX)),
                new HypergraphEdge<>(Collections.singletonList(nodeY))
        );
    }

    @Test
    public void testCNFFromList() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("$false"))).nodes()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("$false"))).edges()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("$true"))).nodes()).isEmpty();
        assertThat(HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("$true"))).edges()).isEmpty();

        Hypergraph<Variable> hypergraph = HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("A")));
        HypergraphNode<Variable> nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        assertThat(hypergraph.nodes()).containsExactly(nodeA);
        assertThat(hypergraph.edges()).containsExactly(new HypergraphEdge<>(Collections.singletonList(nodeA)));

        hypergraph = HypergraphGenerator.fromCNF(Collections.singletonList(p.parse("A | B | ~C")));
        nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        HypergraphNode<Variable> nodeB = new HypergraphNode<>(hypergraph, f.variable("B"));
        HypergraphNode<Variable> nodeC = new HypergraphNode<>(hypergraph, f.variable("C"));
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(nodeA, nodeB, nodeC);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(new HypergraphEdge<>(Arrays.asList(nodeA, nodeB, nodeC)));

        hypergraph = HypergraphGenerator.fromCNF(Arrays.asList(
                p.parse("(A | B | ~C)"),
                p.parse("(B | ~D)"),
                p.parse("(C | ~E)"),
                p.parse("(~B | ~D | E)"),
                p.parse("X"),
                p.parse("~Y")
        ));
        nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        nodeB = new HypergraphNode<>(hypergraph, f.variable("B"));
        nodeC = new HypergraphNode<>(hypergraph, f.variable("C"));
        HypergraphNode<Variable> nodeD = new HypergraphNode<>(hypergraph, f.variable("D"));
        HypergraphNode<Variable> nodeE = new HypergraphNode<>(hypergraph, f.variable("E"));
        HypergraphNode<Variable> nodeX = new HypergraphNode<>(hypergraph, f.variable("X"));
        HypergraphNode<Variable> nodeY = new HypergraphNode<>(hypergraph, f.variable("Y"));
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(nodeA, nodeB, nodeC, nodeD, nodeE, nodeX, nodeY);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(
                new HypergraphEdge<>(Arrays.asList(nodeA, nodeB, nodeC)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD)),
                new HypergraphEdge<>(Arrays.asList(nodeC, nodeE)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD, nodeE)),
                new HypergraphEdge<>(Collections.singletonList(nodeX)),
                new HypergraphEdge<>(Collections.singletonList(nodeY))
        );

        hypergraph = HypergraphGenerator.fromCNF(
                p.parse("(A | B | ~C)"),
                p.parse("(B | ~D)"),
                p.parse("(C | ~E)"),
                p.parse("(~B | ~D | E)"),
                p.parse("X"),
                p.parse("~Y")
        );
        nodeA = new HypergraphNode<>(hypergraph, f.variable("A"));
        nodeB = new HypergraphNode<>(hypergraph, f.variable("B"));
        nodeC = new HypergraphNode<>(hypergraph, f.variable("C"));
        nodeD = new HypergraphNode<>(hypergraph, f.variable("D"));
        nodeE = new HypergraphNode<>(hypergraph, f.variable("E"));
        nodeX = new HypergraphNode<>(hypergraph, f.variable("X"));
        nodeY = new HypergraphNode<>(hypergraph, f.variable("Y"));
        assertThat(hypergraph.nodes()).containsExactlyInAnyOrder(nodeA, nodeB, nodeC, nodeD, nodeE, nodeX, nodeY);
        assertThat(hypergraph.edges()).containsExactlyInAnyOrder(
                new HypergraphEdge<>(Arrays.asList(nodeA, nodeB, nodeC)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD)),
                new HypergraphEdge<>(Arrays.asList(nodeC, nodeE)),
                new HypergraphEdge<>(Arrays.asList(nodeB, nodeD, nodeE)),
                new HypergraphEdge<>(Collections.singletonList(nodeX)),
                new HypergraphEdge<>(Collections.singletonList(nodeY))
        );
    }

    @Test
    public void testNonCNF() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        try {
            HypergraphGenerator.fromCNF(p.parse("A => B"));
        } catch (final IllegalArgumentException e) {
            assertThat(e).hasMessage("Cannot generate a hypergraph from a non-cnf formula");
        }
    }
}
