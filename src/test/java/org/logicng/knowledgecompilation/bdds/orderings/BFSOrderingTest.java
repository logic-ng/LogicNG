// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.orderings;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

/**
 * Unit tests for {@link BFSOrdering}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BFSOrderingTest {

    private final BFSOrdering ordering = new BFSOrdering();

    @Test
    public void testSimpleCases() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(this.ordering.getOrder(p.parse("$true"))).isEmpty();
        assertThat(this.ordering.getOrder(p.parse("$false"))).isEmpty();
        assertThat(this.ordering.getOrder(p.parse("A"))).containsExactly(f.variable("A"));
        assertThat(this.ordering.getOrder(p.parse("A => ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.ordering.getOrder(p.parse("A <=> ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.ordering.getOrder(p.parse("~(A <=> ~B)"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.ordering.getOrder(p.parse("A | ~C | B | D"))).containsExactly(f.variable("A"), f.variable("B"), f.variable("D"), f.variable("C"));
        assertThat(this.ordering.getOrder(p.parse("A & ~C & B & D"))).containsExactly(f.variable("A"), f.variable("B"), f.variable("D"), f.variable("C"));
        assertThat(this.ordering.getOrder(p.parse("A + C + B + D < 2"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
    }

    @Test
    public void testComplexFormula() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Formula formula = p.parse("(A => ~B) & ((A & C) | (D & ~C)) & (A | Y | X) & (Y <=> (X | (W + A + F < 1)))");

        assertThat(this.ordering.getOrder(formula)).containsExactly(
                f.variable("A"),
                f.variable("Y"),
                f.variable("X"),
                f.variable("B"),
                f.variable("C"),
                f.variable("D"),
                f.variable("W"),
                f.variable("F")
        );
    }
}
