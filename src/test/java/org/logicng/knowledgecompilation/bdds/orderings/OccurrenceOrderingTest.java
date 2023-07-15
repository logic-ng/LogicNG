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
 * Unit tests for {@link MinToMaxOrdering} and {@link MaxToMinOrdering}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class OccurrenceOrderingTest {

    private final MinToMaxOrdering min2max = new MinToMaxOrdering();
    private final MaxToMinOrdering max2min = new MaxToMinOrdering();

    @Test
    public void testSimpleCasesMin2Max() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(this.min2max.getOrder(p.parse("$true"))).isEmpty();
        assertThat(this.min2max.getOrder(p.parse("$false"))).isEmpty();
        assertThat(this.min2max.getOrder(p.parse("A"))).containsExactly(f.variable("A"));
        assertThat(this.min2max.getOrder(p.parse("A => ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.min2max.getOrder(p.parse("A <=> ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.min2max.getOrder(p.parse("~(A <=> ~B)"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.min2max.getOrder(p.parse("A | ~C | B | D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
        assertThat(this.min2max.getOrder(p.parse("A & ~C & B & D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
        assertThat(this.min2max.getOrder(p.parse("A + C + B + D < 2"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
    }

    @Test
    public void testSimpleCasesMax2Min() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(this.max2min.getOrder(p.parse("$true"))).isEmpty();
        assertThat(this.max2min.getOrder(p.parse("$false"))).isEmpty();
        assertThat(this.max2min.getOrder(p.parse("A"))).containsExactly(f.variable("A"));
        assertThat(this.max2min.getOrder(p.parse("A => ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.max2min.getOrder(p.parse("A <=> ~B"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.max2min.getOrder(p.parse("~(A <=> ~B)"))).containsExactly(f.variable("A"), f.variable("B"));
        assertThat(this.max2min.getOrder(p.parse("A | ~C | B | D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
        assertThat(this.max2min.getOrder(p.parse("A & ~C & B & D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
        assertThat(this.max2min.getOrder(p.parse("A + C + B + D < 2"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
    }

    @Test
    public void testComplexFormulaMin2Max() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Formula formula = p.parse("(A => ~B) & ((A & C) | (D & ~C)) & (A | Y | X) & (Y <=> (X | (X + W + A + F < 1)))");

        assertThat(this.min2max.getOrder(formula)).containsExactly(
                f.variable("A"),
                f.variable("X"),
                f.variable("C"),
                f.variable("Y"),
                f.variable("B"),
                f.variable("D"),
                f.variable("W"),
                f.variable("F")
        );
    }

    @Test
    public void testComplexFormulaMax2Min() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Formula formula = p.parse("(A => ~B) & ((A & C) | (D & ~C)) & (A | Y | X) & (Y <=> (X | (X + W + A + F < 1)))");

        assertThat(this.max2min.getOrder(formula)).containsExactly(
                f.variable("B"),
                f.variable("D"),
                f.variable("W"),
                f.variable("F"),
                f.variable("C"),
                f.variable("Y"),
                f.variable("X"),
                f.variable("A")
        );
    }
}
