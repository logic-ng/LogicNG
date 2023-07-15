// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.predicates;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

/**
 * Unit Tests for {@link ContainsPBCPredicate}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class ContainsPBCPredicateTest extends TestWithExampleFormulas {

    private final ContainsPBCPredicate predicate = ContainsPBCPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.f.falsum().holds(this.predicate)).isFalse();
        assertThat(this.f.verum().holds(this.predicate)).isFalse();
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.holds(this.predicate)).isFalse();
        assertThat(this.NA.holds(this.predicate)).isFalse();
    }

    @Test
    public void testNot() throws ParserException {
        assertThat(this.f.parse("~a").holds(this.predicate)).isFalse();
        assertThat(this.f.parse("~(a | b)").holds(this.predicate)).isFalse();

        assertThat(this.f.parse("~(a | (a + b = 3))").holds(this.predicate)).isTrue();
        assertThat(this.f.parse("~(a & ~(a + b = 3))").holds(this.predicate)).isTrue();
    }

    @Test
    public void testMixed() throws ParserException {
        assertThat(this.f.parse("a => b").holds(this.predicate)).isFalse();
        assertThat(this.f.parse("a <=> b").holds(this.predicate)).isFalse();
        assertThat(this.f.parse("a => (b | c & ~(e | d))").holds(this.predicate)).isFalse();
        assertThat(this.f.parse("a <=> (b | c & ~(e | d))").holds(this.predicate)).isFalse();

        assertThat(this.f.parse("a => (3*a + ~b <= 4)").holds(this.predicate)).isTrue();
        assertThat(this.f.parse("(3*a + ~b <= 4) <=> b").holds(this.predicate)).isTrue();
        assertThat(this.f.parse("a => (b | c & (3*a + ~b <= 4) & ~(e | d))").holds(this.predicate)).isTrue();
        assertThat(this.f.parse("a <=> (b | c & ~(e | (3*a + ~b <= 4) | d))").holds(this.predicate)).isTrue();
        assertThat(this.f.parse("3*a + ~b <= 4").holds(this.predicate)).isTrue();
    }

    @Test
    @RandomTag
    public void randomWithoutPBCs() {
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(0).seed(i * 42).build());
            final Formula formula = randomizer.formula(5);
            assertThat(formula.holds(this.predicate)).isFalse();
        }
    }

    @Test
    public void testToString() {
        assertThat(this.predicate.toString()).isEqualTo("ContainsPBCPredicate");
    }
}
