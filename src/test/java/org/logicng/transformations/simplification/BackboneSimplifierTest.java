// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link BackboneSimplifier}.
 * @version 2.0.0
 * @since 1.5.0
 */
public class BackboneSimplifierTest {

    private final BackboneSimplifier backboneSimplifier = BackboneSimplifier.get();

    @Test
    public void testTrivialBackbones() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(p.parse("$true").transform(this.backboneSimplifier)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(this.backboneSimplifier)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A & (A => B) & ~B").transform(this.backboneSimplifier)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(this.backboneSimplifier)).isEqualTo(p.parse("A"));
        assertThat(p.parse("A & B").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B"));
        assertThat(p.parse("A | B | C").transform(this.backboneSimplifier)).isEqualTo(p.parse("A | B | C"));
    }

    @Test
    public void testRealBackbones() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(p.parse("A & B & (B | C)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B"));
        assertThat(p.parse("A & B & (~B | C)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B & C"));
        assertThat(p.parse("A & B & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B & C & F"));
        assertThat(p.parse("X & Y & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("X & Y & (~B | C) & (B | D) & (A => F)"));
        assertThat(p.parse("~A & ~B & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("~A & ~B & D"));
    }
}
