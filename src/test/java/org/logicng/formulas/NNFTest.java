// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit Tests for NNF conversion.
 * @version 2.2.0
 * @since 1.0
 */
public class NNFTest extends TestWithExampleFormulas {

    @Test
    public void testConstants() {
        assertThat(this.TRUE.nnf()).isEqualTo(this.TRUE);
        assertThat(this.FALSE.nnf()).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.nnf()).isEqualTo(this.A);
        assertThat(this.NA.nnf()).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.IMP1.nnf()).isEqualTo(p.parse("~a | b"));
        assertThat(this.IMP2.nnf()).isEqualTo(p.parse("a | ~b"));
        assertThat(this.IMP3.nnf()).isEqualTo(p.parse("~a | ~b | x | y"));
        assertThat(this.IMP4.nnf()).isEqualTo(p.parse("(~a | ~b) & (a | b) | (x | ~y) & (y | ~x)"));
        assertThat(this.EQ1.nnf()).isEqualTo(p.parse("(~a | b) & (~b | a)"));
        assertThat(this.EQ2.nnf()).isEqualTo(p.parse("(a | ~b) & (b | ~a)"));
        assertThat(this.EQ3.nnf()).isEqualTo(p.parse("(~a | ~b | x | y) & (~x & ~y | a & b)"));
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.nnf()).isEqualTo(this.AND1);
        assertThat(this.OR1.nnf()).isEqualTo(this.OR1);
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").nnf()).isEqualTo(p.parse("~a & ~b & c & (~x | y) & (~w | z)"));
        assertThat(p.parse("~(a & b) | c | ~(x | ~y) | (w => z)").nnf()).isEqualTo(p.parse("~a  | ~b | c | (~x & y) | (~w | z)"));
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").nnf()).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").nnf()).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").nnf()).isEqualTo(p.parse("a & ~b"));
        assertThat(p.parse("~(~(a | b) => ~(x | y))").nnf()).isEqualTo(p.parse("~a & ~b & (x | y)"));
        assertThat(p.parse("a <=> b").nnf()).isEqualTo(p.parse("(~a | b) & (~b | a)"));
        assertThat(p.parse("~(a <=> b)").nnf()).isEqualTo(p.parse("(~a | ~b) & (a | b)"));
        assertThat(p.parse("~(~(a | b) <=> ~(x | y))").nnf()).isEqualTo(p.parse("((a | b) | (x | y)) & ((~a & ~b) | (~x & ~y))"));
        assertThat(p.parse("~(a & b & ~x & ~y)").nnf()).isEqualTo(p.parse("~a | ~b | x | y"));
        assertThat(p.parse("~(a | b | ~x | ~y)").nnf()).isEqualTo(p.parse("~a & ~b & x & y"));
        assertThat(p.parse("~(a | b | ~x | ~y)").nnf()).isEqualTo(p.parse("~a & ~b & x & y"));
    }
}
