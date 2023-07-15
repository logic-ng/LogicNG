// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for formula restriction.
 * @version 2.0.0
 * @since 1.0
 */
public class RestrictionTest extends TestWithExampleFormulas {

    private final Assignment ass = new Assignment(Arrays.asList(this.A, this.NB, this.NX));

    @Test
    public void testConstantRestrict() {
        assertThat(this.TRUE.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.restrict(this.ass)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiteralRestrict() {
        assertThat(this.A.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.NA.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.X.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.NX.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.C.restrict(this.ass)).isEqualTo(this.C);
        assertThat(this.NY.restrict(this.ass)).isEqualTo(this.NY);
    }

    @Test
    public void testNotRestrict() {
        assertThat(this.NOT1.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.NOT2.restrict(this.ass)).isEqualTo(this.NY);
    }

    @Test
    public void testBinaryRestrict() {
        assertThat(this.IMP1.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.IMP2.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.f.implication(this.NA, this.C).restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.IMP3.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.f.implication(this.A, this.C).restrict(this.ass)).isEqualTo(this.C);

        assertThat(this.EQ1.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.EQ2.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.EQ3.restrict(this.ass)).isEqualTo(this.NY);
        assertThat(this.EQ4.restrict(this.ass)).isEqualTo(this.FALSE);
    }

    @Test
    public void testNAryRestrict() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.OR1.restrict(this.ass)).isEqualTo(this.Y);
        assertThat(this.OR2.restrict(this.ass)).isEqualTo(this.TRUE);
        assertThat(this.OR3.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(p.parse("~a | b | ~c | x | y").restrict(this.ass)).isEqualTo(p.parse("~c | y"));
        assertThat(p.parse("~a | b | ~c | ~x | ~y").restrict(this.ass)).isEqualTo(this.TRUE);

        assertThat(this.AND1.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.AND2.restrict(this.ass)).isEqualTo(this.FALSE);
        assertThat(this.AND3.restrict(this.ass)).isEqualTo(this.Y);
        assertThat(p.parse("a & ~b & c & ~x & ~y").restrict(this.ass)).isEqualTo(p.parse("c & ~y"));
        assertThat(p.parse("a & b & c & ~x & y").restrict(this.ass)).isEqualTo(this.FALSE);
    }
}
