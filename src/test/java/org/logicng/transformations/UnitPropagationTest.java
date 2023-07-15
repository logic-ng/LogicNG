// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link UnitPropagation}.
 * @version 2.0.0
 * @since 1.2
 */
public class UnitPropagationTest extends TestWithExampleFormulas {

    private final UnitPropagation unitPropagation = UnitPropagation.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.unitPropagation)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.unitPropagation)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.unitPropagation)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.unitPropagation)).isEqualTo(this.NA);
    }

    @Test
    public void testNoPropagation() {
        assertThat(this.AND1.transform(this.unitPropagation)).isEqualTo(this.AND1);
        assertThat(this.AND2.transform(this.unitPropagation)).isEqualTo(this.AND2);
        assertThat(this.OR1.transform(this.unitPropagation)).isEqualTo(this.OR1);
        assertThat(this.OR2.transform(this.unitPropagation)).isEqualTo(this.OR2);
    }

    @Test
    public void testPropagations() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.f.and(this.AND1, this.A).transform(this.unitPropagation)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.AND2, this.A).transform(this.unitPropagation)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.OR1, this.X).transform(this.unitPropagation)).isEqualTo(this.X);
        assertThat(this.f.or(this.AND1, this.A).transform(this.unitPropagation)).isEqualTo(this.A);
        assertThat(this.f.or(this.OR1, this.X).transform(this.unitPropagation)).isEqualTo(this.OR1);
        assertThat(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(this.unitPropagation)).isEqualTo(p.parse("(e | g) & (e | ~g | h) & f & c & d & ~a & b"));
    }
}
