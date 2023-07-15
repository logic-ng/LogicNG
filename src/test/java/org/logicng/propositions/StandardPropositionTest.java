// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.propositions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link StandardProposition}.
 * @version 2.0.0
 * @since 1.0
 */
public class StandardPropositionTest {

    private final PropositionalParser p;
    private final StandardProposition prop1;
    private final StandardProposition prop2;

    public StandardPropositionTest() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        this.p = new PropositionalParser(f);
        this.prop1 = new StandardProposition(this.p.parse("a & b"));
        this.prop2 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
    }

    @Test
    public void testGetters() throws ParserException {
        assertThat(this.prop1.formula()).isEqualTo(this.p.parse("a & b"));
        assertThat(this.prop2.formula()).isEqualTo(this.p.parse("a & b & ~c"));

        assertThat(this.prop1.description()).isEqualTo("");
        assertThat(this.prop2.description()).isEqualTo("prop2");
    }

    @Test
    public void testHashCode() throws ParserException {
        final StandardProposition prop11 = new StandardProposition(this.p.parse("a & b"));
        final StandardProposition prop21 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
        assertThat(this.prop1.hashCode()).isEqualTo(this.prop1.hashCode());
        assertThat(prop11.hashCode()).isEqualTo(this.prop1.hashCode());
        assertThat(prop21.hashCode()).isEqualTo(this.prop2.hashCode());
    }

    @Test
    public void testEquals() throws ParserException {
        final StandardProposition prop11 = new StandardProposition(this.p.parse("a & b"));
        final StandardProposition prop21 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
        assertThat(this.prop1.equals(this.prop1)).isTrue();
        assertThat(this.prop1.equals(prop11)).isTrue();
        assertThat(this.prop2.equals(prop21)).isTrue();
        assertThat(this.prop1.equals(this.prop2)).isFalse();
        assertThat(this.prop1.equals(null)).isFalse();
        assertThat(this.prop1.equals("String")).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.prop1.toString()).isEqualTo("StandardProposition{formula=a & b, description=}");
        assertThat(this.prop2.toString()).isEqualTo("StandardProposition{formula=a & b & ~c, description=prop2}");
    }
}
