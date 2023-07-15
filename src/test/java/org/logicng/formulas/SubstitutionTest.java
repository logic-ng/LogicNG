// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Collections;
import java.util.HashMap;

/**
 * Unit tests for the class {@link Substitution}.
 * @version 2.0.0
 * @since 1.0
 */
public class SubstitutionTest extends TestWithExampleFormulas {

    private final Substitution subst;

    public SubstitutionTest() {
        this.subst = new Substitution();
        this.subst.addMapping(this.A, this.NA);
        this.subst.addMapping(this.B, this.OR1);
        this.subst.addMapping(this.X, this.AND1);
    }

    @Test
    public void testConstructor() {
        assertThat(new Substitution()).isNotNull();
    }

    @Test
    public void testCopyConstructor() {
        final Substitution original = new Substitution();
        original.addMapping(this.A, this.NA);
        original.addMapping(this.B, this.NB);
        final Substitution copy = new Substitution(original);
        copy.addMapping(this.X, this.NX);
        original.addMapping(this.Y, this.NY);
        assertThat(original.size()).isEqualTo(3);
        assertThat(copy.size()).isEqualTo(3);
        assertThat(copy.getSubstitution(this.A)).isEqualTo(this.NA);
        assertThat(copy.getSubstitution(this.B)).isEqualTo(this.NB);
        assertThat(original.getSubstitution(this.X)).isNull();
        assertThat(copy.getSubstitution(this.X)).isEqualTo(this.NX);
        assertThat(original.getSubstitution(this.Y)).isEqualTo(this.NY);
        assertThat(copy.getSubstitution(this.Y)).isNull();
    }

    @Test
    public void testConstructionWithMapping() {
        final HashMap<Variable, Formula> mapping = new HashMap<>();
        mapping.put(this.X, this.OR1);
        mapping.put(this.Y, this.AND1);
        final Substitution substitution = new Substitution(mapping);
        assertThat(substitution.getSubstitution(this.X)).isEqualTo(this.OR1);
        assertThat(substitution.getSubstitution(this.Y)).isEqualTo(this.AND1);
        assertThat(substitution.getSubstitution(this.A)).isNull();
        substitution.addMapping(this.A, this.NA);
        assertThat(substitution.getSubstitution(this.A)).isEqualTo(this.NA);
        assertThat(new Substitution(Collections.emptyMap()).size()).isEqualTo(0);
    }

    @Test
    public void testSize() {
        final Substitution subst = new Substitution();
        subst.addMapping(this.A, this.NA);
        subst.addMapping(this.B, this.OR1);
        subst.addMapping(this.C, this.AND1);
        assertThat(subst.size()).isEqualTo(3);
    }

    @Test
    public void testGetSubstitution() {
        final Substitution subst = new Substitution();
        subst.addMapping(this.A, this.NA);
        subst.addMapping(this.B, this.OR1);
        subst.addMapping(this.C, this.AND1);
        assertThat(subst.getSubstitution(this.A)).isEqualTo(this.NA);
        assertThat(subst.getSubstitution(this.B)).isEqualTo(this.OR1);
        assertThat(subst.getSubstitution(this.C)).isEqualTo(this.AND1);
        assertThat(subst.getSubstitution(this.X)).isNull();
        subst.addMapping(this.B, this.AND1);
        assertThat(subst.getSubstitution(this.B)).isEqualTo(this.AND1);
    }

    @Test
    public void testGetMapping() {
        final Substitution subst = new Substitution();
        assertThat(subst.getMapping()).isEqualTo(Collections.emptyMap());
        subst.addMapping(this.A, this.NA);
        subst.addMapping(this.B, this.OR1);
        subst.addMapping(this.C, this.AND1);
        final HashMap<Variable, Formula> expected = new HashMap<>();
        expected.put(this.A, this.NA);
        expected.put(this.B, this.OR1);
        expected.put(this.C, this.AND1);
        assertThat(subst.getMapping()).isEqualTo(expected);
    }

    @Test
    public void testConstantSubstitution() {
        assertThat(this.FALSE.substitute(this.subst)).isEqualTo(this.FALSE);
        assertThat(this.TRUE.substitute(this.subst)).isEqualTo(this.TRUE);
    }

    @Test
    public void testLiteralSubstitution() {
        assertThat(this.C.substitute(this.subst)).isEqualTo(this.C);
        assertThat(this.A.substitute(this.subst)).isEqualTo(this.NA);
        assertThat(this.B.substitute(this.subst)).isEqualTo(this.OR1);
        assertThat(this.X.substitute(this.subst)).isEqualTo(this.AND1);
        assertThat(this.NA.substitute(this.subst)).isEqualTo(this.A);
        assertThat(this.NB.substitute(this.subst)).isEqualTo(this.NOT2);
        assertThat(this.NX.substitute(this.subst)).isEqualTo(this.NOT1);
    }

    @Test
    public void testNotSubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.NOT1.substitute(this.subst)).isEqualTo(p.parse("~(~a & (x | y))"));
        assertThat(this.NOT2.substitute(this.subst)).isEqualTo(p.parse("~(a & b | y)"));
    }

    @Test
    public void testBinarySubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.IMP1.substitute(this.subst)).isEqualTo(p.parse("~a => (x | y)"));
        assertThat(this.IMP4.substitute(this.subst)).isEqualTo(p.parse("(~a <=> (x | y)) => (~(a & b) <=> ~y)"));
        assertThat(this.EQ2.substitute(this.subst)).isEqualTo(p.parse("a <=> ~(x | y)"));
        assertThat(this.EQ3.substitute(this.subst)).isEqualTo(p.parse("(~a & (x | y)) <=> (a & b | y)"));
    }

    @Test
    public void testNArySubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND3.substitute(this.subst)).isEqualTo(p.parse("(a & b | y) & (~(a & b) | ~y)"));
        assertThat(this.f.and(this.NB, this.C, this.X, this.NY).substitute(this.subst)).isEqualTo(p.parse("~(x | y) & c & a & b & ~y"));
        assertThat(this.OR3.substitute(this.subst)).isEqualTo(p.parse("(~a & (x | y)) | (a & ~(x | y))"));
        assertThat(this.f.or(this.A, this.NB, this.C, this.X, this.NY).substitute(this.subst)).isEqualTo(p.parse("~a | ~(x | y) | c | a & b | ~y"));
    }

    @Test
    public void testSingleSubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.A.substitute(this.A, this.OR1)).isEqualTo(p.parse("x | y"));
        assertThat(this.NA.substitute(this.A, this.OR1)).isEqualTo(p.parse("~(x | y)"));
        assertThat(this.IMP1.substitute(this.B, this.OR1)).isEqualTo(p.parse("a => (x | y)"));
        assertThat(this.EQ2.substitute(this.B, this.OR1)).isEqualTo(p.parse("~a <=> ~(x | y)"));
        assertThat(this.f.and(this.A, this.NB, this.C, this.NX, this.NY).substitute(this.Y, this.X)).isEqualTo(p.parse("a & ~b & c & ~x"));
        assertThat(this.f.or(this.A, this.NB, this.C, this.NX, this.NY).substitute(this.Y, this.X)).isEqualTo(p.parse("a | ~b | c | ~x"));
    }

    @Test
    public void testHashCode() {
        final Substitution subst = new Substitution();
        subst.addMapping(this.A, this.NA);
        subst.addMapping(this.B, this.OR1);
        subst.addMapping(this.C, this.AND1);
        final Substitution subst2 = new Substitution();
        subst2.addMapping(this.B, this.OR1);
        subst2.addMapping(this.C, this.AND1);
        subst2.addMapping(this.A, this.NA);
        assertThat(subst2.hashCode()).isEqualTo(subst.hashCode());
    }

    @Test
    public void testEquals() {
        final Substitution subst = new Substitution();
        subst.addMapping(this.A, this.NA);
        subst.addMapping(this.B, this.OR1);
        subst.addMapping(this.C, this.AND1);
        final Substitution subst2 = new Substitution();
        subst2.addMapping(this.B, this.OR1);
        subst2.addMapping(this.C, this.AND1);
        subst2.addMapping(this.A, this.NA);
        final Substitution subst3 = new Substitution();
        subst3.addMapping(this.B, this.OR1);
        subst3.addMapping(this.C, this.AND1);
        assertThat(subst2).isEqualTo(subst);
        assertThat(subst).isEqualTo(subst);
        assertThat(new Assignment()).isNotEqualTo(subst);
        assertThat(subst3).isNotEqualTo(subst);
    }

    @Test
    public void testToString() {
        final Substitution subst = new Substitution();
        assertThat(subst.toString()).isEqualTo("Substitution{}");
        subst.addMapping(this.A, this.NA);
        assertThat(subst.toString()).isEqualTo("Substitution{a=~a}");
        subst.addMapping(this.B, this.OR1);
        assertThat(subst.toString()).isEqualTo("Substitution{a=~a, b=x | y}");
    }

}
