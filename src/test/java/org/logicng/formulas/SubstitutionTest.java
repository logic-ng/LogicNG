///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

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
    }

}
