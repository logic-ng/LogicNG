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
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for the class {@link Substitution}.
 * @version 2.0.0
 * @since 1.0
 */
public class SubstitutionTest {

    private final Substitution subst;

    public SubstitutionTest() {
        this.subst = new Substitution();
        this.subst.addMapping(F.A, F.NA);
        this.subst.addMapping(F.B, F.OR1);
        this.subst.addMapping(F.X, F.AND1);
    }

    @Test
    public void testConstructor() {
        assertThat(new Substitution()).isNotNull();
    }

    @Test
    public void testSize() {
        final Substitution subst = new Substitution();
        subst.addMapping(F.A, F.NA);
        subst.addMapping(F.B, F.OR1);
        subst.addMapping(F.C, F.AND1);
        assertThat(subst.size()).isEqualTo(3);
    }

    @Test
    public void testGetSubstitution() {
        final Substitution subst = new Substitution();
        subst.addMapping(F.A, F.NA);
        subst.addMapping(F.B, F.OR1);
        subst.addMapping(F.C, F.AND1);
        assertThat(subst.getSubstitution(F.A)).isEqualTo(F.NA);
        assertThat(subst.getSubstitution(F.B)).isEqualTo(F.OR1);
        assertThat(subst.getSubstitution(F.C)).isEqualTo(F.AND1);
        assertThat(subst.getSubstitution(F.X)).isNull();
        subst.addMapping(F.B, F.AND1);
        assertThat(subst.getSubstitution(F.B)).isEqualTo(F.AND1);
    }

    @Test
    public void testConstantSubstitution() {
        assertThat(F.FALSE.substitute(this.subst)).isEqualTo(F.FALSE);
        assertThat(F.TRUE.substitute(this.subst)).isEqualTo(F.TRUE);
    }

    @Test
    public void testLiteralSubstitution() {
        assertThat(F.C.substitute(this.subst)).isEqualTo(F.C);
        assertThat(F.A.substitute(this.subst)).isEqualTo(F.NA);
        assertThat(F.B.substitute(this.subst)).isEqualTo(F.OR1);
        assertThat(F.X.substitute(this.subst)).isEqualTo(F.AND1);
        assertThat(F.NA.substitute(this.subst)).isEqualTo(F.A);
        assertThat(F.NB.substitute(this.subst)).isEqualTo(F.NOT2);
        assertThat(F.NX.substitute(this.subst)).isEqualTo(F.NOT1);
    }

    @Test
    public void testNotSubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.NOT1.substitute(this.subst)).isEqualTo(p.parse("~(~a & (x | y))"));
        assertThat(F.NOT2.substitute(this.subst)).isEqualTo(p.parse("~(a & b | y)"));
    }

    @Test
    public void testBinarySubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.IMP1.substitute(this.subst)).isEqualTo(p.parse("~a => (x | y)"));
        assertThat(F.IMP4.substitute(this.subst)).isEqualTo(p.parse("(~a <=> (x | y)) => (~(a & b) <=> ~y)"));
        assertThat(F.EQ2.substitute(this.subst)).isEqualTo(p.parse("a <=> ~(x | y)"));
        assertThat(F.EQ3.substitute(this.subst)).isEqualTo(p.parse("(~a & (x | y)) <=> (a & b | y)"));
    }

    @Test
    public void testNArySubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.AND3.substitute(this.subst)).isEqualTo(p.parse("(a & b | y) & (~(a & b) | ~y)"));
        assertThat(F.f.and(F.NB, F.C, F.X, F.NY).substitute(this.subst)).isEqualTo(p.parse("~(x | y) & c & a & b & ~y"));
        assertThat(F.OR3.substitute(this.subst)).isEqualTo(p.parse("(~a & (x | y)) | (a & ~(x | y))"));
        assertThat(F.f.or(F.A, F.NB, F.C, F.X, F.NY).substitute(this.subst)).isEqualTo(p.parse("~a | ~(x | y) | c | a & b | ~y"));
    }

    @Test
    public void testSingleSubstitution() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.A.substitute(F.A, F.OR1)).isEqualTo(p.parse("x | y"));
        assertThat(F.NA.substitute(F.A, F.OR1)).isEqualTo(p.parse("~(x | y)"));
        assertThat(F.IMP1.substitute(F.B, F.OR1)).isEqualTo(p.parse("a => (x | y)"));
        assertThat(F.EQ2.substitute(F.B, F.OR1)).isEqualTo(p.parse("~a <=> ~(x | y)"));
        assertThat(F.f.and(F.A, F.NB, F.C, F.NX, F.NY).substitute(F.Y, F.X)).isEqualTo(p.parse("a & ~b & c & ~x"));
        assertThat(F.f.or(F.A, F.NB, F.C, F.NX, F.NY).substitute(F.Y, F.X)).isEqualTo(p.parse("a | ~b | c | ~x"));
    }

    @Test
    public void testHashCode() {
        final Substitution subst = new Substitution();
        subst.addMapping(F.A, F.NA);
        subst.addMapping(F.B, F.OR1);
        subst.addMapping(F.C, F.AND1);
        final Substitution subst2 = new Substitution();
        subst2.addMapping(F.B, F.OR1);
        subst2.addMapping(F.C, F.AND1);
        subst2.addMapping(F.A, F.NA);
        assertThat(subst2.hashCode()).isEqualTo(subst.hashCode());
    }

    @Test
    public void testEquals() {
        final Substitution subst = new Substitution();
        subst.addMapping(F.A, F.NA);
        subst.addMapping(F.B, F.OR1);
        subst.addMapping(F.C, F.AND1);
        final Substitution subst2 = new Substitution();
        subst2.addMapping(F.B, F.OR1);
        subst2.addMapping(F.C, F.AND1);
        subst2.addMapping(F.A, F.NA);
        final Substitution subst3 = new Substitution();
        subst3.addMapping(F.B, F.OR1);
        subst3.addMapping(F.C, F.AND1);
        assertThat(subst2).isEqualTo(subst);
        assertThat(subst).isEqualTo(subst);
        assertThat(new Assignment()).isNotEqualTo(subst);
        assertThat(subst3).isNotEqualTo(subst);
    }

    @Test
    public void testToString() {
        final Substitution subst = new Substitution();
        assertThat(subst.toString()).isEqualTo("Substitution{}");
        subst.addMapping(F.A, F.NA);
        assertThat(subst.toString()).isEqualTo("Substitution{a=~a}");
        subst.addMapping(F.B, F.OR1);
    }

}
