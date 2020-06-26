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

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link DistributiveSimplifier}.
 * @version 2.0.0
 * @since 1.3
 */
public class DistributiveSimplifierTest {

    private final DistributiveSimplifier distributiveSimplifier = new DistributiveSimplifier();

    @Test
    public void testConstants() {
        assertThat(F.TRUE.transform(this.distributiveSimplifier)).isEqualTo(F.TRUE);
        assertThat(F.FALSE.transform(this.distributiveSimplifier)).isEqualTo(F.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.transform(this.distributiveSimplifier)).isEqualTo(F.A);
        assertThat(F.NA.transform(this.distributiveSimplifier)).isEqualTo(F.NA);
    }

    @Test
    public void testNoPropagation() {
        assertThat(F.AND1.transform(this.distributiveSimplifier)).isEqualTo(F.AND1);
        assertThat(F.AND2.transform(this.distributiveSimplifier)).isEqualTo(F.AND2);
        assertThat(F.OR1.transform(this.distributiveSimplifier)).isEqualTo(F.OR1);
        assertThat(F.OR2.transform(this.distributiveSimplifier)).isEqualTo(F.OR2);
    }

    @Test
    public void testPropagations() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.f.and(F.AND1, F.A).transform(this.distributiveSimplifier)).isEqualTo(F.AND1);
        assertThat(F.f.and(F.AND2, F.A).transform(this.distributiveSimplifier)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.OR1, F.X).transform(this.distributiveSimplifier)).isEqualTo(F.f.and(F.OR1, F.X));
        assertThat(F.f.and(F.OR2, F.X).transform(this.distributiveSimplifier)).isEqualTo(F.f.and(F.OR2, F.X));
        assertThat(F.f.or(F.AND1, F.A).transform(this.distributiveSimplifier)).isEqualTo(F.f.or(F.AND1, F.A));
        assertThat(F.f.or(F.AND2, F.A).transform(this.distributiveSimplifier)).isEqualTo(F.f.or(F.AND2, F.A));
        assertThat(F.f.or(F.OR1, F.X).transform(this.distributiveSimplifier)).isEqualTo(F.OR1);
        assertThat(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(this.distributiveSimplifier)).isEqualTo(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & f & c & (e | (~b | ~f | g) & (f | g | h) & (~f | ~g | h))"));
    }

    @Test
    public void testFormulaTypes() {
        assertThat(F.IMP1.transform(this.distributiveSimplifier)).isEqualTo(F.IMP1);
        assertThat(F.EQ1.transform(this.distributiveSimplifier)).isEqualTo(F.EQ1);
        assertThat(F.NOT1.transform(this.distributiveSimplifier)).isEqualTo(F.NOT1);
    }

    @Test
    public void testComplexExamples() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        final Formula cAnd = p.parse("(a | b | ~c) & (~a | ~d) & (~c | d | b) & (~c | ~b)");
        final Formula cAndD1 = cAnd.transform(this.distributiveSimplifier);
        assertThat(cAndD1).isEqualTo(p.parse("(~a | ~d) & (~c | (a | b) & (d | b) & ~b)"));
        assertThat(cAndD1.transform(this.distributiveSimplifier)).isEqualTo(p.parse("(~a | ~d) & (~c | ~b & (b | a & d))"));

        assertThat(F.f.not(cAnd).transform(this.distributiveSimplifier)).isEqualTo(F.f.not(cAndD1));

        final Formula cOr = p.parse("(x & y & z) | (x & y & ~z) | (x & ~y & z)");
        final Formula cOrD1 = cOr.transform(this.distributiveSimplifier);
        assertThat(cOrD1).isEqualTo(p.parse("x & (y & z | y & ~z | ~y & z)"));
        assertThat(cOrD1.transform(this.distributiveSimplifier)).isEqualTo(p.parse("x & (~y & z | y)"));

        assertThat(F.f.equivalence(cOr, cAnd).transform(this.distributiveSimplifier)).isEqualTo(F.f.equivalence(cOrD1, cAndD1));
        assertThat(F.f.implication(cOr, cAnd).transform(this.distributiveSimplifier)).isEqualTo(F.f.implication(cOrD1, cAndD1));
    }
}
