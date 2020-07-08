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

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link DistributiveSimplifier}.
 * @version 2.0.0
 * @since 1.3
 */
public class DistributiveSimplifierTest extends TestWithExampleFormulas {

    private final DistributiveSimplifier distributiveSimplifier = new DistributiveSimplifier();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.distributiveSimplifier)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.distributiveSimplifier)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.distributiveSimplifier)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.distributiveSimplifier)).isEqualTo(this.NA);
    }

    @Test
    public void testNoPropagation() {
        assertThat(this.AND1.transform(this.distributiveSimplifier)).isEqualTo(this.AND1);
        assertThat(this.AND2.transform(this.distributiveSimplifier)).isEqualTo(this.AND2);
        assertThat(this.OR1.transform(this.distributiveSimplifier)).isEqualTo(this.OR1);
        assertThat(this.OR2.transform(this.distributiveSimplifier)).isEqualTo(this.OR2);
    }

    @Test
    public void testPropagations() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.f.and(this.AND1, this.A).transform(this.distributiveSimplifier)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.AND2, this.A).transform(this.distributiveSimplifier)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.OR1, this.X).transform(this.distributiveSimplifier)).isEqualTo(this.f.and(this.OR1, this.X));
        assertThat(this.f.and(this.OR2, this.X).transform(this.distributiveSimplifier)).isEqualTo(this.f.and(this.OR2, this.X));
        assertThat(this.f.or(this.AND1, this.A).transform(this.distributiveSimplifier)).isEqualTo(this.f.or(this.AND1, this.A));
        assertThat(this.f.or(this.AND2, this.A).transform(this.distributiveSimplifier)).isEqualTo(this.f.or(this.AND2, this.A));
        assertThat(this.f.or(this.OR1, this.X).transform(this.distributiveSimplifier)).isEqualTo(this.OR1);
        assertThat(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(this.distributiveSimplifier)).isEqualTo(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & f & c & (e | (~b | ~f | g) & (f | g | h) & (~f | ~g | h))"));
    }

    @Test
    public void testFormulaTypes() {
        assertThat(this.IMP1.transform(this.distributiveSimplifier)).isEqualTo(this.IMP1);
        assertThat(this.EQ1.transform(this.distributiveSimplifier)).isEqualTo(this.EQ1);
        assertThat(this.NOT1.transform(this.distributiveSimplifier)).isEqualTo(this.NOT1);
    }

    @Test
    public void testComplexExamples() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        final Formula cAnd = p.parse("(a | b | ~c) & (~a | ~d) & (~c | d | b) & (~c | ~b)");
        final Formula cAndD1 = cAnd.transform(this.distributiveSimplifier);
        assertThat(cAndD1).isEqualTo(p.parse("(~a | ~d) & (~c | (a | b) & (d | b) & ~b)"));
        assertThat(cAndD1.transform(this.distributiveSimplifier)).isEqualTo(p.parse("(~a | ~d) & (~c | ~b & (b | a & d))"));

        assertThat(this.f.not(cAnd).transform(this.distributiveSimplifier)).isEqualTo(this.f.not(cAndD1));

        final Formula cOr = p.parse("(x & y & z) | (x & y & ~z) | (x & ~y & z)");
        final Formula cOrD1 = cOr.transform(this.distributiveSimplifier);
        assertThat(cOrD1).isEqualTo(p.parse("x & (y & z | y & ~z | ~y & z)"));
        assertThat(cOrD1.transform(this.distributiveSimplifier)).isEqualTo(p.parse("x & (~y & z | y)"));

        assertThat(this.f.equivalence(cOr, cAnd).transform(this.distributiveSimplifier)).isEqualTo(this.f.equivalence(cOrD1, cAndD1));
        assertThat(this.f.implication(cOr, cAnd).transform(this.distributiveSimplifier)).isEqualTo(this.f.implication(cOrD1, cAndD1));
    }
}
