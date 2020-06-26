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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for formula restriction.
 * @version 2.0.0
 * @since 1.0
 */
public class RestrictionTest {

    private final Assignment ass = new Assignment(Arrays.asList(F.A, F.NB, F.NX));

    @Test
    public void testConstantRestrict() {
        assertThat(F.TRUE.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.FALSE.restrict(this.ass)).isEqualTo(F.FALSE);
    }

    @Test
    public void testLiteralRestrict() {
        assertThat(F.A.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.NA.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.X.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.NX.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.C.restrict(this.ass)).isEqualTo(F.C);
        assertThat(F.NY.restrict(this.ass)).isEqualTo(F.NY);
    }

    @Test
    public void testNotRestrict() {
        assertThat(F.NOT1.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.NOT2.restrict(this.ass)).isEqualTo(F.NY);
    }

    @Test
    public void testBinaryRestrict() {
        assertThat(F.IMP1.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.IMP2.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.f.implication(F.NA, F.C).restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.IMP3.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.f.implication(F.A, F.C).restrict(this.ass)).isEqualTo(F.C);

        assertThat(F.EQ1.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.EQ2.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.EQ3.restrict(this.ass)).isEqualTo(F.NY);
        assertThat(F.EQ4.restrict(this.ass)).isEqualTo(F.FALSE);
    }

    @Test
    public void testNAryRestrict() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.OR1.restrict(this.ass)).isEqualTo(F.Y);
        assertThat(F.OR2.restrict(this.ass)).isEqualTo(F.TRUE);
        assertThat(F.OR3.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(p.parse("~a | b | ~c | x | y").restrict(this.ass)).isEqualTo(p.parse("~c | y"));
        assertThat(p.parse("~a | b | ~c | ~x | ~y").restrict(this.ass)).isEqualTo(F.TRUE);

        assertThat(F.AND1.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.AND2.restrict(this.ass)).isEqualTo(F.FALSE);
        assertThat(F.AND3.restrict(this.ass)).isEqualTo(F.Y);
        assertThat(p.parse("a & ~b & c & ~x & ~y").restrict(this.ass)).isEqualTo(p.parse("c & ~y"));
        assertThat(p.parse("a & b & c & ~x & y").restrict(this.ass)).isEqualTo(F.FALSE);
    }
}
