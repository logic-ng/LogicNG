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
