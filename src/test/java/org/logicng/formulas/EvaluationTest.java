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
 * Unit tests for formula evaluation.
 * @version 2.0.0
 * @since 1.0
 */
public class EvaluationTest extends TestWithExampleFormulas {

    private final Assignment ass = new Assignment(Arrays.asList(this.A, this.B, this.C, this.NX, this.NY));

    @Test
    public void testConstantEval() {
        assertThat(this.TRUE.evaluate(this.ass)).isTrue();
        assertThat(this.FALSE.evaluate(this.ass)).isFalse();
    }

    @Test
    public void testLiteralEval() {
        assertThat(this.A.evaluate(this.ass)).isTrue();
        assertThat(this.NA.evaluate(this.ass)).isFalse();
        assertThat(this.X.evaluate(this.ass)).isFalse();
        assertThat(this.NX.evaluate(this.ass)).isTrue();
    }

    @Test
    public void testNotEval() {
        assertThat(this.NOT1.evaluate(this.ass)).isFalse();
        assertThat(this.NOT2.evaluate(this.ass)).isTrue();
    }

    @Test
    public void testBinaryEval() {
        assertThat(this.IMP1.evaluate(this.ass)).isTrue();
        assertThat(this.IMP2.evaluate(this.ass)).isTrue();
        assertThat(this.IMP3.evaluate(this.ass)).isFalse();
        assertThat(this.IMP4.evaluate(this.ass)).isTrue();

        assertThat(this.EQ1.evaluate(this.ass)).isTrue();
        assertThat(this.EQ2.evaluate(this.ass)).isTrue();
        assertThat(this.EQ3.evaluate(this.ass)).isFalse();
        assertThat(this.EQ4.evaluate(this.ass)).isTrue();
    }

    @Test
    public void testNAryEval() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.OR1.evaluate(this.ass)).isFalse();
        assertThat(this.OR2.evaluate(this.ass)).isTrue();
        assertThat(this.OR3.evaluate(this.ass)).isTrue();
        assertThat(p.parse("~a | ~b | ~c | x | y").evaluate(this.ass)).isFalse();
        assertThat(p.parse("~a | ~b | ~c | x | ~y").evaluate(this.ass)).isTrue();

        assertThat(this.AND1.evaluate(this.ass)).isTrue();
        assertThat(this.AND2.evaluate(this.ass)).isFalse();
        assertThat(this.AND3.evaluate(this.ass)).isFalse();
        assertThat(p.parse("a & b & c & ~x & ~y").evaluate(this.ass)).isTrue();
        assertThat(p.parse("a & b & c & ~x & y").evaluate(this.ass)).isFalse();
    }
}
