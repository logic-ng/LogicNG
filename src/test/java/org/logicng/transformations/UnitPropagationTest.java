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
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link UnitPropagation}.
 * @version 2.0.0
 * @since 1.2
 */
public class UnitPropagationTest extends TestWithExampleFormulas {

    private final UnitPropagation unitPropagation = new UnitPropagation();

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
