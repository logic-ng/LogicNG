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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link UnitPropagation}.
 * @version 2.0.0
 * @since 1.2
 */
public class UnitPropagationTest {

    private final UnitPropagation unitPropagation = new UnitPropagation();

    @Test
    public void testConstants() {
        assertThat(F.TRUE.transform(this.unitPropagation)).isEqualTo(F.TRUE);
        assertThat(F.FALSE.transform(this.unitPropagation)).isEqualTo(F.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.transform(this.unitPropagation)).isEqualTo(F.A);
        assertThat(F.NA.transform(this.unitPropagation)).isEqualTo(F.NA);
    }

    @Test
    public void testNoPropagation() {
        assertThat(F.AND1.transform(this.unitPropagation)).isEqualTo(F.AND1);
        assertThat(F.AND2.transform(this.unitPropagation)).isEqualTo(F.AND2);
        assertThat(F.OR1.transform(this.unitPropagation)).isEqualTo(F.OR1);
        assertThat(F.OR2.transform(this.unitPropagation)).isEqualTo(F.OR2);
    }

    @Test
    public void testPropagations() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(F.f.and(F.AND1, F.A).transform(this.unitPropagation)).isEqualTo(F.AND1);
        assertThat(F.f.and(F.AND2, F.A).transform(this.unitPropagation)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.OR1, F.X).transform(this.unitPropagation)).isEqualTo(F.X);
        assertThat(F.f.or(F.AND1, F.A).transform(this.unitPropagation)).isEqualTo(F.A);
        assertThat(F.f.or(F.OR1, F.X).transform(this.unitPropagation)).isEqualTo(F.OR1);
        assertThat(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(this.unitPropagation)).isEqualTo(p.parse("(e | g) & (e | ~g | h) & f & c & d & ~a & b"));
    }
}
