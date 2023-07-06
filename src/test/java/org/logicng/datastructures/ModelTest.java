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

package org.logicng.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;

/**
 * Unit tests for the class {@link Model}.
 * @version 2.5.0
 * @since 2.5.0
 */
public class ModelTest extends TestWithExampleFormulas {

    @Test
    public void testCreators() {
        assertThat(new Model(Collections.emptyList()).getLiterals()).isEmpty();
        assertThat(new Model(Arrays.asList(this.A, this.NB, this.X)).getLiterals())
                .containsExactly(this.A, this.NB, this.X);
        assertThat(new Model().getLiterals()).isEmpty();
        assertThat(new Model(this.A, this.NB, this.X).getLiterals())
                .containsExactly(this.A, this.NB, this.X);
    }

    @Test
    public void testSize() {
        assertThat(new Model(Collections.emptyList()).size()).isEqualTo(0);
        assertThat(new Model(Collections.singletonList(this.A)).size()).isEqualTo(1);
        assertThat(new Model(Arrays.asList(this.A, this.NB, this.X)).size()).isEqualTo(3);
    }

    @Test
    public void testAssignment() {
        assertThat(new Model(Collections.emptyList()).assignment(false))
                .isEqualTo(new Assignment(false));
        assertThat(new Model(Arrays.asList(this.A, this.NB, this.X)).assignment(false))
                .isEqualTo(new Assignment(Arrays.asList(this.A, this.NB, this.X), false));
        assertThat(new Model(Collections.emptyList()).assignment(true))
                .isEqualTo(new Assignment(true));
        assertThat(new Model(Arrays.asList(this.A, this.NB, this.X)).assignment(true))
                .isEqualTo(new Assignment(Arrays.asList(this.A, this.NB, this.X), true));
    }

    @Test
    public void testFormula() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(new Model(Collections.singletonList(this.A)).formula(this.f)).isEqualTo(p.parse("a"));
        assertThat(new Model(Collections.singletonList(this.NA)).formula(this.f)).isEqualTo(p.parse("~a"));
        assertThat(new Model(Arrays.asList(this.A, this.B)).formula(this.f)).isEqualTo(p.parse("a & b"));
        assertThat(new Model(Arrays.asList(this.A, this.B, this.NX, this.NY)).formula(this.f)).isEqualTo(p.parse("a & b & ~x & ~y"));
    }

    @Test
    public void testHashCode() {
        final Model model = new Model(this.A, this.B, this.NX, this.NY);
        assertThat(model).hasSameHashCodeAs(model);
        assertThat(model).hasSameHashCodeAs(new Model(Arrays.asList(this.A, this.B, this.NX, this.NY)));
        assertThat(model).hasSameHashCodeAs(new Model(this.A, this.B, this.NX, this.NY));
    }

    @Test
    public void testEquals() {
        final Model model = new Model(this.A, this.B, this.NX, this.NY);
        assertThat(model).isNotEqualTo(null);
        assertThat(model.equals(null)).isFalse();
        assertThat(new Model(Arrays.asList(this.A, this.B, this.NX, this.NY))).isEqualTo(model);
        assertThat(new Model(this.A, this.B, this.NX, this.NY)).isEqualTo(model);
        assertThat(model).isEqualTo(model);
        assertThat(model.equals(model)).isTrue();
        assertThat(new Model(Arrays.asList(this.A, this.B, this.NX))).isNotEqualTo(model);
        assertThat(new Model(Arrays.asList(this.A, this.B, this.NX, this.NY, this.C))).isNotEqualTo(model);
        assertThat(new Model(Arrays.asList(this.B, this.A, this.NX, this.NY))).isNotEqualTo(model);
        assertThat(this.TRUE).isNotEqualTo(model);
    }

    @Test
    public void testToString() {
        assertThat(new Model().toString()).isEqualTo("Model{literals=[]}");
        assertThat(new Model(Collections.singletonList(this.A)).toString()).isEqualTo("Model{literals=[a]}");
        assertThat(new Model(Collections.singletonList(this.NA)).toString()).isEqualTo("Model{literals=[~a]}");
        assertThat(new Model(Arrays.asList(this.A, this.B, this.NX, this.NY, this.C)).toString()).isEqualTo("Model{literals=[a, b, ~x, ~y, c]}");
    }
}
