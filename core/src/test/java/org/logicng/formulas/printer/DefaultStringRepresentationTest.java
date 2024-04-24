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

package org.logicng.formulas.printer;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.CType;

import java.util.ArrayList;

/**
 * Unit tests for {@link DefaultStringRepresentation}
 * @version 2.0.0
 * @since 1.0
 */
public class DefaultStringRepresentationTest extends TestWithExampleFormulas {
    private final FormulaStringRepresentation sr = new DefaultStringRepresentation();

    @Test
    public void testDefaultPrinter() {
        assertThat(this.f.string(this.FALSE, this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.TRUE, this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.X, this.sr)).isEqualTo("x");
        assertThat(this.f.string(this.NA, this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.f.not(this.AND1), this.sr)).isEqualTo("~(a & b)");
        assertThat(this.f.string(this.f.variable("x1"), this.sr)).isEqualTo("x1");
        assertThat(this.f.string(this.f.variable("x190"), this.sr)).isEqualTo("x190");
        assertThat(this.f.string(this.f.variable("x234"), this.sr)).isEqualTo("x234");
        assertThat(this.f.string(this.f.variable("x567"), this.sr)).isEqualTo("x567");
        assertThat(this.f.string(this.f.variable("abc8"), this.sr)).isEqualTo("abc8");
        assertThat(this.f.string(this.IMP2, this.sr)).isEqualTo("~a => ~b");
        assertThat(this.f.string(this.IMP3, this.sr)).isEqualTo("a & b => x | y");
        assertThat(this.f.string(this.EQ4, this.sr)).isEqualTo("a => b <=> ~a => ~b");
        assertThat(this.f.string(this.AND3, this.sr)).isEqualTo("(x | y) & (~x | ~y)");
        assertThat(this.f.string(this.f.and(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("a & b & c & x");
        assertThat(this.f.string(this.f.or(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("a | b | c | x");
        assertThat(this.f.string(this.PBC1, this.sr)).isEqualTo("2*a + -4*b + 3*x = 2");
        assertThat(this.f.string(this.PBC2, this.sr)).isEqualTo("2*a + -4*b + 3*x > 2");
        assertThat(this.f.string(this.PBC3, this.sr)).isEqualTo("2*a + -4*b + 3*x >= 2");
        assertThat(this.f.string(this.PBC4, this.sr)).isEqualTo("2*a + -4*b + 3*x < 2");
        assertThat(this.f.string(this.PBC5, this.sr)).isEqualTo("2*a + -4*b + 3*x <= 2");
        assertThat(this.f.string(this.f.pbc(CType.LT, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.pbc(CType.EQ, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.f.implication(this.A, this.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.f.and(this.A, this.f.exo()), this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.f.or(this.A, this.f.exo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.implication(this.A, this.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.and(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo(), this.f.exo(), this.f.equivalence(this.f.amo(), this.B)), this.sr)).isEqualTo("$true");
    }

    @Test
    public void testToString() {
        assertThat(this.sr.toString()).isEqualTo("DefaultStringRepresentation");
    }

    @Test
    public void testFFDefaultStringRepresentation() {
        assertThat(this.EQ4.toString()).isEqualTo("a => b <=> ~a => ~b");
    }
}
