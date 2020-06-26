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

import org.junit.jupiter.api.Test;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;

import java.util.ArrayList;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link DefaultStringRepresentation}
 * @version 2.0.0
 * @since 1.0
 */
public class DefaultStringRepresentationTest {
    private final FormulaStringRepresentation sr = new DefaultStringRepresentation();

    @Test
    public void testDefaultPrinter() {
        assertThat(F.f.string(F.FALSE, this.sr)).isEqualTo("$false");
        assertThat(F.f.string(F.TRUE, this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.X, this.sr)).isEqualTo("x");
        assertThat(F.f.string(F.NA, this.sr)).isEqualTo("~a");
        assertThat(F.f.string(F.f.not(F.AND1), this.sr)).isEqualTo("~(a & b)");
        assertThat(F.f.string(F.f.variable("x1"), this.sr)).isEqualTo("x1");
        assertThat(F.f.string(F.f.variable("x190"), this.sr)).isEqualTo("x190");
        assertThat(F.f.string(F.f.variable("x234"), this.sr)).isEqualTo("x234");
        assertThat(F.f.string(F.f.variable("x567"), this.sr)).isEqualTo("x567");
        assertThat(F.f.string(F.f.variable("abc8"), this.sr)).isEqualTo("abc8");
        assertThat(F.f.string(F.IMP2, this.sr)).isEqualTo("~a => ~b");
        assertThat(F.f.string(F.IMP3, this.sr)).isEqualTo("a & b => x | y");
        assertThat(F.f.string(F.EQ4, this.sr)).isEqualTo("a => b <=> ~a => ~b");
        assertThat(F.f.string(F.AND3, this.sr)).isEqualTo("(x | y) & (~x | ~y)");
        assertThat(F.f.string(F.f.and(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("a & b & c & x");
        assertThat(F.f.string(F.f.or(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("a | b | c | x");
        assertThat(F.f.string(F.PBC1, this.sr)).isEqualTo("2*a + -4*b + 3*x = 2");
        assertThat(F.f.string(F.PBC2, this.sr)).isEqualTo("2*a + -4*b + 3*x > 2");
        assertThat(F.f.string(F.PBC3, this.sr)).isEqualTo("2*a + -4*b + 3*x >= 2");
        assertThat(F.f.string(F.PBC4, this.sr)).isEqualTo("2*a + -4*b + 3*x < 2");
        assertThat(F.f.string(F.PBC5, this.sr)).isEqualTo("2*a + -4*b + 3*x <= 2");
        assertThat(F.f.string(F.f.pbc(CType.LT, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.f.pbc(CType.EQ, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$false");
        assertThat(F.f.string(F.f.implication(F.A, F.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(F.f.string(F.f.equivalence(F.A, F.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(F.f.string(F.f.and(F.A, F.f.exo()), this.sr)).isEqualTo("$false");
        assertThat(F.f.string(F.f.or(F.A, F.f.exo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.implication(F.A, F.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.f.equivalence(F.A, F.f.amo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.and(F.A, F.f.amo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.or(F.A, F.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.f.or(F.A, F.f.amo(), F.f.exo(), F.f.equivalence(F.f.amo(), F.B)), this.sr)).isEqualTo("$true");
    }

    @Test
    public void testToString() {
        assertThat(this.sr.toString()).isEqualTo("DefaultStringRepresentation");
    }

}
