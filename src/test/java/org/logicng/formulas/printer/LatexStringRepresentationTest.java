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
import org.logicng.formulas.F;
import org.logicng.formulas.Variable;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link LatexStringRepresentation}
 * @version 1.1
 * @since 1.0
 */
public class LatexStringRepresentationTest {
    private final FormulaStringRepresentation sr = new LatexStringRepresentation();

    @Test
    public void testLatexPrinter() {
        assertThat(F.f.string(F.FALSE, this.sr)).isEqualTo("\\bottom");
        assertThat(F.f.string(F.TRUE, this.sr)).isEqualTo("\\top");
        assertThat(F.f.string(F.X, this.sr)).isEqualTo("x");
        assertThat(F.f.string(F.NA, this.sr)).isEqualTo("\\lnot a");
        assertThat(F.f.string(F.f.variable("x1"), this.sr)).isEqualTo("x_{1}");
        assertThat(F.f.string(F.f.variable("x190"), this.sr)).isEqualTo("x_{190}");
        assertThat(F.f.string(F.f.variable("x234"), this.sr)).isEqualTo("x_{234}");
        assertThat(F.f.string(F.f.variable("x567"), this.sr)).isEqualTo("x_{567}");
        assertThat(F.f.string(F.f.variable("abc8"), this.sr)).isEqualTo("abc_{8}");
        assertThat(F.f.string(F.IMP2, this.sr)).isEqualTo("\\lnot a \\rightarrow \\lnot b");
        assertThat(F.f.string(F.IMP3, this.sr)).isEqualTo("a \\land b \\rightarrow x \\lor y");
        assertThat(F.f.string(F.EQ4, this.sr)).isEqualTo("a \\rightarrow b \\leftrightarrow \\lnot a \\rightarrow \\lnot b");
        assertThat(F.f.string(F.AND3, this.sr)).isEqualTo("\\left(x \\lor y\\right) \\land \\left(\\lnot x \\lor \\lnot y\\right)");
        assertThat(F.f.string(F.f.and(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("a \\land b \\land c \\land x");
        assertThat(F.f.string(F.f.or(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("a \\lor b \\lor c \\lor x");
        assertThat(F.f.string(F.PBC1, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x = 2");
        assertThat(F.f.string(F.PBC2, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x > 2");
        assertThat(F.f.string(F.PBC3, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x \\geq 2");
        assertThat(F.f.string(F.PBC4, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x < 2");
        assertThat(F.f.string(F.PBC5, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x \\leq 2");
        assertThat(F.f.string(F.f.implication(F.A, F.f.exo()), this.sr)).isEqualTo("\\lnot a");
        assertThat(F.f.string(F.f.equivalence(F.A, F.f.exo()), this.sr)).isEqualTo("\\lnot a");
        assertThat(F.f.string(F.f.and(F.A, F.f.exo()), this.sr)).isEqualTo("\\bottom");
        assertThat(F.f.string(F.f.or(F.A, F.f.exo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.implication(F.A, F.f.amo()), this.sr)).isEqualTo("\\top");
        assertThat(F.f.string(F.f.equivalence(F.A, F.f.amo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.and(F.A, F.f.amo()), this.sr)).isEqualTo("a");
        assertThat(F.f.string(F.f.or(F.A, F.f.amo()), this.sr)).isEqualTo("\\top");
        assertThat(F.f.string(F.f.or(F.A, F.f.amo(), F.f.exo(), F.f.equivalence(F.f.amo(), F.B)), this.sr)).isEqualTo("\\top");

    }

    @Test
    public void testSpecialCases() {
        final Variable var = F.f.variable("\ntest9t");
        assertThat(F.f.string(var, this.sr)).isEqualTo("\ntest9t");
        assertThat(this.sr.toString()).isEqualTo("LatexStringRepresentation");
    }
}
