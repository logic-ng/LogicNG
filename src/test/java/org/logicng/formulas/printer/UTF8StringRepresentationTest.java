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
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.Variable;

/**
 * Unit tests for {@link UTF8StringRepresentation}
 * @version 2.0.0
 * @since 1.0
 */
public class UTF8StringRepresentationTest extends TestWithExampleFormulas {
    private final FormulaStringRepresentation sr = new UTF8StringRepresentation();

    @Test
    public void testUTF8Printer() {
        assertThat(this.f.string(this.FALSE, this.sr)).isEqualTo("⊥");
        assertThat(this.f.string(this.TRUE, this.sr)).isEqualTo("⊤");
        assertThat(this.f.string(this.X, this.sr)).isEqualTo("x");
        assertThat(this.f.string(this.NA, this.sr)).isEqualTo("¬a");
        assertThat(this.f.string(this.f.variable("x1"), this.sr)).isEqualTo("x₁");
        assertThat(this.f.string(this.f.variable("x190"), this.sr)).isEqualTo("x₁₉₀");
        assertThat(this.f.string(this.f.variable("x234"), this.sr)).isEqualTo("x₂₃₄");
        assertThat(this.f.string(this.f.variable("x567"), this.sr)).isEqualTo("x₅₆₇");
        assertThat(this.f.string(this.f.variable("abc8"), this.sr)).isEqualTo("abc₈");
        assertThat(this.f.string(this.IMP2, this.sr)).isEqualTo("¬a ⇒ ¬b");
        assertThat(this.f.string(this.IMP3, this.sr)).isEqualTo("a ∧ b ⇒ x ∨ y");
        assertThat(this.f.string(this.EQ4, this.sr)).isEqualTo("a ⇒ b ⇔ ¬a ⇒ ¬b");
        assertThat(this.f.string(this.AND3, this.sr)).isEqualTo("(x ∨ y) ∧ (¬x ∨ ¬y)");
        assertThat(this.f.string(this.f.and(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("a ∧ b ∧ c ∧ x");
        assertThat(this.f.string(this.f.or(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("a ∨ b ∨ c ∨ x");
        assertThat(this.f.string(this.PBC1, this.sr)).isEqualTo("2a + -4b + 3x = 2");
        assertThat(this.f.string(this.PBC2, this.sr)).isEqualTo("2a + -4b + 3x > 2");
        assertThat(this.f.string(this.PBC3, this.sr)).isEqualTo("2a + -4b + 3x ≥ 2");
        assertThat(this.f.string(this.PBC4, this.sr)).isEqualTo("2a + -4b + 3x < 2");
        assertThat(this.f.string(this.PBC5, this.sr)).isEqualTo("2a + -4b + 3x ≤ 2");
        assertThat(this.f.string(this.f.implication(this.A, this.f.exo()), this.sr)).isEqualTo("¬a");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.exo()), this.sr)).isEqualTo("¬a");
        assertThat(this.f.string(this.f.and(this.A, this.f.exo()), this.sr)).isEqualTo("⊥");
        assertThat(this.f.string(this.f.or(this.A, this.f.exo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.implication(this.A, this.f.amo()), this.sr)).isEqualTo("⊤");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.and(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo()), this.sr)).isEqualTo("⊤");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo(), this.f.exo(), this.f.equivalence(this.f.amo(), this.B)), this.sr)).isEqualTo("⊤");
    }

    @Test
    public void testSpecialCases() {
        final Variable var = this.f.variable("\ntest9t");
        assertThat(this.f.string(var, this.sr)).isEqualTo("\ntest9t");
        assertThat(this.sr.toString()).isEqualTo("UTF8StringRepresentation");
    }

    @Test
    public void testViaFormulaFactoryConfig() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().stringRepresentation(() -> this.sr).build());
        assertThat(f.importFormula(this.EQ4).toString()).isEqualTo("a ⇒ b ⇔ ¬a ⇒ ¬b");
    }
}
