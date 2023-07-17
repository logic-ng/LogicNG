// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas.printer;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.Variable;

/**
 * Unit tests for {@link LatexStringRepresentation}
 * @version 1.1
 * @since 1.0
 */
public class LatexStringRepresentationTest extends TestWithExampleFormulas {
    private final FormulaStringRepresentation sr = new LatexStringRepresentation();

    @Test
    public void testLatexPrinter() {
        assertThat(this.f.string(this.FALSE, this.sr)).isEqualTo("\\bottom");
        assertThat(this.f.string(this.TRUE, this.sr)).isEqualTo("\\top");
        assertThat(this.f.string(this.X, this.sr)).isEqualTo("x");
        assertThat(this.f.string(this.NA, this.sr)).isEqualTo("\\lnot a");
        assertThat(this.f.string(this.f.variable("x1"), this.sr)).isEqualTo("x_{1}");
        assertThat(this.f.string(this.f.variable("x190"), this.sr)).isEqualTo("x_{190}");
        assertThat(this.f.string(this.f.variable("x234"), this.sr)).isEqualTo("x_{234}");
        assertThat(this.f.string(this.f.variable("x567"), this.sr)).isEqualTo("x_{567}");
        assertThat(this.f.string(this.f.variable("abc8"), this.sr)).isEqualTo("abc_{8}");
        assertThat(this.f.string(this.IMP2, this.sr)).isEqualTo("\\lnot a \\rightarrow \\lnot b");
        assertThat(this.f.string(this.IMP3, this.sr)).isEqualTo("a \\land b \\rightarrow x \\lor y");
        assertThat(this.f.string(this.EQ4, this.sr))
                .isEqualTo("a \\rightarrow b \\leftrightarrow \\lnot a \\rightarrow \\lnot b");
        assertThat(this.f.string(this.AND3, this.sr))
                .isEqualTo("\\left(x \\lor y\\right) \\land \\left(\\lnot x \\lor \\lnot y\\right)");
        assertThat(this.f.string(this.f.and(this.A, this.B, this.C, this.X), this.sr))
                .isEqualTo("a \\land b \\land c \\land x");
        assertThat(this.f.string(this.f.or(this.A, this.B, this.C, this.X), this.sr))
                .isEqualTo("a \\lor b \\lor c \\lor x");
        assertThat(this.f.string(this.PBC1, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x = 2");
        assertThat(this.f.string(this.PBC2, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x > 2");
        assertThat(this.f.string(this.PBC3, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x \\geq 2");
        assertThat(this.f.string(this.PBC4, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x < 2");
        assertThat(this.f.string(this.PBC5, this.sr)).isEqualTo("2\\cdot a + -4\\cdot b + 3\\cdot x \\leq 2");
        assertThat(this.f.string(this.f.implication(this.A, this.f.exo()), this.sr)).isEqualTo("\\lnot a");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.exo()), this.sr)).isEqualTo("\\lnot a");
        assertThat(this.f.string(this.f.and(this.A, this.f.exo()), this.sr)).isEqualTo("\\bottom");
        assertThat(this.f.string(this.f.or(this.A, this.f.exo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.implication(this.A, this.f.amo()), this.sr)).isEqualTo("\\top");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.and(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo()), this.sr)).isEqualTo("\\top");
        assertThat(this.f.string(
                this.f.or(this.A, this.f.amo(), this.f.exo(), this.f.equivalence(this.f.amo(), this.B)), this.sr))
                        .isEqualTo("\\top");

    }

    @Test
    public void testSpecialCases() {
        final Variable var = this.f.variable("\ntest9t");
        assertThat(this.f.string(var, this.sr)).isEqualTo("\ntest9t");
        assertThat(this.sr.toString()).isEqualTo("LatexStringRepresentation");
    }

    @Test
    public void testViaFormulaFactoryConfig() {
        final FormulaFactory f =
                new FormulaFactory(FormulaFactoryConfig.builder().stringRepresentation(() -> this.sr).build());
        assertThat(f.importFormula(this.EQ4).toString())
                .isEqualTo("a \\rightarrow b \\leftrightarrow \\lnot a \\rightarrow \\lnot b");
    }
}
