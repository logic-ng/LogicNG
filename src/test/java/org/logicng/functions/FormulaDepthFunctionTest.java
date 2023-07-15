// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.io.parsers.ParserException;

/**
 * Unit Tests for the class {@link FormulaDepthFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class FormulaDepthFunctionTest extends TestWithExampleFormulas {

    @Test
    public void testAtoms() {
        assertThat(this.TRUE.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.FALSE.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.A.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.NA.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.PBC1.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.PBC2.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.PBC3.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.PBC4.apply(FormulaDepthFunction.get())).isEqualTo(0);
        assertThat(this.PBC5.apply(FormulaDepthFunction.get())).isEqualTo(0);
    }

    @Test
    public void testDeepFormulas() {
        assertThat(this.AND1.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.AND2.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.AND3.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.OR1.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.OR2.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.OR3.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.NOT1.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.NOT2.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.IMP1.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.IMP2.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.IMP3.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.IMP4.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.EQ1.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.EQ2.apply(FormulaDepthFunction.get())).isEqualTo(1);
        assertThat(this.EQ3.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(this.EQ4.apply(FormulaDepthFunction.get())).isEqualTo(2);
    }

    @Test
    public void testDeeperFormulas() {
        Formula formula = this.PBC1;
        for (int i = 0; i < 10; i++) {
            final Variable var = this.f.variable("X" + i);
            formula = i % 2 == 0 ? this.f.or(formula, var) : this.f.and(formula, var);
        }
        assertThat(formula.apply(FormulaDepthFunction.get())).isEqualTo(10);
    }

    @Test
    public void testCache() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.parse("A & B | C");
        assertThat(formula.functionCacheEntry(FunctionCacheEntry.DEPTH)).isNull();
        assertThat(formula.apply(FormulaDepthFunction.get())).isEqualTo(2);
        assertThat(formula.functionCacheEntry(FunctionCacheEntry.DEPTH)).isEqualTo(2);
        assertThat(f.variable("A").functionCacheEntry(FunctionCacheEntry.DEPTH)).isEqualTo(0);

        formula.setFunctionCacheEntry(FunctionCacheEntry.DEPTH, 3);
        assertThat(formula.apply(FormulaDepthFunction.get())).isEqualTo(3);
    }
}
