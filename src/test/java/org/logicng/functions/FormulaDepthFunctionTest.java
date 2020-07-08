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
        assertThat(this.TRUE.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.FALSE.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.A.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.NA.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.PBC1.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.PBC2.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.PBC3.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.PBC4.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(this.PBC5.apply(new FormulaDepthFunction())).isEqualTo(0);
    }

    @Test
    public void testDeepFormulas() {
        assertThat(this.AND1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.AND2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.AND3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.OR1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.OR2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.OR3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.NOT1.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.NOT2.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.IMP1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.IMP2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.IMP3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.IMP4.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.EQ1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.EQ2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(this.EQ3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(this.EQ4.apply(new FormulaDepthFunction())).isEqualTo(2);
    }

    @Test
    public void testDeeperFormulas() {
        Formula formula = this.PBC1;
        for (int i = 0; i < 10; i++) {
            final Variable var = this.f.variable("X" + i);
            formula = i % 2 == 0 ? this.f.or(formula, var) : this.f.and(formula, var);
        }
        assertThat(formula.apply(new FormulaDepthFunction())).isEqualTo(10);
    }

    @Test
    public void testCache() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.parse("A & B | C");
        assertThat(formula.functionCacheEntry(FunctionCacheEntry.DEPTH)).isNull();
        assertThat(formula.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(formula.functionCacheEntry(FunctionCacheEntry.DEPTH)).isEqualTo(2);
        assertThat(f.variable("A").functionCacheEntry(FunctionCacheEntry.DEPTH)).isEqualTo(0);

        formula.setFunctionCacheEntry(FunctionCacheEntry.DEPTH, 3);
        assertThat(formula.apply(new FormulaDepthFunction())).isEqualTo(3);
    }
}
