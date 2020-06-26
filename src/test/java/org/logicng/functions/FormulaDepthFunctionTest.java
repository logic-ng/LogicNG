package org.logicng.functions;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.formulas.cache.FunctionCacheEntry;
import org.logicng.io.parsers.ParserException;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit Tests for the class {@link FormulaDepthFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class FormulaDepthFunctionTest {

    @Test
    public void testAtoms() {
        assertThat(F.TRUE.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.FALSE.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.A.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.NA.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.PBC1.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.PBC2.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.PBC3.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.PBC4.apply(new FormulaDepthFunction())).isEqualTo(0);
        assertThat(F.PBC5.apply(new FormulaDepthFunction())).isEqualTo(0);
    }

    @Test
    public void testDeepFormulas() {
        assertThat(F.AND1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.AND2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.AND3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.OR1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.OR2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.OR3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.NOT1.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.NOT2.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.IMP1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.IMP2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.IMP3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.IMP4.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.EQ1.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.EQ2.apply(new FormulaDepthFunction())).isEqualTo(1);
        assertThat(F.EQ3.apply(new FormulaDepthFunction())).isEqualTo(2);
        assertThat(F.EQ4.apply(new FormulaDepthFunction())).isEqualTo(2);
    }

    @Test
    public void testDeeperFormulas() {
        Formula formula = F.PBC1;
        for (int i = 0; i < 10; i++) {
            final Variable var = F.f.variable("X" + i);
            formula = i % 2 == 0 ? F.f.or(formula, var) : F.f.and(formula, var);
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
