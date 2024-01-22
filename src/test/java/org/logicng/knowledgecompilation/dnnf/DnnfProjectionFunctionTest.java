package org.logicng.knowledgecompilation.dnnf;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfProjectionFunction;
import org.logicng.util.CollectionHelper;

import java.util.Arrays;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

public class DnnfProjectionFunctionTest {

    @Test
    public void test() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.parse("((A1 | ~A2 & A3) & (~B1 & B2 | ~B3 | B4) | (~A1 | A2 | A4 & ~A5) & B1) " +
                "& ((C1 & C2 | ~C1 & C3 | C4) & (D1 | D2 | D3) | (~C1 | ~C2) & (D1 | D2 & D3))");
        final Dnnf dnnf = new Dnnf(formula.variables(), formula);

        final Dnnf projectAll = dnnf.execute(new DnnfProjectionFunction(formula.variables()));
        assertThat(projectAll).isEqualTo(dnnf);

        final Dnnf projectNone = dnnf.execute(new DnnfProjectionFunction(Collections.emptyList()));
        assertThat(projectNone).isEqualTo(new Dnnf(Collections.emptySortedSet(), f.verum()));

        final SortedSet<Variable> elimA1Vars = CollectionHelper.difference(formula.variables(), Collections.singletonList(f.variable("A1")), TreeSet::new);
        final Dnnf elimA1 = dnnf.execute(new DnnfProjectionFunction(elimA1Vars));
        assertThat(elimA1).isEqualTo(new Dnnf(elimA1Vars, f.parse("(~B1 & B2 | ~B3 | B4 | B1) " +
                "& ((C1 & C2 | ~C1 & C3 | C4) & (D1 | D2 | D3) | (~C1 | ~C2) & (D1 | D2 & D3))")));

        final SortedSet<Variable> elimA2C1Vars = CollectionHelper.difference(formula.variables(), Arrays.asList(f.variable("A2"), f.variable("C1")), TreeSet::new);
        final Dnnf elimA2C1 = dnnf.execute(new DnnfProjectionFunction(elimA2C1Vars));
        assertThat(elimA2C1).isEqualTo(new Dnnf(elimA2C1Vars, f.parse("((A1 | A3) & (~B1 & B2 | ~B3 | B4) | B1) " +
                "& ((C2 | C3 | C4) & (D1 | D2 | D3) | (D1 | D2 & D3))")));

        final SortedSet<Variable> elimA3B3C4D2Vars = CollectionHelper.difference(formula.variables(),
                Arrays.asList(f.variable("A3"), f.variable("B3"), f.variable("C4"), f.variable("D2")), TreeSet::new);
        final Dnnf elimA3B3C4D2 = dnnf.execute(new DnnfProjectionFunction(elimA3B3C4D2Vars));
        assertThat(elimA3B3C4D2).isEqualTo(new Dnnf(elimA3B3C4D2Vars, f.parse("((A1 | ~A2) | (~A1 | A2 | A4 & ~A5) & B1)")));
    }
}
