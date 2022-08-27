package org.logicng.solvers.functions.splitvariablesprovider;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class SplitVariableProviderTest extends TestWithExampleFormulas {

    @Test
    public void testFixedVariableProvider() {
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, null)).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, new TreeSet<>())).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(null, varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
        assertThat(new FixedVariableProvider(varSet("A B C D E F")).getSplitVars(MiniSat.miniSat(this.f), varSet("A B X U"))).containsExactlyElementsOf(varSet("A B C D E F"));
    }

    @Test
    public void testLeastCommonVariablesProvider() throws ParserException {
        final SortedSet<Variable> varSet = varSet("a b c d e f g h i j");
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(a | b | c) & (~b | c) & (d | ~e) & (~a | e) & (a | d | b | g | h) & (~h | i) & (f | g | j) & (f | b | j | ~g) & (g | c)"));
        assertThat(new LeastCommonVariablesProvider(.1, 100).getSplitVars(solver, null)).containsExactly(this.f.variable("i"));
        assertThat(new LeastCommonVariablesProvider(.1, 100).getSplitVars(solver, varSet)).containsExactly(this.f.variable("i"));
        assertThat(new LeastCommonVariablesProvider(.0001, 100).getSplitVars(solver, null)).containsExactly(this.f.variable("i"));
        assertThat(new LeastCommonVariablesProvider(.2, 100).getSplitVars(solver, null))
                .hasSize(2).contains(this.f.variable("i")).containsAnyElementsOf(varSet("e d f h j"));
        assertThat(new LeastCommonVariablesProvider(.6, 100).getSplitVars(solver, null)).containsExactlyElementsOf(varSet("e d f i h j"));
        assertThat(new LeastCommonVariablesProvider(.6, 1).getSplitVars(solver, null)).containsExactlyElementsOf(varSet("i"));
        assertThat(new LeastCommonVariablesProvider(.6, 2).getSplitVars(solver, null))
                .hasSize(2).contains(this.f.variable("i")).containsAnyElementsOf(varSet("e d f h j"));
        assertThat(new LeastCommonVariablesProvider(.25, 100).getSplitVars(solver, varSet("a b g"))).containsExactly(this.A);
        assertThat(new LeastCommonVariablesProvider(.5, 100).getSplitVars(solver, varSet("a c b g"))).containsExactlyElementsOf(varSet("a c"));
        assertThat(new LeastCommonVariablesProvider().getSplitVars(solver, varSet("a c b g"))).containsExactlyElementsOf(varSet("a c"));
        assertThat(new LeastCommonVariablesProvider(1, 100).getSplitVars(solver, varSet("a c b g"))).containsExactlyElementsOf(varSet("a c b g"));
        assertThat(new LeastCommonVariablesProvider(1, 100).getSplitVars(solver, null)).containsExactlyElementsOf(varSet);
    }

    @Test
    public void testMostCommonVariablesProvider() throws ParserException {
        final SortedSet<Variable> varSet = varSet("a b c d e f g h i j");
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(a | b | c) & (~b | c) & (d | ~e) & (~a | e) & (a | d | b | g | h) & (~h | i) & (f | g | j) & (f | b | j | ~g) & (g | c)"));
        assertThat(new MostCommonVariablesProvider(.2, 100).getSplitVars(solver, null)).containsExactlyElementsOf(varSet("b g"));
        assertThat(new MostCommonVariablesProvider(.2, 100).getSplitVars(solver, varSet)).containsExactlyElementsOf(varSet("b g"));
        assertThat(new MostCommonVariablesProvider(.0001, 100).getSplitVars(solver, null)).hasSize(1).containsAnyElementsOf(varSet("b g"));
        assertThat(new MostCommonVariablesProvider(.4, 100).getSplitVars(solver, null)).containsExactlyElementsOf(varSet("b g a c"));
        assertThat(new MostCommonVariablesProvider(.9, 100).getSplitVars(solver, null)).containsAll(varSet("a b c d e f g h j"));
        assertThat(new MostCommonVariablesProvider(.9, 2).getSplitVars(solver, null)).containsExactlyElementsOf(varSet("b g"));
        assertThat(new MostCommonVariablesProvider(.25, 100).getSplitVars(solver, varSet("f i c"))).containsExactly(this.C);
        assertThat(new MostCommonVariablesProvider(.5, 100).getSplitVars(solver, varSet("c b f h"))).containsExactlyElementsOf(varSet("b c"));
        assertThat(new MostCommonVariablesProvider().getSplitVars(solver, varSet("c b f h"))).containsExactlyElementsOf(varSet("b c"));
        assertThat(new MostCommonVariablesProvider(1, 100).getSplitVars(solver, varSet("a c b g"))).containsExactlyElementsOf(varSet("a c b g"));
        assertThat(new MostCommonVariablesProvider(1, 100).getSplitVars(solver, null)).containsExactlyElementsOf(varSet);
    }

    private SortedSet<Variable> varSet(final String varString) {
        return Arrays.stream(varString.split(" ")).map(this.f::variable).collect(Collectors.toCollection(TreeSet::new));
    }
}
