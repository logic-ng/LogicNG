package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.datastructures.Model;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.splitVariableProvider.LeastCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.MostCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.RandomSplitVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.SplitVariableProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Units tests for {@link ModelEnumerationFunction}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class ModelEnumerationFunctionRecursiveTest {

    private final FormulaFactory f;

    public ModelEnumerationFunctionRecursiveTest() {
        this.f = new FormulaFactory();
    }

    public static Collection<Object[]> splitProviders() {
        final List<Object[]> providers = new ArrayList<>();
        providers.add(new Object[]{new LeastCommonVariableProvider(.5)});
        providers.add(new Object[]{new MostCommonVariableProvider(.5)});
        providers.add(new Object[]{new RandomSplitVariableProvider(42, .5)});
        return providers;
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple1(final SplitVariableProvider splitProvider) throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Model> models = solver.execute(ModelEnumerationFunctionRecursive.builder()
                .splitVariableProvider(splitProvider)
                .build());
        assertThat(models).containsExactlyInAnyOrder(
                new Model(this.f.variable("A"), this.f.variable("B"), this.f.variable("C")),
                new Model(this.f.variable("A"), this.f.variable("B"), this.f.literal("C", false)),
                new Model(this.f.variable("A"), this.f.literal("B", false), this.f.variable("C"))
        );
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple2(final SplitVariableProvider splitProvider) throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(ModelEnumerationFunctionRecursive.builder()
                .splitVariableProvider(splitProvider)
                .build());
        assertThat(models).hasSize(5);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testMultipleModelEnumeration(final SplitVariableProvider splitProvider) throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final ModelEnumerationFunctionRecursive meFunction = ModelEnumerationFunctionRecursive.builder()
                .splitVariableProvider(splitProvider)
                .build();
        final List<Model> firstRun = solver.execute(meFunction);
        final List<Model> secondRun = solver.execute(meFunction);
        assertThat(firstRun).hasSize(5);
        assertThat(firstRun).containsExactlyInAnyOrderElementsOf(secondRun);
    }
}
