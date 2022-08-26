package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.LongRunningTag;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.splitvariablesprovider.LeastCommonVariablesProvider;
import org.logicng.solvers.functions.splitvariablesprovider.MostCommonVariablesProvider;
import org.logicng.solvers.functions.splitvariablesprovider.SplitVariableProvider;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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
        providers.add(new Object[]{null});
        providers.add(new Object[]{new LeastCommonVariablesProvider()});
        providers.add(new Object[]{new MostCommonVariablesProvider()});
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
        assertThat(toSets(models)).containsExactlyInAnyOrder(
                set(this.f.variable("A"), this.f.variable("B"), this.f.variable("C")),
                set(this.f.variable("A"), this.f.variable("B"), this.f.literal("C", false)),
                set(this.f.variable("A"), this.f.literal("B", false), this.f.variable("C"))
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
        assertThat(toSets(firstRun)).containsExactlyInAnyOrderElementsOf(toSets(secondRun));
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testModelEnumerationWithAdditionalVariables(final SplitVariableProvider splitProvider) throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A | B | C | D | E"));
        final Variable a = this.f.variable("A");
        final Variable b = this.f.variable("B");
        final Variable c = this.f.variable("C");
        final List<Model> models = solver.execute(ModelEnumerationFunctionRecursive.builder()
                .splitVariableProvider(splitProvider)
                .variables(Arrays.asList(a, b))
                .additionalVariables(Arrays.asList(b, c)).build());
        for (final Model model : models) {
            final List<Variable> variables = model.getLiterals().stream().map(Literal::variable).collect(Collectors.toList());
            assertThat(variables).contains(b, c);
        }
    }

    @Test
    @LongRunningTag
    public void testRecursives() {
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(15).build());
            final Formula formula = randomizer.formula(3);

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // no split
            final List<Assignment> modelsNoSplit = solver.execute(ModelEnumerationFunction.builder().build());

            // recursive call: least common vars
            final List<Model> models1 =
                    solver.execute(ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new LeastCommonVariablesProvider(0.5)).build());

            // recursive call: most common vars
            final List<Model> models2 =
                    solver.execute(ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new MostCommonVariablesProvider(0.5)).build());

            assertThat(models1.size()).isEqualTo(modelsNoSplit.size());
            assertThat(models2.size()).isEqualTo(modelsNoSplit.size());

            final List<Set<Literal>> setNoSplit = getSetForAssignments(modelsNoSplit);
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(toSets(models1));
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(toSets(models2));
        }
    }

    // @Test
    // public void testAdditionalVariables() {
    //    final SATSolver solver = MiniSat.miniSat(this.f);
    //
    //    for (int i = 1; i <= 1000; i++) {
    //        // given
    //        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
    //        final Formula formula = randomizer.formula(10);
    //        solver.add(formula);
    //
    //        final List<Variable> varsFormula = new ArrayList<>(formula.variables());
    //        final int numberOfVars = formula.variables().size();
    //        final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 3) + 2;
    //        final SortedSet<Variable> pmeVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));
    //
    //        final int additionalVarsStart = 2 * minNumberOfVars;
    //        final SortedSet<Variable> additionalVars = new TreeSet<>(varsFormula.subList(additionalVarsStart, varsFormula.size()));
    //
    //        // when
    //        final long t1 = System.currentTimeMillis();
    //        final List<Assignment> models1 = solver.execute(ModelEnumerationFunction.builder().variables(pmeVars).additionalVariables(additionalVars).build
    //        ());
    //        final long t1a = System.currentTimeMillis();
    //
    //        final long timeNoSplit = t1a - t1;
    //        System.out.println("\nSeed: " + i);
    //        if (models1.size() < 10) {
    //            continue;
    //        }
    //
    //        System.out.println("Number of combinations: " + models1.size());
    //        System.out.println("Time no split: " + timeNoSplit);
    //
    //        final long t2 = System.currentTimeMillis();
    //        final List<Assignment> models2 =
    //                solver.execute(
    //                        ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f, 3, 50, 70)).variables(pmeVars)
    //                                .additionalVariables(additionalVars).build());
    //
    //        final long t3 = System.currentTimeMillis();
    //        final long timeSplit = t3 - t2;
    //
    //        System.out.println("Time split: " + timeSplit);
    //
    //        final List<Assignment> updatedModels1 = restrictAssignmentsToPmeVars(pmeVars, models1);
    //        final List<Assignment> updatedModels2 = restrictAssignmentsToPmeVars(pmeVars, models2);
    //
    //        assertThat(models1.size()).isEqualTo(models2.size());
    //        assertThat(updatedModels1).containsExactlyInAnyOrderElementsOf(updatedModels2);
    //    }
    // }

    private List<Set<Literal>> toSets(final List<Model> models) {
        return models.stream().map(x -> new HashSet<>(x.getLiterals())).collect(Collectors.toList());
    }

    private List<Set<Literal>> getSetForAssignments(final List<Assignment> models) {
        return models.stream().map(x -> new HashSet<>(x.literals())).collect(Collectors.toList());
    }

    private Set<Literal> set(final Literal... literals) {
        return new HashSet<>(Arrays.asList(literals));
    }

}
