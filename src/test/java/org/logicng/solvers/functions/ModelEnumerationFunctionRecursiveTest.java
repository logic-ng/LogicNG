package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.solvers.functions.AdvancedModelEnumerationFunction.ModelEnumerationCollector.getCartesianProduct;
import static org.logicng.testutils.TestUtil.getDontCareVariables;
import static org.logicng.testutils.TestUtil.modelCount;
import static org.logicng.util.CollectionHelper.union;
import static org.logicng.util.FormulaHelper.strings2literals;
import static org.logicng.util.FormulaHelper.strings2vars;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.RandomTag;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.datastructures.Tristate;
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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
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
    public void testEmptyEnumerationVariables(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("A & (B | C)");
        solver.add(formula);
        List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().variables().configuration(config).build());
        assertThat(models).containsExactly(new Model());
        models = solver.execute(AdvancedModelEnumerationFunction.builder().variables().additionalVariables(formula.variables()).configuration(config).build());
        assertThat(models).hasSize(1);
        assertThat(variables(models.get(0))).containsAll(formula.variables());
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(modelsToSets(models)).containsExactlyInAnyOrder(
                set(this.f.variable("A"), this.f.variable("B"), this.f.variable("C")),
                set(this.f.variable("A"), this.f.variable("B"), this.f.literal("C", false)),
                set(this.f.variable("A"), this.f.literal("B", false), this.f.variable("C"))
        );
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple2(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(models).hasSize(5);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testMultipleModelEnumeration(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final AdvancedModelEnumerationFunction meFunction = AdvancedModelEnumerationFunction.builder().configuration(config).build();
        final List<Model> firstRun = solver.execute(meFunction);
        final List<Model> secondRun = solver.execute(meFunction);
        assertThat(firstRun).hasSize(5);
        assertThat(modelsToSets(firstRun)).containsExactlyInAnyOrderElementsOf(modelsToSets(secondRun));
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testAdditionalVariablesSimple(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).maxNumberOfModels(3).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & C | B & ~C"));
        final Variable a = this.f.variable("A");
        final Variable b = this.f.variable("B");
        final Variable c = this.f.variable("C");
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(Arrays.asList(a, b))
                .additionalVariables(Collections.singletonList(c))
                .configuration(config)
                .build());
        assertThat(models).hasSize(3); // (A, B), (A, ~B), (~A, B)
        for (final Model model : models) {
            assertThat(variables(model)).containsExactly(a, b, c);
        }
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDontCareVariables1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(strings2vars(Arrays.asList("A", "B", "C", "D"), this.f))
                .configuration(config)
                .build());
        assertThat(modelsToSets(models)).containsExactlyInAnyOrder(
                // models with ~D
                strings2literals(Arrays.asList("A", "~B", "C", "~D"), "~", this.f),
                strings2literals(Arrays.asList("A", "B", "C", "~D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~B", "~C", "~D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~B", "C", "~D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "B", "C", "~D"), "~", this.f),
                // models with D
                strings2literals(Arrays.asList("A", "~B", "C", "D"), "~", this.f),
                strings2literals(Arrays.asList("A", "B", "C", "D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~B", "~C", "D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~B", "C", "D"), "~", this.f),
                strings2literals(Arrays.asList("~A", "B", "C", "D"), "~", this.f)
        );
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDontCareVariables2(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder().splitVariableProvider(splitProvider).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(strings2vars(Arrays.asList("A", "C", "D", "E"), this.f))
                .configuration(config)
                .build());
        assertThat(modelsToSets(models)).containsExactlyInAnyOrder(
                // models with ~D, ~E
                strings2literals(Arrays.asList("A", "C", "~D", "~E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "C", "~D", "~E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~C", "~D", "~E"), "~", this.f),
                // models with ~D, E
                strings2literals(Arrays.asList("A", "C", "~D", "E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "C", "~D", "E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~C", "~D", "E"), "~", this.f),
                // models with D, ~E
                strings2literals(Arrays.asList("A", "C", "D", "~E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "C", "D", "~E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~C", "D", "~E"), "~", this.f),
                // models with D, E
                strings2literals(Arrays.asList("A", "C", "D", "E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "C", "D", "E"), "~", this.f),
                strings2literals(Arrays.asList("~A", "~C", "D", "E"), "~", this.f)
        );
    }

    @Test
    @RandomTag
    public void testRecursives() {
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(15).build());
            final Formula formula = randomizer.formula(3);

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // no split
            final List<Assignment> modelsNoSplit = solver.execute(ModelEnumerationFunction.builder().build());

            // recursive call: least common vars
            final AdvancedModelEnumerationConfig configLcv =
                    AdvancedModelEnumerationConfig.builder().splitVariableProvider(new LeastCommonVariablesProvider()).build();
            final List<Model> models1 = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(configLcv).build());

            // recursive call: most common vars
            final AdvancedModelEnumerationConfig configMcv =
                    AdvancedModelEnumerationConfig.builder().splitVariableProvider(new MostCommonVariablesProvider()).build();
            final List<Model> models2 = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(configMcv).build());

            assertThat(models1.size()).isEqualTo(modelsNoSplit.size());
            assertThat(models2.size()).isEqualTo(modelsNoSplit.size());

            final List<Set<Literal>> setNoSplit = assignmentsToSets(modelsNoSplit);
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(modelsToSets(models1));
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(modelsToSets(models2));
        }
    }

    @Test
    @RandomTag
    public void testRecursivesCounting() {
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(15).build());
            final Formula formula = randomizer.formula(3);

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // no split
            final List<Assignment> modelsNoSplit = solver.execute(ModelEnumerationFunction.builder().build());

            // recursive call: least common vars
            final AdvancedModelEnumerationConfig configLcv =
                    AdvancedModelEnumerationConfig.builder().splitVariableProvider(new LeastCommonVariablesProvider()).build();
            final BigInteger count1 =
                    solver.execute(ModelCountingFunction.builder().configuration(configLcv).build());

            // recursive call: most common vars
            final AdvancedModelEnumerationConfig configMcv =
                    AdvancedModelEnumerationConfig.builder().splitVariableProvider(new MostCommonVariablesProvider()).build();
            final BigInteger count2 =
                    solver.execute(ModelCountingFunction.builder().configuration(configMcv).build());

            assertThat(count1).isEqualTo(modelsNoSplit.size());
            assertThat(count2).isEqualTo(modelsNoSplit.size());
        }
    }

    @Test
    public void testAdditionalVariables() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder()
                .splitVariableProvider(new LeastCommonVariablesProvider())
                .maxNumberOfModels(10)
                .build();

        for (int i = 1; i <= 1000; i++) {
            // given
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(20).build());
            final Formula formula = randomizer.formula(4);
            solver.add(formula);

            final List<Variable> varsFormula = new ArrayList<>(formula.variables());
            final int numberOfVars = formula.variables().size();
            final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 5);
            final SortedSet<Variable> pmeVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));

            final int additionalVarsStart = Math.min(4 * minNumberOfVars, numberOfVars);
            final SortedSet<Variable> additionalVars = new TreeSet<>(varsFormula.subList(additionalVarsStart, varsFormula.size()));

            // when
            final List<Model> modelsRecursive = solver.execute(AdvancedModelEnumerationFunction.builder()
                    .variables(pmeVars)
                    .additionalVariables(additionalVars)
                    .configuration(config).build());

            final List<Assignment> modelsOld =
                    solver.execute(ModelEnumerationFunction.builder().variables(pmeVars).additionalVariables(additionalVars).build());

            final List<Assignment> updatedModels1 = restrictModelsToPmeVars(pmeVars, modelsRecursive);
            final List<Assignment> updatedModels2 = extendByDontCares(restrictAssignmentsToPmeVars(pmeVars, modelsOld), pmeVars);

            assertThat(BigInteger.valueOf(modelsRecursive.size())).isEqualTo(modelCount(modelsOld, pmeVars));
            assertThat(assignmentsToSets(updatedModels1)).containsExactlyInAnyOrderElementsOf(assignmentsToSets(updatedModels2));

            // check that models are buildable and every model contains all additional variables
            for (final Model model : modelsRecursive) {
                assertThat(variables(model)).containsAll(additionalVars);
                solver.add(model.getLiterals());
                assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
                solver.reset();
            }
            solver.reset();
        }
    }

    private static List<Assignment> extendByDontCares(final List<Assignment> assignments, final SortedSet<Variable> variables) {
        final SortedSet<Variable> dontCareVars = getDontCareVariables(assignments, variables);
        final List<List<Literal>> cartesianProduct = getCartesianProduct(dontCareVars);
        final List<Assignment> extendedAssignments = new ArrayList<>();
        for (final Assignment assignment : assignments) {
            final SortedSet<Literal> assignmentLiterals = assignment.literals();
            for (final List<Literal> literals : cartesianProduct) {
                extendedAssignments.add(new Assignment(union(assignmentLiterals, literals, TreeSet::new)));
            }
        }
        return extendedAssignments;
    }

    private List<Assignment> restrictAssignmentsToPmeVars(final SortedSet<Variable> pmeVars, final List<Assignment> models) {
        final List<Assignment> updatedModels = new ArrayList<>();
        for (final Assignment assignment : models) {
            final Assignment updatedAssignment = new Assignment();
            for (final Literal literal : assignment.literals()) {
                if (pmeVars.contains(literal.variable())) {
                    updatedAssignment.addLiteral(literal);
                }
            }
            updatedModels.add(updatedAssignment);
        }
        return updatedModels;
    }

    private List<Assignment> restrictModelsToPmeVars(final SortedSet<Variable> pmeVars, final List<Model> models) {
        final List<Assignment> updatedModels = new ArrayList<>();
        for (final Model assignment : models) {
            final Assignment updatedAssignment = new Assignment();
            for (final Literal literal : assignment.getLiterals()) {
                if (pmeVars.contains(literal.variable())) {
                    updatedAssignment.addLiteral(literal);
                }
            }
            updatedModels.add(updatedAssignment);
        }
        return updatedModels;
    }

    private static List<Set<Literal>> modelsToSets(final List<Model> models) {
        return models.stream().map(x -> new HashSet<>(x.getLiterals())).collect(Collectors.toList());
    }

    private static List<Set<Literal>> assignmentsToSets(final List<Assignment> models) {
        return models.stream().map(x -> new HashSet<>(x.literals())).collect(Collectors.toList());
    }

    private static Set<Literal> set(final Literal... literals) {
        return new HashSet<>(Arrays.asList(literals));
    }

    private static List<Variable> variables(final Model model) {
        return model.getLiterals().stream().map(Literal::variable).collect(Collectors.toList());
    }
}
