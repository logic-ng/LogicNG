package org.logicng.solvers.functions;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptySortedSet;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.solvers.functions.AdvancedModelEnumerationFunction.ModelEnumerationCollector.getCartesianProduct;
import static org.logicng.testutils.TestUtil.getDontCareVariables;
import static org.logicng.testutils.TestUtil.modelCount;
import static org.logicng.util.CollectionHelper.union;
import static org.logicng.util.FormulaHelper.strings2literals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.*;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.DefaultAdvancedModelEnumerationStrategy;
import org.logicng.solvers.functions.modelenumeration.EnumerationCollectorTestHandler;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.FixedVariableProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.LeastCommonVariablesProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.MostCommonVariablesProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.SplitVariableProvider;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Units tests for {@link AdvancedModelEnumerationFunction}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class AdvancedModelEnumerationFunctionTest extends TestWithExampleFormulas {

    private FormulaFactory f;

    public static Collection<Object[]> splitProviders() {
        final List<Object[]> providers = new ArrayList<>();
        providers.add(new Object[]{null});
        providers.add(new Object[]{new LeastCommonVariablesProvider()});
        providers.add(new Object[]{new MostCommonVariablesProvider()});
        return providers;
    }

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testContradiction(final SplitVariableProvider splitProvider) {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.literal("A", true));
        solver.add(this.f.literal("A", false));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().variables().configuration(config).build());
        assertThat(models).isEmpty();
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testTautology(final SplitVariableProvider splitProvider) {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().variables().configuration(config).build());
        assertThat(models).containsExactly(new Model());
        final List<Variable> additionalVars = this.f.variables("A", "B");
        models = solver.execute(AdvancedModelEnumerationFunction.builder().variables().additionalVariables(additionalVars).configuration(config).build());
        assertThat(models).hasSize(1);
        assertThat(variables(models.get(0))).containsAll(additionalVars);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testEmptyEnumerationVariables(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
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
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
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
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(models).hasSize(5);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testResultLiteralOrderIndependentFromInputOrder(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        final List<Model> modelsABC = solver.execute(AdvancedModelEnumerationFunction.builder().variables(this.f.variables("A", "B", "C")).configuration(config).build());
        final List<Model> modelsBCA = solver.execute(AdvancedModelEnumerationFunction.builder().variables(this.f.variables("B", "C", "A")).configuration(config).build());

        assertThat(modelsToSets(models)).containsExactlyInAnyOrder(
                set(this.f.variable("A"), this.f.variable("B"), this.f.variable("C")),
                set(this.f.variable("A"), this.f.variable("B"), this.f.literal("C", false)),
                set(this.f.variable("A"), this.f.literal("B", false), this.f.variable("C"))
        );
        assertThat(models).containsExactlyInAnyOrderElementsOf(modelsABC);
        assertThat(modelsABC).containsExactlyInAnyOrderElementsOf(modelsBCA);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDuplicateEnumerationVariables(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(this.f.variables("A", "A", "B"))
                .configuration(config).build());
        assertThat(modelsToSets(models)).containsExactlyInAnyOrder(
                set(this.f.variable("A"), this.f.variable("B")),
                set(this.f.variable("A"), this.f.literal("B", false))
        );
        assertThat(models).extracting(Model::size).allMatch(size -> size == 2);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testMultipleModelEnumeration(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
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
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
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
    public void testDuplicateAdditionalVariables(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(this.f.variables("A"))
                .additionalVariables(this.f.variables("B", "B"))
                .configuration(config).build());
        assertThat(models).hasSize(1);
        assertThat(models).extracting(Model::size).allMatch(size -> size == 2);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDontCareVariables1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(this.f.variables("A", "B", "C", "D"))
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
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(this.f.variables("A", "C", "D", "E"))
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
    public void testDontCareVariables3() throws ParserException {
        final FixedVariableProvider splitProvider = new FixedVariableProvider(new TreeSet<>(this.f.variables("X")));
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(3).build()).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("A | B | (X & ~X)"); // X will be simplified out and become a don't care variable unknown by the solver
        solver.add(formula);
        final SortedSet<Variable> enumerationVars = new TreeSet<>(this.f.variables("A", "B", "X"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                .variables(enumerationVars)
                .configuration(config)
                .build());
        assertThat(models).hasSize(6);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testHandlerWithNumModelsLimit(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedNumberOfModelsHandler handler = new AdvancedNumberOfModelsHandler(3);
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().handler(handler)
                        .strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(3).build()).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(handler.aborted()).isTrue();
        assertThat(models).hasSize(3);
    }

    @Test
    public void testAdditionalVariables() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final AdvancedModelEnumerationConfig config = AdvancedModelEnumerationConfig.builder()
                .strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new LeastCommonVariablesProvider()).maxNumberOfModels(10).build())
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

    @Test
    @RandomTag
    public void testRandomFormulas() {
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(15).build());
            final Formula formula = randomizer.formula(3);

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // no split
            final List<Assignment> modelsNoSplit = solver.execute(ModelEnumerationFunction.builder().build());

            // recursive call: least common vars
            final AdvancedModelEnumerationConfig configLcv =
                    AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new LeastCommonVariablesProvider()).maxNumberOfModels(500).build()).build();
            final List<Model> models1 = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(configLcv).build());

            // recursive call: most common vars
            final AdvancedModelEnumerationConfig configMcv =
                    AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new MostCommonVariablesProvider()).maxNumberOfModels(500).build()).build();
            final List<Model> models2 = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(configMcv).build());

            assertThat(models1.size()).isEqualTo(modelsNoSplit.size());
            assertThat(models2.size()).isEqualTo(modelsNoSplit.size());

            final List<Set<Literal>> setNoSplit = assignmentsToSets(modelsNoSplit);
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(modelsToSets(models1));
            assertThat(setNoSplit).containsExactlyInAnyOrderElementsOf(modelsToSets(models2));
        }
    }

    @Test
    public void testCollector() {
        final MiniSat solver = MiniSat.miniSat(this.f);
        solver.add(this.EQ1);
        solver.sat();

        final EnumerationCollectorTestHandler handler = new EnumerationCollectorTestHandler();
        final AdvancedModelEnumerationFunction.ModelEnumerationCollector collector = new AdvancedModelEnumerationFunction.ModelEnumerationCollector(emptySortedSet(), emptySortedSet());
        assertThat(collector.getResult()).isEmpty();
        assertThat(handler.getFoundModels()).isZero();
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        final LNGBooleanVector modelFromSolver1 = new LNGBooleanVector(true, true);
        final LNGBooleanVector modelFromSolver2 = new LNGBooleanVector(false, false);

        final Model expectedModel1 = new Model(this.A, this.B);
        final Model expectedModel2 = new Model(this.NA, this.NB);

        collector.addModel(modelFromSolver1, solver, null, handler);
        assertThat(collector.getResult()).isEmpty();
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        collector.commit(handler);
        assertThat(collector.getResult()).containsExactly(expectedModel1);
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isZero();
        final List<Model> result1 = collector.getResult();

        collector.addModel(modelFromSolver2, solver, null, handler);
        assertThat(collector.getResult()).isEqualTo(result1);
        assertThat(handler.getFoundModels()).isEqualTo(2);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isZero();

        collector.rollback(handler);
        assertThat(collector.getResult()).isEqualTo(result1);
        assertThat(handler.getFoundModels()).isEqualTo(2);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isEqualTo(1);

        collector.addModel(modelFromSolver2, solver, null, handler);
        final List<Model> rollbackModels = collector.rollbackAndReturnModels(solver, handler);
        assertThat(rollbackModels).containsExactly(expectedModel2);
        assertThat(collector.getResult()).isEqualTo(result1);
        assertThat(handler.getFoundModels()).isEqualTo(3);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isEqualTo(2);

        collector.addModel(modelFromSolver2, solver, null, handler);
        collector.commit(handler);
        assertThat(collector.getResult()).containsExactlyInAnyOrder(expectedModel1, expectedModel2);
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(2);
        final List<Model> result2 = collector.getResult();

        collector.rollback(handler);
        assertThat(collector.getResult()).isEqualTo(result2);
        assertThat(collector.rollbackAndReturnModels(solver, handler)).isEmpty();
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(4);
    }

    @Test
    public void testGetCartesianProduct() {
        assertThat(getCartesianProduct(emptySortedSet())).containsExactly(emptyList());
        assertThat(getCartesianProduct(new TreeSet<>(singletonList(this.A)))).containsExactly(
                singletonList(this.A),
                singletonList(this.NA));
        assertThat(getCartesianProduct(new TreeSet<>(asList(this.A, this.B, this.C)))).containsExactly(
                asList(this.A, this.B, this.C),
                asList(this.A, this.B, this.NC),
                asList(this.A, this.NB, this.C),
                asList(this.A, this.NB, this.NC),
                asList(this.NA, this.B, this.C),
                asList(this.NA, this.B, this.NC),
                asList(this.NA, this.NB, this.C),
                asList(this.NA, this.NB, this.NC)
        );
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
