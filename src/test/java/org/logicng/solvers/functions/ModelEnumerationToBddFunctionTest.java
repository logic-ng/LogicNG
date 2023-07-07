package org.logicng.solvers.functions;

import static java.util.Collections.emptySortedSet;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.collections.LNGBooleanVector;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Model;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.AdvancedNumberOfModelsHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.DefaultAdvancedModelEnumerationStrategy;
import org.logicng.solvers.functions.modelenumeration.EnumerationCollectorTestHandler;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.FixedVariableProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.LeastCommonVariablesProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.MostCommonVariablesProvider;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.SplitVariableProvider;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Units tests for {@link ModelEnumerationToBddFunction}.
 * @version 2.5.0
 * @since 2.5.0
 */
public class ModelEnumerationToBddFunctionTest extends TestWithExampleFormulas {

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

    @Test
    public void testNonIncrementalSolver() throws ParserException {
        final MiniSat solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        solver.add(this.f.parse("A | B | C"));
        assertThatThrownBy(() -> solver.execute(ModelEnumerationToBddFunction.builder().build()))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Recursive model enumeration function can only be applied to solvers with load/save state capability.");
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
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().variables().configuration(config).build());
        assertThat(bdd.isContradiction()).isTrue();
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testTautology(final SplitVariableProvider splitProvider) {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().variables().configuration(config).build());
        assertThat(bdd.isTautology()).isTrue();
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
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().variables().configuration(config).build());
        assertThat(bdd.isTautology()).isTrue();
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("A & (B | C)");
        solver.add(formula);
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().configuration(config).build());
        compareModels(formula, formula.variables(), bdd);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple2(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("(~A | C) & (~B | C)");
        solver.add(formula);
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().configuration(config).build());
        assertThat(bdd.modelCount()).isEqualTo(5);
        compareModels(formula, formula.variables(), bdd);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testMultipleModelEnumeration(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("(~A | C) & (~B | C)");
        solver.add(formula);
        final ModelEnumerationToBddFunction meFunction = ModelEnumerationToBddFunction.builder().configuration(config).build();
        final BDD firstRun = solver.execute(meFunction);
        final BDD secondRun = solver.execute(meFunction);
        assertThat(firstRun.modelCount()).isEqualTo(5);
        assertThat(secondRun.modelCount()).isEqualTo(5);
        compareModels(formula, formula.variables(), firstRun);
        compareModels(formula, formula.variables(), secondRun);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDontCareVariables1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("(~A | C) & (~B | C)");
        final List<Variable> variables = this.f.variables("A", "B", "C", "D");
        solver.add(formula);
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(bdd.modelCount()).isEqualTo(10);
        compareModels(formula, variables, bdd);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testDontCareVariables2(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("(~A | C) & (~B | C)");
        final List<Variable> variables = this.f.variables("A", "C", "D", "E");
        solver.add(formula);
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(bdd.modelCount()).isEqualTo(12);
        compareModels(formula, variables, bdd);
    }

    @Test
    public void testDontCareVariables3() throws ParserException {
        final FixedVariableProvider splitProvider = new FixedVariableProvider(new TreeSet<>(this.f.variables("X")));
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(3).build()).build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Formula formula = this.f.parse("A | B | (X & ~X)"); // X will be simplified out and become a don't care variable unknown by the solver
        solver.add(formula);
        final SortedSet<Variable> variables = new TreeSet<>(this.f.variables("A", "B", "X"));
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(bdd.modelCount()).isEqualTo(6);
        compareModels(formula, variables, bdd);
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
        final BDD bdd = solver.execute(ModelEnumerationToBddFunction.builder().configuration(config).build());
        assertThat(handler.aborted()).isTrue();
        assertThat(bdd.modelCount()).isEqualTo(3);
    }

    @RandomTag
    @Test
    public void testRandomFormulas() {
        for (int i = 1; i <= 50; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(this.f, FormulaRandomizerConfig.builder().seed(i).numVars(15).build());
            final Formula formula = randomizer.formula(3);

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // recursive call: least common vars
            final AdvancedModelEnumerationConfig configLcv =
                    AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new LeastCommonVariablesProvider()).maxNumberOfModels(500).build()).build();
            final BDD bdd1 = solver.execute(ModelEnumerationToBddFunction.builder().variables(formula.variables()).configuration(configLcv).build());

            // recursive call: most common vars
            final AdvancedModelEnumerationConfig configMcv =
                    AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new MostCommonVariablesProvider()).maxNumberOfModels(500).build()).build();
            final BDD bdd2 = solver.execute(ModelEnumerationToBddFunction.builder().variables(formula.variables()).configuration(configMcv).build());

            compareModels(formula, formula.variables(), bdd1);
            compareModels(formula, formula.variables(), bdd2);
        }
    }

    @Test
    public void testCollector() {
        final MiniSat solver = MiniSat.miniSat(this.f);
        solver.add(this.EQ1);
        solver.sat();

        final EnumerationCollectorTestHandler handler = new EnumerationCollectorTestHandler();
        final ModelEnumerationToBddFunction.BddModelEnumerationCollector collector = new ModelEnumerationToBddFunction.BddModelEnumerationCollector(this.f, this.EQ1.variables(), emptySortedSet(), 0);
        assertThat(collector.getResult().modelCount()).isZero();
        assertThat(handler.getFoundModels()).isZero();
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        final LNGBooleanVector modelFromSolver1 = new LNGBooleanVector(true, true);
        final LNGBooleanVector modelFromSolver2 = new LNGBooleanVector(false, false);

        final Model expectedModel1 = new Model(this.A, this.B);
        final Model expectedModel2 = new Model(this.NA, this.NB);

        collector.addModel(modelFromSolver1, solver, null, handler);
        assertThat(collector.getResult().enumerateAllModels()).isEmpty();
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        collector.commit(handler);
        assertThat(collector.getResult().enumerateAllModels()).containsExactly(expectedModel1.assignment(false));
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isZero();
        final BDD result1 = collector.getResult();

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
        assertThat(collector.getResult().enumerateAllModels()).containsExactlyInAnyOrder(expectedModel1.assignment(false), expectedModel2.assignment(false));
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(2);
        final BDD result2 = collector.getResult();

        collector.rollback(handler);
        assertThat(collector.getResult()).isEqualTo(result2);
        assertThat(collector.rollbackAndReturnModels(solver, handler)).isEmpty();
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(4);
    }

    private void compareModels(final Formula formula, final Collection<Variable> variables, final BDD bdd) {
        final FormulaFactory factory = formula.factory();
        final SATSolver solver = MiniSat.miniSat(factory);
        solver.add(formula);
        final Variable taut = factory.variable("@TAUT");
        for (final Variable variable : variables) {
            solver.add(factory.or(taut.negate(), variable));
        }
        solver.add(taut.negate());
        final List<Assignment> formulaModels = solver.enumerateAllModels(variables);
        final List<Assignment> bddModels = bdd.enumerateAllModels(variables);
        assertThat(formulaModels).containsExactlyInAnyOrderElementsOf(bddModels);
    }
}
