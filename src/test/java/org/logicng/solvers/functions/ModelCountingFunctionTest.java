package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Units tests for {@link ModelCountingFunction}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class ModelCountingFunctionTest extends TestWithExampleFormulas {

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
    public void testTautology(final SplitVariableProvider splitProvider) {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder().variables().configuration(config).build());
        assertThat(numberOfModels).isEqualTo(1);
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
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder().variables().configuration(config).build());
        assertThat(numberOfModels).isEqualTo(1);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple1(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder().configuration(config).build());
        assertThat(numberOfModels).isEqualTo(3);
    }

    @ParameterizedTest
    @MethodSource("splitProviders")
    public void testSimple2(final SplitVariableProvider splitProvider) throws ParserException {
        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(splitProvider == null ? null : DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(splitProvider).maxNumberOfModels(2).build())
                        .build();
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(~A | C) & (~B | C)"));
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder().configuration(config).build());
        assertThat(numberOfModels).isEqualTo(5);
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
        final ModelCountingFunction meFunction = ModelCountingFunction.builder().configuration(config).build();
        final BigInteger firstRun = solver.execute(meFunction);
        final BigInteger secondRun = solver.execute(meFunction);
        assertThat(firstRun).isEqualTo(5);
        assertThat(secondRun).isEqualTo(5);
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
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(numberOfModels).isEqualTo(10);
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
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(numberOfModels).isEqualTo(12);
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
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder()
                .variables(variables)
                .configuration(config)
                .build());
        assertThat(numberOfModels).isEqualTo(6);
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
        final BigInteger numberOfModels = solver.execute(ModelCountingFunction.builder().configuration(config).build());
        assertThat(handler.aborted()).isTrue();
        assertThat(numberOfModels).isEqualTo(3);
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
            final BigInteger count1 =
                    solver.execute(ModelCountingFunction.builder().configuration(configLcv).build());

            // recursive call: most common vars
            final AdvancedModelEnumerationConfig configMcv =
                    AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new MostCommonVariablesProvider()).maxNumberOfModels(500).build()).build();
            final BigInteger count2 =
                    solver.execute(ModelCountingFunction.builder().configuration(configMcv).build());

            assertThat(count1).isEqualTo(modelsNoSplit.size());
            assertThat(count2).isEqualTo(modelsNoSplit.size());
        }
    }

    @Test
    public void testCollector() {
        final MiniSat solver = MiniSat.miniSat(this.f);
        solver.add(this.EQ1);
        solver.sat();

        final EnumerationCollectorTestHandler handler = new EnumerationCollectorTestHandler();
        final ModelCountingFunction.ModelCountCollector collector = new ModelCountingFunction.ModelCountCollector(0);
        assertThat(collector.getResult().intValue()).isZero();
        assertThat(handler.getFoundModels()).isZero();
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        final LNGBooleanVector modelFromSolver1 = new LNGBooleanVector(true, true);
        final LNGBooleanVector modelFromSolver2 = new LNGBooleanVector(false, false);

        //final Model expectedModel1 = new Model(this.A, this.B);
        final Model expectedModel2 = new Model(this.NA, this.NB);

        collector.addModel(modelFromSolver1, solver, null, handler);
        assertThat(collector.getResult().intValue()).isZero();
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isZero();
        assertThat(handler.getRollbackCalls()).isZero();

        collector.commit(handler);
        assertThat(collector.getResult().intValue()).isEqualTo(1);
        assertThat(handler.getFoundModels()).isEqualTo(1);
        assertThat(handler.getCommitCalls()).isEqualTo(1);
        assertThat(handler.getRollbackCalls()).isZero();
        final BigInteger result1 = collector.getResult();

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
        assertThat(collector.getResult().intValue()).isEqualTo(2);
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(2);
        final BigInteger result2 = collector.getResult();

        collector.rollback(handler);
        assertThat(collector.getResult()).isEqualTo(result2);
        assertThat(collector.rollbackAndReturnModels(solver, handler)).isEmpty();
        assertThat(handler.getFoundModels()).isEqualTo(4);
        assertThat(handler.getCommitCalls()).isEqualTo(2);
        assertThat(handler.getRollbackCalls()).isEqualTo(4);
    }
}
