package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.backbones.Backbone;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrdering;
import org.logicng.modelcounting.ModelCounter;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.testutils.NQueensGenerator;
import org.logicng.transformations.cnf.CNFConfig;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaHelper;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Unit tests for {@link ModelCounterFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class ModelCounterFunctionTest extends TestWithExampleFormulas {

    private SortedSet<Variable> vars(final String... vars) {
        return Arrays.stream(vars).map(this.f::variable).collect(Collectors.toCollection(TreeSet::new));
    }

    @Test
    public void testWrongArgument() {
        final MiniSat miniSat = MiniSat.miniSat(f);
        final BigInteger modelcount = miniSat.execute(ModelCounterFunction.builder().build());

        assertThrows(IllegalArgumentException.class, () ->
                ModelCounter.count(Collections.singletonList(this.f.parse("a & b")), new TreeSet<>(Collections.singletonList(this.A))));
    }

    @Test
    public void testConstants() {
        assertThat(ModelCounter.count(Collections.singleton(this.f.falsum()), Collections.emptySortedSet()))
                .isEqualTo(BigInteger.valueOf(0));
        assertThat(ModelCounter.count(Collections.singleton(this.f.falsum()), vars("a", "b")))
                .isEqualTo(BigInteger.valueOf(0));

        assertThat(ModelCounter.count(Collections.singleton(this.f.verum()), Collections.emptySortedSet()))
                .isEqualTo(BigInteger.valueOf(1));
        assertThat(ModelCounter.count(Collections.singleton(this.f.verum()), vars("a", "b")))
                .isEqualTo(BigInteger.valueOf(4));
    }

    @Test
    public void testSimple() throws ParserException {
        final Formula formula01 = this.f.parse("(~v1 => ~v0) | ~v1 | v0");
        assertThat(ModelCounter.count(Collections.singletonList(formula01), formula01.variables())).isEqualTo(BigInteger.valueOf(4));

        final List<Formula> formulas02 = Arrays.asList(this.f.parse("(a & b) | ~b"), this.f.parse("a"));
        assertThat(ModelCounter.count(formulas02, FormulaHelper.variables(formulas02))).isEqualTo(BigInteger.valueOf(2));

        final List<Formula> formulas03 = Arrays.asList(this.f.parse("a & b & c"), this.f.parse("c & d"));
        assertThat(ModelCounter.count(formulas03, FormulaHelper.variables(formulas03))).isEqualTo(BigInteger.valueOf(1));
    }

    @Test
    public void testAmoAndExo() throws ParserException {
        final List<Formula> formulas01 = Arrays.asList(this.f.parse("a & b"), this.f.parse("a + b + c + d <= 1"));
        assertThat(ModelCounter.count(formulas01, FormulaHelper.variables(formulas01))).isEqualTo(BigInteger.valueOf(0));

        final List<Formula> formulas02 = Arrays.asList(this.f.parse("a & b & (a + b + c + d <= 1)"), this.f.parse("a | b"));
        assertThat(ModelCounter.count(formulas02, FormulaHelper.variables(formulas02))).isEqualTo(BigInteger.valueOf(0));

        final List<Formula> formulas03 = Arrays.asList(this.f.parse("a & (a + b + c + d <= 1)"), this.f.parse("a | b"));
        assertThat(ModelCounter.count(formulas03, FormulaHelper.variables(formulas03))).isEqualTo(BigInteger.valueOf(1));

        final List<Formula> formulas04 = Arrays.asList(this.f.parse("a & (a + b + c + d = 1)"), this.f.parse("a | b"));
        assertThat(ModelCounter.count(formulas04, FormulaHelper.variables(formulas04))).isEqualTo(BigInteger.valueOf(1));
    }

    @Test
    public void testNonAmoAndExo() throws ParserException {
        final List<Formula> formulas01 = Arrays.asList(this.f.parse("a & b"), this.f.parse("a + b + c + d = 2"));
        assertThatThrownBy(() -> ModelCounter.count(formulas01, FormulaHelper.variables(formulas01)))
                .isInstanceOf(UnsupportedOperationException.class)
                .hasMessage("Pure encoding for a PBC of type other than AMO or EXO is currently not supported.");

        final List<Formula> formulas02 = Arrays.asList(this.f.parse("a & b"), this.f.parse("c | a & (b + c + d <= 4)"));
        assertThatThrownBy(() -> ModelCounter.count(formulas02, FormulaHelper.variables(formulas02)))
                .isInstanceOf(UnsupportedOperationException.class)
                .hasMessage("Pure encoding for a PBC of type other than AMO or EXO is currently not supported.");
    }

    @Test
    public void testQueens() {
        final NQueensGenerator generator = new NQueensGenerator(this.f);
        testQueens(generator, 4, 2);
        testQueens(generator, 5, 10);
        testQueens(generator, 6, 4);
        testQueens(generator, 7, 40);
        testQueens(generator, 8, 92);
    }

    private void testQueens(final NQueensGenerator generator, final int size, final int models) {
        final Formula queens = generator.generate(size);
        assertThat(ModelCounter.count(Collections.singletonList(queens), queens.variables())).isEqualTo(BigInteger.valueOf(models));
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            if (formula.type() == FType.PBC) {
                final PBConstraint pbc = (PBConstraint) formula;
                if (!pbc.isAmo() && !pbc.isExo()) {
                    assertThatThrownBy(() -> ModelCounter.count(Collections.singletonList(formula), formula.variables()))
                            .isInstanceOf(UnsupportedOperationException.class);
                    continue;
                }
            }
            final BigInteger expCount = enumerationBasedModelCount(Collections.singletonList(formula), f);
            final BigInteger count = ModelCounter.count(Collections.singleton(formula), formula.variables());
            assertThat(count).isEqualTo(expCount);
        }
    }

    @Test
    @RandomTag
    public void testRandom() {
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(5)
                    .weightAmo(5)
                    .weightExo(5)
                    .seed(i * 42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);

            final Formula formula = randomizer.formula(4);
            final BigInteger expCount = enumerationBasedModelCount(Collections.singletonList(formula), f);
            final BigInteger count = ModelCounter.count(Collections.singleton(formula), formula.variables());
            assertThat(count).isEqualTo(expCount);
        }
    }

    @Test
    @RandomTag
    public void testRandomWithFormulaList() {
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(5)
                    .weightAmo(5)
                    .weightExo(5)
                    .seed(i * 42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);

            final List<Formula> formulas = IntStream.range(1, 5).mapToObj(j -> randomizer.formula(4)).collect(Collectors.toList());
            final BigInteger expCount = enumerationBasedModelCount(formulas, f);
            final BigInteger count = ModelCounter.count(formulas, FormulaHelper.variables(formulas));
            assertThat(count).isEqualTo(expCount);
        }
    }

    @Test
    @RandomTag
    public void testRandomWithFormulaListWithoutPBC() {
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(5)
                    .weightPbc(0)
                    .seed(i * 42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);

            final List<Formula> formulas = IntStream.range(1, 5).mapToObj(j -> randomizer.formula(4)).collect(Collectors.toList());
            final BigInteger expCount = enumerationBasedModelCount(formulas, f);
            final BigInteger count = ModelCounter.count(formulas, FormulaHelper.variables(formulas));
            assertThat(count).isEqualTo(expCount);
            final Formula formula = f.and(formulas);
            if (!formula.variables().isEmpty()) {
                // Without PB constraints we can use the BDD model count as reference
                assertThat(count).isEqualTo(formula.bdd(VariableOrdering.FORCE).modelCount());
            }
        }
    }

    private static BigInteger enumerationBasedModelCount(final List<Formula> formulas, final FormulaFactory f) {
        final MiniSat solver = MiniSat.miniSat(f);
        solver.add(formulas);
        final SortedSet<Variable> variables = FormulaHelper.variables(formulas);
        final List<Assignment> models = solver.enumerateAllModels(variables);
        return modelCount(models, variables);
    }

    private static BigInteger modelCount(final List<Assignment> models, final SortedSet<Variable> variables) {
        if (models.isEmpty()) {
            return BigInteger.ZERO;
        } else {
            final Assignment firstModel = models.get(0);
            final SortedSet<Variable> modelVars = new TreeSet<>(firstModel.positiveVariables());
            modelVars.addAll(firstModel.negativeVariables());
            final SortedSet<Variable> dontCareVars = variables.stream()
                    .filter(var -> !modelVars.contains(var))
                    .collect(Collectors.toCollection(TreeSet::new));
            return BigInteger.valueOf(models.size()).multiply(BigInteger.valueOf(2).pow(dontCareVars.size()));
        }
    }

    @Test
    public void simpleTest() {
        final MiniSat miniSat = MiniSat.miniSat(f);
        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Formula f1 = f.or(a, b, c);
        miniSat.add(f1);
        final BigInteger modelcount = miniSat.execute(ModelCounterFunction.builder().build());
        System.out.println(modelcount);
        System.out.println("**");
        final List<Assignment> assignments = miniSat.execute(ModelEnumerationFunction.builder().build());
        for (final Assignment assignment : assignments) {
            System.out.println(assignment);
        }
        System.out.println(assignments.size());
    }

    @Test
    public void simpleTestTautology() {
        final FormulaFactoryConfig config = FormulaFactoryConfig.builder().simplifyComplementaryOperands(false).build();
        final FormulaFactory g = new FormulaFactory(config);

        final Variable a = f.variable("A");
        final Formula f1 = f.or(a, a.negate());
        System.out.println(f1);
        final MiniSat miniSat = MiniSat.miniSat(f);
        miniSat.add(f1);
        final BigInteger modelcount = miniSat.execute(ModelCounterFunction.builder().build());
        System.out.println("new implementation: " + modelcount);


        final BigInteger modelcountOriginal = ModelCounter.count(Collections.singletonList(f1), new TreeSet<>(Collections.singletonList(a)));
        System.out.println("count original: " + modelcountOriginal);
        System.out.println("**");
        final List<Assignment> assignments = miniSat.execute(ModelEnumerationFunction.builder().build());
        for (final Assignment assignment : assignments) {
            System.out.println(assignment);
        }

    }


    @ParameterizedTest
    @MethodSource("solvers")
    public void test(final MiniSat solver) {
        solver.reset();
        SolverState state = null;
        if (solver.underlyingSolver() instanceof MiniSat2Solver) {
            state = solver.saveState();
        }
        solver.add(f.falsum());
        Backbone backbone = solver.backbone(v("a b c"));
        assertThat(backbone.isSat()).isFalse();
        assertThat(backbone.getCompleteBackbone()).isEmpty();
        if (solver.underlyingSolver() instanceof MiniSat2Solver) {
            solver.loadState(state);
        } else {
            solver.reset();
        }
        solver.add(f.verum());
        backbone = solver.backbone(v("a b c"));
        final BigInteger modelcount = solver.execute(ModelCounterFunction.builder().build());

        System.out.println(modelcount);
        assertThat(backbone.isSat()).isTrue();
        assertThat(backbone.getCompleteBackbone()).isEmpty();
    }

    private SortedSet<Variable> v(final String s) {
        final SortedSet<Variable> vars = new TreeSet<>();
        for (final String name : s.split(" ")) {
            vars.add(f.variable(name));
        }
        return vars;
    }

}
