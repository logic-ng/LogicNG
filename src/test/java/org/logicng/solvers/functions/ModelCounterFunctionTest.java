package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrdering;
import org.logicng.modelcounting.ModelCounter;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.testutils.NQueensGenerator;
import org.logicng.transformations.cnf.CNFConfig;
import org.logicng.transformations.cnf.CNFFactorization;
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
    private void init() {
        final CNFConfig configCnf = CNFConfig.builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build();
        g.putConfiguration(configCnf);
        f.putConfiguration(configCnf);
    }

    private SortedSet<Variable> vars(final String... vars) {
        return Arrays.stream(vars).map(this.f::variable).collect(Collectors.toCollection(TreeSet::new));
    }


    @Test
    public void testConstants() {
        final MiniSat solver = MiniSat.miniSat(f);
        final SolverState state = solver.saveState();

        solver.add(this.f.falsum());
        assertThat(solver.execute(ModelCounterFunction.builder().variables(Collections.emptySortedSet()).build())).isEqualTo(BigInteger.ZERO);
        solver.loadState(state);

        solver.add(this.f.falsum());
        assertThat(solver.execute(ModelCounterFunction.builder().variables(vars("a", "b")).build())).isEqualTo(BigInteger.ZERO);
        solver.loadState(state);

        solver.add(this.f.verum());
        assertThat(solver.execute(ModelCounterFunction.builder().variables(Collections.emptySortedSet()).build())).isEqualTo(BigInteger.ONE);
        solver.loadState(state);

        solver.add(this.f.falsum());
        assertThat(solver.execute(ModelCounterFunction.builder().variables(vars("a", "b")).build())).isEqualTo(BigInteger.valueOf(0));
        solver.loadState(state);
    }

    @Test
    public void testSimple() throws ParserException {
        init();
        final MiniSat solver = MiniSat.miniSat(f);
        final SolverState initialState = solver.saveState();

        final Formula formula01 = g.parse("(~v1 => ~v0) | ~v1 | v0");
        solver.add(formula01);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(4));
        solver.loadState(initialState);

        final List<Formula> formulas02 = Arrays.asList(this.f.parse("(a & b) | ~b"), this.f.parse("a"));
        final Formula fAnd = f.and(formulas02);
        solver.add(fAnd);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(2));
        solver.loadState(initialState);

        final List<Formula> formulas03 = Arrays.asList(this.f.parse("a & b & c"), this.f.parse("c & d"));
        solver.add(formulas03);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(1));
        solver.loadState(initialState);
    }

    @Test
    public void testAmoAndExo() throws ParserException {
        final MiniSat solver = MiniSat.miniSat(f);
        final SolverState initialState = solver.saveState();

        final List<Formula> formulas01 = Arrays.asList(this.f.parse("a & b"), this.f.parse("a + b + c + d <= 1"));
        formulas01.forEach(solver::add);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(0));
        solver.loadState(initialState);

        final List<Formula> formulas02 = Arrays.asList(this.f.parse("a & b & (a + b + c + d <= 1)"), this.f.parse("a | b"));
        formulas02.forEach(solver::add);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(0));
        solver.loadState(initialState);

        final List<Formula> formulas03 = Arrays.asList(this.f.parse("a & (a + b + c + d <= 1)"), this.f.parse("a | b"));
        formulas03.forEach(solver::add);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(1));
        solver.loadState(initialState);

        final List<Formula> formulas04 = Arrays.asList(this.f.parse("a & (a + b + c + d = 1)"), this.f.parse("a | b"));
        formulas04.forEach(solver::add);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(1));
        solver.loadState(initialState);
    }

    @Test
    public void testQueens() {
        final MiniSat solver = MiniSat.miniSat(f);
        final NQueensGenerator generator = new NQueensGenerator(this.f);
        final long start = System.currentTimeMillis();
        testQueens(solver, generator, 4, 2);
        testQueens(solver, generator, 5, 10);
        testQueens(solver, generator, 6, 4);
        testQueens(solver, generator, 7, 40);
        testQueens(solver, generator, 8, 92);
        final long mid = System.currentTimeMillis();

        final NQueensGenerator generatorO = new NQueensGenerator(this.f);
        testQueensOriginal(generatorO, 4, 2);
        testQueensOriginal(generatorO, 5, 10);
        testQueensOriginal(generatorO, 6, 4);
        testQueensOriginal(generatorO, 7, 40);
        testQueensOriginal(generatorO, 8, 92);
        final long end = System.currentTimeMillis();

        System.out.println("Time new: " + (mid - start));
        System.out.println("Time old: " + (end - mid));
    }

    private void testQueens(final MiniSat solver, final NQueensGenerator generator, final int size, final int models) {
        final SolverState initialState = solver.saveState();
        final Formula queens = generator.generate(size);
        solver.add(queens);
        assertThat(solver.execute(ModelCounterFunction.builder().build())).isEqualTo(BigInteger.valueOf(models));
        solver.loadState(initialState);
    }

    private void testQueensOriginal(final NQueensGenerator generator, final int size, final int models) {
        final Formula queens = generator.generate(size);
        assertThat(ModelCounter.count(Collections.singletonList(queens), queens.variables())).isEqualTo(BigInteger.valueOf(models));
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final MiniSat solver = MiniSat.miniSat(f);
        final SolverState initialState = solver.saveState();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            if (formula.type() == FType.PBC) {
                final PBConstraint pbc = (PBConstraint) formula;
                // TODO deal with pbc
                continue;
            }
            solver.add(formula);
            final BigInteger expCount = enumerationBasedModelCount(Collections.singletonList(formula), f);
            final BigInteger count = solver.execute(ModelCounterFunction.builder().build());
            solver.loadState(initialState);
            assertThat(count).isEqualTo(expCount);
        }
    }

    // @Test
    // @RandomTag
    // public void testRandom() {
    //     final MiniSat solver = MiniSat.miniSat(f);
    //     final SolverState initialState = solver.saveState();
    //     for (int i = 0; i < 200; i++) {
    //         System.out.println("int: " + i);
    //         final FormulaFactory f = new FormulaFactory();
    //         f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    //         final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
    //                 .numVars(5)
    //                 .weightAmo(5)
    //                 .weightExo(5)
    //                 .seed(i * 42).build();
    //         final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);
    //         final Formula formula = randomizer.formula(3);
    //         System.out.println("\nformula = " + formula);
    //         final BigInteger expCount = enumerationBasedModelCount(Collections.singletonList(formula), f);
    //         System.out.println("expCount = " + expCount);
    //         solver.add(formula);
    //         // something strange happens: Check it
    //         final BigInteger count = solver.execute(ModelCounterFunction.builder().build());
    //         System.out.println("count = " + count);
    //         solver.loadState(initialState);
    //         assertThat(count).isEqualTo(expCount);
    //     }
    // }

    @Test
    public void compareWithModelEnumeration() throws ParserException {
        final Formula formula = f.parse("(v2 + v2 + v3 + v1 = 1) & (v4 + v4 + v3 + v3 + v1 = 1) & (v3 + v2 = 1) & (v4 + v3 + v1 + v2 + v4 <= 1) | " +
                "~v1 | (v3 + v2 + v1 + v0 + v0 = 1) | (v2 + v2 + v3 + v2 = 1) | (v2 + v3 = 1) & v4 & (v0 + v1 = 1) | (v0 + v4 = 1) | (v3 + v1 + v1 + v4 + v4 " +
                "= 1) | (v4 + v3 + v2 <= 1) | (v4 + v1 <= 1) | (v4 + v1 + v0 + v4 <= 1) | v2 | ~v3 | (v0 + v2 <= 1) | (v1 + v4 <= 1) | (v3 + v2 + v0 = 1) | " +
                "(v2 + v4 + v1 = 1) | (~v4 | (v4 + v0 + v4 + v2 <= 1) | v1 | (v3 + v3 + v3 + v0 <= 1)) & (v3 + v2 + v2 + v4 + v3 = 1) & (v2 + v2 + v1 + v3 + " +
                "v0 = 1) & (v3 + v0 + v0 <= 1) & (v4 + v3 = 1) & ((v0 + v0 + v0 + v4 + v1 = 1) | (v4 + v3 + v1 = 1)) & ((v0 + v1 + v2 = 1) | (v2 + v4 <= 1) |" +
                " v2 | (v3 + v1 + v1 + v2 + v2 <= 1)) | (v0 + v3 + v4 <= 1) | (v3 + v4 + v4 = 1) | (v4 + v0 = 1) | (v1 + v1 <= 1) | (v3 + v2 + v1 + v1 = 1) |" +
                " (v4 + v0 + v1 <= 1) | (v1 + v3 + v3 + v1 <= 1) | (v4 + v0 + v4 <= 1) | ~v4 | (v4 | (v0 + v2 <= 1)) & (v3 + v0 + v4 <= 1) & v4 & (v2 + v0 + " +
                "v4 + v0 = 1) & (v1 + v0 + v2 + v0 + v1 = 1) & v2 | v1 & (v4 + v0 + v3 + v4 = 1) & (v0 + v2 + v0 + v3 = 1) & (v3 + v0 + v2 + v4 + v4 <= 1) | " +
                "(v1 + v3 + v4 <= 1) & (v0 + v2 + v4 + v0 + v3 = 1) & (v2 + v4 = 1) | ((v0 + v3 + v1 = 1) | (v4 + v0 + v1 + v3 = 1) | (v4 + v4 + v0 + v0 + v0" +
                " = 1) | ((v1 + v4 + v0 + v4 <= 1) <=> (v0 + v1 + v4 + v0 + v0 = 1))) & (v1 + v0 + v3 + v3 + v3 = 1) & ((v2 + v2 + v0 + v2 + v0 = 1) | (v3 + " +
                "v1 + v1 + v2 = 1) | (v4 + v1 <= 1)) & (~v1 | (v4 + v0 + v2 + v4 <= 1) | ~v2 | (v4 + v3 + v1 = 1)) & ((v2 + v4 + v3 + v3 + v0 <= 1) | v0 | " +
                "(v3 + v0 + v2 + v3 + v3 <= 1) | ~v1 | (v4 + v0 + v0 = 1) & v1 | (v2 + v0 + v4 + v0 <= 1) & (v1 + v3 <= 1) | (v0 + v4 + v1 + v1 + v3 = 1) | " +
                "(v0 + v1 = 1))");
        final SATSolver solver = MiniSat.miniSat(f);
        solver.add(formula);
        System.out.println(formula);

        final List<Assignment> assignments = solver.enumerateAllModels();
        System.out.println("assignments: " + assignments.size());

        final SolverState initialState = solver.saveState();
        final BigInteger count = solver.execute(ModelCounterFunction.builder().build());
        System.out.println("count: " + count);
        solver.loadState(initialState);


    }

    // Sieht gut aus, habe irgendwann abgebrochen.
    // @Test
    // @RandomTag
    // public void testRandomWithFormulaList() {
    //     final MiniSat solver = MiniSat.miniSat(f);
    //     final SolverState initialState = solver.saveState();
    //     for (int i = 0; i < 500; i++) {
    //         final FormulaFactory f = new FormulaFactory();
    //         f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    //         final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
    //                 .numVars(5)
    //                 .weightAmo(5)
    //                 .weightExo(5)
    //                 .seed(i * 42).build();
    //         final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);
    //         final List<Formula> formulas = IntStream.range(1, 5).mapToObj(j -> randomizer.formula(4)).collect(Collectors.toList());
    //         formulas.forEach(solver::add);
    //         final BigInteger expCount = enumerationBasedModelCount(formulas, f);
    //         final BigInteger count = solver.execute(ModelCounterFunction.builder().build());
    //         solver.loadState(initialState);
    //         assertThat(count).isEqualTo(expCount);
    //     }
    // }

    @Test
    @RandomTag
    public void testRandomWithFormulaListWithoutPBC() {
        final MiniSat solver = MiniSat.miniSat(f);
        final SolverState initialState = solver.saveState();
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(5)
                    .weightPbc(0)
                    .seed(i * 42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);
            final List<Formula> formulas = IntStream.range(1, 5).mapToObj(j -> randomizer.formula(4)).collect(Collectors.toList());
            formulas.forEach(solver::add);
            final BigInteger expCount = enumerationBasedModelCount(formulas, f);
            final BigInteger count = solver.execute(ModelCounterFunction.builder().build());
            solver.loadState(initialState);
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
        final Variable d = f.variable("D");
        final Formula f1 = f.and(f.or(a, b), f.or(c, d));
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
    public void simpleTestME() {
        final MiniSat miniSat = MiniSat.miniSat(f);
        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Variable c = f.variable("C");
        final Variable d = f.variable("D");
        final Formula f1 = f.and(f.or(a, b), f.or(c, d));
        final Formula noAuxiliaryVars = f1.transform(new CNFFactorization());
        miniSat.add(noAuxiliaryVars);
        final List<Assignment> assignments = miniSat.execute(ModelEnumerationFunction.builder().build());
        for (final Assignment assignment : assignments) {
            System.out.println(assignment);
        }
        System.out.println(assignments.size());
    }

    @Test
    public void simpleTestTautology() {
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


    // @ParameterizedTest
    // @MethodSource("solvers")
    // public void test(final MiniSat solver) {
    //     solver.reset();
    //     SolverState state = null;
    //     if (solver.underlyingSolver() instanceof MiniSat2Solver) {
    //         state = solver.saveState();
    //     }
    //     solver.add(f.falsum());
    //     Backbone backbone = solver.backbone(v("a b c"));
    //     assertThat(backbone.isSat()).isFalse();
    //     assertThat(backbone.getCompleteBackbone()).isEmpty();
    //     if (solver.underlyingSolver() instanceof MiniSat2Solver) {
    //         solver.loadState(state);
    //     } else {
    //         solver.reset();
    //     }
    //     solver.add(f.verum());
    //     backbone = solver.backbone(v("a b c"));
    //     final BigInteger modelcount = solver.execute(ModelCounterFunction.builder().build());
    //
    //     System.out.println(modelcount);
    //     assertThat(backbone.isSat()).isTrue();
    //     assertThat(backbone.getCompleteBackbone()).isEmpty();
    // }

    private SortedSet<Variable> v(final String s) {
        final SortedSet<Variable> vars = new TreeSet<>();
        for (final String name : s.split(" ")) {
            vars.add(f.variable(name));
        }
        return vars;
    }

}
