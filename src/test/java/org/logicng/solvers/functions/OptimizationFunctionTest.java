///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.solvers.functions;

import static java.util.stream.Collectors.toCollection;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.LogicNGTest;
import org.logicng.LongRunningTag;
import org.logicng.RandomTag;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.CType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.predicates.satisfiability.SATPredicate;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link OptimizationFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class OptimizationFunctionTest implements LogicNGTest {

    public static Collection<Object[]> solvers() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final List<Object[]> solvers = new ArrayList<>();
        solvers.add(new Object[]{MiniSat.miniSat(f, MiniSatConfig.builder().initialPhase(true).build())});
        solvers.add(new Object[]{MiniSat.miniSat(f, MiniSatConfig.builder().initialPhase(false).build())});
        solvers.add(new Object[]{MiniSat.miniSat(f, MiniSatConfig.builder().initialPhase(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build())});
        solvers.add(new Object[]{MiniSat.miniSat(f, MiniSatConfig.builder().initialPhase(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).proofGeneration(true).build())});
        solvers.add(new Object[]{MiniSat.miniSat(f, MiniSatConfig.builder().initialPhase(false).build())});
        solvers.add(new Object[]{MiniSat.miniCard(f, MiniSatConfig.builder().initialPhase(true).build())});
        solvers.add(new Object[]{MiniSat.miniCard(f, MiniSatConfig.builder().initialPhase(false).build())});
        solvers.add(new Object[]{MiniSat.glucose(f, MiniSatConfig.builder().initialPhase(true).build(), GlucoseConfig.builder().build())});
        solvers.add(new Object[]{MiniSat.glucose(f, MiniSatConfig.builder().initialPhase(false).build(), GlucoseConfig.builder().build())});
        return solvers;
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testUnsatFormula(final SATSolver solver) throws ParserException {
        final Formula formula = solver.factory().parse("a & b & (a => ~b)");
        final Assignment minimumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), false, solver);
        assertThat(minimumModel).isNull();
        final Assignment maximumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), true, solver);
        assertThat(maximumModel).isNull();
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testSingleModel(final SATSolver solver) throws ParserException {
        final Formula formula = solver.factory().parse("~a & ~b & ~c");
        final Assignment minimumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), false, solver);
        testMinimumModel(formula, minimumModel, formula.variables());
        final Assignment maximumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), true, solver);
        testMaximumModel(formula, maximumModel, formula.variables());
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testExoModel(final SATSolver solver) {
        final FormulaFactory f = solver.factory();
        final CardinalityConstraint exo = (CardinalityConstraint) f.exo(f.variable("a"), f.variable("b"), f.variable("c"));
        final Assignment minimumModel = optimize(Collections.singleton(exo), exo.variables(), Collections.emptyList(), false, solver);
        testMinimumModel(exo, minimumModel, exo.variables());
        final Assignment maximumModel = optimize(Collections.singleton(exo), exo.variables(), Collections.emptyList(), true, solver);
        testMaximumModel(exo, maximumModel, exo.variables());
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testCornerCases(final SATSolver solver) {
        final FormulaCornerCases cornerCases = new FormulaCornerCases(solver.factory());
        for (final Formula formula : cornerCases.cornerCases()) {
            final Set<Variable> targetLiterals = cornerCases.getVariables();

            final Assignment minimumModel = optimize(Collections.singleton(formula), targetLiterals, Collections.emptySet(), false, solver);
            testMinimumModel(formula, minimumModel, targetLiterals);

            final Assignment maximumModel = optimize(Collections.singleton(formula), targetLiterals, Collections.emptySet(), true, solver);
            testMaximumModel(formula, maximumModel, targetLiterals);
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    @RandomTag
    public void testRandomSmall(final SATSolver solver) {
        final FormulaFactory f = solver.factory();
        final Random random = new Random(42);
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(6).weightPbc(2).seed(42).build());
        for (int i = 0; i < 1000; i++) {
            final Formula formula = randomizer.formula(2);
            final List<Variable> variables = new ArrayList<>(formula.variables());

            final Set<Literal> targetLiterals = randomTargetLiterals(random, randomSubset(random, variables, Math.min(variables.size(), 5)), f);
            final Set<Variable> additionalVariables = randomSubset(random, variables, Math.min(variables.size(), 3));

            final Assignment minimumModel = optimize(Collections.singleton(formula), targetLiterals, additionalVariables, false, solver);
            testMinimumModel(formula, minimumModel, targetLiterals);

            final Assignment maximumModel = optimize(Collections.singleton(formula), targetLiterals, additionalVariables, true, solver);
            testMaximumModel(formula, maximumModel, targetLiterals);
        }
    }

    private static <T> Set<T> randomSubset(final Random random, final List<T> elements, final int subsetSize) {
        if (subsetSize > elements.size()) {
            throw new IllegalArgumentException();
        }
        final Set<T> subset = new HashSet<>();
        while (subset.size() < subsetSize) {
            subset.add(elements.get(random.nextInt(elements.size())));
        }
        return subset;
    }

    private static SortedSet<Literal> randomTargetLiterals(final Random random, final Collection<Variable> variables, final FormulaFactory f) {
        return variables.stream().map(var -> f.literal(var.name(), random.nextBoolean())).collect(toCollection(TreeSet::new));
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testIncrementalityMinimizeAndMaximize(final MiniSat solver) throws ParserException {
        final FormulaFactory f = solver.factory();
        if (solver.getStyle() != MiniSat.SolverStyle.MINISAT || !solver.isIncremental()) {
            return;
        }
        solver.reset();
        Formula formula = f.parse("(a|b|c|d|e) & (p|q) & (x|y|z)");
        final SortedSet<Variable> vars = new TreeSet<>(formula.variables());
        solver.add(formula);

        Assignment minimumModel = solver.execute(OptimizationFunction.builder().minimize().literals(vars).build());
        Assignment maximumModel = solver.execute(OptimizationFunction.builder().maximize().literals(vars).build());
        assertThat(minimumModel.positiveVariables()).hasSize(3);
        assertThat(maximumModel.positiveVariables()).hasSize(10);

        formula = f.parse("~p");
        vars.addAll(formula.variables());
        solver.add(formula);
        minimumModel = solver.execute(OptimizationFunction.builder().minimize().literals(vars).build());
        maximumModel = solver.execute(OptimizationFunction.builder().maximize().literals(vars).build());
        assertThat(minimumModel.positiveVariables()).hasSize(3).contains(f.variable("q"));
        assertThat(maximumModel.positiveVariables()).hasSize(9).contains(f.variable("q"));

        formula = f.parse("(x => n) & (y => m) & (a => ~b & ~c)");
        vars.addAll(formula.variables());
        solver.add(formula);
        minimumModel = solver.execute(OptimizationFunction.builder().minimize().literals(vars).build());
        maximumModel = solver.execute(OptimizationFunction.builder().maximize().literals(vars).build());
        assertThat(minimumModel.positiveVariables()).hasSize(3).contains(f.variable("q"), f.variable("z"));
        assertThat(maximumModel.positiveVariables()).hasSize(10)
                .contains(f.variable("q"), f.variable("z"))
                .doesNotContain(f.variable("a"));

        formula = f.parse("(z => v & w) & (m => v) & (b => ~c & ~d & ~e)");
        vars.addAll(formula.variables());
        solver.add(formula);
        minimumModel = solver.execute(OptimizationFunction.builder().minimize().literals(vars).build());
        maximumModel = solver.execute(OptimizationFunction.builder().maximize().literals(vars).build());
        assertThat(minimumModel.positiveVariables()).hasSize(4).contains(f.variable("q"), f.variable("x"), f.variable("n"));
        assertThat(maximumModel.positiveVariables()).hasSize(11)
                .contains(f.variable("q"), f.variable("x"), f.variable("n"), f.variable("v"), f.variable("w"))
                .doesNotContain(f.variable("b"));

        formula = f.parse("~q");
        vars.addAll(formula.variables());
        solver.add(formula);
        minimumModel = solver.execute(OptimizationFunction.builder().minimize().literals(vars).build());
        maximumModel = solver.execute(OptimizationFunction.builder().maximize().literals(vars).build());
        assertThat(minimumModel).isNull();
        assertThat(maximumModel).isNull();
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testAdditionalVariables(final SATSolver solver) throws ParserException {
        final FormulaFactory f = solver.factory();
        final Variable a = f.variable("a");
        final Literal na = f.literal("a", false);
        final Variable b = f.variable("b");
        final Literal nb = f.literal("b", false);
        final Variable c = f.variable("c");
        final Variable x = f.variable("x");
        final Literal nx = f.literal("x", false);
        final Variable y = f.variable("y");

        solver.reset();
        final Formula formula = f.parse("(a|b) & (~a => c) & (x|y)");

        final List<Literal> literalsANBX = Arrays.asList(a, nb, x);
        final Assignment minimumModel = optimize(Collections.singleton(formula), literalsANBX, Collections.emptyList(), false, solver);
        assertThat(minimumModel.literals()).containsExactlyInAnyOrder(na, b, nx);
        final Assignment minimumModelWithY = optimize(Collections.singleton(formula), literalsANBX, Collections.singleton(y), false, solver);
        assertThat(minimumModelWithY.literals()).containsExactlyInAnyOrder(na, b, nx, y);
        final Assignment minimumModelWithCY = optimize(Collections.singleton(formula), literalsANBX, Arrays.asList(c, y), false, solver);
        assertThat(minimumModelWithCY.literals()).containsExactlyInAnyOrder(na, b, c, nx, y);

        final List<Literal> literalsNBNX = Arrays.asList(na, nx);
        final Assignment maximumModel = optimize(Collections.singleton(formula), literalsNBNX, Collections.emptyList(), true, solver);
        assertThat(maximumModel.literals()).containsExactlyInAnyOrder(na, nx);
        final Assignment maximumModelWithC = optimize(Collections.singleton(formula), literalsNBNX, Collections.singleton(c), true, solver);
        assertThat(maximumModelWithC.literals()).containsExactlyInAnyOrder(na, c, nx);
        final Assignment maximumModelWithACY = optimize(Collections.singleton(formula), literalsNBNX, Arrays.asList(a, c, y), true, solver);
        assertThat(maximumModelWithACY.literals()).containsExactlyInAnyOrder(na, c, nx, y);
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testLargeFormulaMinimize(final SATSolver solver) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        final Assignment minimumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), false, solver);
        testMinimumModel(formula, minimumModel, formula.variables());
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testLargeFormulaMaximize(final SATSolver solver) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        final Assignment maximumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), true, solver);
        testMaximumModel(formula, maximumModel, formula.variables());
    }

    @ParameterizedTest
    @MethodSource("solvers")
    @LongRunningTag
    public void testLargerFormulaMinimize(final SATSolver solver) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
        final Assignment minimumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), false, solver);
        testMinimumModel(formula, minimumModel, formula.variables());
    }

    @ParameterizedTest
    @MethodSource("solvers")
    @LongRunningTag
    public void testLargerFormulaMaximize(final SATSolver solver) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
        final Assignment maximumModel = optimize(Collections.singleton(formula), formula.variables(), Collections.emptyList(), true, solver);
        testMaximumModel(formula, maximumModel, formula.variables());
    }

    @Test
    public void compareWithMaxSat() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/formulas/large_formula.txt"));
        final List<Formula> formulas = new ArrayList<>();
        final SortedSet<Variable> variables = new TreeSet<>();
        while (reader.ready()) {
            final Formula parsed = p.parse(reader.readLine());
            formulas.add(parsed);
            variables.addAll(parsed.variables());
        }
        final int expected = 25;
        assertThat(solveMaxSat(formulas, variables, MaxSATSolver.incWBO())).isEqualTo(expected);
        assertThat(solveMaxSat(formulas, variables, MaxSATSolver.linearSU())).isEqualTo(expected);
        assertThat(solveMaxSat(formulas, variables, MaxSATSolver.linearUS())).isEqualTo(expected);
        assertThat(solveMaxSat(formulas, variables, MaxSATSolver.msu3())).isEqualTo(expected);
        assertThat(solveMaxSat(formulas, variables, MaxSATSolver.wbo())).isEqualTo(expected);
        assertThat(satisfiedLiterals(optimize(formulas, variables, Collections.emptyList(), false, MiniSat.miniSat(f)), variables).size()).isEqualTo(expected);
        assertThat(satisfiedLiterals(optimize(formulas, variables, Collections.emptyList(), false, MiniSat.miniCard(f)), variables).size()).isEqualTo(expected);
        assertThat(satisfiedLiterals(optimize(formulas, variables, Collections.emptyList(), false, MiniSat.glucose(f)), variables).size()).isEqualTo(expected);
    }

    private int solveMaxSat(final List<Formula> formulas, final SortedSet<Variable> variables, final MaxSATSolver solver) {
        formulas.forEach(solver::addHardFormula);
        variables.forEach(v -> solver.addSoftFormula(v.negate(), 1));
        solver.solve();
        return solver.result();
    }

    private SortedSet<Literal> satisfiedLiterals(final Assignment assignment, final Collection<? extends Literal> literals) {
        final SortedSet<Literal> modelLiterals = assignment.literals();
        return literals.stream().filter(modelLiterals::contains).collect(toCollection(TreeSet::new));
    }

    private static Assignment optimize(final Collection<Formula> formulas, final Collection<? extends Literal> literals,
                                       final Collection<Variable> additionalVariables, final boolean maximize, final SATSolver solver) {
        solver.reset();
        formulas.forEach(solver::add);
        if (maximize) {
            return solver.execute(OptimizationFunction.builder().maximize().literals(literals)
                    .additionalVariables(additionalVariables).build());
        } else {
            return solver.execute(OptimizationFunction.builder().minimize().literals(literals)
                    .additionalVariables(additionalVariables).build());
        }
    }

    private void testMinimumModel(final Formula formula, final Assignment resultModel, final Collection<? extends Literal> literals) {
        testOptimumModel(formula, resultModel, literals, false);
    }

    private void testMaximumModel(final Formula formula, final Assignment resultModel, final Collection<? extends Literal> literals) {
        testOptimumModel(formula, resultModel, literals, true);
    }

    private void testOptimumModel(final Formula formula, final Assignment optimumModel, final Collection<? extends Literal> literals, final boolean maximize) {
        final FormulaFactory f = formula.factory();
        final SATPredicate satPredicate = new SATPredicate(f);
        if (formula.holds(satPredicate)) {
            assertThat(f.and(formula, f.and(optimumModel.literals())).holds(satPredicate));
            final int numSatisfiedLiterals = satisfiedLiterals(optimumModel, literals).size();
            final SortedSet<Variable> selVars = new TreeSet<>();
            final SATSolver solver = MiniSat.miniSat(formula.factory());
            solver.add(formula);
            for (final Literal lit : literals) {
                final Variable selVar = f.variable("SEL_VAR_" + selVars.size());
                if (maximize) {
                    solver.add(f.equivalence(selVar.negate(), lit));
                } else {
                    solver.add(f.equivalence(selVar.negate(), lit.negate()));
                }
            }
            solver.add(formula.factory().cc(CType.GT, numSatisfiedLiterals + 1, selVars));
            assertSolverUnsat(solver);
        } else {
            assertThat(optimumModel).isNull();
        }
    }
}
