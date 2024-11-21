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

package org.logicng.primecomputation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.primecomputation.PrimeResult.CoverageType.IMPLICANTS_COMPLETE;
import static org.logicng.primecomputation.PrimeResult.CoverageType.IMPLICATES_COMPLETE;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_INCWBO;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_LINEAR_SU;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_LINEAR_US;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_MSU3;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_OLL;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_WBO;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.SAT_OPTIMIZATION;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.BoundedOptimizationHandler;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.handlers.TimeoutHandler;
import org.logicng.handlers.TimeoutOptimizationHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.solvers.maxsat.OptimizationConfig;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;
import org.logicng.util.Pair;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for the class {@link PrimeCompiler}.
 * @version 2.6.0
 * @since 2.0.0
 */
public class PrimeCompilerTest extends TestWithExampleFormulas {

    public static Collection<Object[]> configs() {
        final List<Object[]> configs = new ArrayList<>();
        configs.add(new Object[]{OptimizationConfig.sat(null), "SAT"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_INCWBO, null, null), "INCWBO"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_LINEAR_SU, null, null), "LINEAR_SU"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_LINEAR_US, null, null), "LINEAR_US"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_MSU3, null, null), "MSU3"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_OLL, null, null), "OLL"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_WBO, null, null), "WBO"});
        return configs;
    }

    public static Collection<Object[]> fastConfigs() {
        final List<Object[]> configs = new ArrayList<>();
        configs.add(new Object[]{OptimizationConfig.sat(null), "SAT"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_LINEAR_SU, null, null), "LINEAR_SU"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_LINEAR_US, null, null), "LINEAR_US"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_MSU3, null, null), "MSU3"});
        configs.add(new Object[]{OptimizationConfig.maxsat(MAXSAT_OLL, null, null), "OLL"});
        return configs;
    }

    @ParameterizedTest
    @MethodSource("configs")
    public void testSimple(final OptimizationConfig cfg) {
        computeAndVerify(cfg, this.TRUE);
        computeAndVerify(cfg, this.FALSE);
        computeAndVerify(cfg, this.A);
        computeAndVerify(cfg, this.NA);
        computeAndVerify(cfg, this.AND1);
        computeAndVerify(cfg, this.AND2);
        computeAndVerify(cfg, this.AND3);
        computeAndVerify(cfg, this.OR1);
        computeAndVerify(cfg, this.OR2);
        computeAndVerify(cfg, this.OR3);
        computeAndVerify(cfg, this.NOT1);
        computeAndVerify(cfg, this.NOT2);
        computeAndVerify(cfg, this.IMP1);
        computeAndVerify(cfg, this.IMP2);
        computeAndVerify(cfg, this.IMP3);
        computeAndVerify(cfg, this.IMP4);
        computeAndVerify(cfg, this.EQ1);
        computeAndVerify(cfg, this.EQ2);
        computeAndVerify(cfg, this.EQ3);
        computeAndVerify(cfg, this.EQ4);
        computeAndVerify(cfg, this.PBC1);
        computeAndVerify(cfg, this.PBC2);
        computeAndVerify(cfg, this.PBC3);
        computeAndVerify(cfg, this.PBC4);
        computeAndVerify(cfg, this.PBC5);
    }

    @ParameterizedTest
    @MethodSource("configs")
    public void testCornerCases(final OptimizationConfig cfg) {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        cornerCases.cornerCases().forEach(it -> computeAndVerify(cfg, it));
    }

    @ParameterizedTest
    @MethodSource("configs")
    @RandomTag
    public void testRandomized(final OptimizationConfig cfg) {
        for (int i = 0; i < 200; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(4);
            computeAndVerify(cfg, formula);
        }
    }

    @ParameterizedTest
    @MethodSource("fastConfigs")
    public void testOriginalFormulas(final OptimizationConfig cfg) throws IOException {
        Files.lines(Paths.get("src/test/resources/formulas/simplify_formulas.txt"))
                .filter(s -> !s.isEmpty())
                .forEach(s -> {
                    final Formula formula = parse(this.f, s);
                    final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, IMPLICANTS_COMPLETE , cfg);
                    verify(resultImplicantsMin, formula);
                    final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, IMPLICATES_COMPLETE, cfg);
                    verify(resultImplicatesMin, formula);
                });
    }

    @Test
    public void testTimeoutHandlerSmall() {
        final List<Pair<PrimeCompiler, PrimeResult.CoverageType>> compilers = Arrays.asList(
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICATES_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICATES_COMPLETE));
        for (final Pair<PrimeCompiler, PrimeResult.CoverageType> compiler : compilers) {
            final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                    new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                    new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                    new TimeoutOptimizationHandler(System.currentTimeMillis() + 5_000L, TimeoutHandler.TimerType.FIXED_END)
            );
            final Formula formula = parse(this.f, "a & b | ~c & a");
            for (final TimeoutOptimizationHandler handler : handlers) {
                testHandler(handler, formula, compiler.first(), compiler.second(), false);
            }
        }
    }

    @Test
    public void testTimeoutHandlerLarge() throws IOException, ParserException {
        final List<Pair<PrimeCompiler, PrimeResult.CoverageType>> compilers = Arrays.asList(
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICATES_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICATES_COMPLETE));
        for (final Pair<PrimeCompiler, PrimeResult.CoverageType> compiler : compilers) {
            final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                    new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                    new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                    new TimeoutOptimizationHandler(System.currentTimeMillis() + 1L, TimeoutHandler.TimerType.FIXED_END)
            );
            final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", this.f);
            for (final TimeoutOptimizationHandler handler : handlers) {
                testHandler(handler, formula, compiler.first(), compiler.second(), true);
            }
        }
    }

    @Test
    public void testCancellationPoints() throws IOException {
        final Formula formula = parse(this.f, Files.readAllLines(Paths.get("src/test/resources/formulas/simplify_formulas.txt")).get(0));
        final List<Pair<PrimeCompiler, PrimeResult.CoverageType>> compilers = Arrays.asList(
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMaximization(), IMPLICATES_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICANTS_COMPLETE),
                new Pair<>(PrimeCompiler.getWithMinimization(), IMPLICATES_COMPLETE));
        for (final Pair<PrimeCompiler, PrimeResult.CoverageType> compiler : compilers) {
            for (int numOptimizationStarts = 1; numOptimizationStarts < 5; numOptimizationStarts++) {
                for (int numSatHandlerStarts = 1; numSatHandlerStarts < 10; numSatHandlerStarts++) {
                    final OptimizationHandler handler = new BoundedOptimizationHandler(numSatHandlerStarts, numOptimizationStarts);
                    testHandler(handler, formula, compiler.first(), compiler.second(), true);
                }
            }
        }
    }

    private void computeAndVerify(final OptimizationConfig cfg, final Formula formula) {
        final PrimeResult resultImplicantsMax = PrimeCompiler.getWithMaximization().compute(formula, IMPLICANTS_COMPLETE, cfg);
        verify(resultImplicantsMax, formula);
        final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, IMPLICANTS_COMPLETE, cfg);
        verify(resultImplicantsMin, formula);
        assertThat(resultImplicantsMax.getCoverageType()).isEqualTo(IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMin.getCoverageType()).isEqualTo(IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMax.getPrimeImplicants()).containsExactlyInAnyOrderElementsOf(resultImplicantsMin.getPrimeImplicants());

        final PrimeResult resultImplicatesMax = PrimeCompiler.getWithMaximization().compute(formula, IMPLICATES_COMPLETE, cfg);
        verify(resultImplicatesMax, formula);
        final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, IMPLICATES_COMPLETE, cfg);
        verify(resultImplicatesMin, formula);
        assertThat(resultImplicatesMax.getCoverageType()).isEqualTo(IMPLICATES_COMPLETE);
        assertThat(resultImplicatesMin.getCoverageType()).isEqualTo(IMPLICATES_COMPLETE);
        assertThat(resultImplicatesMax.getPrimeImplicates()).containsExactlyInAnyOrderElementsOf(resultImplicatesMin.getPrimeImplicates());
    }

    private void verify(final PrimeResult result, final Formula formula) {
        verifyImplicants(result.getPrimeImplicants(), formula);
        verifyImplicates(result.getPrimeImplicates(), formula);
    }

    private void verifyImplicants(final List<SortedSet<Literal>> implicantSets, final Formula formula) {
        final FormulaFactory f = formula.factory();
        final List<Formula> implicants = new ArrayList<>();
        for (final SortedSet<Literal> implicant : implicantSets) {
            implicants.add(f.and(implicant));
            PrimeImplicantReductionTest.testPrimeImplicantProperty(formula, implicant);
        }
        assertThat(f.equivalence(f.or(implicants), formula).holds(new TautologyPredicate(f)))
                .as("Disjunction of implicants should be equivalent to the original formula.")
                .isTrue();
    }

    private void verifyImplicates(final List<SortedSet<Literal>> implicateSets, final Formula formula) {
        final FormulaFactory f = formula.factory();
        final List<Formula> implicates = new ArrayList<>();
        for (final SortedSet<Literal> implicate : implicateSets) {
            implicates.add(f.or(implicate));
            PrimeImplicateReductionTest.testPrimeImplicateProperty(formula, implicate);
        }
        assertThat(f.equivalence(f.and(implicates), formula).holds(new TautologyPredicate(f)))
                .as("Conjunction of implicates should be equivalent to the original formula.")
                .isTrue();
    }

    private void testHandler(final OptimizationHandler handler, final Formula formula, final PrimeCompiler compiler, final PrimeResult.CoverageType coverageType,
                             final boolean expAborted) {
        final PrimeResult result = compiler.compute(formula, coverageType, handler);
        assertThat(handler.aborted()).isEqualTo(expAborted);
        if (expAborted) {
            assertThat(result).isNull();
        } else {
            assertThat(result).isNotNull();
        }
    }
}
