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

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_INCWBO;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_LINEAR_SU;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_LINEAR_US;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_MSU3;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_OLL;
import static org.logicng.solvers.maxsat.OptimizationConfig.OptimizationType.MAXSAT_WBO;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.LongRunningTag;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Unit Tests for the class {@link AdvancedSimplifier}.
 * @version 2.6.0
 * @since 2.0.0
 */
public class AdvancedSimplifierTest extends TestWithExampleFormulas {

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

    @ParameterizedTest
    @MethodSource("configs")
    public void testConstants(final OptimizationConfig cfg) {
        final AdvancedSimplifier simplifier = new AdvancedSimplifier(AdvancedSimplifierConfig.builder().optimizationConfig(cfg).build());
        assertThat(this.f.falsum().transform(simplifier)).isEqualTo(this.f.falsum());
        assertThat(this.f.verum().transform(simplifier)).isEqualTo(this.f.verum());
    }

    @ParameterizedTest
    @MethodSource("configs")
    public void testCornerCases(final OptimizationConfig cfg) {
        final AdvancedSimplifier simplifier = new AdvancedSimplifier(AdvancedSimplifierConfig.builder().optimizationConfig(cfg).build());
        final FormulaCornerCases cornerCases = new FormulaCornerCases(this.f);
        cornerCases.cornerCases().forEach(it -> computeAndVerify(it, simplifier));
    }

    @ParameterizedTest
    @MethodSource("configs")
    @RandomTag
    public void testRandomized(final OptimizationConfig cfg) {
        final AdvancedSimplifier simplifier = new AdvancedSimplifier(AdvancedSimplifierConfig.builder().optimizationConfig(cfg).build());
        for (int i = 0; i < 100; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(8).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(5);
            computeAndVerify(formula, simplifier);
        }
    }

    @ParameterizedTest
    @MethodSource("configs")
    public void testOriginalFormula(final OptimizationConfig cfg) throws IOException {
        final AdvancedSimplifier simplifier = new AdvancedSimplifier(AdvancedSimplifierConfig.builder().optimizationConfig(cfg).build());
        Files.lines(Paths.get("src/test/resources/formulas/simplify_formulas.txt"))
                .filter(s -> !s.isEmpty())
                .forEach(s -> {
                    final FormulaFactory f = new FormulaFactory();
                    final Formula formula = parse(f, s);
                    computeAndVerify(formula, simplifier);
                });
    }

    @Test
    public void testTimeoutHandlerSmall() {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 5_000L, TimeoutHandler.TimerType.FIXED_END)
        );
        final Formula formula = parse(this.f, "a & b | ~c & a");
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formula, false, false);
            testHandler(handler, formula, false, true);
        }
    }

    @Test
    public void testTimeoutHandlerLarge() throws IOException, ParserException {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 1L, TimeoutHandler.TimerType.FIXED_END)
        );
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", this.f);
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formula, true, false);
            testHandler(handler, formula, true, true);
        }
    }

    @Test
    public void testPrimeCompilerIsCancelled() {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 0);
        final Formula formula = parse(this.f, "a&(b|c)");
        testHandler(handler, formula, true, false);
        testHandler(handler, formula, true, true);
    }

    @Test
    public void testSmusComputationIsCancelled() {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 5);
        final Formula formula = parse(this.f, "a&(b|c)");
        testHandler(handler, formula, true, false);
        testHandler(handler, formula, true, true);
    }

    @LongRunningTag
    @Test
    public void testCancellationPoints() {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = parse(f, "~v16 & ~v22 & ~v12 & (~v4 | ~v14) & (~v4 | ~v15) & (v3 | v4) & (v3 | ~v14) & (v3 | ~v15) " +
                "& (~v20 | ~v8) & (v9 | ~v20) & (~v21 | ~v8) & (v9 | ~v21) & (~v21 | ~v10) & (~v21 | ~v11) & v19");
        for (int numOptimizationStarts = 1; numOptimizationStarts < 30; numOptimizationStarts++) {
            for (int numSatHandlerStarts = 1; numSatHandlerStarts < 500; numSatHandlerStarts++) {
                final OptimizationHandler handler = new BoundedOptimizationHandler(numSatHandlerStarts, numOptimizationStarts);
                testHandler(handler, formula, true, false);
            }
        }
    }

    @Test
    public void testAdvancedSimplifierConfig() {
        final FormulaFactory f = new FormulaFactory();
        final List<AdvancedSimplifierConfig> configs = Arrays.asList(
                AdvancedSimplifierConfig.builder().build(),
                AdvancedSimplifierConfig.builder().restrictBackbone(false).factorOut(false).simplifyNegations(false).build(),
                AdvancedSimplifierConfig.builder().factorOut(false).simplifyNegations(false).build(),
                AdvancedSimplifierConfig.builder().restrictBackbone(false).simplifyNegations(false).build(),
                AdvancedSimplifierConfig.builder().restrictBackbone(false).factorOut(false).build(),
                AdvancedSimplifierConfig.builder().restrictBackbone(false).build(),
                AdvancedSimplifierConfig.builder().factorOut(false).build(),
                AdvancedSimplifierConfig.builder().simplifyNegations(false).build());

        for (final AdvancedSimplifierConfig config : configs) {
            final AdvancedSimplifier advancedSimplifier = new AdvancedSimplifier(config);
            for (int i = 1; i < 10; i++) {
                final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
                final Formula formula = randomizer.formula(3);
                final Formula simplified = formula.transform(advancedSimplifier);
                if (simplified != null) {
                    assertThat(f.equivalence(formula, simplified).holds(new TautologyPredicate(f))).isTrue();
                    assertThat(formula.toString().length()).isGreaterThanOrEqualTo(simplified.toString().length());
                }
            }
        }
    }

    private void computeAndVerify(final Formula formula, final AdvancedSimplifier simplifier) {
        final Formula simplified = formula.transform(simplifier);
        assertThat(formula.factory().equivalence(formula, simplified).holds(new TautologyPredicate(this.f)))
                .as("Minimized formula is equivalent to original Formula")
                .isTrue();
    }

    private void testHandler(final OptimizationHandler handler, final Formula formula, final boolean expAborted, final boolean expIntermediate) {
        final AdvancedSimplifier simplifierWithHandler = new AdvancedSimplifier(AdvancedSimplifierConfig.builder()
                .optimizationConfig(OptimizationConfig.sat(handler))
                .returnIntermediateResult(expIntermediate)
                .build());
        final Formula simplified = formula.transform(simplifierWithHandler);
        assertThat(handler.aborted()).isEqualTo(expAborted);
        if (expAborted) {
            assertThat(handler.aborted()).isTrue();
            if (!expIntermediate) {
                assertThat(simplified).isNull();
            } else {
                assertThat(simplified).isNotNull();
                assertThat(simplified.isEquivalentTo(formula)).isTrue();
            }
        } else {
            assertThat(handler.aborted()).isFalse();
            assertThat(simplified).isNotNull();
        }
    }
}
