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

import org.junit.jupiter.api.Test;
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
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Unit Tests for the class {@link AdvancedSimplifier}.
 * @version 2.1.0
 * @since 2.0.0
 */
public class AdvancedSimplifierTest extends TestWithExampleFormulas {

    private final AdvancedSimplifier simplifier = new AdvancedSimplifier();

    @Test
    public void testConstants() {
        assertThat(this.f.falsum().transform(this.simplifier)).isEqualTo(this.f.falsum());
        assertThat(this.f.verum().transform(this.simplifier)).isEqualTo(this.f.verum());
    }

    @Test
    public void testCornerCases() {
        final FormulaCornerCases cornerCases = new FormulaCornerCases(this.f);
        cornerCases.cornerCases().forEach(this::computeAndVerify);
    }

    @Test
    @RandomTag
    public void testRandomized() {
        for (int i = 0; i < 100; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(8).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(5);
            computeAndVerify(formula);
        }
    }

    @Test
    public void testTimeoutHandlerSmall() throws ParserException {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 5_000L, TimeoutHandler.TimerType.FIXED_END)
        );
        final Formula formula = this.f.parse("a & b | ~c & a");
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formula, false);
        }
    }

    @Test
    public void testTimeoutHandlerLarge() throws ParserException, IOException {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 1L, TimeoutHandler.TimerType.FIXED_END)
        );
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", this.f);
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formula, true);
        }
    }

    @Test
    public void testPrimeCompilerIsCancelled() throws ParserException {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 0);
        final Formula formula = this.f.parse("a&(b|c)");
        testHandler(handler, formula, true);
    }

    @Test
    public void testSmusComputationIsCancelled() throws ParserException {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 5);
        final Formula formula = this.f.parse("a&(b|c)");
        testHandler(handler, formula, true);
    }

    @LongRunningTag
    @Test
    public void testCancellationPoints() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = f.parse("~v16 & ~v22 & ~v12 & (~v4 | ~v14) & (~v4 | ~v15) & (v3 | v4) & (v3 | ~v14) & (v3 | ~v15) " +
                "& (~v20 | ~v8) & (v9 | ~v20) & (~v21 | ~v8) & (v9 | ~v21) & (~v21 | ~v10) & (~v21 | ~v11) & v19");
        for (int numOptimizationStarts = 1; numOptimizationStarts < 30; numOptimizationStarts++) {
            for (int numSatHandlerStarts = 1; numSatHandlerStarts < 500; numSatHandlerStarts++) {
                final OptimizationHandler handler = new BoundedOptimizationHandler(numSatHandlerStarts, numOptimizationStarts);
                testHandler(handler, formula, true);
            }
        }
    }

    @Test
    public void testAdvancedSimplifierConfig(){
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
            AdvancedSimplifier advancedSimplifier = new AdvancedSimplifier(config);
            for (int i = 0; i < 10; i++) {
                FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
                Formula formula = randomizer.formula(3);
                Formula simplified = formula.transform(advancedSimplifier);
                assertThat(f.equivalence(formula, simplified).holds(new TautologyPredicate(f))).isTrue();
                assertThat(formula.toString().length()).isGreaterThanOrEqualTo(simplified.toString().length());
            }
        }
    }

    private void computeAndVerify(final Formula formula) {
        final Formula simplified = formula.transform(this.simplifier);
        assertThat(formula.factory().equivalence(formula, simplified).holds(new TautologyPredicate(this.f)))
                .as("Minimized formula is equivalent to original Formula")
                .isTrue();
    }

    private void testHandler(final OptimizationHandler handler, final Formula formula, final boolean expAborted) {
        final AdvancedSimplifier simplifierWithHandler = new AdvancedSimplifier(new DefaultRatingFunction(), handler);
        final Formula simplified = formula.transform(simplifierWithHandler);
        assertThat(handler.aborted()).isEqualTo(expAborted);
        if (expAborted) {
            assertThat(handler.aborted()).isTrue();
            assertThat(simplified).isNull();
        } else {
            assertThat(handler.aborted()).isFalse();
            assertThat(simplified).isNotNull();
        }
    }
}
