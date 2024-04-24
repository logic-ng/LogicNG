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

package org.logicng.explanations.smus;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.handlers.BoundedOptimizationHandler;
import org.logicng.handlers.OptimizationHandler;
import org.logicng.handlers.TimeoutHandler;
import org.logicng.handlers.TimeoutOptimizationHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.DimacsReader;
import org.logicng.io.readers.FormulaReader;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Unit Tests for the class {@link SmusComputation}.
 * @version 2.1.0
 * @since 2.0.0
 */
public class SmusComputationTest extends TestWithExampleFormulas {

    @Test
    public void testFromPaper() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l"),
                parse(this.f, "~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(parse(this.f, "~s"), parse(this.f, "s|~p"), parse(this.f, "p"));
    }

    @Test
    public void testWithAdditionalConstraint() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l"),
                parse(this.f, "~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(parse(this.f, "n|l")), this.f);
        assertThat(result).containsExactlyInAnyOrder(parse(this.f, "~n"), parse(this.f, "~l"));
    }

    @Test
    public void testSatisfiable() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(parse(this.f, "n|l")), this.f);
        assertThat(result).isNull();
    }

    @Test
    public void testUnsatisfiableAdditionalConstraints() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Arrays.asList(parse(this.f, "~a&b"), parse(this.f, "a|~b")), this.f);
        assertThat(result).isEmpty();
    }

    @Test
    public void testTrivialUnsatFormula() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l"),
                parse(this.f, "~l"),
                parse(this.f, "a&~a")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(parse(this.f, "n|l")), this.f);
        assertThat(result).containsExactly(this.f.falsum());
    }

    @Test
    public void testUnsatFormula() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l"),
                parse(this.f, "~l"),
                parse(this.f, "(a<=>b)&(~a<=>b)")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(parse(this.f, "n|l")), this.f);
        assertThat(result).containsExactly(parse(this.f, "(a<=>b)&(~a<=>b)"));
    }

    @Test
    public void testShorterConflict() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p"),
                parse(this.f, "p&~s"),
                parse(this.f, "~p|m"),
                parse(this.f, "~m|n"),
                parse(this.f, "~n"),
                parse(this.f, "~m|l"),
                parse(this.f, "~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(parse(this.f, "s|~p"), parse(this.f, "p&~s"));
    }

    @Test
    public void testCompleteConflict() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p|~m"),
                parse(this.f, "m|~n"),
                parse(this.f, "n|~l"),
                parse(this.f, "l|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrderElementsOf(input);
    }

    @Test
    public void testLongConflictWithShortcut() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p|~m"),
                parse(this.f, "m|~n"),
                parse(this.f, "n|~l"),
                parse(this.f, "l|s"),
                parse(this.f, "n|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(parse(this.f, "~s"),
                parse(this.f, "s|~p"),
                parse(this.f, "p|~m"),
                parse(this.f, "m|~n"),
                parse(this.f, "n|s"));
    }

    @Test
    public void testManyConflicts() {
        final List<Formula> input = Arrays.asList(
                parse(this.f, "a"),
                parse(this.f, "~a|b"),
                parse(this.f, "~b|c"),
                parse(this.f, "~c|~a"),
                parse(this.f, "a1"),
                parse(this.f, "~a1|b1"),
                parse(this.f, "~b1|c1"),
                parse(this.f, "~c1|~a1"),
                parse(this.f, "a2"),
                parse(this.f, "~a2|b2"),
                parse(this.f, "~b2|c2"),
                parse(this.f, "~c2|~a2"),
                parse(this.f, "a3"),
                parse(this.f, "~a3|b3"),
                parse(this.f, "~b3|c3"),
                parse(this.f, "~c3|~a3"),
                parse(this.f, "a1|a2|a3|a4|b1|x|y"),
                parse(this.f, "x&~y"),
                parse(this.f, "x=>y")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(parse(this.f, "x&~y"), parse(this.f, "x=>y"));
    }

    @Test
    public void testTimeoutHandlerSmall() {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 5_000L, TimeoutHandler.TimerType.FIXED_END)
        );
        final List<Formula> formulas = Arrays.asList(
                parse(this.f, "a"),
                parse(this.f, "~a")
        );
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formulas, false);
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
        final List<Formula> formulas = formula.stream().collect(Collectors.toList());
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formulas, true);
        }
    }

    @Test
    public void testCancellationPoints() throws IOException {
        final List<Formula> formulas = DimacsReader.readCNF("src/test/resources/sat/unsat/bf0432-007.cnf", this.f);
        for (int numOptimizationStarts = 1; numOptimizationStarts < 5; numOptimizationStarts++) {
            for (int numSatHandlerStarts = 1; numSatHandlerStarts < 10; numSatHandlerStarts++) {
                final OptimizationHandler handler = new BoundedOptimizationHandler(numSatHandlerStarts, numOptimizationStarts);
                testHandler(handler, formulas, true);
            }
        }
    }

    @Test
    public void testMinimumHittingSetCancelled() {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 0);
        final List<Formula> formulas = Arrays.asList(
                parse(this.f, "a"),
                parse(this.f, "~a")
        );
        testHandler(handler, formulas, true);
    }

    @Test
    public void testHSolverCancelled() {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 3);
        final List<Formula> formulas = Arrays.asList(
                parse(this.f, "a"),
                parse(this.f, "~a"),
                parse(this.f, "c")
        );
        testHandler(handler, formulas, true);
    }

    private void testHandler(final OptimizationHandler handler, final List<Formula> formulas, final boolean expAborted) {
        final List<Formula> result = SmusComputation.computeSmusForFormulas(formulas, Collections.emptyList(), this.f, handler);
        assertThat(handler.aborted()).isEqualTo(expAborted);
        if (expAborted) {
            assertThat(result).isNull();
        } else {
            assertThat(result).isNotNull();
        }
    }
}
