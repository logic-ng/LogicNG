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

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Unit Tests for the class {@link SmusComputation}.
 * @version 2.1.0
 * @since 2.0.0
 */
public class SmusComputationTest extends TestWithExampleFormulas {

    @Test
    public void testFromPaper() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l"),
                this.f.parse("~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(this.f.parse("~s"), this.f.parse("s|~p"), this.f.parse("p"));
    }

    @Test
    public void testWithAdditionalConstraint() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l"),
                this.f.parse("~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(this.f.parse("n|l")), this.f);
        assertThat(result).containsExactlyInAnyOrder(this.f.parse("~n"), this.f.parse("~l"));
    }

    @Test
    public void testSatisfiable() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(this.f.parse("n|l")), this.f);
        assertThat(result).isNull();
    }

    @Test
    public void testUnsatisfiableAdditionalConstraints() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Arrays.asList(this.f.parse("~a&b"), this.f.parse("a|~b")), this.f);
        assertThat(result).isEmpty();
    }

    @Test
    public void testTrivialUnsatFormula() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l"),
                this.f.parse("~l"),
                this.f.parse("a&~a")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(this.f.parse("n|l")), this.f);
        assertThat(result).containsExactly(this.f.falsum());
    }

    @Test
    public void testUnsatFormula() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l"),
                this.f.parse("~l"),
                this.f.parse("(a<=>b)&(~a<=>b)")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.singletonList(this.f.parse("n|l")), this.f);
        assertThat(result).containsExactly(this.f.parse("(a<=>b)&(~a<=>b)"));
    }

    @Test
    public void testShorterConflict() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p"),
                this.f.parse("p&~s"),
                this.f.parse("~p|m"),
                this.f.parse("~m|n"),
                this.f.parse("~n"),
                this.f.parse("~m|l"),
                this.f.parse("~l")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(this.f.parse("s|~p"), this.f.parse("p&~s"));
    }

    @Test
    public void testCompleteConflict() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p|~m"),
                this.f.parse("m|~n"),
                this.f.parse("n|~l"),
                this.f.parse("l|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrderElementsOf(input);
    }

    @Test
    public void testLongConflictWithShortcut() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p|~m"),
                this.f.parse("m|~n"),
                this.f.parse("n|~l"),
                this.f.parse("l|s"),
                this.f.parse("n|s")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(this.f.parse("~s"),
                this.f.parse("s|~p"),
                this.f.parse("p|~m"),
                this.f.parse("m|~n"),
                this.f.parse("n|s"));
    }

    @Test
    public void testManyConflicts() throws ParserException {
        final List<Formula> input = Arrays.asList(
                this.f.parse("a"),
                this.f.parse("~a|b"),
                this.f.parse("~b|c"),
                this.f.parse("~c|~a"),
                this.f.parse("a1"),
                this.f.parse("~a1|b1"),
                this.f.parse("~b1|c1"),
                this.f.parse("~c1|~a1"),
                this.f.parse("a2"),
                this.f.parse("~a2|b2"),
                this.f.parse("~b2|c2"),
                this.f.parse("~c2|~a2"),
                this.f.parse("a3"),
                this.f.parse("~a3|b3"),
                this.f.parse("~b3|c3"),
                this.f.parse("~c3|~a3"),
                this.f.parse("a1|a2|a3|a4|b1|x|y"),
                this.f.parse("x&~y"),
                this.f.parse("x=>y")
        );
        final List<Formula> result = SmusComputation.computeSmusForFormulas(input, Collections.emptyList(), this.f);
        assertThat(result).containsExactlyInAnyOrder(this.f.parse("x&~y"), this.f.parse("x=>y"));
    }

    @Test
    public void testTimeoutHandlerSmall() throws ParserException {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(5_000L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 5_000L, TimeoutHandler.TimerType.FIXED_END)
        );
        final List<Formula> formulas = Arrays.asList(
                this.f.parse("a"),
                this.f.parse("~a")
        );
        for (final TimeoutOptimizationHandler handler : handlers) {
            testHandler(handler, formulas, false);
        }
    }

    @Test
    public void testTimeoutHandlerLarge() throws ParserException {
        final List<TimeoutOptimizationHandler> handlers = Arrays.asList(
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.SINGLE_TIMEOUT),
                new TimeoutOptimizationHandler(1L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT),
                new TimeoutOptimizationHandler(System.currentTimeMillis() + 1L, TimeoutHandler.TimerType.FIXED_END)
        );
        final List<Formula> formulas = Arrays.asList(
                this.f.parse("a"),
                this.f.parse("~a|b"),
                this.f.parse("~b|c"),
                this.f.parse("~c|~a"),
                this.f.parse("a1"),
                this.f.parse("~a1|b1"),
                this.f.parse("~b1|c1"),
                this.f.parse("~c1|~a1"),
                this.f.parse("a2"),
                this.f.parse("~a2|b2"),
                this.f.parse("~b2|c2"),
                this.f.parse("~c2|~a2"),
                this.f.parse("a3"),
                this.f.parse("~a3|b3"),
                this.f.parse("~b3|c3"),
                this.f.parse("~c3|~a3"),
                this.f.parse("a1|a2|a3|a4|b1|x|y"),
                this.f.parse("x&~y"),
                this.f.parse("x=>y")
        );
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
    public void testMinimumHittingSetCancelled() throws ParserException {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 0);
        final List<Formula> formulas = Arrays.asList(
                this.f.parse("a"),
                this.f.parse("~a")
        );
        testHandler(handler, formulas, true);
    }

    @Test
    public void testHSolverCancelled() throws ParserException {
        final OptimizationHandler handler = new BoundedOptimizationHandler(-1, 3);
        final List<Formula> formulas = Arrays.asList(
                this.f.parse("a"),
                this.f.parse("~a"),
                this.f.parse("c")
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
