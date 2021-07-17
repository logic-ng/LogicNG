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

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.BoundedSatHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.MiniSat;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link NaivePrimeReduction}.
 * @version 2.1.0
 * @since 2.0.0
 */
public class PrimeImplicantReductionTest extends TestWithExampleFormulas {

    @Test
    public void testSimple() throws ParserException {
        final NaivePrimeReduction naiveTautology = new NaivePrimeReduction(this.f.verum());
        assertThat(naiveTautology.reduceImplicant(new TreeSet<>(Arrays.asList(this.A, this.B)))).isEmpty();

        final NaivePrimeReduction naive01 = new NaivePrimeReduction(this.f.parse("a&b|c&d"));
        assertThat(naive01.reduceImplicant(new TreeSet<>(Arrays.asList(this.A, this.B, this.C, this.D.negate()))))
                .containsExactlyInAnyOrder(this.A, this.B);
        assertThat(naive01.reduceImplicant(new TreeSet<>(Arrays.asList(this.A.negate(), this.B, this.C, this.D))))
                .containsExactlyInAnyOrder(this.C, this.D);

        final NaivePrimeReduction naive02 = new NaivePrimeReduction(this.f.parse("a|b|~a&~b"));
        assertThat(naive02.reduceImplicant(new TreeSet<>(Arrays.asList(this.A.negate(), this.B))))
                .containsExactlyInAnyOrder();
        assertThat(naive02.reduceImplicant(new TreeSet<>(Arrays.asList(this.A.negate(), this.B))))
                .containsExactlyInAnyOrder();

        final NaivePrimeReduction naive03 = new NaivePrimeReduction(this.f.parse("(a => b) | b | c"));
        assertThat(naive03.reduceImplicant(new TreeSet<>(Arrays.asList(this.A, this.B, this.C.negate()))))
                .containsExactlyInAnyOrder(this.B);
        assertThat(naive03.reduceImplicant(new TreeSet<>(Arrays.asList(this.A, this.B.negate(), this.C))))
                .containsExactlyInAnyOrder(this.C);
    }

    @Test
    public void testFormula1() throws IOException, ParserException {
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/formula1.txt", this.f);
        testFormula(formula);
    }

    @Test
    public void testSimplifyFormulas() throws IOException, ParserException {
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/simplify_formulas.txt", this.f);
        testFormula(formula);
    }

    @Test
    public void testLargeFormula() throws IOException, ParserException {
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", this.f);
        testFormula(formula);
    }

    @Test
    public void testSmallFormulas() throws IOException, ParserException {
        final List<String> lines = Files.readAllLines(Paths.get("src/test/resources/formulas/small_formulas.txt"));
        for (final String line : lines) {
            testFormula(this.f.parse(line));
        }
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        cornerCases.cornerCases().forEach(this::testFormula);
    }

    @Test
    @RandomTag
    public void testRandom() {
        for (int i = 0; i < 500; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(20).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(4);
            testFormula(formula);
        }
    }

    @Test
    public void testCancellationPoints() throws ParserException, IOException {
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", this.f);
        for (int numStarts = 0; numStarts < 20; numStarts++) {
            final SATHandler handler = new BoundedSatHandler(numStarts);
            testFormula(formula, handler, true);
        }
    }

    private void testFormula(final Formula formula) {
        testFormula(formula, null, false);
    }

    private void testFormula(final Formula formula, final SATHandler handler, final boolean expAborted) {
        final FormulaFactory f = formula.factory();
        final MiniSat solver = MiniSat.miniSat(f);
        solver.add(formula);
        final boolean isSAT = solver.sat() == Tristate.TRUE;
        if (!isSAT) {
            return;
        }
        final SortedSet<Literal> model = solver.model().literals();
        final NaivePrimeReduction naive = new NaivePrimeReduction(formula);
        final SortedSet<Literal> primeImplicant = naive.reduceImplicant(model, handler);
        if (expAborted) {
            assertThat(handler.aborted()).isTrue();
            assertThat(primeImplicant).isNull();
        } else {
            assertThat(model).containsAll(primeImplicant);
            testPrimeImplicantProperty(formula, primeImplicant);
        }
    }

    public static void testPrimeImplicantProperty(final Formula formula, final SortedSet<Literal> primeImplicant) {
        final FormulaFactory f = formula.factory();
        final MiniSat solver = MiniSat.miniSat(f);
        solver.add(formula.negate());
        assertThat(solver.sat(primeImplicant)).isEqualTo(Tristate.FALSE);
        for (final Literal lit : primeImplicant) {
            final SortedSet<Literal> reducedPrimeImplicant = new TreeSet<>(primeImplicant);
            reducedPrimeImplicant.remove(lit);
            assertThat(solver.sat(reducedPrimeImplicant)).isEqualTo(Tristate.TRUE);
        }
    }
}
