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
import static org.assertj.core.api.Assertions.fail;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for the class {@link PrimeCompiler}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class PrimeCompilerTest extends TestWithExampleFormulas {

    @Test
    public void testSimple() {
        computeAndVerify(this.TRUE);
        computeAndVerify(this.FALSE);
        computeAndVerify(this.A);
        computeAndVerify(this.NA);
        computeAndVerify(this.AND1);
        computeAndVerify(this.AND2);
        computeAndVerify(this.AND3);
        computeAndVerify(this.OR1);
        computeAndVerify(this.OR2);
        computeAndVerify(this.OR3);
        computeAndVerify(this.NOT1);
        computeAndVerify(this.NOT2);
        computeAndVerify(this.IMP1);
        computeAndVerify(this.IMP2);
        computeAndVerify(this.IMP3);
        computeAndVerify(this.IMP4);
        computeAndVerify(this.EQ1);
        computeAndVerify(this.EQ2);
        computeAndVerify(this.EQ3);
        computeAndVerify(this.EQ4);
        computeAndVerify(this.PBC1);
        computeAndVerify(this.PBC2);
        computeAndVerify(this.PBC3);
        computeAndVerify(this.PBC4);
        computeAndVerify(this.PBC5);
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        cornerCases.cornerCases().forEach(this::computeAndVerify);
    }

    @Test
    @RandomTag
    public void testRandomized() {
        for (int i = 0; i < 200; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(2).seed(i * 42).build());
            final Formula formula = randomizer.formula(4);
            computeAndVerify(formula);
        }
    }

    @Test
    public void testOriginalFormulas() throws IOException {
        Files.lines(Paths.get("src/test/resources/formulas/simplify_formulas.txt"))
                .filter(s -> !s.isEmpty())
                .forEach(s -> {
                    try {
                        final Formula formula = this.f.parse(s);
                        final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
                        verify(resultImplicantsMin, formula);
                        final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
                        verify(resultImplicatesMin, formula);
                    } catch (final ParserException e) {
                        fail(e.toString());
                    }
                });
    }

    private void computeAndVerify(final Formula formula) {
        final PrimeResult resultImplicantsMax = PrimeCompiler.getWithMaximization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        verify(resultImplicantsMax, formula);
        final PrimeResult resultImplicantsMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        verify(resultImplicantsMin, formula);
        assertThat(resultImplicantsMax.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMin.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICANTS_COMPLETE);
        assertThat(resultImplicantsMax.getPrimeImplicants()).containsExactlyInAnyOrderElementsOf(resultImplicantsMin.getPrimeImplicants());

        final PrimeResult resultImplicatesMax = PrimeCompiler.getWithMaximization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        verify(resultImplicatesMax, formula);
        final PrimeResult resultImplicatesMin = PrimeCompiler.getWithMinimization().compute(formula, PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        verify(resultImplicatesMin, formula);
        assertThat(resultImplicatesMax.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICATES_COMPLETE);
        assertThat(resultImplicatesMin.getCoverageType()).isEqualTo(PrimeResult.CoverageType.IMPLICATES_COMPLETE);
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
}
