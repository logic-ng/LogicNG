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

package org.logicng.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.predicates.satisfiability.TautologyPredicate;

import java.io.IOException;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link MinimumPrimeImplicantFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class MinimumPrimeImplicantTest {

    final FormulaFactory f = new FormulaFactory();

    public MinimumPrimeImplicantTest() {
        this.f.putConfiguration(CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.PURE).build());
    }

    @Test
    public void testSimpleCases() throws ParserException {
        Formula formula = this.f.parse("a");
        SortedSet<Literal> pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(1);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("a | b | c");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(1);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("a & b & (~a|~b)");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).isNull();

        formula = this.f.parse("a & b & c");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(3);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("a | b | ~c => e & d & f");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(3);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("a | b | ~c <=> e & d & f");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(4);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("(a | b | ~c <=> e & d & f) | (a | b | ~c => e & d & f)");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(3);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("(a | b | ~c <=> e & d & f) | (a | b | ~c => e & d & f) | (a & b)");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(2);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("(a | b | ~c <=> e & d & f) | (a | b | ~c => e & d & f) | (a & b) | (f => g)");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(1);
        isPrimeImplicant(formula, pi);
    }

    @Test
    public void testSmallExamples() throws ParserException {
        Formula formula = this.f.parse("(~(v17 | v18) | ~v1494 & (v17 | v18)) & ~v687 => v686");
        SortedSet<Literal> pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(1);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("(~(v17 | v18) | ~v1494 & (v17 | v18)) & v687 => ~v686");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(1);
        isPrimeImplicant(formula, pi);

        formula = this.f.parse("v173 + v174 + v451 + v258 + v317 + v259 + v452 + v453 + v175 + v176 + v177 + v178 + v179 + v180 + v181 + v182 + v183 + v102 + v103 + v104 + v105 = 1");
        pi = formula.apply(MinimumPrimeImplicantFunction.get());
        assertThat(pi).hasSize(21);
        isPrimeImplicant(formula, pi);
    }

    @Test
    public void testMiddleExamples() throws IOException, ParserException {
        final Formula parsed = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/formula1.txt", this.f);
        for (final Formula formula : parsed) {
            isPrimeImplicant(formula, formula.apply(MinimumPrimeImplicantFunction.get()));
        }
    }

    @Test
    public void testLargeExamples() throws IOException, ParserException {
        final Formula parsed = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", this.f);
        for (final Formula formula : parsed) {
            isPrimeImplicant(formula, formula.apply(MinimumPrimeImplicantFunction.get()));
        }
    }

    private void isPrimeImplicant(final Formula formula, final SortedSet<Literal> pi) {
        assertThat(this.f.implication(this.f.and(pi), formula).holds(new TautologyPredicate(this.f))).isTrue();
        for (final Literal literal : pi) {
            final TreeSet<Literal> newSet = new TreeSet<>(pi);
            newSet.remove(literal);
            if (!newSet.isEmpty()) {
                assertThat(this.f.implication(this.f.and(newSet), formula).holds(new TautologyPredicate(this.f))).isFalse();
            }
        }
    }

}
