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

package org.logicng.formulas.printer;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for {@link SortedStringRepresentation}
 * @version 2.0.0
 * @since 1.5
 */
public class SortedStringRepresentationTest extends TestWithExampleFormulas {

    private final List<Variable> varOrder = new ArrayList<>(Arrays.asList(this.Y, this.X, this.B, this.A, this.C));

    private final FormulaStringRepresentation sr = new SortedStringRepresentation(this.varOrder);

    @Test
    public void testFormulaComparator() {
        final SortedStringRepresentation.FormulaComparator comparator = new SortedStringRepresentation.FormulaComparator(this.varOrder);
        assertThat(comparator.compare(this.FALSE, this.TRUE)).isZero();
        assertThat(comparator.compare(this.A, this.A)).isZero();
        assertThat(comparator.compare(this.A, this.B)).isPositive();
        assertThat(comparator.compare(this.A, this.C)).isNegative();
        assertThat(comparator.compare(this.A, this.AND1)).isPositive();
        assertThat(comparator.compare(this.B, this.AND1)).isNegative();
        assertThat(comparator.compare(this.EQ4, this.IMP4)).isPositive();
        assertThat(comparator.compare(this.AND3, this.IMP4)).isNegative();
        assertThat(comparator.compare(this.AND3, this.IMP4)).isNegative();
        assertThat(comparator.compare(this.NOT1, this.NOT2)).isPositive();
        assertThat(comparator.compare(this.OR1, this.PBC2)).isNegative();
        assertThat(comparator.compare(this.PBC1, this.PBC2)).isZero();
    }

    @Test
    public void testSortedPrinter() {
        assertThat(this.f.string(this.FALSE, this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.TRUE, this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.X, this.sr)).isEqualTo("x");
        assertThat(this.f.string(this.NA, this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.IMP2, this.sr)).isEqualTo("~a => ~b");
        assertThat(this.f.string(this.f.not(this.f.equivalence(this.A, this.B)), this.sr)).isEqualTo("~(b <=> a)");
        assertThat(this.f.string(this.IMP3, this.sr)).isEqualTo("b & a => y | x");
        assertThat(this.f.string(this.EQ3, this.sr)).isEqualTo("y | x <=> b & a");
        assertThat(this.f.string(this.EQ4, this.sr)).isEqualTo("a => b <=> ~a => ~b");
        assertThat(this.f.string(this.AND3, this.sr)).isEqualTo("(y | x) & (~y | ~x)");
        assertThat(this.f.string(this.f.and(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("x & b & a & c");
        assertThat(this.f.string(this.f.or(this.A, this.B, this.C, this.X), this.sr)).isEqualTo("x | b | a | c");
        assertThat(this.f.string(this.PBC2, this.sr)).isEqualTo("3*x + -4*b + 2*a > 2");
        assertThat(this.f.string(this.f.and(this.NB, this.PBC1), this.sr)).isEqualTo("(3*x + -4*b + 2*a = 2) & ~b");
        assertThat(this.f.string(this.f.pbc(CType.EQ, 42, new ArrayList<Literal>(Arrays.asList(this.A, this.B)), new ArrayList<>(Arrays.asList(1, 1))), this.sr)).isEqualTo("b + a = 42");
        assertThat(this.f.string(this.f.pbc(CType.LT, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.pbc(CType.EQ, 42, new ArrayList<>(), new ArrayList<>()), this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.f.implication(this.A, this.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.exo()), this.sr)).isEqualTo("~a");
        assertThat(this.f.string(this.f.and(this.A, this.f.exo()), this.sr)).isEqualTo("$false");
        assertThat(this.f.string(this.f.or(this.A, this.f.exo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.implication(this.A, this.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.equivalence(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.and(this.A, this.f.amo()), this.sr)).isEqualTo("a");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo()), this.sr)).isEqualTo("$true");
        assertThat(this.f.string(this.f.or(this.A, this.f.amo(), this.f.exo(), this.f.equivalence(this.f.amo(), this.B)), this.sr)).isEqualTo("$true");

        // some variables not in varOrder
        this.varOrder.remove(this.X);
        assertThat(this.f.string(this.f.or(this.f.variable("x"), this.f.variable("a")), this.sr)).isEqualTo("a | x");
        assertThat(this.f.string(this.PBC2, this.sr)).isEqualTo("-4*b + 2*a + 3*x > 2");

        // empty varOrder
        assertThat(this.f.string(this.EQ3, new SortedStringRepresentation(new ArrayList<>()))).isEqualTo("a & b <=> x | y");
    }

    @Test
    public void testToString() {
        assertThat(this.sr.toString()).isEqualTo("SortedStringRepresentation");
    }

    @Test
    public void testViaFormulaFactoryConfig() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().stringRepresentation(() -> this.sr).build());
        assertThat(f.importFormula(this.EQ4).toString()).isEqualTo("a => b <=> ~a => ~b");
    }
}
