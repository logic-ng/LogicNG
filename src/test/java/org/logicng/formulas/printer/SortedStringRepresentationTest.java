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

import org.junit.Test;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Java6Assertions.assertThat;

/**
 * Unit tests for {@link SortedStringRepresentation}
 *
 * @version 1.5
 * @since 1.5
 */
public class SortedStringRepresentationTest {

    private final List<Variable> varOrder = new ArrayList<>(Arrays.asList(F.Y, F.X, F.B, F.A, F.C));

    private final FormulaStringRepresentation sr = new SortedStringRepresentation(this.varOrder);

    @Test
    public void testFormulaComparator() {
        final SortedStringRepresentation.FormulaComparator comparator = new SortedStringRepresentation.FormulaComparator(this.varOrder);
        assertThat(comparator.compare(F.FALSE, F.TRUE)).isZero();
        assertThat(comparator.compare(F.A, F.A)).isZero();
        assertThat(comparator.compare(F.A, F.B)).isPositive();
        assertThat(comparator.compare(F.A, F.C)).isNegative();
        assertThat(comparator.compare(F.A, F.AND1)).isPositive();
        assertThat(comparator.compare(F.B, F.AND1)).isNegative();
        assertThat(comparator.compare(F.EQ4, F.IMP4)).isPositive();
        assertThat(comparator.compare(F.AND3, F.IMP4)).isNegative();
        assertThat(comparator.compare(F.AND3, F.IMP4)).isNegative();
        assertThat(comparator.compare(F.NOT1, F.NOT2)).isPositive();
        assertThat(comparator.compare(F.OR1, F.PBC2)).isNegative();
        assertThat(comparator.compare(F.PBC1, F.PBC2)).isZero();
    }

    @Test
    public void testSortedPrinter() {
        assertThat(F.f.string(F.FALSE, this.sr)).isEqualTo("$false");
        assertThat(F.f.string(F.TRUE, this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.X, this.sr)).isEqualTo("x");
        assertThat(F.f.string(F.NA, this.sr)).isEqualTo("~a");
        assertThat(F.f.string(F.IMP2, this.sr)).isEqualTo("~a => ~b");
        assertThat(F.f.string(F.f.not(F.f.equivalence(F.A, F.B)), this.sr)).isEqualTo("~(b <=> a)");
        assertThat(F.f.string(F.IMP3, this.sr)).isEqualTo("b & a => y | x");
        assertThat(F.f.string(F.EQ3, this.sr)).isEqualTo("y | x <=> b & a");
        assertThat(F.f.string(F.EQ4, this.sr)).isEqualTo("a => b <=> ~a => ~b");
        assertThat(F.f.string(F.AND3, this.sr)).isEqualTo("(y | x) & (~y | ~x)");
        assertThat(F.f.string(F.f.and(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("x & b & a & c");
        assertThat(F.f.string(F.f.or(F.A, F.B, F.C, F.X), this.sr)).isEqualTo("x | b | a | c");
        assertThat(F.f.string(F.PBC2, this.sr)).isEqualTo("3*x + -4*b + 2*a > 2");
        assertThat(F.f.string(F.f.and(F.NB, F.PBC1), this.sr)).isEqualTo("(3*x + -4*b + 2*a = 2) & ~b");
        assertThat(F.f.string(F.f.pbc(CType.EQ, 42, new ArrayList<Literal>(Arrays.asList(F.A, F.B)), new ArrayList<>(Arrays.asList(1, 1))), this.sr)).isEqualTo("b + a = 42");
        assertThat(F.f.string(F.f.pbc(CType.LT, 42, new ArrayList<Literal>(), new ArrayList<Integer>()), this.sr)).isEqualTo("$true");
        assertThat(F.f.string(F.f.pbc(CType.EQ, 42, new ArrayList<Literal>(), new ArrayList<Integer>()), this.sr)).isEqualTo("$false");

        // some variables not in varOrder
        this.varOrder.remove(F.X);
        assertThat(F.f.string(F.f.or(F.f.variable("x"), F.f.variable("a")), this.sr)).isEqualTo("a | x");
        assertThat(F.f.string(F.PBC2, this.sr)).isEqualTo("-4*b + 2*a + 3*x > 2");

        // empty varOrder
        assertThat(F.f.string(F.EQ3, new SortedStringRepresentation(new ArrayList<Variable>()))).isEqualTo("a & b <=> x | y");
    }

    @Test
    public void testToString() {
        assertThat(this.sr.toString()).isEqualTo("SortedStringRepresentation");
    }
}
