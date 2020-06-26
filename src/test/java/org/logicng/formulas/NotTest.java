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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Not}.
 * @version 2.0.0
 * @since 1.0
 */
public class NotTest {

    @Test
    public void testType() {
        assertThat(F.NOT1.type()).isEqualTo(FType.NOT);
    }

    @Test
    public void testCreator() {
        assertThat(F.f.not(F.FALSE)).isEqualTo(F.TRUE);
        assertThat(F.f.not(F.TRUE)).isEqualTo(F.FALSE);
        assertThat(F.f.not(F.NA)).isEqualTo(F.A);
        assertThat(F.f.not(F.A)).isEqualTo(F.NA);
        assertThat(F.f.not(F.f.not(F.IMP3))).isEqualTo(F.IMP3);
        assertThat(F.f.not(F.AND1)).isEqualTo(F.NOT1);
    }

    @Test
    public void testGetters() {
        assertThat(((Not) F.NOT1).operand()).isEqualTo(F.AND1);
        assertThat(((Not) F.NOT2).operand()).isEqualTo(F.OR1);
    }

    @Test
    public void testVariables() {
        assertThat(F.NOT1.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(F.NOT1.variables()).isEqualTo(lits);

        assertThat(F.NOT2.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(F.X, F.Y));
        assertThat(F.NOT2.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(F.NOT1.literals().size()).isEqualTo(2);
        SortedSet<? extends Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(F.NOT1.literals()).isEqualTo(lits);

        final Formula not = F.f.not(F.f.and(F.A, F.NB, F.f.implication(F.B, F.NA)));
        assertThat(not.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
        assertThat(not.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        assertThat(F.NOT1.toString()).isEqualTo("~(a & b)");
        assertThat(F.NOT2.toString()).isEqualTo("~(x | y)");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.not(F.AND1)).isEqualTo(F.NOT1);
        assertThat(F.f.not(F.OR1)).isEqualTo(F.NOT2);
        assertThat(F.NOT1).isEqualTo(F.NOT1);
        assertThat(F.NOT2).isNotEqualTo(F.NOT1);
        assertThat(F.NOT2).isNotEqualTo("String");
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.not(F.AND1)).isEqualTo(F.NOT1);
        assertThat(F.g.not(F.g.or(F.g.variable("x"), F.g.variable("y")))).isEqualTo(F.NOT2);
        assertThat(F.g.not(F.g.or(F.g.variable("a"), F.g.variable("b")))).isNotEqualTo(F.NOT2);
    }

    @Test
    public void testHash() {
        final Formula not = F.f.not(F.AND1);
        assertThat(not.hashCode()).isEqualTo(F.NOT1.hashCode());
        assertThat(not.hashCode()).isEqualTo(F.NOT1.hashCode());
        assertThat(F.f.not(F.OR1).hashCode()).isEqualTo(F.NOT2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.NOT1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.NOT1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.NOT2.numberOfAtoms()).isEqualTo(2);
        assertThat(F.OR1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.OR1.numberOfAtoms()).isEqualTo(2);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.NOT1.numberOfNodes()).isEqualTo(4);
        assertThat(F.NOT2.numberOfNodes()).isEqualTo(4);
        assertThat(F.NOT2.numberOfNodes()).isEqualTo(4);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula eq = new PropositionalParser(F.f).parse("a & (b | c) <=> ~(d => (b | c))");
        assertThat(F.NOT1.numberOfInternalNodes()).isEqualTo(4);
        assertThat(eq.numberOfInternalNodes()).isEqualTo(9);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.NOT1.numberOfOperands()).isEqualTo(1);
        assertThat(F.f.not(F.EQ1).numberOfOperands()).isEqualTo(1);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.NOT1.isConstantFormula()).isFalse();
        assertThat(F.NOT2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.NOT1.isAtomicFormula()).isFalse();
        assertThat(F.NOT2.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(F.NOT1.containsVariable(F.f.variable("a"))).isTrue();
        assertThat(F.NOT1.containsVariable(F.f.variable("x"))).isFalse();
    }
}
