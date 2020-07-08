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
import org.logicng.TestWithExampleFormulas;
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
public class NotTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.NOT1.type()).isEqualTo(FType.NOT);
    }

    @Test
    public void testCreator() {
        assertThat(this.f.not(this.FALSE)).isEqualTo(this.TRUE);
        assertThat(this.f.not(this.TRUE)).isEqualTo(this.FALSE);
        assertThat(this.f.not(this.NA)).isEqualTo(this.A);
        assertThat(this.f.not(this.A)).isEqualTo(this.NA);
        assertThat(this.f.not(this.f.not(this.IMP3))).isEqualTo(this.IMP3);
        assertThat(this.f.not(this.AND1)).isEqualTo(this.NOT1);
    }

    @Test
    public void testGetters() {
        assertThat(((Not) this.NOT1).operand()).isEqualTo(this.AND1);
        assertThat(((Not) this.NOT2).operand()).isEqualTo(this.OR1);
    }

    @Test
    public void testVariables() {
        assertThat(this.NOT1.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(this.NOT1.variables()).isEqualTo(lits);

        assertThat(this.NOT2.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(this.X, this.Y));
        assertThat(this.NOT2.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(this.NOT1.literals().size()).isEqualTo(2);
        SortedSet<? extends Literal> lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(this.NOT1.literals()).isEqualTo(lits);

        final Formula not = this.f.not(this.f.and(this.A, this.NB, this.f.implication(this.B, this.NA)));
        assertThat(not.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.NA, this.B, this.NB));
        assertThat(not.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        assertThat(this.NOT1.toString()).isEqualTo("~(a & b)");
        assertThat(this.NOT2.toString()).isEqualTo("~(x | y)");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.not(this.AND1)).isEqualTo(this.NOT1);
        assertThat(this.f.not(this.OR1)).isEqualTo(this.NOT2);
        assertThat(this.NOT1).isEqualTo(this.NOT1);
        assertThat(this.NOT2).isNotEqualTo(this.NOT1);
        assertThat(this.NOT2).isNotEqualTo("String");
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(g.not(this.AND1)).isEqualTo(this.NOT1);
        assertThat(g.not(g.or(g.variable("x"), g.variable("y")))).isEqualTo(this.NOT2);
        assertThat(g.not(g.or(g.variable("a"), g.variable("b")))).isNotEqualTo(this.NOT2);
    }

    @Test
    public void testHash() {
        final Formula not = this.f.not(this.AND1);
        assertThat(not.hashCode()).isEqualTo(this.NOT1.hashCode());
        assertThat(not.hashCode()).isEqualTo(this.NOT1.hashCode());
        assertThat(this.f.not(this.OR1).hashCode()).isEqualTo(this.NOT2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.NOT1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.NOT1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.NOT2.numberOfAtoms()).isEqualTo(2);
        assertThat(this.OR1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.OR1.numberOfAtoms()).isEqualTo(2);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.NOT1.numberOfNodes()).isEqualTo(4);
        assertThat(this.NOT2.numberOfNodes()).isEqualTo(4);
        assertThat(this.NOT2.numberOfNodes()).isEqualTo(4);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula eq = new PropositionalParser(this.f).parse("a & (b | c) <=> ~(d => (b | c))");
        assertThat(this.NOT1.numberOfInternalNodes()).isEqualTo(4);
        assertThat(eq.numberOfInternalNodes()).isEqualTo(9);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.NOT1.numberOfOperands()).isEqualTo(1);
        assertThat(this.f.not(this.EQ1).numberOfOperands()).isEqualTo(1);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.NOT1.isConstantFormula()).isFalse();
        assertThat(this.NOT2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.NOT1.isAtomicFormula()).isFalse();
        assertThat(this.NOT2.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(this.NOT1.containsVariable(this.f.variable("a"))).isTrue();
        assertThat(this.NOT1.containsVariable(this.f.variable("x"))).isFalse();
    }
}
