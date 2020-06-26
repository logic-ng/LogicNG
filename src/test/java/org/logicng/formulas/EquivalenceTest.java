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
 * Unit Tests for the class {@link Equivalence}.
 * @version 2.0.0
 * @since 1.0
 */
public class EquivalenceTest {

    @Test
    public void testType() {
        assertThat(F.EQ1.type()).isEqualTo(FType.EQUIV);
    }

    @Test
    public void testCreator() {
        assertThat(F.f.equivalence(F.TRUE, F.AND1)).isEqualTo(F.AND1);
        assertThat(F.f.equivalence(F.AND1, F.TRUE)).isEqualTo(F.AND1);
        assertThat(F.f.equivalence(F.FALSE, F.AND1)).isEqualTo(F.NOT1);
        assertThat(F.f.equivalence(F.AND1, F.FALSE)).isEqualTo(F.NOT1);
        assertThat(F.f.equivalence(F.OR1, F.OR1)).isEqualTo(F.TRUE);
        assertThat(F.f.equivalence(F.NOT1, F.AND1)).isEqualTo(F.FALSE);
        assertThat(F.f.equivalence(F.AND1, F.NOT1)).isEqualTo(F.FALSE);
        assertThat(F.f.equivalence(F.OR1, F.NOT2)).isEqualTo(F.FALSE);
        assertThat(F.f.equivalence(F.NOT2, F.OR1)).isEqualTo(F.FALSE);
        assertThat(F.f.binaryOperator(FType.EQUIV, F.AND1, F.OR1)).isEqualTo(F.EQ3);
    }

    @Test
    public void testGetters() {
        assertThat(((Equivalence) F.EQ2).left()).isEqualTo(F.NA);
        assertThat(((Equivalence) F.EQ2).right()).isEqualTo(F.NB);
    }

    @Test
    public void testVariables() {
        assertThat(F.IMP3.variables().size()).isEqualTo(4);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(F.IMP3.variables()).isEqualTo(lits);

        final Formula equiv = F.f.equivalence(F.AND1, F.AND2);
        assertThat(equiv.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(equiv.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(F.IMP3.literals().size()).isEqualTo(4);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(F.IMP3.literals()).isEqualTo(lits);

        Formula equiv = F.f.equivalence(F.AND1, F.AND2);
        assertThat(equiv.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.NA, F.NB));
        assertThat(equiv.literals()).isEqualTo(lits);

        equiv = F.f.equivalence(F.AND1, F.A);
        assertThat(equiv.literals().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(equiv.literals()).isEqualTo(lits);
    }

    @Test
    public void testNegation() {
        assertThat(F.EQ1.negate()).isEqualTo(F.f.not(F.EQ1));
        assertThat(F.EQ2.negate()).isEqualTo(F.f.not(F.EQ2));
        assertThat(F.EQ3.negate()).isEqualTo(F.f.not(F.EQ3));
        assertThat(F.EQ4.negate()).isEqualTo(F.f.not(F.EQ4));
    }

    @Test
    public void testToString() {
        assertThat(F.EQ1.toString()).isEqualTo("a <=> b");
        assertThat(F.EQ2.toString()).isEqualTo("~a <=> ~b");
        assertThat(F.EQ3.toString()).isEqualTo("a & b <=> x | y");
        assertThat(F.EQ4.toString()).isEqualTo("a => b <=> ~a => ~b");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.equivalence(F.A, F.B)).isEqualTo(F.EQ1);
        assertThat(F.f.equivalence(F.B, F.A)).isEqualTo(F.EQ1);
        assertThat(F.f.equivalence(F.AND1, F.OR1)).isEqualTo(F.EQ3);
        assertThat(F.EQ4).isEqualTo(F.EQ4);
        assertThat(F.EQ2).isNotEqualTo(F.EQ1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.equivalence(F.g.variable("a"), F.g.variable("b"))).isEqualTo(F.EQ1);
        assertThat(F.g.equivalence(F.B, F.A)).isEqualTo(F.EQ1);
        assertThat(F.g.equivalence(F.AND1, F.OR1)).isEqualTo(F.EQ3);
        assertThat(F.g.equivalence(F.g.literal("a", false), F.g.variable("b"))).isNotEqualTo(F.EQ1);
        assertThat(F.g.equivalence(F.g.variable("a"), F.g.literal("b", false))).isNotEqualTo(F.EQ1);
    }

    @Test
    public void testHash() {
        final Formula eq = F.f.equivalence(F.IMP1, F.IMP2);
        assertThat(eq.hashCode()).isEqualTo(F.EQ4.hashCode());
        assertThat(eq.hashCode()).isEqualTo(F.EQ4.hashCode());
        assertThat(F.f.equivalence(F.AND1, F.OR1).hashCode()).isEqualTo(F.EQ3.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.EQ1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.EQ4.numberOfAtoms()).isEqualTo(4);
        assertThat(F.EQ4.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.EQ1.numberOfNodes()).isEqualTo(3);
        assertThat(F.EQ4.numberOfNodes()).isEqualTo(7);
        assertThat(F.EQ4.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula eq = new PropositionalParser(F.f).parse("a & (b | c) <=> (d => (b | c))");
        assertThat(F.EQ4.numberOfInternalNodes()).isEqualTo(7);
        assertThat(eq.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.EQ1.numberOfOperands()).isEqualTo(2);
        assertThat(F.EQ3.numberOfOperands()).isEqualTo(2);
        assertThat(F.EQ4.numberOfOperands()).isEqualTo(2);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.EQ1.isConstantFormula()).isFalse();
        assertThat(F.EQ2.isConstantFormula()).isFalse();
        assertThat(F.EQ3.isConstantFormula()).isFalse();
        assertThat(F.EQ4.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.EQ1.isAtomicFormula()).isFalse();
        assertThat(F.EQ4.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(F.EQ4.containsVariable(F.f.variable("a"))).isTrue();
        assertThat(F.EQ4.containsVariable(F.f.variable("x"))).isFalse();
        assertThat(F.EQ4.containsNode(F.IMP1)).isTrue();
        assertThat(F.EQ4.containsNode(F.IMP4)).isFalse();
    }
}
