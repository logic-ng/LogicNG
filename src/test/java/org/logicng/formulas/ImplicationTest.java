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
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Implication}.
 * @version 2.0.0
 * @since 1.0
 */
public class ImplicationTest {

    @Test
    public void testType() {
        assertThat(F.IMP1.type()).isEqualTo(FType.IMPL);
    }

    @Test
    public void testCreator() {
        assertThat(F.f.implication(F.FALSE, F.A)).isEqualTo(F.TRUE);
        assertThat(F.f.implication(F.A, F.TRUE)).isEqualTo(F.TRUE);
        assertThat(F.f.implication(F.TRUE, F.A)).isEqualTo(F.A);
        assertThat(F.f.implication(F.A, F.FALSE)).isEqualTo(F.NA);
        assertThat(F.f.implication(F.A, F.A)).isEqualTo(F.TRUE);
        assertThat(F.f.binaryOperator(FType.IMPL, F.AND1, F.OR1)).isEqualTo(F.IMP3);
    }

    @Test
    public void testIllegalCreation() {
        assertThatThrownBy(() -> F.f.binaryOperator(FType.NOT, F.AND1, F.OR1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testGetters() {
        assertThat(((Implication) F.IMP2).left()).isEqualTo(F.NA);
        assertThat(((Implication) F.IMP2).right()).isEqualTo(F.NB);
    }

    @Test
    public void testVariables() {
        assertThat(F.IMP3.variables().size()).isEqualTo(4);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(F.IMP3.variables()).isEqualTo(lits);

        final Formula imp = F.f.implication(F.AND1, F.AND2);
        assertThat(imp.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(imp.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(F.IMP3.literals().size()).isEqualTo(4);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(F.IMP3.literals()).isEqualTo(lits);

        Formula imp = F.f.implication(F.AND1, F.AND2);
        assertThat(imp.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.NA, F.NB));
        assertThat(imp.literals()).isEqualTo(lits);

        imp = F.f.implication(F.AND1, F.A);
        assertThat(imp.literals().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(imp.literals()).isEqualTo(lits);
    }

    @Test
    public void testNegation() {
        assertThat(F.IMP1.negate()).isEqualTo(F.f.not(F.IMP1));
        assertThat(F.IMP2.negate()).isEqualTo(F.f.not(F.IMP2));
        assertThat(F.IMP3.negate()).isEqualTo(F.f.not(F.IMP3));
        assertThat(F.IMP4.negate()).isEqualTo(F.f.not(F.IMP4));
    }

    @Test
    public void testToString() {
        assertThat(F.IMP1.toString()).isEqualTo("a => b");
        assertThat(F.IMP2.toString()).isEqualTo("~a => ~b");
        assertThat(F.IMP3.toString()).isEqualTo("a & b => x | y");
        assertThat(F.IMP4.toString()).isEqualTo("(a <=> b) => (~x <=> ~y)");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.implication(F.A, F.B)).isEqualTo(F.IMP1);
        assertThat(F.f.implication(F.AND1, F.OR1)).isEqualTo(F.IMP3);
        assertThat(F.IMP2).isEqualTo(F.IMP2);
        assertThat(F.IMP2).isNotEqualTo(F.IMP1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.implication(F.g.variable("a"), F.g.variable("b"))).isEqualTo(F.IMP1);
        assertThat(F.g.implication(F.AND1, F.OR1)).isEqualTo(F.IMP3);
        assertThat(F.g.implication(F.g.variable("b"), F.g.variable("a"))).isNotEqualTo(F.IMP1);
        assertThat(F.g.implication(F.g.literal("a", false), F.g.variable("b"))).isNotEqualTo(F.IMP1);
        assertThat(F.g.implication(F.g.variable("a"), F.g.literal("b", false))).isNotEqualTo(F.IMP1);
    }

    @Test
    public void testHash() {
        final Formula imp = F.f.implication(F.NA, F.NB);
        assertThat(imp.hashCode()).isEqualTo(F.IMP2.hashCode());
        assertThat(imp.hashCode()).isEqualTo(F.IMP2.hashCode());
        assertThat(F.f.implication(F.AND1, F.OR1).hashCode()).isEqualTo(F.IMP3.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.IMP1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.IMP3.numberOfAtoms()).isEqualTo(4);
        assertThat(F.IMP3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.IMP1.numberOfNodes()).isEqualTo(3);
        assertThat(F.IMP4.numberOfNodes()).isEqualTo(7);
        assertThat(F.IMP4.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula imp = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(F.IMP4.numberOfInternalNodes()).isEqualTo(7);
        assertThat(imp.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.IMP1.numberOfOperands()).isEqualTo(2);
        assertThat(F.IMP3.numberOfOperands()).isEqualTo(2);
        assertThat(F.IMP4.numberOfOperands()).isEqualTo(2);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.IMP1.isConstantFormula()).isFalse();
        assertThat(F.IMP2.isConstantFormula()).isFalse();
        assertThat(F.IMP3.isConstantFormula()).isFalse();
        assertThat(F.IMP4.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.IMP1.isAtomicFormula()).isFalse();
        assertThat(F.IMP4.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(F.IMP4.containsVariable(F.f.variable("a"))).isTrue();
        assertThat(F.IMP4.containsVariable(F.f.variable("c"))).isFalse();
    }
}
