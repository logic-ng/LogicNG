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
import org.logicng.TestWithExampleFormulas;
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
public class ImplicationTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.IMP1.type()).isEqualTo(FType.IMPL);
    }

    @Test
    public void testCreator() {
        assertThat(this.f.implication(this.FALSE, this.A)).isEqualTo(this.TRUE);
        assertThat(this.f.implication(this.A, this.TRUE)).isEqualTo(this.TRUE);
        assertThat(this.f.implication(this.TRUE, this.A)).isEqualTo(this.A);
        assertThat(this.f.implication(this.A, this.FALSE)).isEqualTo(this.NA);
        assertThat(this.f.implication(this.A, this.A)).isEqualTo(this.TRUE);
        assertThat(this.f.binaryOperator(FType.IMPL, this.AND1, this.OR1)).isEqualTo(this.IMP3);
    }

    @Test
    public void testIllegalCreation() {
        assertThatThrownBy(() -> this.f.binaryOperator(FType.NOT, this.AND1, this.OR1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testGetters() {
        assertThat(((Implication) this.IMP2).left()).isEqualTo(this.NA);
        assertThat(((Implication) this.IMP2).right()).isEqualTo(this.NB);
    }

    @Test
    public void testVariables() {
        assertThat(this.IMP3.variables().size()).isEqualTo(4);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(this.IMP3.variables()).isEqualTo(lits);

        final Formula imp = this.f.implication(this.AND1, this.AND2);
        assertThat(imp.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(imp.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(this.IMP3.literals().size()).isEqualTo(4);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(this.IMP3.literals()).isEqualTo(lits);

        Formula imp = this.f.implication(this.AND1, this.AND2);
        assertThat(imp.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.NA, this.NB));
        assertThat(imp.literals()).isEqualTo(lits);

        imp = this.f.implication(this.AND1, this.A);
        assertThat(imp.literals().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(imp.literals()).isEqualTo(lits);
    }

    @Test
    public void testNegation() {
        assertThat(this.IMP1.negate()).isEqualTo(this.f.not(this.IMP1));
        assertThat(this.IMP2.negate()).isEqualTo(this.f.not(this.IMP2));
        assertThat(this.IMP3.negate()).isEqualTo(this.f.not(this.IMP3));
        assertThat(this.IMP4.negate()).isEqualTo(this.f.not(this.IMP4));
    }

    @Test
    public void testToString() {
        assertThat(this.IMP1.toString()).isEqualTo("a => b");
        assertThat(this.IMP2.toString()).isEqualTo("~a => ~b");
        assertThat(this.IMP3.toString()).isEqualTo("a & b => x | y");
        assertThat(this.IMP4.toString()).isEqualTo("(a <=> b) => (~x <=> ~y)");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.implication(this.A, this.B)).isEqualTo(this.IMP1);
        assertThat(this.f.implication(this.AND1, this.OR1)).isEqualTo(this.IMP3);
        assertThat(this.IMP2).isEqualTo(this.IMP2);
        assertThat(this.IMP2).isNotEqualTo(this.IMP1);
        assertThat(this.IMP2).isNotEqualTo("String");
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(g.implication(g.variable("a"), g.variable("b"))).isEqualTo(this.IMP1);
        assertThat(g.implication(this.AND1, this.OR1)).isEqualTo(this.IMP3);
        assertThat(g.implication(g.variable("b"), g.variable("a"))).isNotEqualTo(this.IMP1);
        assertThat(g.implication(g.literal("a", false), g.variable("b"))).isNotEqualTo(this.IMP1);
        assertThat(g.implication(g.variable("a"), g.literal("b", false))).isNotEqualTo(this.IMP1);
    }

    @Test
    public void testHash() {
        final Formula imp = this.f.implication(this.NA, this.NB);
        assertThat(imp.hashCode()).isEqualTo(this.IMP2.hashCode());
        assertThat(imp.hashCode()).isEqualTo(this.IMP2.hashCode());
        assertThat(this.f.implication(this.AND1, this.OR1).hashCode()).isEqualTo(this.IMP3.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.IMP1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.IMP3.numberOfAtoms()).isEqualTo(4);
        assertThat(this.IMP3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.IMP1.numberOfNodes()).isEqualTo(3);
        assertThat(this.IMP4.numberOfNodes()).isEqualTo(7);
        assertThat(this.IMP4.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula imp = new PropositionalParser(this.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(this.IMP4.numberOfInternalNodes()).isEqualTo(7);
        assertThat(imp.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.IMP1.numberOfOperands()).isEqualTo(2);
        assertThat(this.IMP3.numberOfOperands()).isEqualTo(2);
        assertThat(this.IMP4.numberOfOperands()).isEqualTo(2);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.IMP1.isConstantFormula()).isFalse();
        assertThat(this.IMP2.isConstantFormula()).isFalse();
        assertThat(this.IMP3.isConstantFormula()).isFalse();
        assertThat(this.IMP4.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.IMP1.isAtomicFormula()).isFalse();
        assertThat(this.IMP4.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(this.IMP4.containsVariable(this.f.variable("a"))).isTrue();
        assertThat(this.IMP4.containsVariable(this.f.variable("c"))).isFalse();
    }
}
