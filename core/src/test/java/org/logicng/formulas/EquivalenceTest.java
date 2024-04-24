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
 * Unit Tests for the class {@link Equivalence}.
 * @version 2.3.0
 * @since 1.0
 */
public class EquivalenceTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.EQ1.type()).isEqualTo(FType.EQUIV);
    }

    @Test
    public void testCreator() {
        assertThat(this.f.equivalence(this.TRUE, this.AND1)).isEqualTo(this.AND1);
        assertThat(this.f.equivalence(this.AND1, this.TRUE)).isEqualTo(this.AND1);
        assertThat(this.f.equivalence(this.FALSE, this.AND1)).isEqualTo(this.NOT1);
        assertThat(this.f.equivalence(this.AND1, this.FALSE)).isEqualTo(this.NOT1);
        assertThat(this.f.equivalence(this.OR1, this.OR1)).isEqualTo(this.TRUE);
        assertThat(this.f.equivalence(this.NOT1, this.AND1)).isEqualTo(this.FALSE);
        assertThat(this.f.equivalence(this.AND1, this.NOT1)).isEqualTo(this.FALSE);
        assertThat(this.f.equivalence(this.OR1, this.NOT2)).isEqualTo(this.FALSE);
        assertThat(this.f.equivalence(this.NOT2, this.OR1)).isEqualTo(this.FALSE);
        assertThat(this.f.binaryOperator(FType.EQUIV, this.AND1, this.OR1)).isEqualTo(this.EQ3);
    }

    @Test
    public void testGetters() {
        assertThat(((Equivalence) this.EQ2).left()).isEqualTo(this.NA);
        assertThat(((Equivalence) this.EQ2).right()).isEqualTo(this.NB);
    }

    @Test
    public void testVariables() {
        assertThat(this.IMP3.variables().size()).isEqualTo(4);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(this.IMP3.variables()).isEqualTo(lits);

        final Formula equiv = this.f.equivalence(this.AND1, this.AND2);
        assertThat(equiv.variables().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(equiv.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(this.IMP3.literals().size()).isEqualTo(4);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(this.IMP3.literals()).isEqualTo(lits);

        Formula equiv = this.f.equivalence(this.AND1, this.AND2);
        assertThat(equiv.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.NA, this.NB));
        assertThat(equiv.literals()).isEqualTo(lits);

        equiv = this.f.equivalence(this.AND1, this.A);
        assertThat(equiv.literals().size()).isEqualTo(2);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(equiv.literals()).isEqualTo(lits);
    }

    @Test
    public void testNegation() {
        assertThat(this.EQ1.negate()).isEqualTo(this.f.not(this.EQ1));
        assertThat(this.EQ2.negate()).isEqualTo(this.f.not(this.EQ2));
        assertThat(this.EQ3.negate()).isEqualTo(this.f.not(this.EQ3));
        assertThat(this.EQ4.negate()).isEqualTo(this.f.not(this.EQ4));
    }

    @Test
    public void testToString() {
        assertThat(this.EQ1.toString()).isEqualTo("a <=> b");
        assertThat(this.EQ2.toString()).isEqualTo("~a <=> ~b");
        assertThat(this.EQ3.toString()).isEqualTo("a & b <=> x | y");
        assertThat(this.EQ4.toString()).isEqualTo("a => b <=> ~a => ~b");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.equivalence(this.A, this.B)).isEqualTo(this.EQ1);
        assertThat(this.f.equivalence(this.B, this.A)).isEqualTo(this.EQ1);
        assertThat(this.f.equivalence(this.AND1, this.OR1)).isEqualTo(this.EQ3);
        assertThat(this.EQ4).isEqualTo(this.EQ4);
        assertThat(this.EQ2).isNotEqualTo(this.EQ1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(g.equivalence(g.variable("a"), g.variable("b"))).isEqualTo(this.EQ1);
        assertThat(g.equivalence(this.B, this.A)).isEqualTo(this.EQ1);
        assertThat(g.equivalence(this.AND1, this.OR1)).isEqualTo(this.EQ3);
        assertThat(g.equivalence(g.literal("a", false), g.variable("b"))).isNotEqualTo(this.EQ1);
        assertThat(g.equivalence(g.variable("a"), g.literal("b", false))).isNotEqualTo(this.EQ1);
    }

    @Test
    public void testHash() {
        final Formula eq = this.f.equivalence(this.IMP1, this.IMP2);
        assertThat(eq.hashCode()).isEqualTo(this.EQ4.hashCode());
        assertThat(eq.hashCode()).isEqualTo(this.EQ4.hashCode());
        assertThat(this.f.equivalence(this.AND1, this.OR1).hashCode()).isEqualTo(this.EQ3.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.EQ1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.EQ4.numberOfAtoms()).isEqualTo(4);
        assertThat(this.EQ4.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.EQ1.numberOfNodes()).isEqualTo(3);
        assertThat(this.EQ4.numberOfNodes()).isEqualTo(7);
        assertThat(this.EQ4.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula eq = new PropositionalParser(this.f).parse("a & (b | c) <=> (d => (b | c))");
        assertThat(this.EQ4.numberOfInternalNodes()).isEqualTo(7);
        assertThat(eq.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.EQ1.numberOfOperands()).isEqualTo(2);
        assertThat(this.EQ3.numberOfOperands()).isEqualTo(2);
        assertThat(this.EQ4.numberOfOperands()).isEqualTo(2);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.EQ1.isConstantFormula()).isFalse();
        assertThat(this.EQ2.isConstantFormula()).isFalse();
        assertThat(this.EQ3.isConstantFormula()).isFalse();
        assertThat(this.EQ4.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.EQ1.isAtomicFormula()).isFalse();
        assertThat(this.EQ4.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() {
        assertThat(this.EQ4.containsVariable(this.f.variable("a"))).isTrue();
        assertThat(this.EQ4.containsVariable(this.f.variable("x"))).isFalse();
        assertThat(this.EQ4.containsNode(this.IMP1)).isTrue();
        assertThat(this.EQ4.containsNode(this.IMP4)).isFalse();
    }

    @Test
    public void testIsNNF() {
        assertThat(this.EQ1.isNNF()).isFalse();
        assertThat(this.EQ2.isNNF()).isFalse();
        assertThat(this.EQ3.isNNF()).isFalse();
        assertThat(this.EQ4.isNNF()).isFalse();
    }

    @Test
    public void testIsDNF() {
        assertThat(this.EQ1.isDNF()).isFalse();
        assertThat(this.EQ2.isDNF()).isFalse();
        assertThat(this.EQ3.isDNF()).isFalse();
        assertThat(this.EQ4.isDNF()).isFalse();
    }

    @Test
    public void testIsCNF() {
        assertThat(this.EQ1.isCNF()).isFalse();
        assertThat(this.EQ2.isCNF()).isFalse();
        assertThat(this.EQ3.isCNF()).isFalse();
        assertThat(this.EQ4.isCNF()).isFalse();
    }
}
