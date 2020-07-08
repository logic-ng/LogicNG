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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link And}.
 * @version 2.0.0
 * @since 1.0
 */
public class AndTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.AND1.type()).isEqualTo(FType.AND);
    }

    @Test
    public void testCreator() {
        assertThat(this.f.and()).isEqualTo(this.TRUE);
        assertThat(this.f.and(this.TRUE)).isEqualTo(this.TRUE);
        assertThat(this.f.and(this.FALSE)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.TRUE, this.FALSE)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.FALSE, this.TRUE)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.NA)).isEqualTo(this.NA);
        assertThat(this.f.and(this.A, this.B, this.A, this.B, this.A)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.f.and(this.A, this.B), this.A, this.f.and(this.B, this.A))).isEqualTo(this.AND1);
        assertThat(this.f.and(this.TRUE, this.A, this.B, this.TRUE)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.NA, this.NA, this.NA)).isEqualTo(this.NA);
        assertThat(this.f.and(this.NA, this.NA, this.TRUE, this.TRUE)).isEqualTo(this.NA);
        assertThat(this.f.and(this.NA, this.NA, this.FALSE, this.TRUE)).isEqualTo(this.FALSE);
        final List<Literal> lits = new ArrayList<>();
        lits.add(this.A);
        lits.add(this.B);
        assertThat(this.f.and(lits)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.A, this.B, this.X, this.FALSE)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.f.and(this.A, this.B), this.f.and(this.X, this.Y))).isEqualTo(this.f.and(this.A, this.B, this.X, this.Y));
        assertThat(this.f.cnf(this.f.clause(this.X, this.Y), this.f.and(this.f.or(this.f.and(this.NX, this.NX), this.NY), this.f.or(this.f.and(this.NX, this.TRUE), this.NY)))).isEqualTo(this.AND3);
        assertThat(this.f.naryOperator(FType.AND, this.A, this.B, this.A, this.B, this.A)).isEqualTo(this.AND1);
        assertThat(this.f.naryOperator(FType.AND, Arrays.asList(this.A, this.B, this.A, this.B, this.A))).isEqualTo(this.AND1);
    }

    @Test
    public void testComplementaryCheck() {
        assertThat(this.f.and(this.A, this.NA)).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.A, this.B, this.f.and(this.C, this.X, this.NB))).isEqualTo(this.FALSE);
        assertThat(this.f.and(this.A, this.B, this.f.and(this.NX, this.B, this.X))).isEqualTo(this.FALSE);
    }

    @Test
    public void testIllegalCreation() {
        assertThatThrownBy(() -> this.f.naryOperator(FType.EQUIV, this.A, this.B, this.C)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testVariables() {
        assertThat(this.AND2.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(this.A, this.B));
        assertThat(this.AND2.variables()).isEqualTo(lits);

        final Formula and = this.f.and(this.A, this.A, this.B, this.IMP3);
        assertThat(and.variables().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(and.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(this.AND2.literals().size()).isEqualTo(2);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(this.NA, this.NB));
        assertThat(this.AND2.literals()).isEqualTo(lits);

        final Formula and = this.f.and(this.A, this.A, this.B, this.f.implication(this.NA, this.NB));
        assertThat(and.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.NA, this.B, this.NB));
        assertThat(and.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(this.AND1.toString()).isEqualTo("a & b");
        assertThat(this.AND2.toString()).isEqualTo("~a & ~b");
        assertThat(this.AND3.toString()).isEqualTo("(x | y) & (~x | ~y)");
        assertThat(f.and(this.A, this.B, this.NX, this.NY).toString()).isEqualTo("a & b & ~x & ~y");
        assertThat(f.and(this.IMP1, this.IMP2).toString()).isEqualTo("(a => b) & (~a => ~b)");
        assertThat(f.and(this.EQ1, this.EQ2).toString()).isEqualTo("(a <=> b) & (~a <=> ~b)");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.and(this.A, this.B)).isEqualTo(this.AND1);
        assertThat(this.f.and(this.OR1, this.OR2)).isEqualTo(this.AND3);
        assertThat(this.AND2).isEqualTo(this.AND2);
        assertThat(this.f.and(this.f.or(this.f.literal("y", false), this.f.variable("x")), this.f.or(this.f.variable("b"), this.f.variable("a"))))
                .isEqualTo(this.f.and(this.f.or(this.f.variable("a"), this.f.variable("b")), this.f.or(this.f.variable("x"), this.f.literal("y", false))));
        assertThat(this.f.and(this.NX, this.A, this.NB, this.OR1)).isEqualTo(this.f.and(this.A, this.NB, this.OR1, this.NX));
        assertThat(this.AND2).isNotEqualTo(this.AND1);
        assertThat(this.f.and(this.A, this.B, this.C)).isNotEqualTo(this.AND1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(g.and(g.variable("a"), g.variable("b"))).isEqualTo(this.AND1);
        assertThat(g.and(this.OR1, this.OR2)).isEqualTo(this.AND3);
        assertThat(g.and(g.or(g.literal("y", false), g.variable("x")), f.or(g.variable("b"), g.variable("a"))))
                .isEqualTo(f.and(f.or(f.variable("a"), f.variable("b")), f.or(f.variable("x"), f.literal("y", false))));
        assertThat(g.and(g.literal("x", false), g.variable("a"), g.literal("b", false), g.or(g.variable("x"), g.variable("y"))))
                .isEqualTo(f.and(this.A, this.NB, this.OR1, this.NX));
        assertThat(g.and(g.literal("a", false), g.variable("b"))).isNotEqualTo(this.AND1);
        assertThat(g.and(g.variable("a"), g.literal("b", false))).isNotEqualTo(this.AND1);
        assertThat(f.and(this.A, this.B, g.variable("c"))).isNotEqualTo(this.AND1);
    }

    @Test
    public void testHash() {
        final Formula and = this.f.and(this.OR1, this.OR2);
        assertThat(and.hashCode()).isEqualTo(this.AND3.hashCode());
        assertThat(and.hashCode()).isEqualTo(this.AND3.hashCode());
        assertThat(this.f.and(this.NA, this.NB).hashCode()).isEqualTo(this.AND2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.AND1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.AND2.numberOfAtoms()).isEqualTo(2);
        assertThat(this.AND3.numberOfAtoms()).isEqualTo(4);
        assertThat(this.AND3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.AND1.numberOfNodes()).isEqualTo(3);
        assertThat(this.AND2.numberOfNodes()).isEqualTo(3);
        assertThat(this.AND3.numberOfNodes()).isEqualTo(7);
        assertThat(this.AND3.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula and = new PropositionalParser(this.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(this.AND3.numberOfInternalNodes()).isEqualTo(7);
        assertThat(and.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.AND1.numberOfOperands()).isEqualTo(2);
        assertThat(this.AND3.numberOfOperands()).isEqualTo(2);
        assertThat(this.f.and(this.A, this.NX, this.EQ1).numberOfOperands()).isEqualTo(3);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.AND1.isConstantFormula()).isFalse();
        assertThat(this.AND2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.AND1.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() throws ParserException {
        assertThat(this.AND3.containsVariable(this.f.variable("x"))).isTrue();
        assertThat(this.AND3.containsVariable(this.f.variable("a"))).isFalse();
        final PropositionalParser parser = new PropositionalParser(this.f);
        final Formula contAnd = parser.parse("a & b & (c | (d & e))");
        assertThat(contAnd.containsNode(parser.parse("d & e"))).isTrue();
    }
}
