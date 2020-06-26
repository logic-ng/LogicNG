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

import org.junit.jupiter.api.Test;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.formulas.F.OR1;

/**
 * Unit Tests for the class {@link And}.
 * @version 2.0.0
 * @since 1.0
 */
public class AndTest {

    @Test
    public void testType() {
        assertThat(F.AND1.type()).isEqualTo(FType.AND);
    }

    @Test
    public void testCreator() {
        assertThat(F.f.and()).isEqualTo(F.TRUE);
        assertThat(F.f.and(F.TRUE)).isEqualTo(F.TRUE);
        assertThat(F.f.and(F.FALSE)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.TRUE, F.FALSE)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.FALSE, F.TRUE)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.NA)).isEqualTo(F.NA);
        assertThat(F.f.and(F.A, F.B, F.A, F.B, F.A)).isEqualTo(F.AND1);
        assertThat(F.f.and(F.f.and(F.A, F.B), F.A, F.f.and(F.B, F.A))).isEqualTo(F.AND1);
        assertThat(F.f.and(F.TRUE, F.A, F.B, F.TRUE)).isEqualTo(F.AND1);
        assertThat(F.f.and(F.NA, F.NA, F.NA)).isEqualTo(F.NA);
        assertThat(F.f.and(F.NA, F.NA, F.TRUE, F.TRUE)).isEqualTo(F.NA);
        assertThat(F.f.and(F.NA, F.NA, F.FALSE, F.TRUE)).isEqualTo(F.FALSE);
        final List<Literal> lits = new ArrayList<>();
        lits.add(F.A);
        lits.add(F.B);
        assertThat(F.f.and(lits)).isEqualTo(F.AND1);
        assertThat(F.f.and(F.A, F.B, F.X, F.FALSE)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.f.and(F.A, F.B), F.f.and(F.X, F.Y))).isEqualTo(F.f.and(F.A, F.B, F.X, F.Y));
        assertThat(F.f.cnf(F.f.clause(F.X, F.Y), F.f.and(F.f.or(F.f.and(F.NX, F.NX), F.NY), F.f.or(F.f.and(F.NX, F.TRUE), F.NY)))).isEqualTo(F.AND3);
        assertThat(F.f.naryOperator(FType.AND, F.A, F.B, F.A, F.B, F.A)).isEqualTo(F.AND1);
        assertThat(F.f.naryOperator(FType.AND, Arrays.asList(F.A, F.B, F.A, F.B, F.A))).isEqualTo(F.AND1);
    }

    @Test
    public void testComplementaryCheck() {
        assertThat(F.f.and(F.A, F.NA)).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.A, F.B, F.f.and(F.C, F.X, F.NB))).isEqualTo(F.FALSE);
        assertThat(F.f.and(F.A, F.B, F.f.and(F.NX, F.B, F.X))).isEqualTo(F.FALSE);
    }

    @Test
    public void testIllegalCreation() {
        assertThatThrownBy(() -> F.f.naryOperator(FType.EQUIV, F.A, F.B, F.C)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testVariables() {
        assertThat(F.AND2.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
        assertThat(F.AND2.variables()).isEqualTo(lits);

        final Formula and = F.f.and(F.A, F.A, F.B, F.IMP3);
        assertThat(and.variables().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(and.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(F.AND2.literals().size()).isEqualTo(2);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.NA, F.NB));
        assertThat(F.AND2.literals()).isEqualTo(lits);

        final Formula and = F.f.and(F.A, F.A, F.B, F.f.implication(F.NA, F.NB));
        assertThat(and.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
        assertThat(and.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(F.AND1.toString()).isEqualTo("a & b");
        assertThat(F.AND2.toString()).isEqualTo("~a & ~b");
        assertThat(F.AND3.toString()).isEqualTo("(x | y) & (~x | ~y)");
        assertThat(f.and(F.A, F.B, F.NX, F.NY).toString()).isEqualTo("a & b & ~x & ~y");
        assertThat(f.and(F.IMP1, F.IMP2).toString()).isEqualTo("(a => b) & (~a => ~b)");
        assertThat(f.and(F.EQ1, F.EQ2).toString()).isEqualTo("(a <=> b) & (~a <=> ~b)");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.and(F.A, F.B)).isEqualTo(F.AND1);
        assertThat(F.f.and(OR1, F.OR2)).isEqualTo(F.AND3);
        assertThat(F.AND2).isEqualTo(F.AND2);
        assertThat(F.f.and(F.f.or(F.f.literal("y", false), F.f.variable("x")), F.f.or(F.f.variable("b"), F.f.variable("a")))).isEqualTo(F.f.and(F.f.or(F.f.variable("a"), F.f.variable("b")), F.f.or(F.f.variable("x"), F.f.literal("y", false))));
        assertThat(F.f.and(F.NX, F.A, F.NB, OR1)).isEqualTo(F.f.and(F.A, F.NB, OR1, F.NX));
        assertThat(F.AND2).isNotEqualTo(F.AND1);
        assertThat(F.f.and(F.A, F.B, F.C)).isNotEqualTo(F.AND1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.and(F.g.variable("a"), F.g.variable("b"))).isEqualTo(F.AND1);
        assertThat(F.g.and(OR1, F.OR2)).isEqualTo(F.AND3);
        assertThat(F.g.and(F.g.or(F.g.literal("y", false), F.g.variable("x")), F.f.or(F.g.variable("b"), F.g.variable("a")))).isEqualTo(F.f.and(F.f.or(F.f.variable("a"), F.f.variable("b")), F.f.or(F.f.variable("x"), F.f.literal("y", false))));
        assertThat(F.g.and(F.g.literal("x", false), F.g.variable("a"), F.g.literal("b", false), F.g.or(F.g.variable("x"), F.g.variable("y")))).isEqualTo(F.f.and(F.A, F.NB, OR1, F.NX));
        assertThat(F.g.and(F.g.literal("a", false), F.g.variable("b"))).isNotEqualTo(F.AND1);
        assertThat(F.g.and(F.g.variable("a"), F.g.literal("b", false))).isNotEqualTo(F.AND1);
        assertThat(F.f.and(F.A, F.B, F.g.variable("c"))).isNotEqualTo(F.AND1);
    }

    @Test
    public void testHash() {
        final Formula and = F.f.and(OR1, F.OR2);
        assertThat(and.hashCode()).isEqualTo(F.AND3.hashCode());
        assertThat(and.hashCode()).isEqualTo(F.AND3.hashCode());
        assertThat(F.f.and(F.NA, F.NB).hashCode()).isEqualTo(F.AND2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.AND1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.AND2.numberOfAtoms()).isEqualTo(2);
        assertThat(F.AND3.numberOfAtoms()).isEqualTo(4);
        assertThat(F.AND3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.AND1.numberOfNodes()).isEqualTo(3);
        assertThat(F.AND2.numberOfNodes()).isEqualTo(3);
        assertThat(F.AND3.numberOfNodes()).isEqualTo(7);
        assertThat(F.AND3.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula and = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(F.AND3.numberOfInternalNodes()).isEqualTo(7);
        assertThat(and.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.AND1.numberOfOperands()).isEqualTo(2);
        assertThat(F.AND3.numberOfOperands()).isEqualTo(2);
        assertThat(F.f.and(F.A, F.NX, F.EQ1).numberOfOperands()).isEqualTo(3);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.AND1.isConstantFormula()).isFalse();
        assertThat(F.AND2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.AND1.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() throws ParserException {
        assertThat(F.AND3.containsVariable(F.f.variable("x"))).isTrue();
        assertThat(F.AND3.containsVariable(F.f.variable("a"))).isFalse();
        final PropositionalParser parser = new PropositionalParser(F.f);
        final Formula contAnd = parser.parse("a & b & (c | (d & e))");
        assertThat(contAnd.containsNode(parser.parse("d & e"))).isTrue();
    }
}
