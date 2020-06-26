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
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Or}.
 * @version 2.0.0
 * @since 1.0
 */
public class OrTest {

    @Test
    public void testType() {
        assertThat(F.OR1.type()).isEqualTo(FType.OR);
    }

    @Test
    public void testCreator() {
        assertThat(F.f.or()).isEqualTo(F.FALSE);
        assertThat(F.f.or(F.TRUE)).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.FALSE)).isEqualTo(F.FALSE);
        assertThat(F.f.or(F.TRUE, F.FALSE)).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.FALSE, F.TRUE)).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.NA)).isEqualTo(F.NA);
        assertThat(F.f.or(F.X, F.Y, F.X, F.Y, F.X)).isEqualTo(F.OR1);
        assertThat(F.f.or(F.f.or(F.X, F.Y), F.X, F.f.or(F.X, F.Y))).isEqualTo(F.OR1);
        assertThat(F.f.or(F.FALSE, F.X, F.Y, F.FALSE)).isEqualTo(F.OR1);
        assertThat(F.f.or(F.NA, F.NA, F.NA)).isEqualTo(F.NA);
        assertThat(F.f.or(F.NA, F.NA, F.FALSE, F.FALSE)).isEqualTo(F.NA);
        assertThat(F.f.or(F.NA, F.NA, F.TRUE, F.FALSE)).isEqualTo(F.TRUE);
        final List<Literal> lits = Arrays.asList(F.X, F.Y);
        assertThat(F.f.or(lits)).isEqualTo(F.OR1);
        assertThat(F.f.or(F.A, F.B, F.X, F.TRUE)).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.f.or(F.A, F.B), F.f.or(F.X, F.Y))).isEqualTo(F.f.or(F.A, F.B, F.X, F.Y));
        assertThat(F.f.or(F.f.and(F.A, F.B), F.f.or(F.f.and(F.f.and(F.NA, F.NB)), F.f.and(F.f.or(F.NA, F.FALSE), F.NB)))).isEqualTo(F.OR3);
        assertThat(F.f.naryOperator(FType.OR, Arrays.asList(F.X, F.Y, F.X, F.Y, F.X))).isEqualTo(F.OR1);
    }

    @Test
    public void testComplementaryCheck() {
        assertThat(F.f.or(F.A, F.NA)).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.A, F.B, F.f.or(F.C, F.X, F.NB))).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.A, F.B, F.f.or(F.NX, F.B, F.X))).isEqualTo(F.TRUE);
        assertThat(F.f.or(F.X, F.Y, F.f.and(F.NX, F.B, F.X))).isEqualTo(F.OR1);
    }

    @Test
    public void testVariables() {
        assertThat(F.OR2.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.X, F.Y));
        assertThat(F.OR2.variables()).isEqualTo(lits);

        final Formula or = F.f.or(F.A, F.A, F.B, F.IMP3);
        assertThat(or.variables().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(or.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(F.OR2.literals().size()).isEqualTo(2);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.NX, F.NY));
        assertThat(F.OR2.literals()).isEqualTo(lits);

        final Formula or = F.f.or(F.A, F.A, F.B, F.f.implication(F.NB, F.NA));
        assertThat(or.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
        assertThat(or.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(F.OR1.toString()).isEqualTo("x | y");
        assertThat(F.OR2.toString()).isEqualTo("~x | ~y");
        assertThat(F.OR3.toString()).isEqualTo("a & b | ~a & ~b");
        assertThat(f.or(F.A, F.B, F.NX, F.NY).toString()).isEqualTo("a | b | ~x | ~y");
        assertThat(f.or(F.IMP1, F.IMP2).toString()).isEqualTo("(a => b) | (~a => ~b)");
        assertThat(f.or(F.EQ1, F.EQ2).toString()).isEqualTo("(a <=> b) | (~a <=> ~b)");
    }

    @Test
    public void testEquals() {
        assertThat(F.f.or(F.X, F.Y)).isEqualTo(F.OR1);
        assertThat(F.f.or(F.AND1, F.AND2)).isEqualTo(F.OR3);
        assertThat(F.OR2).isEqualTo(F.OR2);
        assertThat(F.f.or(F.NX, F.A, F.NB, F.AND1)).isEqualTo(F.f.or(F.A, F.NB, F.AND1, F.NX));
        assertThat(F.OR2).isNotEqualTo(F.OR1);
        assertThat(F.f.or(F.A, F.B, F.C)).isNotEqualTo(F.OR1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(F.g.or(F.g.variable("x"), F.g.variable("y"))).isEqualTo(F.OR1);
        assertThat(F.g.or(F.AND1, F.AND2)).isEqualTo(F.OR3);
        assertThat(F.g.or(F.g.and(F.g.literal("y", false), F.g.variable("x")), F.f.and(F.g.variable("b"), F.g.variable("a")))).isEqualTo(F.f.or(F.f.and(F.f.variable("a"), F.f.variable("b")), F.f.and(F.f.variable("x"), F.f.literal("y", false))));
        assertThat(F.g.or(F.g.literal("x", false), F.g.variable("a"), F.g.literal("b", false), F.g.and(F.g.variable("a"), F.g.variable("b")))).isEqualTo(F.f.or(F.A, F.NB, F.AND1, F.NX));
        assertThat(F.g.or(F.g.literal("a", false), F.g.variable("b"))).isNotEqualTo(F.OR1);
        assertThat(F.g.or(F.g.variable("a"), F.g.literal("b", false))).isNotEqualTo(F.OR1);
        assertThat(F.f.or(F.A, F.B, F.g.variable("c"))).isNotEqualTo(F.OR1);
    }

    @Test
    public void testHash() {
        final Formula or = F.f.or(F.AND1, F.AND2);
        assertThat(or.hashCode()).isEqualTo(F.OR3.hashCode());
        assertThat(or.hashCode()).isEqualTo(F.OR3.hashCode());
        assertThat(F.f.or(F.NX, F.NY).hashCode()).isEqualTo(F.OR2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(F.OR1.numberOfAtoms()).isEqualTo(2);
        assertThat(F.OR2.numberOfAtoms()).isEqualTo(2);
        assertThat(F.OR3.numberOfAtoms()).isEqualTo(4);
        assertThat(F.OR3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(F.OR1.numberOfNodes()).isEqualTo(3);
        assertThat(F.OR2.numberOfNodes()).isEqualTo(3);
        assertThat(F.OR3.numberOfNodes()).isEqualTo(7);
        assertThat(F.OR3.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula or = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(F.OR3.numberOfInternalNodes()).isEqualTo(7);
        assertThat(or.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(F.OR1.numberOfOperands()).isEqualTo(2);
        assertThat(F.OR3.numberOfOperands()).isEqualTo(2);
        assertThat(F.f.or(F.A, F.NX, F.EQ1).numberOfOperands()).isEqualTo(3);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(F.OR1.isConstantFormula()).isFalse();
        assertThat(F.OR2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(F.OR1.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() throws ParserException {
        assertThat(F.OR1.containsVariable(F.f.variable("x"))).isTrue();
        assertThat(F.OR1.containsVariable(F.f.variable("a"))).isFalse();
        final PropositionalParser parser = new PropositionalParser(F.f);
        final Formula contAnd = parser.parse("a | b | (c & (d | e))");
        assertThat(contAnd.containsNode(parser.parse("d | e"))).isTrue();
    }
}
