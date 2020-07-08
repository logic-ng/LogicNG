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
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Or}.
 * @version 2.0.0
 * @since 1.0
 */
public class OrTest extends TestWithExampleFormulas {

    @Test
    public void testType() {
        assertThat(this.OR1.type()).isEqualTo(FType.OR);
    }

    @Test
    public void testCreator() {
        assertThat(this.f.or()).isEqualTo(this.FALSE);
        assertThat(this.f.or(this.TRUE)).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.FALSE)).isEqualTo(this.FALSE);
        assertThat(this.f.or(this.TRUE, this.FALSE)).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.FALSE, this.TRUE)).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.NA)).isEqualTo(this.NA);
        assertThat(this.f.or(this.X, this.Y, this.X, this.Y, this.X)).isEqualTo(this.OR1);
        assertThat(this.f.or(this.f.or(this.X, this.Y), this.X, this.f.or(this.X, this.Y))).isEqualTo(this.OR1);
        assertThat(this.f.or(this.FALSE, this.X, this.Y, this.FALSE)).isEqualTo(this.OR1);
        assertThat(this.f.or(this.NA, this.NA, this.NA)).isEqualTo(this.NA);
        assertThat(this.f.or(this.NA, this.NA, this.FALSE, this.FALSE)).isEqualTo(this.NA);
        assertThat(this.f.or(this.NA, this.NA, this.TRUE, this.FALSE)).isEqualTo(this.TRUE);
        final List<Literal> lits = Arrays.asList(this.X, this.Y);
        assertThat(this.f.or(lits)).isEqualTo(this.OR1);
        assertThat(this.f.or(this.A, this.B, this.X, this.TRUE)).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.f.or(this.A, this.B), this.f.or(this.X, this.Y))).isEqualTo(this.f.or(this.A, this.B, this.X, this.Y));
        assertThat(this.f.or(this.f.and(this.A, this.B), this.f.or(this.f.and(this.f.and(this.NA, this.NB)), this.f.and(this.f.or(this.NA, this.FALSE), this.NB)))).isEqualTo(this.OR3);
        assertThat(this.f.naryOperator(FType.OR, Arrays.asList(this.X, this.Y, this.X, this.Y, this.X))).isEqualTo(this.OR1);
    }

    @Test
    public void testComplementaryCheck() {
        assertThat(this.f.or(this.A, this.NA)).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.A, this.B, this.f.or(this.C, this.X, this.NB))).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.A, this.B, this.f.or(this.NX, this.B, this.X))).isEqualTo(this.TRUE);
        assertThat(this.f.or(this.X, this.Y, this.f.and(this.NX, this.B, this.X))).isEqualTo(this.OR1);
    }

    @Test
    public void testVariables() {
        assertThat(this.OR2.variables().size()).isEqualTo(2);
        SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(this.X, this.Y));
        assertThat(this.OR2.variables()).isEqualTo(lits);

        final Formula or = this.f.or(this.A, this.A, this.B, this.IMP3);
        assertThat(or.variables().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(or.variables()).isEqualTo(lits);
    }

    @Test
    public void testLiterals() {
        assertThat(this.OR2.literals().size()).isEqualTo(2);
        SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(this.NX, this.NY));
        assertThat(this.OR2.literals()).isEqualTo(lits);

        final Formula or = this.f.or(this.A, this.A, this.B, this.f.implication(this.NB, this.NA));
        assertThat(or.literals().size()).isEqualTo(4);
        lits = new TreeSet<>(Arrays.asList(this.A, this.NA, this.B, this.NB));
        assertThat(or.literals()).isEqualTo(lits);
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(this.OR1.toString()).isEqualTo("x | y");
        assertThat(this.OR2.toString()).isEqualTo("~x | ~y");
        assertThat(this.OR3.toString()).isEqualTo("a & b | ~a & ~b");
        assertThat(f.or(this.A, this.B, this.NX, this.NY).toString()).isEqualTo("a | b | ~x | ~y");
        assertThat(f.or(this.IMP1, this.IMP2).toString()).isEqualTo("(a => b) | (~a => ~b)");
        assertThat(f.or(this.EQ1, this.EQ2).toString()).isEqualTo("(a <=> b) | (~a <=> ~b)");
    }

    @Test
    public void testEquals() {
        assertThat(this.f.or(this.X, this.Y)).isEqualTo(this.OR1);
        assertThat(this.f.or(this.AND1, this.AND2)).isEqualTo(this.OR3);
        assertThat(this.OR2).isEqualTo(this.OR2);
        assertThat(this.f.or(this.NX, this.A, this.NB, this.AND1)).isEqualTo(this.f.or(this.A, this.NB, this.AND1, this.NX));
        assertThat(this.OR2).isNotEqualTo(this.OR1);
        assertThat(this.f.or(this.A, this.B, this.C)).isNotEqualTo(this.OR1);
    }

    @Test
    public void testEqualsDifferentFormulaFactory() {
        assertThat(this.g.or(this.g.variable("x"), this.g.variable("y"))).isEqualTo(this.OR1);
        assertThat(this.g.or(this.AND1, this.AND2)).isEqualTo(this.OR3);
        assertThat(this.g.or(this.g.and(this.g.literal("y", false), this.g.variable("x")), this.f.and(this.g.variable("b"), this.g.variable("a")))).isEqualTo(this.f.or(this.f.and(this.f.variable("a"), this.f.variable("b")), this.f.and(this.f.variable("x"), this.f.literal("y", false))));
        assertThat(this.g.or(this.g.literal("x", false), this.g.variable("a"), this.g.literal("b", false), this.g.and(this.g.variable("a"), this.g.variable("b")))).isEqualTo(this.f.or(this.A, this.NB, this.AND1, this.NX));
        assertThat(this.g.or(this.g.literal("a", false), this.g.variable("b"))).isNotEqualTo(this.OR1);
        assertThat(this.g.or(this.g.variable("a"), this.g.literal("b", false))).isNotEqualTo(this.OR1);
        assertThat(this.f.or(this.A, this.B, this.g.variable("c"))).isNotEqualTo(this.OR1);
    }

    @Test
    public void testHash() {
        final Formula or = this.f.or(this.AND1, this.AND2);
        assertThat(or.hashCode()).isEqualTo(this.OR3.hashCode());
        assertThat(or.hashCode()).isEqualTo(this.OR3.hashCode());
        assertThat(this.f.or(this.NX, this.NY).hashCode()).isEqualTo(this.OR2.hashCode());
    }

    @Test
    public void testNumberOfAtoms() {
        assertThat(this.OR1.numberOfAtoms()).isEqualTo(2);
        assertThat(this.OR2.numberOfAtoms()).isEqualTo(2);
        assertThat(this.OR3.numberOfAtoms()).isEqualTo(4);
        assertThat(this.OR3.numberOfAtoms()).isEqualTo(4);
    }

    @Test
    public void testNumberOfNodes() {
        assertThat(this.OR1.numberOfNodes()).isEqualTo(3);
        assertThat(this.OR2.numberOfNodes()).isEqualTo(3);
        assertThat(this.OR3.numberOfNodes()).isEqualTo(7);
        assertThat(this.OR3.numberOfNodes()).isEqualTo(7);
    }

    @Test
    public void testNumberOfInternalNodes() throws ParserException {
        final Formula or = new PropositionalParser(this.f).parse("a & (b | c) => (d <=> (b | c))");
        assertThat(this.OR3.numberOfInternalNodes()).isEqualTo(7);
        assertThat(or.numberOfInternalNodes()).isEqualTo(8);
    }

    @Test
    public void testNumberOfOperands() {
        assertThat(this.OR1.numberOfOperands()).isEqualTo(2);
        assertThat(this.OR3.numberOfOperands()).isEqualTo(2);
        assertThat(this.f.or(this.A, this.NX, this.EQ1).numberOfOperands()).isEqualTo(3);
    }

    @Test
    public void testIsConstantFormula() {
        assertThat(this.OR1.isConstantFormula()).isFalse();
        assertThat(this.OR2.isConstantFormula()).isFalse();
    }

    @Test
    public void testAtomicFormula() {
        assertThat(this.OR1.isAtomicFormula()).isFalse();
    }

    @Test
    public void testContains() throws ParserException {
        assertThat(this.OR1.containsVariable(this.f.variable("x"))).isTrue();
        assertThat(this.OR1.containsVariable(this.f.variable("a"))).isFalse();
        final PropositionalParser parser = new PropositionalParser(this.f);
        final Formula contAnd = parser.parse("a | b | (c & (d | e))");
        assertThat(contAnd.containsNode(parser.parse("d | e"))).isTrue();
    }
}
