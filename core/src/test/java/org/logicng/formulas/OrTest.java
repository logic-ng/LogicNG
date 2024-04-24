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

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
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
 * @version 2.5.0
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
        assertThat(this.f.dnf(emptyList())).isEqualTo(this.f.falsum());
        assertThat(this.f.dnf(singletonList(this.f.falsum()))).isEqualTo(this.f.falsum());
        assertThat(this.f.dnf(singletonList(this.f.verum()))).isEqualTo(this.f.verum());
        assertThat(this.f.dnf(this.f.or(this.f.and(this.A, this.B), this.f.and(this.NA, this.NB)))).isEqualTo(this.OR3);
        assertThat(this.f.clause()).isEqualTo(this.f.falsum());
        assertThat(this.f.clause(this.A)).isEqualTo(this.A);
        assertThat(this.f.clause(this.A, this.NB)).isEqualTo(this.f.or(this.A, this.NB));
        assertThat(this.f.clause(emptyList())).isEqualTo(this.f.falsum());
        assertThat(this.f.clause(singletonList(this.A))).isEqualTo(this.A);
        assertThat(this.f.clause(Arrays.asList(this.A, this.NB))).isEqualTo(this.f.or(this.A, this.NB));
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
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
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
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        final FormulaFactory g = new FormulaFactory(FormulaFactoryConfig.builder().formulaMergeStrategy(FormulaFactoryConfig.FormulaMergeStrategy.IMPORT).build());
        assertThat(g.or(g.variable("x"), g.variable("y"))).isEqualTo(this.OR1);
        assertThat(g.or(this.AND1, this.AND2)).isEqualTo(this.OR3);
        assertThat(g.or(g.and(g.literal("y", false), g.variable("x")), f.and(g.variable("b"), g.variable("a")))).isEqualTo(f.or(f.and(f.variable("a"), f.variable("b")), f.and(f.variable("x"), f.literal("y", false))));
        assertThat(g.or(g.literal("x", false), g.variable("a"), g.literal("b", false), g.and(g.variable("a"), g.variable("b")))).isEqualTo(f.or(this.A, this.NB, this.AND1, this.NX));
        assertThat(g.or(g.literal("a", false), g.variable("b"))).isNotEqualTo(this.OR1);
        assertThat(g.or(g.variable("a"), g.literal("b", false))).isNotEqualTo(this.OR1);
        assertThat(f.or(this.A, this.B, g.variable("c"))).isNotEqualTo(this.OR1);
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

    @Test
    public void testIsNNF() {
        assertThat(this.OR1.isNNF()).isTrue();
        assertThat(this.OR2.isNNF()).isTrue();
        assertThat(this.OR3.isNNF()).isTrue();
    }

    @Test
    public void testIsDNF() {
        assertThat(this.OR1.isDNF()).isTrue();
        assertThat(this.OR2.isDNF()).isTrue();
        assertThat(this.OR3.isDNF()).isTrue();
        assertThat(this.f.clause().isDNF()).isTrue();
        assertThat(this.f.clause(this.A).isDNF()).isTrue();
        assertThat(this.f.clause(this.A, this.NB).isDNF()).isTrue();
        assertThat(this.f.dnf(this.f.or(this.f.and(this.A, this.B), this.f.and(this.NA, this.NB))).isDNF()).isTrue();
    }

    @Test
    public void testIsCNF() {
        assertThat(this.OR1.isCNF()).isTrue();
        assertThat(this.OR2.isCNF()).isTrue();
        assertThat(this.OR3.isCNF()).isFalse();
        assertThat(this.f.clause().isCNF()).isTrue();
        assertThat(this.f.clause(this.A).isCNF()).isTrue();
        assertThat(this.f.clause(this.A, this.NB).isCNF()).isTrue();
        assertThat(this.f.dnf(this.A).isCNF()).isTrue();
        assertThat(this.f.dnf(this.A, this.NB, this.C).isCNF()).isTrue();
        assertThat(this.f.dnf(this.A, this.NB, this.f.and(this.A, this.C)).isCNF()).isFalse();
        assertThat(this.f.dnf(this.f.or(this.f.and(this.A, this.B), this.f.and(this.NA, this.NB))).isCNF()).isFalse();
    }
}
