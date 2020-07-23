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

package org.logicng.datastructures;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Unit tests for the class {@link Assignment}.
 * @version 2.0.0
 * @since 1.0
 */
public class AssignmentTest extends TestWithExampleFormulas {

    @Test
    public void testCreators() {
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.X, this.Y))).isNotNull();
    }

    @Test
    public void testSize() {
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.X, this.Y), true).size()).isEqualTo(4);
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), false).size()).isEqualTo(4);
        assertThat(new Assignment(Arrays.asList(this.A, this.NB)).size()).isEqualTo(2);
    }

    @Test
    public void testPositiveVariables() {
        final Variable[] a = {this.A, this.B, this.X, this.Y};
        Assignment ass1 = new Assignment(Arrays.asList(a), false);
        assertThat(ass1.positiveVariables()).containsExactly(a);
        ass1 = new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY));
        assertThat(ass1.positiveVariables()).containsExactly(this.A, this.B);
        ass1 = new Assignment(Arrays.asList(this.NA, this.NB, this.NX, this.NY));
        assertThat(ass1.positiveVariables().size()).isEqualTo(0);
    }

    @Test
    public void testNegativeLiterals() {
        final Literal[] a = {this.NA, this.NB, this.NX, this.NY};
        Assignment ass = new Assignment(Arrays.asList(a));
        assertThat(ass.negativeLiterals()).containsExactly(a);
        ass = new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY));
        assertThat(ass.negativeLiterals()).containsExactly(this.NX, this.NY);
        ass = new Assignment(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(ass.negativeLiterals().size()).isEqualTo(0);
    }

    @Test
    public void testNegativeVariables() {
        final Variable[] a = {this.A, this.B, this.X, this.Y};
        final Literal[] na = {this.NA, this.NB, this.NX, this.NY};
        Assignment ass = new Assignment(Arrays.asList(na));
        assertThat(ass.negativeVariables()).containsExactly(a);
        ass = new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY));
        assertThat(ass.negativeVariables()).containsExactly(this.X, this.Y);
        ass = new Assignment(Arrays.asList(this.A, this.B, this.X, this.Y));
        assertThat(ass.negativeVariables().size()).isEqualTo(0);
    }

    @Test
    public void testAddLiteral() {
        final Assignment ass = new Assignment();
        ass.addLiteral(this.A);
        ass.addLiteral(this.B);
        ass.addLiteral(this.NX);
        ass.addLiteral(this.NY);
        assertThat(ass.positiveVariables()).containsExactly(this.A, this.B);
        assertThat(ass.negativeLiterals()).containsExactly(this.NX, this.NY);
    }

    @Test
    public void testEvaluateLit() {
        final Assignment ass = new Assignment(Arrays.asList(this.A, this.NX));
        assertThat(ass.evaluateLit(this.A)).isTrue();
        assertThat(ass.evaluateLit(this.NX)).isTrue();
        assertThat(ass.evaluateLit(this.NB)).isTrue();
        assertThat(ass.evaluateLit(this.NA)).isFalse();
        assertThat(ass.evaluateLit(this.X)).isFalse();
        assertThat(ass.evaluateLit(this.B)).isFalse();
    }

    @Test
    public void testRestrictLit() {
        final Assignment ass = new Assignment(Arrays.asList(this.A, this.NX));
        assertThat(ass.restrictLit(this.A)).isEqualTo(this.TRUE);
        assertThat(ass.restrictLit(this.NX)).isEqualTo(this.TRUE);
        assertThat(ass.restrictLit(this.NA)).isEqualTo(this.FALSE);
        assertThat(ass.restrictLit(this.X)).isEqualTo(this.FALSE);
        assertThat(ass.restrictLit(this.B)).isEqualTo(this.B);
        assertThat(ass.restrictLit(this.NB)).isEqualTo(this.NB);
    }

    @Test
    public void testFormula() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(new Assignment(Collections.singletonList(this.A)).formula(this.f)).isEqualTo(p.parse("a"));
        assertThat(new Assignment(Collections.singletonList(this.NA)).formula(this.f)).isEqualTo(p.parse("~a"));
        assertThat(new Assignment(Arrays.asList(this.A, this.B)).formula(this.f)).isEqualTo(p.parse("a & b"));
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY)).formula(this.f)).isEqualTo(p.parse("a & b & ~x & ~y"));
    }

    @Test
    public void testFastEvaluable() {
        Assignment ass = new Assignment(Arrays.asList(this.A, this.NX), false);
        assertThat(ass.fastEvaluable()).isFalse();
        ass.convertToFastEvaluable();
        assertThat(ass.fastEvaluable()).isTrue();
        assertThat(ass.positiveVariables()).containsExactly(this.A);
        assertThat(ass.negativeLiterals()).containsExactly(this.NX);
        assertThat(ass.negativeVariables()).containsExactly(this.X);
        ass.addLiteral(this.NB);
        ass.addLiteral(this.Y);
        assertThat(ass.positiveVariables()).containsExactly(this.A, this.Y);
        assertThat(ass.negativeLiterals()).containsExactly(this.NB, this.NX);
        assertThat(ass.negativeVariables()).containsExactly(this.X, this.B);
        assertThat(ass.evaluateLit(this.Y)).isTrue();
        assertThat(ass.evaluateLit(this.B)).isFalse();
        assertThat(ass.restrictLit(this.NB)).isEqualTo(this.TRUE);
        assertThat(ass.restrictLit(this.X)).isEqualTo(this.FALSE);
        assertThat(ass.restrictLit(this.C)).isEqualTo(this.C);
        assertThat(ass.formula(this.f)).isEqualTo(this.f.and(this.A, this.NX, this.NB, this.Y));
        ass = new Assignment(Arrays.asList(this.A, this.NX), true);
        assertThat(ass.fastEvaluable()).isTrue();
        ass.convertToFastEvaluable();
        assertThat(ass.fastEvaluable()).isTrue();
    }

    @Test
    public void testHashCode() {
        final Assignment ass = new Assignment();
        ass.addLiteral(this.A);
        ass.addLiteral(this.B);
        ass.addLiteral(this.NX);
        ass.addLiteral(this.NY);
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY)).hashCode()).isEqualTo(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY)).hashCode());
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY)).hashCode()).isEqualTo(ass.hashCode());
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY)).hashCode()).isEqualTo(ass.hashCode());
    }

    @Test
    public void testEquals() {
        final Assignment ass = new Assignment();
        ass.addLiteral(this.A);
        ass.addLiteral(this.B);
        ass.addLiteral(this.NX);
        ass.addLiteral(this.NY);
        assertThat(ass).isNotEqualTo(null);
        assertThat(ass.equals(null)).isFalse();
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), false)).isEqualTo(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), false));
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), true)).isEqualTo(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), false));
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), false)).isEqualTo(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), true));
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), true)).isEqualTo(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY), true));
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY))).isEqualTo(ass);
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY))).isEqualTo(ass);
        assertThat(ass).isEqualTo(ass);
        assertThat(ass.equals(ass)).isTrue();
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX))).isNotEqualTo(ass);
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY, this.C))).isNotEqualTo(ass);
        assertThat(this.TRUE).isNotEqualTo(ass);
    }

    @Test
    public void testBlockingClause() throws ParserException {
        final Assignment ass = new Assignment();
        ass.addLiteral(this.A);
        ass.addLiteral(this.B);
        ass.addLiteral(this.NX);
        ass.addLiteral(this.NY);
        final Formula bc01 = ass.blockingClause(this.f);
        assertThat(bc01.containsVariable(this.C)).isFalse();
        assertThat(bc01).isEqualTo(this.f.parse("~a | ~b | x | y"));
        final Formula bc02 = ass.blockingClause(this.f, null);
        assertThat(bc02.containsVariable(this.C)).isFalse();
        assertThat(bc02).isEqualTo(this.f.parse("~a | ~b | x | y"));
        final List<Literal> lits = Arrays.asList(this.A, this.X, this.C);
        final Formula bcProjected = ass.blockingClause(this.f, lits);
        assertThat(bcProjected.containsVariable(this.C)).isFalse();
        assertThat(bcProjected).isEqualTo(this.f.parse("~a | x"));
    }

    @Test
    public void testToString() {
        assertThat(new Assignment().toString()).isEqualTo("Assignment{pos=[], neg=[]}");
        assertThat(new Assignment(Collections.singletonList(this.A)).toString()).isEqualTo("Assignment{pos=[a], neg=[]}");
        assertThat(new Assignment(Collections.singletonList(this.NA)).toString()).isEqualTo("Assignment{pos=[], neg=[~a]}");
        assertThat(new Assignment(Arrays.asList(this.A, this.B, this.NX, this.NY, this.C)).toString()).isEqualTo("Assignment{pos=[a, b, c], neg=[~x, ~y]}");
    }
}
