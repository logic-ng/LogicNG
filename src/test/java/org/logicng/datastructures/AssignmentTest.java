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
import org.logicng.formulas.F;
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
public class AssignmentTest {

    @Test
    public void testCreators() {
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y))).isNotNull();
    }

    @Test
    public void testSize() {
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y), true).size()).isEqualTo(4);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false).size()).isEqualTo(4);
        assertThat(new Assignment(Arrays.asList(F.A, F.NB)).size()).isEqualTo(2);
    }

    @Test
    public void testPositiveVariables() {
        final Variable[] a = {F.A, F.B, F.X, F.Y};
        Assignment ass1 = new Assignment(Arrays.asList(a), false);
        assertThat(ass1.positiveVariables()).containsExactly(a);
        assertThat(ass1.positiveLiterals()).containsExactly(a);
        ass1 = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
        assertThat(ass1.positiveVariables()).containsExactly(F.A, F.B);
        assertThat(ass1.positiveLiterals()).containsExactly(F.A, F.B);
        ass1 = new Assignment(Arrays.asList(F.NA, F.NB, F.NX, F.NY));
        assertThat(ass1.positiveVariables().size()).isEqualTo(0);
        assertThat(ass1.positiveLiterals().size()).isEqualTo(0);
    }

    @Test
    public void testNegativeLiterals() {
        final Literal[] a = {F.NA, F.NB, F.NX, F.NY};
        Assignment ass = new Assignment(Arrays.asList(a));
        assertThat(ass.negativeLiterals()).containsExactly(a);
        ass = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
        assertThat(ass.negativeLiterals()).containsExactly(F.NX, F.NY);
        ass = new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(ass.negativeLiterals().size()).isEqualTo(0);
    }

    @Test
    public void testNegativeVariables() {
        final Variable[] a = {F.A, F.B, F.X, F.Y};
        final Literal[] na = {F.NA, F.NB, F.NX, F.NY};
        Assignment ass = new Assignment(Arrays.asList(na));
        assertThat(ass.negativeVariables()).containsExactly(a);
        ass = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
        assertThat(ass.negativeVariables()).containsExactly(F.X, F.Y);
        ass = new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y));
        assertThat(ass.negativeVariables().size()).isEqualTo(0);
    }

    @Test
    public void testAddLiteral() {
        final Assignment ass = new Assignment();
        ass.addLiteral(F.A);
        ass.addLiteral(F.B);
        ass.addLiteral(F.NX);
        ass.addLiteral(F.NY);
        assertThat(ass.positiveVariables()).containsExactly(F.A, F.B);
        assertThat(ass.negativeLiterals()).containsExactly(F.NX, F.NY);
    }

    @Test
    public void testEvaluateLit() {
        final Assignment ass = new Assignment(Arrays.asList(F.A, F.NX));
        assertThat(ass.evaluateLit(F.A)).isTrue();
        assertThat(ass.evaluateLit(F.NX)).isTrue();
        assertThat(ass.evaluateLit(F.NB)).isTrue();
        assertThat(ass.evaluateLit(F.NA)).isFalse();
        assertThat(ass.evaluateLit(F.X)).isFalse();
        assertThat(ass.evaluateLit(F.B)).isFalse();
    }

    @Test
    public void testRestrictLit() {
        final Assignment ass = new Assignment(Arrays.asList(F.A, F.NX));
        assertThat(ass.restrictLit(F.A)).isEqualTo(F.TRUE);
        assertThat(ass.restrictLit(F.NX)).isEqualTo(F.TRUE);
        assertThat(ass.restrictLit(F.NA)).isEqualTo(F.FALSE);
        assertThat(ass.restrictLit(F.X)).isEqualTo(F.FALSE);
        assertThat(ass.restrictLit(F.B)).isEqualTo(F.B);
        assertThat(ass.restrictLit(F.NB)).isEqualTo(F.NB);
    }

    @Test
    public void testFormula() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        assertThat(new Assignment(Collections.singletonList(F.A)).formula(F.f)).isEqualTo(p.parse("a"));
        assertThat(new Assignment(Collections.singletonList(F.NA)).formula(F.f)).isEqualTo(p.parse("~a"));
        assertThat(new Assignment(Arrays.asList(F.A, F.B)).formula(F.f)).isEqualTo(p.parse("a & b"));
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).formula(F.f)).isEqualTo(p.parse("a & b & ~x & ~y"));
    }

    @Test
    public void testFastEvaluable() {
        Assignment ass = new Assignment(Arrays.asList(F.A, F.NX), false);
        assertThat(ass.fastEvaluable()).isFalse();
        ass.convertToFastEvaluable();
        assertThat(ass.fastEvaluable()).isTrue();
        assertThat(ass.positiveVariables()).containsExactly(F.A);
        assertThat(ass.negativeLiterals()).containsExactly(F.NX);
        assertThat(ass.negativeVariables()).containsExactly(F.X);
        ass.addLiteral(F.NB);
        ass.addLiteral(F.Y);
        assertThat(ass.positiveVariables()).containsExactly(F.A, F.Y);
        assertThat(ass.negativeLiterals()).containsExactly(F.NB, F.NX);
        assertThat(ass.negativeVariables()).containsExactly(F.X, F.B);
        assertThat(ass.evaluateLit(F.Y)).isTrue();
        assertThat(ass.evaluateLit(F.B)).isFalse();
        assertThat(ass.restrictLit(F.NB)).isEqualTo(F.TRUE);
        assertThat(ass.restrictLit(F.X)).isEqualTo(F.FALSE);
        assertThat(ass.restrictLit(F.C)).isEqualTo(F.C);
        assertThat(ass.formula(F.f)).isEqualTo(F.f.and(F.A, F.NX, F.NB, F.Y));
        ass = new Assignment(Arrays.asList(F.A, F.NX), true);
        assertThat(ass.fastEvaluable()).isTrue();
        ass.convertToFastEvaluable();
        assertThat(ass.fastEvaluable()).isTrue();
    }

    @Test
    public void testHashCode() {
        final Assignment ass = new Assignment();
        ass.addLiteral(F.A);
        ass.addLiteral(F.B);
        ass.addLiteral(F.NX);
        ass.addLiteral(F.NY);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode()).isEqualTo(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode());
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode()).isEqualTo(ass.hashCode());
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode()).isEqualTo(ass.hashCode());
    }

    @Test
    public void testEquals() {
        final Assignment ass = new Assignment();
        ass.addLiteral(F.A);
        ass.addLiteral(F.B);
        ass.addLiteral(F.NX);
        ass.addLiteral(F.NY);
        assertThat(ass).isNotEqualTo(null);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false)).isEqualTo(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false));
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true)).isEqualTo(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false));
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false)).isEqualTo(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true));
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true)).isEqualTo(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true));
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY))).isEqualTo(ass);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY))).isEqualTo(ass);
        assertThat(ass).isEqualTo(ass);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX))).isNotEqualTo(ass);
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY, F.C))).isNotEqualTo(ass);
        assertThat(F.TRUE).isNotEqualTo(ass);
    }

    @Test
    public void testBlockingClause() throws ParserException {
        final Assignment ass = new Assignment();
        ass.addLiteral(F.A);
        ass.addLiteral(F.B);
        ass.addLiteral(F.NX);
        ass.addLiteral(F.NY);
        final Formula bc01 = ass.blockingClause(F.f);
        assertThat(bc01.containsVariable(F.C)).isFalse();
        assertThat(bc01).isEqualTo(F.f.parse("~a | ~b | x | y"));
        final Formula bc02 = ass.blockingClause(F.f, null);
        assertThat(bc02.containsVariable(F.C)).isFalse();
        assertThat(bc02).isEqualTo(F.f.parse("~a | ~b | x | y"));
        final List<Literal> lits = Arrays.asList(F.A, F.X, F.C);
        final Formula bcProjected = ass.blockingClause(F.f, lits);
        assertThat(bcProjected.containsVariable(F.C)).isFalse();
        assertThat(bcProjected).isEqualTo(F.f.parse("~a | x"));
    }

    @Test
    public void testToString() {
        assertThat(new Assignment().toString()).isEqualTo("Assignment{pos=[], neg=[]}");
        assertThat(new Assignment(Collections.singletonList(F.A)).toString()).isEqualTo("Assignment{pos=[a], neg=[]}");
        assertThat(new Assignment(Collections.singletonList(F.NA)).toString()).isEqualTo("Assignment{pos=[], neg=[~a]}");
        assertThat(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY, F.C)).toString()).isEqualTo("Assignment{pos=[a, b, c], neg=[~x, ~y]}");
    }
}
