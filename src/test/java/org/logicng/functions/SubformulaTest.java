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

package org.logicng.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.formulas.cache.FunctionCacheEntry.SUBFORMULAS;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.LinkedHashSet;

/**
 * Unit tests for {@link SubNodeFunction}.
 * @version 2.0.0
 * @since 1.0
 */
public class SubformulaTest {

    @Test
    public void testConstants() {
        assertThat(F.TRUE.containsNode(F.TRUE)).isTrue();
        assertThat(F.FALSE.containsNode(F.FALSE)).isTrue();
        assertThat(F.TRUE.containsNode(F.FALSE)).isFalse();
        assertThat(F.FALSE.containsNode(F.TRUE)).isFalse();
        assertThat(F.FALSE.containsNode(F.A)).isFalse();
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.containsNode(F.A)).isTrue();
        assertThat(F.A.containsNode(F.f.variable("a"))).isTrue();
        assertThat(F.NA.containsNode(F.A)).isFalse();
        assertThat(F.NA.containsNode(F.f.literal("a", false))).isTrue();
        assertThat(F.A.containsNode(F.NA)).isFalse();
        assertThat(F.A.containsNode(F.B)).isFalse();
        assertThat(F.NA.containsNode(F.NB)).isFalse();
        assertThat(F.A.containsNode(F.FALSE)).isFalse();
        assertThat(F.NA.containsNode(F.TRUE)).isFalse();
    }

    @Test
    public void testNot() {
        assertThat(F.NOT1.containsNode(F.NOT1)).isTrue();
        assertThat(F.NOT1.containsNode(F.f.not(F.AND1))).isTrue();
        assertThat(F.NOT1.containsNode(F.AND1)).isTrue();
        assertThat(F.NOT1.containsNode(F.A)).isTrue();
        assertThat(F.NOT1.containsNode(F.f.variable("b"))).isTrue();
        assertThat(F.NOT2.containsNode(F.NOT2)).isTrue();
        assertThat(F.NOT2.containsNode(F.OR1)).isTrue();
        assertThat(F.NOT2.containsNode(F.X)).isTrue();
        assertThat(F.NOT2.containsNode(F.Y)).isTrue();

        assertThat(F.NOT1.containsNode(F.OR1)).isFalse();
        assertThat(F.NOT1.containsNode(F.X)).isFalse();
        assertThat(F.NOT2.containsNode(F.NOT1)).isFalse();
        assertThat(F.NOT2.containsNode(F.AND1)).isFalse();
    }

    @Test
    public void testImplication() {
        assertThat(F.IMP1.containsNode(F.IMP1)).isTrue();
        assertThat(F.IMP1.containsNode(F.f.implication(F.A, F.B))).isTrue();
        assertThat(F.IMP2.containsNode(F.IMP2)).isTrue();
        assertThat(F.IMP3.containsNode(F.IMP3)).isTrue();
        assertThat(F.IMP4.containsNode(F.IMP4)).isTrue();
        assertThat(F.IMP1.containsNode(F.A)).isTrue();
        assertThat(F.IMP1.containsNode(F.B)).isTrue();
        assertThat(F.IMP2.containsNode(F.NA)).isTrue();
        assertThat(F.IMP2.containsNode(F.NB)).isTrue();
        assertThat(F.IMP2.containsNode(F.A)).isFalse();
        assertThat(F.IMP2.containsNode(F.B)).isFalse();
        assertThat(F.IMP3.containsNode(F.AND1)).isTrue();
        assertThat(F.IMP3.containsNode(F.OR1)).isTrue();
        assertThat(F.IMP3.containsNode(F.A)).isTrue();
        assertThat(F.IMP3.containsNode(F.B)).isTrue();
        assertThat(F.IMP3.containsNode(F.X)).isTrue();
        assertThat(F.IMP3.containsNode(F.Y)).isTrue();
        assertThat(F.IMP4.containsNode(F.f.equivalence(F.A, F.B))).isTrue();
        assertThat(F.IMP4.containsNode(F.f.equivalence(F.NX, F.NY))).isTrue();

        assertThat(F.IMP4.containsNode(F.C)).isFalse();
        assertThat(F.IMP4.containsNode(F.NOT1)).isFalse();
        assertThat(F.IMP4.containsNode(F.f.equivalence(F.X, F.NY))).isFalse();
        assertThat(F.IMP4.containsNode(F.f.equivalence(F.NY, F.X))).isFalse();
    }

    @Test
    public void testEquivalence() {
        assertThat(F.EQ1.containsNode(F.EQ1)).isTrue();
        assertThat(F.EQ1.containsNode(F.f.equivalence(F.A, F.B))).isTrue();
        assertThat(F.EQ4.containsNode(F.IMP1)).isTrue();
        assertThat(F.EQ4.containsNode(F.IMP2)).isTrue();
        assertThat(F.EQ4.containsNode(F.A)).isTrue();
        assertThat(F.EQ4.containsNode(F.B)).isTrue();

        assertThat(F.EQ2.containsNode(F.C)).isFalse();
        assertThat(F.EQ2.containsNode(F.NOT1)).isFalse();
    }

    @Test
    public void testOr() {
        assertThat(F.OR1.containsNode(F.f.or(F.X, F.Y))).isTrue();
        assertThat(F.OR1.containsNode(F.X)).isTrue();
        assertThat(F.OR1.containsNode(F.f.variable("y"))).isTrue();
        assertThat(F.OR3.containsNode(F.AND1)).isTrue();
        assertThat(F.OR3.containsNode(F.AND2)).isTrue();
        assertThat(F.OR3.containsNode(F.NA)).isTrue();
        assertThat(F.OR3.containsNode(F.NB)).isTrue();
        assertThat(F.OR3.containsNode(F.A)).isTrue();
        assertThat(F.OR3.containsNode(F.B)).isTrue();
        assertThat(F.f.or(F.A, F.B, F.NX, F.NY, F.C).containsNode(F.f.or(F.A, F.NX, F.C))).isTrue();
        assertThat(F.f.or(F.A, F.B, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.X, F.Y, F.AND1))).isTrue();
        assertThat(F.f.or(F.A, F.B, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.AND1, F.X))).isTrue();

        assertThat(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.B))).isFalse();
        assertThat(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.NY)).isFalse();
        assertThat(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.C))).isFalse();
    }

    @Test
    public void testAnd() {
        assertThat(F.AND1.containsNode(F.f.and(F.A, F.B))).isTrue();
        assertThat(F.AND1.containsNode(F.A)).isTrue();
        assertThat(F.AND1.containsNode(F.f.variable("b"))).isTrue();
        assertThat(F.AND3.containsNode(F.OR1)).isTrue();
        assertThat(F.AND3.containsNode(F.OR2)).isTrue();
        assertThat(F.AND3.containsNode(F.NX)).isTrue();
        assertThat(F.AND3.containsNode(F.NY)).isTrue();
        assertThat(F.AND3.containsNode(F.X)).isTrue();
        assertThat(F.AND3.containsNode(F.Y)).isTrue();
        assertThat(F.f.and(F.A, F.B, F.NX, F.NY, F.C).containsNode(F.f.and(F.A, F.NX, F.C))).isTrue();
        assertThat(F.f.and(F.X, F.Y, F.OR1, F.C, F.AND1).containsNode(F.f.and(F.A, F.B, F.C))).isTrue();
        assertThat(F.f.and(F.A, F.B, F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.and(F.A, F.OR1, F.NX))).isTrue();
        assertThat(F.f.and(F.A, F.B, F.NX, F.IMP1, F.C).containsNode(F.IMP1)).isTrue();

        assertThat(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.B))).isFalse();
        assertThat(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.NY)).isFalse();
        assertThat(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.C))).isFalse();
    }

    @Test
    public void subformulasTest() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        final Formula f1 = p.parse("((a & ~b & c) | (d & (~e | c))) & (a => (~x | y) & (x | ~z))");
        final LinkedHashSet<Formula> expected = new LinkedHashSet<>();
        expected.add(p.parse("a"));
        expected.add(p.parse("~b"));
        expected.add(p.parse("c"));
        expected.add(p.parse("a & ~b & c"));
        expected.add(p.parse("d"));
        expected.add(p.parse("~e"));
        expected.add(p.parse("~e | c"));
        expected.add(p.parse("d & (~e | c)"));
        expected.add(p.parse("(a & ~b & c) | (d & (~e | c))"));
        expected.add(p.parse("~x"));
        expected.add(p.parse("y"));
        expected.add(p.parse("~x | y"));
        expected.add(p.parse("x"));
        expected.add(p.parse("~z"));
        expected.add(p.parse("x | ~z"));
        expected.add(p.parse("(~x | y) & (x | ~z)"));
        expected.add(p.parse("a => (~x | y) & (x | ~z)"));
        expected.add(p.parse("((a & ~b & c) | (d & (~e | c))) & (a => (~x | y) & (x | ~z))"));
        assertThat(f1.apply(new SubNodeFunction())).isEqualTo(expected);
    }

    @Test
    public void testNotCache() throws ParserException {
        final PropositionalParser p = new PropositionalParser(F.f);
        final Formula f1 = p.parse("(d | (a & b)) & (c | (a & b)) | (a & b )");
        f1.apply(new SubNodeFunction(), false);
        assertThat(f1.functionCacheEntry(SUBFORMULAS)).isNull();
    }
}
