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
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.LinkedHashSet;

/**
 * Unit tests for {@link SubNodeFunction}.
 * @version 2.0.0
 * @since 1.0
 */
public class SubformulaTest extends TestWithExampleFormulas {

    @Test
    public void testConstants() {
        assertThat(this.TRUE.containsNode(this.TRUE)).isTrue();
        assertThat(this.FALSE.containsNode(this.FALSE)).isTrue();
        assertThat(this.TRUE.containsNode(this.FALSE)).isFalse();
        assertThat(this.FALSE.containsNode(this.TRUE)).isFalse();
        assertThat(this.FALSE.containsNode(this.A)).isFalse();
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.containsNode(this.A)).isTrue();
        assertThat(this.A.containsNode(this.f.variable("a"))).isTrue();
        assertThat(this.NA.containsNode(this.A)).isFalse();
        assertThat(this.NA.containsNode(this.f.literal("a", false))).isTrue();
        assertThat(this.A.containsNode(this.NA)).isFalse();
        assertThat(this.A.containsNode(this.B)).isFalse();
        assertThat(this.NA.containsNode(this.NB)).isFalse();
        assertThat(this.A.containsNode(this.FALSE)).isFalse();
        assertThat(this.NA.containsNode(this.TRUE)).isFalse();
    }

    @Test
    public void testNot() {
        assertThat(this.NOT1.containsNode(this.NOT1)).isTrue();
        assertThat(this.NOT1.containsNode(this.f.not(this.AND1))).isTrue();
        assertThat(this.NOT1.containsNode(this.AND1)).isTrue();
        assertThat(this.NOT1.containsNode(this.A)).isTrue();
        assertThat(this.NOT1.containsNode(this.f.variable("b"))).isTrue();
        assertThat(this.NOT2.containsNode(this.NOT2)).isTrue();
        assertThat(this.NOT2.containsNode(this.OR1)).isTrue();
        assertThat(this.NOT2.containsNode(this.X)).isTrue();
        assertThat(this.NOT2.containsNode(this.Y)).isTrue();

        assertThat(this.NOT1.containsNode(this.OR1)).isFalse();
        assertThat(this.NOT1.containsNode(this.X)).isFalse();
        assertThat(this.NOT2.containsNode(this.NOT1)).isFalse();
        assertThat(this.NOT2.containsNode(this.AND1)).isFalse();
    }

    @Test
    public void testImplication() {
        assertThat(this.IMP1.containsNode(this.IMP1)).isTrue();
        assertThat(this.IMP1.containsNode(this.f.implication(this.A, this.B))).isTrue();
        assertThat(this.IMP2.containsNode(this.IMP2)).isTrue();
        assertThat(this.IMP3.containsNode(this.IMP3)).isTrue();
        assertThat(this.IMP4.containsNode(this.IMP4)).isTrue();
        assertThat(this.IMP1.containsNode(this.A)).isTrue();
        assertThat(this.IMP1.containsNode(this.B)).isTrue();
        assertThat(this.IMP2.containsNode(this.NA)).isTrue();
        assertThat(this.IMP2.containsNode(this.NB)).isTrue();
        assertThat(this.IMP2.containsNode(this.A)).isFalse();
        assertThat(this.IMP2.containsNode(this.B)).isFalse();
        assertThat(this.IMP3.containsNode(this.AND1)).isTrue();
        assertThat(this.IMP3.containsNode(this.OR1)).isTrue();
        assertThat(this.IMP3.containsNode(this.A)).isTrue();
        assertThat(this.IMP3.containsNode(this.B)).isTrue();
        assertThat(this.IMP3.containsNode(this.X)).isTrue();
        assertThat(this.IMP3.containsNode(this.Y)).isTrue();
        assertThat(this.IMP4.containsNode(this.f.equivalence(this.A, this.B))).isTrue();
        assertThat(this.IMP4.containsNode(this.f.equivalence(this.NX, this.NY))).isTrue();

        assertThat(this.IMP4.containsNode(this.C)).isFalse();
        assertThat(this.IMP4.containsNode(this.NOT1)).isFalse();
        assertThat(this.IMP4.containsNode(this.f.equivalence(this.X, this.NY))).isFalse();
        assertThat(this.IMP4.containsNode(this.f.equivalence(this.NY, this.X))).isFalse();
    }

    @Test
    public void testEquivalence() {
        assertThat(this.EQ1.containsNode(this.EQ1)).isTrue();
        assertThat(this.EQ1.containsNode(this.f.equivalence(this.A, this.B))).isTrue();
        assertThat(this.EQ4.containsNode(this.IMP1)).isTrue();
        assertThat(this.EQ4.containsNode(this.IMP2)).isTrue();
        assertThat(this.EQ4.containsNode(this.A)).isTrue();
        assertThat(this.EQ4.containsNode(this.B)).isTrue();

        assertThat(this.EQ2.containsNode(this.C)).isFalse();
        assertThat(this.EQ2.containsNode(this.NOT1)).isFalse();
    }

    @Test
    public void testOr() {
        assertThat(this.OR1.containsNode(this.f.or(this.X, this.Y))).isTrue();
        assertThat(this.OR1.containsNode(this.X)).isTrue();
        assertThat(this.OR1.containsNode(this.f.variable("y"))).isTrue();
        assertThat(this.OR3.containsNode(this.AND1)).isTrue();
        assertThat(this.OR3.containsNode(this.AND2)).isTrue();
        assertThat(this.OR3.containsNode(this.NA)).isTrue();
        assertThat(this.OR3.containsNode(this.NB)).isTrue();
        assertThat(this.OR3.containsNode(this.A)).isTrue();
        assertThat(this.OR3.containsNode(this.B)).isTrue();
        assertThat(this.f.or(this.A, this.B, this.NX, this.NY, this.C).containsNode(this.f.or(this.A, this.NX, this.C))).isTrue();
        assertThat(this.f.or(this.A, this.B, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.X, this.Y, this.AND1))).isTrue();
        assertThat(this.f.or(this.A, this.B, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.A, this.AND1, this.X))).isTrue();

        assertThat(this.f.or(this.NX, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.A, this.B))).isFalse();
        assertThat(this.f.or(this.NX, this.OR1, this.C, this.AND1).containsNode(this.NY)).isFalse();
        assertThat(this.f.or(this.NX, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.A, this.C))).isFalse();
    }

    @Test
    public void testAnd() {
        assertThat(this.AND1.containsNode(this.f.and(this.A, this.B))).isTrue();
        assertThat(this.AND1.containsNode(this.A)).isTrue();
        assertThat(this.AND1.containsNode(this.f.variable("b"))).isTrue();
        assertThat(this.AND3.containsNode(this.OR1)).isTrue();
        assertThat(this.AND3.containsNode(this.OR2)).isTrue();
        assertThat(this.AND3.containsNode(this.NX)).isTrue();
        assertThat(this.AND3.containsNode(this.NY)).isTrue();
        assertThat(this.AND3.containsNode(this.X)).isTrue();
        assertThat(this.AND3.containsNode(this.Y)).isTrue();
        assertThat(this.f.and(this.A, this.B, this.NX, this.NY, this.C).containsNode(this.f.and(this.A, this.NX, this.C))).isTrue();
        assertThat(this.f.and(this.X, this.Y, this.OR1, this.C, this.AND1).containsNode(this.f.and(this.A, this.B, this.C))).isTrue();
        assertThat(this.f.and(this.A, this.B, this.NX, this.OR1, this.C, this.AND1).containsNode(this.f.and(this.A, this.OR1, this.NX))).isTrue();
        assertThat(this.f.and(this.A, this.B, this.NX, this.IMP1, this.C).containsNode(this.IMP1)).isTrue();

        assertThat(this.f.and(this.NX, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.A, this.B))).isFalse();
        assertThat(this.f.and(this.NX, this.OR1, this.C, this.AND1).containsNode(this.NY)).isFalse();
        assertThat(this.f.and(this.NX, this.OR1, this.C, this.AND1).containsNode(this.f.or(this.A, this.C))).isFalse();
    }

    @Test
    public void subformulasTest() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
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
        final PropositionalParser p = new PropositionalParser(this.f);
        final Formula f1 = p.parse("(d | (a & b)) & (c | (a & b)) | (a & b )");
        f1.apply(new SubNodeFunction(), false);
        assertThat(f1.functionCacheEntry(SUBFORMULAS)).isNull();
    }
}
