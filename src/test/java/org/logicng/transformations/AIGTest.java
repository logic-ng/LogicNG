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

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.AIGPredicate;

/**
 * Unit Tests for AIG conversion.
 * @version 2.0.0
 * @since 1.0
 */
public class AIGTest extends TestWithExampleFormulas {

    private final AIGTransformation aigTrans = new AIGTransformation();
    private final AIGPredicate aigPred = AIGPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.aigTrans)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.aigTrans)).isEqualTo(this.FALSE);
        assertThat(this.TRUE.holds(this.aigPred)).isTrue();
        assertThat(this.FALSE.holds(this.aigPred)).isTrue();
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.aigTrans)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.aigTrans)).isEqualTo(this.NA);
        assertThat(this.A.holds(this.aigPred)).isTrue();
        assertThat(this.NA.holds(this.aigPred)).isTrue();
    }

    @Test
    public void testBinaryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.IMP1.transform(this.aigTrans)).isEqualTo(p.parse("~(a & ~b)"));
        assertThat(this.IMP2.transform(this.aigTrans)).isEqualTo(p.parse("~(~a & b)"));
        assertThat(this.IMP3.transform(this.aigTrans)).isEqualTo(p.parse("~((a & b) & (~x & ~y))"));
        assertThat(this.EQ1.transform(this.aigTrans)).isEqualTo(p.parse("~(a & ~b) & ~(~a & b)"));
        assertThat(this.EQ2.transform(this.aigTrans)).isEqualTo(p.parse("~(a & ~b) & ~(~a & b)"));
        assertThat(this.IMP1.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.IMP2.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.IMP3.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.EQ1.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.EQ2.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.IMP1.holds(this.aigPred)).isFalse();
        assertThat(this.IMP2.holds(this.aigPred)).isFalse();
        assertThat(this.IMP3.holds(this.aigPred)).isFalse();
        assertThat(this.EQ1.holds(this.aigPred)).isFalse();
        assertThat(this.EQ2.holds(this.aigPred)).isFalse();
        final Formula impl = p.parse("m => n");
        impl.transform(this.aigTrans, false);
        final Formula aigIMPL = impl.transformationCacheEntry(TransformationCacheEntry.AIG);
        assertThat(aigIMPL).isNull();
        final Formula equi = p.parse("m <=> n");
        equi.transform(this.aigTrans, false);
        final Formula aigEQUI = impl.transformationCacheEntry(TransformationCacheEntry.AIG);
        assertThat(aigEQUI).isNull();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.transform(this.aigTrans)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.aigTrans)).isEqualTo(p.parse("~(~x & ~y)"));
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.aigTrans)).isEqualTo(p.parse("(~a & ~b) & c & ~(x & ~y) & ~(w & ~z)"));
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.aigTrans)).isEqualTo(p.parse("~(a & b & ~c & ~(~x & y))"));
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.aigTrans)).isEqualTo(p.parse("~(~a & ~b & ~(~x & ~y))"));
        assertThat(this.AND1.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.OR1.transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.aigTrans).holds(this.aigPred)).isTrue();
        assertThat(this.AND1.holds(this.aigPred)).isTrue();
        assertThat(this.f.and(this.AND1, this.PBC1).holds(this.aigPred)).isFalse();
        assertThat(this.OR1.holds(this.aigPred)).isFalse();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").holds(this.aigPred)).isFalse();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").holds(this.aigPred)).isFalse();
        assertThat(p.parse("a | b | (~x & ~y)").holds(this.aigPred)).isFalse();
        final Formula or = p.parse("m | n | o");
        or.transform(this.aigTrans, false);
        final Formula aigOR = or.transformationCacheEntry(TransformationCacheEntry.AIG);
        assertThat(aigOR).isNull();
        final Formula and = p.parse("m & n & o");
        and.transform(this.aigTrans, false);
        final Formula aigAND = and.transformationCacheEntry(TransformationCacheEntry.AIG);
        assertThat(aigAND).isNull();
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.aigTrans)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.aigTrans)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").transform(this.aigTrans)).isEqualTo(p.parse("a & ~b"));
        assertThat(p.parse("~(~(a | b) => ~(x | y))").transform(this.aigTrans)).isEqualTo(p.parse("(~a & ~b) & ~(~x & ~y)"));
        assertThat(p.parse("~(a <=> b)").transform(this.aigTrans)).isEqualTo(p.parse("~(~(a & ~b) & ~(~a & b))"));
        assertThat(p.parse("~(~(a | b) <=> ~(x | y))").transform(this.aigTrans)).isEqualTo(p.parse("~(~(~a & ~b & ~(~x & ~y)) & ~((a | b) & ~(x | y)))"));
        assertThat(p.parse("~(a & b & ~x & ~y)").transform(this.aigTrans)).isEqualTo(p.parse("~(a & b & ~x & ~y)"));
        assertThat(p.parse("~(a | b | ~x | ~y)").transform(this.aigTrans)).isEqualTo(p.parse("~a & ~b & x & y"));
        assertThat(p.parse("~(a | b | ~x | ~y)").transform(this.aigTrans)).isEqualTo(p.parse("~a & ~b & x & y")); // test caching
        final Formula not = p.parse("~(m | n)");
        not.transform(this.aigTrans, false);
        final Formula aig = not.transformationCacheEntry(TransformationCacheEntry.AIG);
        assertThat(aig).isNull();
    }

    @Test
    public void testPBC() {
        assertThat(this.PBC1.transform(this.aigTrans).holds(this.aigPred)).isTrue();
    }

    @Test
    public void testToString() {
        assertThat(this.aigTrans.toString()).isEqualTo("AIGTransformation");
        assertThat(this.aigPred.toString()).isEqualTo("AIGPredicate");
    }
}
