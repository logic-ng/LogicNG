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

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link BackboneSimplifier}.
 * @version 2.0.0
 * @since 1.5.0
 */
public class BackboneSimplifierTest {

    private final BackboneSimplifier backboneSimplifier = new BackboneSimplifier();

    @Test
    public void testTrivialBackbones() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(p.parse("$true").transform(this.backboneSimplifier)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(this.backboneSimplifier)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A & (A => B) & ~B").transform(this.backboneSimplifier)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(this.backboneSimplifier)).isEqualTo(p.parse("A"));
        assertThat(p.parse("A & B").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B"));
        assertThat(p.parse("A | B | C").transform(this.backboneSimplifier)).isEqualTo(p.parse("A | B | C"));
    }

    @Test
    public void testRealBackbones() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(p.parse("A & B & (B | C)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B"));
        assertThat(p.parse("A & B & (~B | C)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B & C"));
        assertThat(p.parse("A & B & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("A & B & C & F"));
        assertThat(p.parse("X & Y & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("X & Y & (~B | C) & (B | D) & (A => F)"));
        assertThat(p.parse("~A & ~B & (~B | C) & (B | D) & (A => F)").transform(this.backboneSimplifier)).isEqualTo(p.parse("~A & ~B & D"));
    }
}
