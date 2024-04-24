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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;

import java.util.HashMap;
import java.util.Map;

/**
 * Unit tests for {@link LiteralSubstitution}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class LiteralSubstitutionTest {

    private final FormulaFactory f = new FormulaFactory();
    private LiteralSubstitution s1;

    @BeforeEach
    public void init() {
        final Map<Literal, Literal> map = new HashMap<>();
        map.put(this.f.literal("a", true), this.f.literal("a_t", true));
        map.put(this.f.literal("a", false), this.f.literal("a_f", true));
        map.put(this.f.literal("b", false), this.f.literal("x", true));
        this.s1 = new LiteralSubstitution(map);
        this.s1.addSubstitution(this.f.literal("c", true), this.f.literal("y", true));
    }

    @Test
    public void testSimpleFormula() throws ParserException {
        assertThat(this.f.parse("$true").transform(this.s1)).isEqualTo(this.f.parse("$true"));
        assertThat(this.f.parse("$false").transform(this.s1)).isEqualTo(this.f.parse("$false"));
    }

    @Test
    public void testLiterals() throws ParserException {
        assertThat(this.f.parse("m").transform(this.s1)).isEqualTo(this.f.parse("m"));
        assertThat(this.f.parse("~m").transform(this.s1)).isEqualTo(this.f.parse("~m"));
        assertThat(this.f.parse("a").transform(this.s1)).isEqualTo(this.f.parse("a_t"));
        assertThat(this.f.parse("~a").transform(this.s1)).isEqualTo(this.f.parse("a_f"));
        assertThat(this.f.parse("b").transform(this.s1)).isEqualTo(this.f.parse("b"));
        assertThat(this.f.parse("~b").transform(this.s1)).isEqualTo(this.f.parse("x"));
        assertThat(this.f.parse("c").transform(this.s1)).isEqualTo(this.f.parse("y"));
        assertThat(this.f.parse("~c").transform(this.s1)).isEqualTo(this.f.parse("~y"));
    }

    @Test
    public void testFormulas() throws ParserException {
        assertThat(this.f.parse("~(a & b & ~c & x)").transform(this.s1)).isEqualTo(this.f.parse("~(a_t & b & ~y & x)"));
        assertThat(this.f.parse("a & b & ~c & x").transform(this.s1)).isEqualTo(this.f.parse("a_t & b & ~y & x"));
        assertThat(this.f.parse("a | b | ~c | x").transform(this.s1)).isEqualTo(this.f.parse("a_t | b | ~y | x"));
        assertThat(this.f.parse("(a | b) => (~c | x)").transform(this.s1)).isEqualTo(this.f.parse("(a_t | b) => (~y | x)"));
        assertThat(this.f.parse("(a | b) <=> (~c | x)").transform(this.s1)).isEqualTo(this.f.parse("(a_t | b) <=> (~y | x)"));
        assertThat(this.f.parse("2*a + 3*~b + -4*~c + x <= 5").transform(this.s1)).isEqualTo(this.f.parse("2*a_t + 3*x + -4*~y + x <= 5"));
    }

    @Test
    public void testEmptySubstitution() throws ParserException {
        assertThat(this.f.parse("2*a + 3*~b + -4*~c + x <= 5").transform(new LiteralSubstitution())).isEqualTo(this.f.parse("2*a + 3*~b + -4*~c + x <= 5"));
    }

}
