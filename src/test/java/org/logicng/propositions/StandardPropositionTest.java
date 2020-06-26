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

package org.logicng.propositions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link StandardProposition}.
 * @version 2.0.0
 * @since 1.0
 */
public class StandardPropositionTest {

    private final PropositionalParser p;
    private final StandardProposition prop1;
    private final StandardProposition prop2;

    public StandardPropositionTest() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        this.p = new PropositionalParser(f);
        this.prop1 = new StandardProposition(this.p.parse("a & b"));
        this.prop2 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
    }

    @Test
    public void testGetters() throws ParserException {
        assertThat(this.prop1.formula()).isEqualTo(this.p.parse("a & b"));
        assertThat(this.prop2.formula()).isEqualTo(this.p.parse("a & b & ~c"));

        assertThat(this.prop1.description()).isEqualTo("");
        assertThat(this.prop2.description()).isEqualTo("prop2");
    }

    @Test
    public void testHashCode() throws ParserException {
        final StandardProposition prop11 = new StandardProposition(this.p.parse("a & b"));
        final StandardProposition prop21 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
        assertThat(this.prop1.hashCode()).isEqualTo(this.prop1.hashCode());
        assertThat(prop11.hashCode()).isEqualTo(this.prop1.hashCode());
        assertThat(prop21.hashCode()).isEqualTo(this.prop2.hashCode());
    }

    @Test
    public void testEquals() throws ParserException {
        final StandardProposition prop11 = new StandardProposition(this.p.parse("a & b"));
        final StandardProposition prop21 = new StandardProposition("prop2", this.p.parse("a & b & ~c"));
        assertThat(this.prop1.equals(this.prop1)).isTrue();
        assertThat(this.prop1.equals(prop11)).isTrue();
        assertThat(this.prop2.equals(prop21)).isTrue();
        assertThat(this.prop1.equals(this.prop2)).isFalse();
        assertThat(this.prop1.equals(null)).isFalse();
        assertThat(this.prop1.equals("String")).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.prop1.toString()).isEqualTo("StandardProposition{formula=a & b, description=}");
        assertThat(this.prop2.toString()).isEqualTo("StandardProposition{formula=a & b & ~c, description=prop2}");
    }
}
