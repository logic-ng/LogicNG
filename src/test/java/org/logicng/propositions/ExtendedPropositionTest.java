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

import java.util.Objects;

/**
 * Unit tests for {@link ExtendedProposition}.
 * @version 2.0.0
 * @since 1.0
 */
public class ExtendedPropositionTest {

    private final PropositionalParser p;
    private final ExtendedProposition<Backpack> prop1;
    private final ExtendedProposition<Backpack> prop2;

    public ExtendedPropositionTest() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        this.p = new PropositionalParser(f);
        this.prop1 = new ExtendedProposition<>(new Backpack("prop1"), this.p.parse("a & b"));
        this.prop2 = new ExtendedProposition<>(new Backpack("prop2"), this.p.parse("a & b & ~c"));
    }

    @Test
    public void testGetters() throws ParserException {
        assertThat(this.prop1.formula()).isEqualTo(this.p.parse("a & b"));
        assertThat(this.prop2.formula()).isEqualTo(this.p.parse("a & b & ~c"));

        assertThat(this.prop1.backpack().description).isEqualTo("prop1");
        assertThat(this.prop2.backpack().description).isEqualTo("prop2");
    }

    @Test
    public void testHashCode() throws ParserException {
        final ExtendedProposition<Backpack> prop11 = new ExtendedProposition<>(new Backpack("prop1"), this.p.parse("a & b"));
        assertThat(this.prop1.hashCode()).isEqualTo(this.prop1.hashCode());
        assertThat(prop11.hashCode()).isEqualTo(this.prop1.hashCode());
    }

    @Test
    public void testEquals() throws ParserException {
        final ExtendedProposition<Backpack> prop11 = new ExtendedProposition<>(new Backpack("prop1"), this.p.parse("a & b"));
        final ExtendedProposition<Backpack> prop21 = new ExtendedProposition<>(new Backpack("prop2"), this.p.parse("a & b & ~c"));
        assertThat(this.prop1.equals(this.prop1)).isTrue();
        assertThat(this.prop1.equals(prop11)).isTrue();
        assertThat(this.prop2.equals(prop21)).isTrue();
        assertThat(this.prop1.equals(this.prop2)).isFalse();
        assertThat(this.prop1.equals(null)).isFalse();
        assertThat(this.prop1.equals("String")).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.prop1.toString()).isEqualTo("ExtendedProposition{formula=a & b, backpack=prop1}");
        assertThat(this.prop2.toString()).isEqualTo("ExtendedProposition{formula=a & b & ~c, backpack=prop2}");
    }

    private static final class Backpack implements PropositionBackpack {
        private final String description;

        private Backpack(final String description) {
            this.description = description;
        }

        @Override
        public int hashCode() {
            return this.description.hashCode();
        }

        @Override
        public boolean equals(final Object obj) {
            return obj instanceof Backpack && Objects.equals(this.description, ((Backpack) obj).description);
        }

        @Override
        public String toString() {
            return this.description;
        }
    }
}
