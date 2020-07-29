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

package org.logicng.explanations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.propositions.StandardProposition;

import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for {@link UNSATCore}.
 * @version 2.0.0
 * @since 1.1
 */
public class UNSATCoreTest {

    private final List<StandardProposition> props1;
    private final List<StandardProposition> props2;
    private final UNSATCore<?> core1;
    private final UNSATCore<?> core2;

    public UNSATCoreTest() throws ParserException {
        this.props1 = new ArrayList<>();
        this.props2 = new ArrayList<>();
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        this.props1.add(new StandardProposition(parser.parse("a | b")));
        this.props1.add(new StandardProposition(parser.parse("~a | b")));
        this.props1.add(new StandardProposition(parser.parse("a | ~b")));
        this.props1.add(new StandardProposition(parser.parse("~a | ~b")));
        this.props2.add(new StandardProposition(parser.parse("a | b")));
        this.props2.add(new StandardProposition(parser.parse("~a | b")));
        this.props2.add(new StandardProposition(parser.parse("a | ~b")));
        this.props2.add(new StandardProposition(parser.parse("~a | ~b")));
        this.props2.add(new StandardProposition(parser.parse("~a | ~b | c")));
        this.core1 = new UNSATCore<>(this.props1, true);
        this.core2 = new UNSATCore<>(this.props2, false);
    }

    @Test
    public void testGetters() {
        assertThat(this.core1.propositions()).isEqualTo(this.props1);
        assertThat(this.core2.propositions()).isEqualTo(this.props2);
        assertThat(this.core1.isMUS()).isTrue();
        assertThat(this.core2.isMUS()).isFalse();
    }

    @Test
    public void testHashCode() {
        assertThat(this.core1.hashCode()).isEqualTo(this.core1.hashCode());
        assertThat(new UNSATCore<>(this.props2, false).hashCode()).isEqualTo(this.core2.hashCode());
    }

    @Test
    public void testEquals() {
        assertThat(this.core1).isEqualTo(this.core1);
        assertThat(this.core1.equals(this.core1)).isTrue();
        assertThat(new UNSATCore<>(this.props1, true)).isEqualTo(this.core1);
        assertThat(this.core2).isNotEqualTo(this.core1);
        assertThat(new UNSATCore<>(this.props1, false)).isNotEqualTo(this.core1);
        assertThat(new UNSATCore<>(this.props2, true)).isNotEqualTo(this.core1);
        assertThat("String").isNotEqualTo(this.core1);
        assertThat(this.core1).isNotEqualTo("String");
    }

    @Test
    public void testToString() {
        final String exp1 = "UNSATCore{isMUS=true, propositions=[StandardProposition{formula=a | b, description=}, " +
                "StandardProposition{formula=~a | b, description=}, StandardProposition{formula=a | ~b, " +
                "description=}, StandardProposition{formula=~a | ~b, description=}]}";
        final String exp2 = "UNSATCore{isMUS=false, propositions=[StandardProposition{formula=a | b, description=}, " +
                "StandardProposition{formula=~a | b, description=}, StandardProposition{formula=a | ~b, " +
                "description=}, StandardProposition{formula=~a | ~b, description=}, " +
                "StandardProposition{formula=~a | ~b | c, description=}]}";
        assertThat(this.core1.toString()).isEqualTo(exp1);
        assertThat(this.core2.toString()).isEqualTo(exp2);
    }
}
