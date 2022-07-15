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

package org.logicng.predicates;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;

/**
 * Unit tests for the term predicate.
 * @version 2.3.0
 * @since 2.2.0
 */
public class TermPredicateTest extends TestWithExampleFormulas {

    private final TermPredicate mintermPredicate = TermPredicate.minterm();
    private final TermPredicate maxtermPredicate = TermPredicate.maxterm();

    @Test
    public void testMintermPredicate() {
        assertThat(this.f.verum().holds(this.mintermPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.mintermPredicate)).isTrue();
        assertThat(this.A.holds(this.mintermPredicate)).isTrue();
        assertThat(this.NA.holds(this.mintermPredicate)).isTrue();
        assertThat(this.AND1.holds(this.mintermPredicate)).isTrue();
        assertThat(this.OR1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.OR3.holds(this.mintermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.AND2, this.A, this.NY).holds(this.mintermPredicate)).isFalse();
        assertThat(this.PBC1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.AND3.holds(this.mintermPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.mintermPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.mintermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.EQ1).holds(this.mintermPredicate)).isFalse();
    }

    @Test
    public void testMaxtermPredicate() {
        assertThat(this.f.verum().holds(this.maxtermPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.maxtermPredicate)).isTrue();
        assertThat(this.A.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.NA.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.AND1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.OR1.holds(this.maxtermPredicate)).isTrue();
        assertThat(this.OR3.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.AND2, this.A, this.NY).holds(this.maxtermPredicate)).isFalse();
        assertThat(this.PBC1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.AND3.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.maxtermPredicate)).isFalse();
        assertThat(this.f.or(this.AND1, this.EQ1).holds(this.maxtermPredicate)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.mintermPredicate.toString()).isEqualTo("TermPredicate");
        assertThat(this.maxtermPredicate.toString()).isEqualTo("TermPredicate");
    }
}
