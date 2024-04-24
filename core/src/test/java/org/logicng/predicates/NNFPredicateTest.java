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
 * Unit tests for the nnf predicate.
 * @version 2.0.0
 * @since 1.5.1
 */
public class NNFPredicateTest extends TestWithExampleFormulas {

    private final NNFPredicate nnfPredicate = NNFPredicate.get();

    @Test
    public void test() {
        assertThat(this.f.verum().holds(this.nnfPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.nnfPredicate)).isTrue();
        assertThat(this.A.holds(this.nnfPredicate)).isTrue();
        assertThat(this.NA.holds(this.nnfPredicate)).isTrue();
        assertThat(this.OR1.holds(this.nnfPredicate)).isTrue();
        assertThat(this.AND1.holds(this.nnfPredicate)).isTrue();
        assertThat(this.AND3.holds(this.nnfPredicate)).isTrue();
        assertThat(this.f.and(this.OR1, this.OR2, this.A, this.NY).holds(this.nnfPredicate)).isTrue();
        assertThat(this.f.and(this.OR1, this.OR2, this.AND1, this.AND2, this.AND3, this.A, this.NY).holds(this.nnfPredicate)).isTrue();
        assertThat(this.OR3.holds(this.nnfPredicate)).isTrue();
        assertThat(this.PBC1.holds(this.nnfPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.nnfPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.nnfPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.nnfPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.nnfPredicate)).isFalse();
        assertThat(this.f.and(this.OR1, this.f.not(this.OR2), this.A, this.NY).holds(this.nnfPredicate)).isFalse();
        assertThat(this.f.and(this.OR1, this.EQ1).holds(this.nnfPredicate)).isFalse();
        assertThat(this.f.and(this.OR1, this.IMP1, this.AND1).holds(this.nnfPredicate)).isFalse();
        assertThat(this.f.and(this.OR1, this.PBC1, this.AND1).holds(this.nnfPredicate)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.nnfPredicate.toString()).isEqualTo("NNFPredicate");
    }
}
