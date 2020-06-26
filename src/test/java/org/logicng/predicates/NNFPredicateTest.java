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

import org.junit.jupiter.api.Test;
import org.logicng.formulas.F;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for the nnf predicate.
 * @version 2.0.0
 * @since 1.5.1
 */
public class NNFPredicateTest {

    private final NNFPredicate nnfPredicate = NNFPredicate.get();

    @Test
    public void test() {
        assertThat(F.f.verum().holds(this.nnfPredicate)).isTrue();
        assertThat(F.f.falsum().holds(this.nnfPredicate)).isTrue();
        assertThat(F.A.holds(this.nnfPredicate)).isTrue();
        assertThat(F.NA.holds(this.nnfPredicate)).isTrue();
        assertThat(F.OR1.holds(this.nnfPredicate)).isTrue();
        assertThat(F.AND1.holds(this.nnfPredicate)).isTrue();
        assertThat(F.AND3.holds(this.nnfPredicate)).isTrue();
        assertThat(F.f.and(F.OR1, F.OR2, F.A, F.NY).holds(this.nnfPredicate)).isTrue();
        assertThat(F.f.and(F.OR1, F.OR2, F.AND1, F.AND2, F.AND3, F.A, F.NY).holds(this.nnfPredicate)).isTrue();
        assertThat(F.OR3.holds(this.nnfPredicate)).isTrue();
        assertThat(F.PBC1.holds(this.nnfPredicate)).isFalse();
        assertThat(F.IMP1.holds(this.nnfPredicate)).isFalse();
        assertThat(F.EQ1.holds(this.nnfPredicate)).isFalse();
        assertThat(F.NOT1.holds(this.nnfPredicate)).isFalse();
        assertThat(F.NOT2.holds(this.nnfPredicate)).isFalse();
        assertThat(F.f.and(F.OR1, F.f.not(F.OR2), F.A, F.NY).holds(this.nnfPredicate)).isFalse();
        assertThat(F.f.and(F.OR1, F.EQ1).holds(this.nnfPredicate)).isFalse();
        assertThat(F.f.and(F.OR1, F.IMP1, F.AND1).holds(this.nnfPredicate)).isFalse();
        assertThat(F.f.and(F.OR1, F.PBC1, F.AND1).holds(this.nnfPredicate)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.nnfPredicate.toString()).isEqualTo("NNFPredicate");
    }
}
