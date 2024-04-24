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
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;

/**
 * Unit tests for the cnf predicate.
 * @version 2.0.0
 * @since 1.0
 */
public class CNFPredicateTest extends TestWithExampleFormulas {

    private final CNFPredicate cnfPredicate = CNFPredicate.get();

    @Test
    public void test() {
        assertThat(this.f.verum().holds(this.cnfPredicate)).isTrue();
        assertThat(this.f.falsum().holds(this.cnfPredicate)).isTrue();
        assertThat(this.A.holds(this.cnfPredicate)).isTrue();
        assertThat(this.NA.holds(this.cnfPredicate)).isTrue();
        assertThat(this.OR1.holds(this.cnfPredicate)).isTrue();
        assertThat(this.AND1.holds(this.cnfPredicate)).isTrue();
        assertThat(this.AND3.holds(this.cnfPredicate)).isTrue();
        assertThat(this.f.and(this.OR1, this.OR2, this.A, this.NY).holds(this.cnfPredicate)).isTrue();
        assertThat(this.PBC1.holds(this.cnfPredicate)).isFalse();
        assertThat(this.OR3.holds(this.cnfPredicate)).isFalse();
        assertThat(this.IMP1.holds(this.cnfPredicate)).isFalse();
        assertThat(this.EQ1.holds(this.cnfPredicate)).isFalse();
        assertThat(this.NOT1.holds(this.cnfPredicate)).isFalse();
        assertThat(this.NOT2.holds(this.cnfPredicate)).isFalse();
        assertThat(this.f.and(this.OR1, this.EQ1).holds(this.cnfPredicate)).isFalse();
    }

    @Test
    public void testIllegalAnd() {
        this.AND1.setPredicateCacheEntry(IS_CNF, null);
        assertThatThrownBy(() -> this.AND1.holds(this.cnfPredicate)).isInstanceOf(IllegalStateException.class);
    }

    @Test
    public void testToString() {
        assertThat(this.cnfPredicate.toString()).isEqualTo("CNFPredicate");
    }
}
