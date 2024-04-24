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

package org.logicng.util;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link Pair}.
 * @version 2.0.0
 * @since 1.0
 */
public class PairTest {

    private final Pair<String, Integer> pair1 = new Pair<>("abc", 12);
    private final Pair<String, Integer> pair2 = new Pair<>("cde", 12);
    private final Pair<String, Integer> pair3 = new Pair<>("cde", 42);

    @Test
    public void testGetters() {
        assertThat(this.pair1.first()).isEqualTo("abc");
        assertThat(this.pair2.first()).isEqualTo("cde");
        assertThat(this.pair3.first()).isEqualTo("cde");
        assertThat((int) this.pair1.second()).isEqualTo(12);
        assertThat((int) this.pair2.second()).isEqualTo(12);
        assertThat((int) this.pair3.second()).isEqualTo(42);
    }

    @Test
    public void testHashCode() {
        assertThat(this.pair1.hashCode()).isEqualTo(this.pair1.hashCode());
        assertThat(new Pair<>("abc", 12).hashCode()).isEqualTo(this.pair1.hashCode());
    }

    @Test
    public void testEquals() {
        assertThat(this.pair1.equals(this.pair1)).isTrue();
        assertThat(this.pair1.equals(new Pair<>("abc", 12))).isTrue();
        assertThat(this.pair1.equals(this.pair2)).isFalse();
        assertThat(this.pair2.equals(this.pair3)).isFalse();
        assertThat(this.pair1.equals(this.pair3)).isFalse();
        assertThat(this.pair1.equals("String")).isFalse();
        assertThat(this.pair1.equals(null)).isFalse();
    }

    @Test
    public void testToString() {
        assertThat(this.pair1.toString()).isEqualTo("<abc, 12>");
        assertThat(this.pair2.toString()).isEqualTo("<cde, 12>");
        assertThat(this.pair3.toString()).isEqualTo("<cde, 42>");
    }

}
