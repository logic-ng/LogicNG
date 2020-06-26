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

package org.logicng.collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link LNGBooleanVector}.
 * @version 2.0.0
 * @since 1.0
 */
public class LNGBooleanVectorTest {

    @Test
    public void testVectorCreation() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.size()).isEqualTo(0);
        assertThat(v1.empty()).isTrue();
        final LNGBooleanVector v2 = new LNGBooleanVector(10);
        assertThat(v2.size()).isEqualTo(0);
        assertThat(v2.empty()).isTrue();
        final LNGBooleanVector v3 = new LNGBooleanVector(10, true);
        assertThat(v3.size()).isEqualTo(10);
        for (int i = 0; i < v3.size(); i++) {
            assertThat(v3.get(i)).isTrue();
        }
        assertThat(v3.empty()).isFalse();
        final LNGBooleanVector v4 = new LNGBooleanVector(v3);
        assertThat(v4.size()).isEqualTo(10);
        for (int i = 0; i < v4.size(); i++) {
            assertThat(v4.get(i)).isTrue();
        }
        assertThat(v4.empty()).isFalse();
        final LNGBooleanVector v5 = new LNGBooleanVector(true, true, true, false, false);
        assertThat(v5.size()).isEqualTo(5);
        for (int i = 0; i < 5; i++) {
            if (i < 3) {
                assertThat(v5.get(i)).isTrue();
            } else {
                assertThat(v5.get(i)).isFalse();
            }
        }
    }

    @Test
    public void testVectorAddElements() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i % 2 == 0);
            assertThat(v1.get(i)).isEqualTo(i % 2 == 0);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void legalUnsafePush() {
        final LNGBooleanVector v1 = new LNGBooleanVector(1000);
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.unsafePush(i % 2 == 0);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i % 2 == 0);
            assertThat(v1.get(i)).isEqualTo(i % 2 == 0);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void illegalUnsafePush() {
        final LNGBooleanVector v1 = new LNGBooleanVector(100);
        assertThat(v1.empty()).isTrue();
        assertThatThrownBy(() -> {
            for (int i = 0; i < 1000; i++) {
                v1.unsafePush(i % 2 == 0);
            }
        }).isInstanceOf(ArrayIndexOutOfBoundsException.class);
    }

    @Test
    public void testGettingSettingAndPopping() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
        }
        for (int i = 999; i >= 0; i--) {
            v1.set(i, true);
            assertThat(v1.get(i)).isEqualTo(true);
        }
        for (int i = 999; i >= 0; i--) {
            v1.pop();
            assertThat(v1.size()).isEqualTo(i);
        }
    }

    @Test
    public void testVectorShrink() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
        }
        assertThat(v1.empty()).isFalse();
        final int beforeSize = v1.size();
        v1.shrinkTo(v1.size() + 50);
        assertThat(beforeSize).isEqualTo(v1.size());
        for (int i = 500; i > 0; i--) {
            v1.shrinkTo(i);
            assertThat(v1.back()).isEqualTo((i - 1) % 2 == 0);
        }
    }

    @Test
    public void testGrowTo() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 1001; i += 10) {
            v1.growTo(1000 + i, true);
            assertThat(v1.size()).isEqualTo(1000 + i);
            for (int j = 0; j < 1000; j++) {
                assertThat(v1.get(j)).isEqualTo(j % 2 == 0);
            }
            for (int j = 1000; j < 1000 + i; j++) {
                assertThat(v1.get(j)).isEqualTo(true);
            }
        }
        assertThat(v1.size()).isEqualTo(2000);
        v1.growTo(100, true);
        assertThat(v1.size()).isEqualTo(2000);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(i % 2 == 0);
        }
        for (int i = 1000; i < 2000; i++) {
            assertThat(v1.get(i)).isEqualTo(true);
        }
    }

    @Test
    public void testRemoveElements() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 9; i++) {
            v1.removeElements(100);
            assertThat(v1.size()).isEqualTo(1000 - (i + 1) * 100);
            assertThat(v1.back()).isEqualTo((1000 - (i + 1) * 100 - 1) % 2 == 0);
        }
        assertThat(v1.size()).isEqualTo(100);
        v1.removeElements(100);
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void testReverseInplace() {
        final LNGBooleanVector v1 = new LNGBooleanVector(true, true, false, true, false, false, true, true);
        v1.shrinkTo(7);
        v1.reverseInplace();
        assertThat(v1.get(0)).isEqualTo(true);
        assertThat(v1.get(1)).isEqualTo(false);
        assertThat(v1.get(2)).isEqualTo(false);
        assertThat(v1.get(3)).isEqualTo(true);
        assertThat(v1.get(4)).isEqualTo(false);
        assertThat(v1.get(5)).isEqualTo(true);
        assertThat(v1.get(6)).isEqualTo(true);
    }

    @Test
    public void testToArray() {
        final LNGBooleanVector v1 = new LNGBooleanVector(1000);
        final boolean[] expected = new boolean[500];
        for (int i = 0; i < 1000; i++) {
            v1.push(i % 2 == 0);
            if (i < 500) {
                expected[i] = i % 2 == 0;
            }
        }
        v1.shrinkTo(500);
        assertThat(v1.toArray()).containsExactly(expected);
    }

    @Test
    public void testToString() {
        final LNGBooleanVector v1 = new LNGBooleanVector();
        assertThat(v1.toString()).isEqualTo("[]");
        v1.push(true);
        assertThat(v1.toString()).isEqualTo("[true]");
        v1.push(false);
        assertThat(v1.toString()).isEqualTo("[true, false]");
        v1.push(false);
        assertThat(v1.toString()).isEqualTo("[true, false, false]");
        v1.push(true);
        assertThat(v1.toString()).isEqualTo("[true, false, false, true]");
    }
}
