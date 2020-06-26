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
 * Unit tests for {@link LNGIntVector}.
 * @version 2.0.0
 * @since 1.0
 */
public class LNGIntVectorTest {

    @Test
    public void testVectorCreation() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.size()).isEqualTo(0);
        assertThat(v1.empty()).isTrue();
        final LNGIntVector v2 = new LNGIntVector(10);
        assertThat(v2.size()).isEqualTo(0);
        assertThat(v2.empty()).isTrue();
        final LNGIntVector v3 = new LNGIntVector(10, 42);
        assertThat(v3.size()).isEqualTo(10);
        for (int i = 0; i < v3.size(); i++) {
            assertThat(v3.get(i)).isEqualTo(42);
        }
        assertThat(v3.empty()).isFalse();
        final LNGIntVector v4 = new LNGIntVector(v3);
        assertThat(v4.size()).isEqualTo(10);
        for (int i = 0; i < v4.size(); i++) {
            assertThat(v4.get(i)).isEqualTo(42);
        }
        assertThat(v4.empty()).isFalse();
        final LNGIntVector v5 = new LNGIntVector(0, 1, 2, 3, 4);
        assertThat(v5.size()).isEqualTo(5);
        for (int i = 0; i < 5; i++) {
            assertThat(v5.get(i)).isEqualTo(i);
        }
    }

    @Test
    public void testVectorAddElements() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i);
            assertThat(v1.get(i)).isEqualTo(i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void legalUnsafePush() {
        final LNGIntVector v1 = new LNGIntVector(1000);
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.unsafePush(i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i);
            assertThat(v1.get(i)).isEqualTo(i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void illegalUnsafePush() {
        final LNGIntVector v1 = new LNGIntVector(100);
        assertThat(v1.empty()).isTrue();
        assertThatThrownBy(() -> {
            for (int i = 0; i < 1000; i++) {
                v1.unsafePush(i);
            }
        }).isInstanceOf(ArrayIndexOutOfBoundsException.class);
    }

    @Test
    public void testGettingSettingAndPopping() {
        final LNGIntVector v1 = new LNGIntVector();
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
        }
        for (int i = 999; i >= 0; i--) {
            v1.set(i, 42);
            assertThat(v1.get(i)).isEqualTo(42);
        }
        for (int i = 999; i >= 0; i--) {
            v1.pop();
            assertThat(v1.size()).isEqualTo(i);
        }
    }

    @Test
    public void testVectorShrink() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 500; i > 0; i--) {
            v1.shrinkTo(i);
            assertThat(v1.back()).isEqualTo((i - 1));
        }
    }

    @Test
    public void testGrowTo() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 1001; i += 10) {
            v1.growTo(1000 + i, 1001);
            assertThat(v1.size()).isEqualTo(1000 + i);
            for (int j = 0; j < 1000; j++) {
                assertThat(v1.get(j)).isEqualTo(j);
            }
            for (int j = 1000; j < 1000 + i; j++) {
                assertThat(v1.get(j)).isEqualTo(1001);
            }
        }
        assertThat(v1.size()).isEqualTo(2000);
        v1.growTo(100, 1001);
        assertThat(v1.size()).isEqualTo(2000);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(i);
        }
        for (int i = 1000; i < 2000; i++) {
            assertThat(v1.get(i)).isEqualTo(1001);
        }
    }

    @Test
    public void testRemoveElements() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 9; i++) {
            v1.removeElements(100);
            assertThat(v1.size()).isEqualTo(1000 - (i + 1) * 100);
            assertThat(v1.back()).isEqualTo(1000 - (i + 1) * 100 - 1);
        }
        assertThat(v1.size()).isEqualTo(100);
        v1.removeElements(100);
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void testSort() {
        final LNGIntVector v1 = new LNGIntVector(1000);
        final LNGIntVector v2 = new LNGIntVector(1000);
        for (int i = 999; i >= 0; i--) {
            v1.push(i);
        }
        for (int i = 0; i < 1000; i++) {
            v2.push(i);
        }
        v1.sort();
        v2.sort();
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 999) {
                assertThat(v1.get(i) < v1.get(i + 1)).isTrue();
            }
        }
        final LNGIntVector v3 = new LNGIntVector(1000);
        v3.sort();
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testSortReverse() {
        final LNGIntVector v1 = new LNGIntVector(1000);
        final LNGIntVector v2 = new LNGIntVector(1000);
        for (int i = 999; i >= 0; i--) {
            v1.push(i);
        }
        for (int i = 0; i < 1000; i++) {
            v2.push(i);
        }
        v1.sortReverse();
        v2.sortReverse();
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 999) {
                assertThat(v1.get(i) > v1.get(i + 1)).isTrue();
            }
        }
        final LNGIntVector v3 = new LNGIntVector(1000);
        v3.sortReverse();
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testToArray() {
        final LNGIntVector v1 = new LNGIntVector(1000);
        final int[] expected = new int[500];
        for (int i = 0; i < 1000; i++) {
            v1.push(i);
            if (i < 500) {
                expected[i] = i;
            }
        }
        v1.shrinkTo(500);
        assertThat(v1.toArray()).containsExactly(expected);
    }

    @Test
    public void testToString() {
        final LNGIntVector v1 = new LNGIntVector();
        assertThat(v1.toString()).isEqualTo("[]");
        v1.push(1);
        assertThat(v1.toString()).isEqualTo("[1]");
        v1.push(2);
        assertThat(v1.toString()).isEqualTo("[1, 2]");
        v1.push(3);
        assertThat(v1.toString()).isEqualTo("[1, 2, 3]");
        v1.push(4);
        assertThat(v1.toString()).isEqualTo("[1, 2, 3, 4]");
    }
}
