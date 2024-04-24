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
 * Unit tests for {@link LNGByteVector}.
 * @version 2.0.0
 * @since 1.0
 */
public class LNGByteVectorTest {

    @Test
    public void testVectorCreation() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.size()).isEqualTo(0);
        assertThat(v1.empty()).isTrue();
        final LNGByteVector v2 = new LNGByteVector(10);
        assertThat(v2.size()).isEqualTo(0);
        assertThat(v2.empty()).isTrue();
        final LNGByteVector v3 = new LNGByteVector(10, (byte) 42);
        assertThat(v3.size()).isEqualTo(10);
        for (int i = 0; i < v3.size(); i++) {
            assertThat(v3.get(i)).isEqualTo((byte) 42);
        }
        assertThat(v3.empty()).isFalse();
        final LNGByteVector v4 = new LNGByteVector(v3);
        assertThat(v4.size()).isEqualTo(10);
        for (int i = 0; i < v4.size(); i++) {
            assertThat(v4.get(i)).isEqualTo((byte) 42);
        }
        assertThat(v4.empty()).isFalse();
        final LNGByteVector v5 = new LNGByteVector((byte) 0, (byte) 1, (byte) 2, (byte) 3, (byte) 4);
        assertThat(v5.size()).isEqualTo(5);
        for (int i = 0; i < 5; i++) {
            assertThat(v5.get(i)).isEqualTo((byte) i);
        }
    }

    @Test
    public void testVectorAddElements() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 100; i++) {
            v1.push((byte) i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i);
            assertThat(v1.get(i)).isEqualTo((byte) i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void legalUnsafePush() {
        final LNGByteVector v1 = new LNGByteVector(100);
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 100; i++) {
            v1.unsafePush((byte) i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo(i);
            assertThat(v1.get(i)).isEqualTo((byte) i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void illegalUnsafePush() {
        final LNGByteVector v1 = new LNGByteVector(100);
        assertThat(v1.empty()).isTrue();
        assertThatThrownBy(() -> {
            for (int i = 0; i < 1000; i++) {
                v1.unsafePush((byte) i);
            }
        }).isInstanceOf(ArrayIndexOutOfBoundsException.class);
    }

    @Test
    public void testGettingSettingAndPopping() {
        final LNGByteVector v1 = new LNGByteVector();
        for (int i = 0; i < 100; i++) {
            v1.push((byte) i);
        }
        for (int i = 99; i >= 0; i--) {
            v1.set(i, (byte) 42);
            assertThat(v1.get(i)).isEqualTo((byte) 42);
        }
        for (int i = 99; i >= 0; i--) {
            v1.pop();
            assertThat(v1.size()).isEqualTo(i);
        }
    }

    @Test
    public void testVectorShrink() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 100; i++) {
            v1.push((byte) i);
        }
        assertThat(v1.empty()).isFalse();
        final int beforeSize = v1.size();
        v1.shrinkTo(v1.size() + 50);
        assertThat(beforeSize).isEqualTo(v1.size());
        for (int i = 50; i > 0; i--) {
            v1.shrinkTo(i);
            assertThat(v1.back()).isEqualTo((i - 1));
        }
    }

    @Test
    public void testGrowTo() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 50; i++) {
            v1.push((byte) i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 51; i += 10) {
            v1.growTo(50 + i, (byte) 51);
            assertThat(v1.size()).isEqualTo(50 + i);
            for (int j = 0; j < 50; j++) {
                assertThat(v1.get(j)).isEqualTo((byte) j);
            }
            for (int j = 50; j < 50 + i; j++) {
                assertThat(v1.get(j)).isEqualTo((byte) 51);
            }
        }
        assertThat(v1.size()).isEqualTo(100);
        v1.growTo(100, (byte) 51);
        assertThat(v1.size()).isEqualTo(100);
        for (int i = 0; i < 50; i++) {
            assertThat(v1.get(i)).isEqualTo((byte) i);
        }
        for (int i = 50; i < 100; i++) {
            assertThat(v1.get(i)).isEqualTo((byte) 51);
        }
    }

    @Test
    public void testRemoveElements() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 100; i++) {
            v1.push((byte) i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 9; i++) {
            v1.removeElements(10);
            assertThat(v1.size()).isEqualTo(100 - (i + 1) * 10);
            assertThat(v1.back()).isEqualTo(100 - (i + 1) * 10 - 1);
        }
        assertThat(v1.size()).isEqualTo(10);
        v1.removeElements(10);
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void testSort() {
        final LNGByteVector v1 = new LNGByteVector(100);
        final LNGByteVector v2 = new LNGByteVector(100);
        for (int i = 99; i >= 0; i--) {
            v1.push((byte) i);
        }
        for (int i = 0; i < 100; i++) {
            v2.push((byte) i);
        }
        v1.sort();
        v2.sort();
        for (int i = 0; i < 100; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 99) {
                assertThat(v1.get(i) < v1.get(i + 1)).isTrue();
            }
        }
        final LNGByteVector v3 = new LNGByteVector(100);
        v3.sort();
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testSortReverse() {
        final LNGByteVector v1 = new LNGByteVector(100);
        final LNGByteVector v2 = new LNGByteVector(100);
        for (int i = 99; i >= 0; i--) {
            v1.push((byte) i);
        }
        for (int i = 0; i < 100; i++) {
            v2.push((byte) i);
        }
        v1.sortReverse();
        v2.sortReverse();
        for (int i = 0; i < 100; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 99) {
                assertThat(v1.get(i) > v1.get(i + 1)).isTrue();
            }
        }
        final LNGByteVector v3 = new LNGByteVector(100);
        v3.sortReverse();
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testToArray() {
        final LNGByteVector v1 = new LNGByteVector(100);
        final byte[] expected = new byte[50];
        for (int i = 0; i < 100; i++) {
            v1.push((byte) i);
            if (i < 50) {
                expected[i] = (byte) i;
            }
        }
        v1.shrinkTo(50);
        assertThat(v1.toArray()).containsExactly(expected);
    }

    @Test
    public void testToString() {
        final LNGByteVector v1 = new LNGByteVector();
        assertThat(v1.toString()).isEqualTo("[]");
        v1.push((byte) 1);
        assertThat(v1.toString()).isEqualTo("[1]");
        v1.push((byte) 2);
        assertThat(v1.toString()).isEqualTo("[1, 2]");
        v1.push((byte) 3);
        assertThat(v1.toString()).isEqualTo("[1, 2, 3]");
        v1.push((byte) 4);
        assertThat(v1.toString()).isEqualTo("[1, 2, 3, 4]");
    }
}
