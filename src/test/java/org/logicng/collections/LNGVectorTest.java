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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Unit tests for {@link LNGVector}.
 * @version 2.0.0
 * @since 1.0
 */
public class LNGVectorTest {

    private final Comparator<String> stringComparator;

    public LNGVectorTest() {
        this.stringComparator = String::compareTo;
    }

    @Test
    public void testVectorCreation() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.size()).isEqualTo(0);
        assertThat(v1.empty()).isTrue();
        final LNGVector<String> v2 = new LNGVector<>(10);
        assertThat(v2.size()).isEqualTo(0);
        assertThat(v2.empty()).isTrue();
        final LNGVector<String> v3 = new LNGVector<>(10, "string");
        assertThat(v3.size()).isEqualTo(10);
        for (int i = 0; i < v3.size(); i++) {
            assertThat(v3.get(i)).isEqualTo("string");
        }
        assertThat(v3.empty()).isFalse();
        final LNGVector<String> v4 = new LNGVector<>("s1", "s2", "s3", "s4", "s5");
        assertThat(v4.size()).isEqualTo(5);
        int count = 1;
        for (final String s : v4) {
            assertThat(s).isEqualTo("s" + count++);
        }
        final List<String> list = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            list.add("s" + i);
        }
        final LNGVector<String> v5 = new LNGVector<>(list);
        assertThat(v5.size()).isEqualTo(1000);
        for (int i = 0; i < 1000; i++) {
            assertThat(v5.get(i)).isEqualTo("s" + i);
        }
    }

    @Test
    public void testVectorAddElements() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo("s" + i);
            assertThat(v1.get(i)).isEqualTo("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void testRelease() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo("s" + i);
            assertThat(v1.get(i)).isEqualTo("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        v1.release();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void legalUnsafePush() {
        final LNGVector<String> v1 = new LNGVector<>(1000);
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.unsafePush("s" + i);
            assertThat(v1.size()).isEqualTo(i + 1);
            assertThat(v1.back()).isEqualTo("s" + i);
            assertThat(v1.get(i)).isEqualTo("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        v1.clear();
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void illegalUnsafePush() {
        final LNGVector<String> v1 = new LNGVector<>(100);
        assertThat(v1.empty()).isTrue();
        assertThatThrownBy(() -> {
            for (int i = 0; i < 1000; i++) {
                v1.unsafePush("s" + i);
            }
        }).isInstanceOf(ArrayIndexOutOfBoundsException.class);
    }

    @Test
    public void testGettingSettingAndPopping() {
        final LNGVector<String> v1 = new LNGVector<>();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        for (int i = 999; i >= 0; i--) {
            v1.set(i, "string");
            assertThat(v1.get(i)).isEqualTo("string");
        }
        for (int i = 999; i >= 0; i--) {
            v1.pop();
            assertThat(v1.size()).isEqualTo(i);
        }
    }

    @Test
    public void testVectorShrink() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        final int beforeSize = v1.size();
        v1.shrinkTo(v1.size() + 50);
        assertThat(beforeSize).isEqualTo(v1.size());
        for (int i = 500; i > 0; i--) {
            v1.shrinkTo(i);
            assertThat(v1.back()).isEqualTo("s" + (i - 1));
        }
    }

    @Test
    public void testGrowToWithPad() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 1001; i += 10) {
            v1.growTo(1000 + i, "string");
            assertThat(v1.size()).isEqualTo(1000 + i);
            for (int j = 0; j < 1000; j++) {
                assertThat(v1.get(j)).isEqualTo("s" + j);
            }
            for (int j = 1000; j < 1000 + i; j++) {
                assertThat(v1.get(j)).isEqualTo("string");
            }
        }
        assertThat(v1.size()).isEqualTo(2000);
        v1.growTo(100, "string");
        assertThat(v1.size()).isEqualTo(2000);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo("s" + i);
        }
        for (int i = 1000; i < 2000; i++) {
            assertThat(v1.get(i)).isEqualTo("string");
        }
    }

    @Test
    public void testGrowTo() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 2000; i++) {
            v1.push("s" + i);
        }
        v1.shrinkTo(1000);
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 1001; i += 10) {
            v1.growTo(1000 + i);
            assertThat(v1.size()).isEqualTo(1000 + i);
            for (int j = 0; j < 1000; j++) {
                assertThat(v1.get(j)).isEqualTo("s" + j);
            }
            for (int j = 1000; j < 1000 + i; j++) {
                assertThat(v1.get(j)).isEqualTo(null);
            }
        }
        assertThat(v1.size()).isEqualTo(2000);
        v1.growTo(100, "string");
        assertThat(v1.size()).isEqualTo(2000);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo("s" + i);
        }
        for (int i = 1000; i < 2000; i++) {
            assertThat(v1.get(i)).isEqualTo(null);
        }
    }

    @Test
    public void testRemoveElements() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        assertThat(v1.empty()).isFalse();
        for (int i = 0; i < 9; i++) {
            v1.removeElements(100);
            assertThat(v1.size()).isEqualTo(1000 - (i + 1) * 100);
            assertThat(v1.back()).isEqualTo("s" + (1000 - (i + 1) * 100 - 1));
        }
        assertThat(v1.size()).isEqualTo(100);
        v1.removeElements(100);
        assertThat(v1.empty()).isTrue();
    }

    @Test
    public void testInplaceReplace() {
        final LNGVector<String> v1 = new LNGVector<>();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        final LNGVector<String> v2 = new LNGVector<>();
        for (int i = 0; i < 500; i++) {
            v2.push("str" + i);
        }
        final LNGVector<String> v3 = new LNGVector<>();
        for (int i = 0; i < 2000; i++) {
            v3.push("string" + i);
        }
        v1.replaceInplace(v2);
        assertThat(v1.size()).isEqualTo(500);
        for (int i = 0; i < 500; i++) {
            assertThat(v1.get(i)).isEqualTo("str" + i);
        }
        v2.replaceInplace(v3);
        assertThat(v2.size()).isEqualTo(2000);
        for (int i = 0; i < 2000; i++) {
            assertThat(v2.get(i)).isEqualTo("string" + i);
        }
    }

    @Test
    public void testIllegalInplaceReplace() {
        final LNGVector<String> v1 = new LNGVector<>();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        assertThatThrownBy(() -> v1.replaceInplace(v1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testManualSort() {
        final LNGVector<String> v1 = new LNGVector<>(1000);
        final LNGVector<String> v2 = new LNGVector<>(1000);
        for (int i = 999; i >= 0; i--) {
            v1.push("s" + i);
        }
        for (int i = 0; i < 1000; i++) {
            v2.push("s" + i);
        }
        v1.manualSort(this.stringComparator);
        v2.manualSort(this.stringComparator);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 999) {
                assertThat(v1.get(i).compareTo(v1.get(i + 1)) < 0).isTrue();
            }
        }
        final LNGVector<String> v3 = new LNGVector<>(1000);
        v3.manualSort(this.stringComparator);
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testSort() {
        final LNGVector<String> v1 = new LNGVector<>(1000);
        final LNGVector<String> v2 = new LNGVector<>(1000);
        for (int i = 999; i >= 0; i--) {
            v1.push("s" + i);
        }
        for (int i = 0; i < 1000; i++) {
            v2.push("s" + i);
        }
        v1.sort(this.stringComparator);
        v2.sort(this.stringComparator);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 999) {
                assertThat(v1.get(i).compareTo(v1.get(i + 1)) < 0).isTrue();
            }
        }
        final LNGVector<String> v3 = new LNGVector<>(1000);
        v3.sort(this.stringComparator);
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testSortReverse() {
        final LNGVector<String> v1 = new LNGVector<>(1000);
        final LNGVector<String> v2 = new LNGVector<>(1000);
        for (int i = 999; i >= 0; i--) {
            v1.push("s" + i);
        }
        for (int i = 0; i < 1000; i++) {
            v2.push("s" + i);
        }
        v1.sortReverse(this.stringComparator);
        v2.sortReverse(this.stringComparator);
        for (int i = 0; i < 1000; i++) {
            assertThat(v1.get(i)).isEqualTo(v2.get(i));
            if (i != 999) {
                assertThat(v1.get(i).compareTo(v1.get(i + 1)) > 0).isTrue();
            }
        }
        final LNGVector<String> v3 = new LNGVector<>(1000);
        v3.sortReverse(this.stringComparator);
        assertThat(v3.empty()).isTrue();
    }

    @Test
    public void testRemove() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        v1.remove("s500");
        assertThat(v1.size()).isEqualTo(999);
        assertThat(v1.get(499)).isEqualTo("s499");
        assertThat(v1.get(500)).isEqualTo("s501");
        v1.remove("s0");
        assertThat(v1.size()).isEqualTo(998);
        assertThat(v1.get(0)).isEqualTo("s1");
        assertThat(v1.get(498)).isEqualTo("s499");
        assertThat(v1.get(499)).isEqualTo("s501");
        v1.remove("s999");
        assertThat(v1.size()).isEqualTo(997);
        assertThat(v1.get(0)).isEqualTo("s1");
        assertThat(v1.get(498)).isEqualTo("s499");
        assertThat(v1.get(499)).isEqualTo("s501");
        assertThat(v1.get(996)).isEqualTo("s998");
        v1.remove("s1001");
        assertThat(v1.size()).isEqualTo(997);
        assertThat(v1.get(0)).isEqualTo("s1");
        assertThat(v1.get(498)).isEqualTo("s499");
        assertThat(v1.get(499)).isEqualTo("s501");
        assertThat(v1.get(996)).isEqualTo("s998");
        final LNGVector<String> v2 = new LNGVector<>("s1", "s1", "s2", "s5", "s8");
        v2.remove("s1");
        assertThat(v2.size()).isEqualTo(4);
        assertThat(v2.toString()).isEqualTo("[s1, s2, s5, s8]");
    }

    @Test
    public void testToArray() {
        final LNGVector<String> v1 = new LNGVector<>(1000);
        final String[] expected = new String[500];
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
            if (i < 500) {
                expected[i] = "s" + i;
            }
        }
        v1.shrinkTo(500);
        for (int i = 0; i < expected.length; i++) {
            assertThat(v1.get(i)).isEqualTo(expected[i]);
        }
    }

    @Test
    public void testIterator() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        int count = 0;
        for (final String s : v1) {
            assertThat(s).isEqualTo("s" + count++);
        }
    }

    @Test
    public void testIllegalIteratorRemoval() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        final Iterator<String> it = v1.iterator();
        assertThat(it.hasNext()).isTrue();
        it.next();
        assertThatThrownBy(it::remove).isInstanceOf(UnsupportedOperationException.class);
    }

    @Test
    public void testIllegalIteratorTraversal() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.empty()).isTrue();
        for (int i = 0; i < 1000; i++) {
            v1.push("s" + i);
        }
        final Iterator<String> it = v1.iterator();
        assertThat(it.hasNext()).isTrue();
        assertThatThrownBy(() -> {
            for (int i = 0; i < 1001; i++) {
                it.next();
            }
        }).isInstanceOf(NoSuchElementException.class);
    }

    @Test
    public void testToString() {
        final LNGVector<String> v1 = new LNGVector<>();
        assertThat(v1.toString()).isEqualTo("[]");
        v1.push("s1");
        assertThat(v1.toString()).isEqualTo("[s1]");
        v1.push("s2");
        assertThat(v1.toString()).isEqualTo("[s1, s2]");
        v1.push("s3");
        assertThat(v1.toString()).isEqualTo("[s1, s2, s3]");
        v1.push("s4");
        assertThat(v1.toString()).isEqualTo("[s1, s2, s3, s4]");
    }
}
