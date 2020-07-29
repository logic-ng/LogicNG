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
import static org.logicng.util.CollectionHelper.nullOrEmpty;
import static org.logicng.util.CollectionHelper.nullSafe;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for {@link CollectionHelper}
 * @version 2.0.0
 * @since 2.0.0
 */
public class CollectionHelperTest {

    @Test
    public void testNullOrEmpty() {
        assertThat(nullOrEmpty(null)).isTrue();
        assertThat(nullOrEmpty(Collections.emptySet())).isTrue();
        assertThat(nullOrEmpty(Collections.emptyMap().entrySet())).isTrue();
        assertThat(nullOrEmpty(Collections.singletonList(2))).isFalse();
    }

    @Test
    public void testNullSafe() {
        assertThat(nullSafe(null)).isEmpty();
        final List<String> strings = Arrays.asList("a", "b", "c");
        assertThat(nullSafe(strings)).isEqualTo(strings);
    }

    @Test
    public void testIntersection() {
        final List<String> strings1 = Arrays.asList("a", "b", "c", "a");
        List<String> strings2 = Arrays.asList("c", "d", "e");
        assertThat(CollectionHelper.<String, TreeSet<String>>intersection(strings1, null, TreeSet::new)).isEmpty();
        assertThat(CollectionHelper.<String, TreeSet<String>>intersection(null, strings2, TreeSet::new)).isEmpty();
        assertThat(CollectionHelper.<String, TreeSet<String>>intersection(strings1, strings2, TreeSet::new)).containsExactly("c");
        strings2 = Arrays.asList("c", "d", "e", "a", "a", "a");
        assertThat(CollectionHelper.<String, List<String>>intersection(strings1, strings2, ArrayList::new)).containsExactly("a", "c", "a");
    }

    @Test
    public void testUnion() {
        final List<String> strings1 = Arrays.asList("a", "b", "c", "a");
        final List<String> strings2 = Arrays.asList("c", "d", "e");
        assertThat(CollectionHelper.<String, ArrayList<String>>union(strings1, null, ArrayList::new)).containsExactly("a", "b", "c", "a");
        assertThat(CollectionHelper.<String, ArrayList<String>>union(null, strings2, ArrayList::new)).containsExactly("c", "d", "e");
        assertThat(CollectionHelper.<String, ArrayList<String>>union(strings1, strings2, ArrayList::new)).containsExactly("a", "b", "c", "a", "c", "d", "e");
    }

    @Test
    public void testMkString() {
        assertThat(CollectionHelper.mkString(Collections.emptyList(), ",")).isEqualTo("");
        assertThat(CollectionHelper.mkString(Collections.emptyList(), ",")).isEqualTo("");
        assertThat(CollectionHelper.mkString(Collections.emptySet(), "-")).isEqualTo("");
        assertThat(CollectionHelper.mkString(Collections.emptyMap().entrySet(), "!")).isEqualTo("");
        assertThat(CollectionHelper.mkString(Collections.emptyMap().entrySet(), "[", ",", "]")).isEqualTo("[]");
        assertThat(CollectionHelper.mkString(new Integer[0], "!")).isEqualTo("");
        assertThat(CollectionHelper.mkString(new Integer[0], "(", "!", ")")).isEqualTo("()");

        final List<String> collection01 = Arrays.asList("a", "b", "c");
        assertThat(CollectionHelper.mkString(collection01, ",")).isEqualTo("a,b,c");
        assertThat(CollectionHelper.mkString(collection01, ", ")).isEqualTo("a, b, c");
        assertThat(CollectionHelper.mkString(collection01, "")).isEqualTo("abc");
        assertThat(CollectionHelper.mkString(collection01, "---")).isEqualTo("a---b---c");
        assertThat(CollectionHelper.mkString(collection01, "[", "---", "]")).isEqualTo("[a---b---c]");

        final List<Integer> collection02 = Arrays.asList(1, 2, 3);
        assertThat(CollectionHelper.mkString(collection02, ",")).isEqualTo("1,2,3");
        assertThat(CollectionHelper.mkString(collection02, ", ")).isEqualTo("1, 2, 3");
        assertThat(CollectionHelper.mkString(collection02, "")).isEqualTo("123");
        assertThat(CollectionHelper.mkString(collection02, "---")).isEqualTo("1---2---3");
        assertThat(CollectionHelper.mkString(collection02, "[", "---", "]")).isEqualTo("[1---2---3]");

        final SortedSet<Character> collection03 = new TreeSet<>(Arrays.asList('x', 'y'));
        assertThat(CollectionHelper.mkString(collection03, ",")).isEqualTo("x,y");
        assertThat(CollectionHelper.mkString(collection03, ", ")).isEqualTo("x, y");
        assertThat(CollectionHelper.mkString(collection03, "")).isEqualTo("xy");
        assertThat(CollectionHelper.mkString(collection03, "---")).isEqualTo("x---y");
        assertThat(CollectionHelper.mkString(collection03, "[", "---", "]")).isEqualTo("[x---y]");

        final String[] array01 = new String[]{"x", "y"};
        assertThat(CollectionHelper.mkString(array01, ",")).isEqualTo("x,y");
        assertThat(CollectionHelper.mkString(array01, ", ")).isEqualTo("x, y");
        assertThat(CollectionHelper.mkString(array01, "")).isEqualTo("xy");
        assertThat(CollectionHelper.mkString(array01, "---")).isEqualTo("x---y");
        assertThat(CollectionHelper.mkString(array01, "[", "---", "]")).isEqualTo("[x---y]");

        final List<Integer> array02 = Arrays.asList(1, 2, 3);
        assertThat(CollectionHelper.mkString(array02, ",")).isEqualTo("1,2,3");
        assertThat(CollectionHelper.mkString(array02, ", ")).isEqualTo("1, 2, 3");
        assertThat(CollectionHelper.mkString(array02, "")).isEqualTo("123");
        assertThat(CollectionHelper.mkString(array02, "---")).isEqualTo("1---2---3");
        assertThat(CollectionHelper.mkString(array02, "[", "---", "]")).isEqualTo("[1---2---3]");
    }
}
