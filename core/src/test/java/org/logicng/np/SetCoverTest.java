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

package org.logicng.np;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Unit tests for {@link SetCover}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class SetCoverTest {

    @Test
    public void smallTest() {
        final List<Set<String>> sets = new ArrayList<>();
        sets.add(new TreeSet<>(Arrays.asList("a", "b", "c", "d", "e", "f")));
        sets.add(new TreeSet<>(Arrays.asList("e", "f", "h", "i")));
        sets.add(new TreeSet<>(Arrays.asList("a", "d", "g", "j")));
        sets.add(new TreeSet<>(Arrays.asList("b", "e", "h", "k")));
        sets.add(new TreeSet<>(Arrays.asList("c", "f", "i", "l")));
        sets.add(new TreeSet<>(Arrays.asList("j", "k", "l")));
        assertThat(SetCover.compute(sets)).containsExactlyInAnyOrder(sets.get(2), sets.get(3), sets.get(4));
    }

    @Test
    public void cornerCasesTest() {
        final List<Set<String>> sets = new ArrayList<>();
        assertThat(SetCover.compute(sets)).isEmpty();
        sets.add(Collections.emptySet());
        assertThat(SetCover.compute(sets)).isEmpty();
        sets.add(new HashSet<>(Collections.singletonList("A")));
        sets.add(new HashSet<>(Collections.singletonList("A")));
        sets.add(new HashSet<>(Collections.singletonList("A")));
        assertThat(SetCover.compute(sets)).hasSize(1);
        sets.add(new HashSet<>(Collections.singletonList("B")));
        assertThat(SetCover.compute(sets)).containsExactlyInAnyOrder(
                new HashSet<>(Collections.singletonList("A")),
                new HashSet<>(Collections.singletonList("B"))
        );
        sets.add(new HashSet<>(Arrays.asList("A", "B")));
        assertThat(SetCover.compute(sets)).hasSize(1).containsExactly(
                new HashSet<>(Arrays.asList("A", "B"))
        );
    }
}
