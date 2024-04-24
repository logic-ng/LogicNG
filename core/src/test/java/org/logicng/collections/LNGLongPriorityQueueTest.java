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
 * Unit tests for {@link LNGLongPriorityQueue}.
 * @version 2.0.0
 * @since 1.0
 */
public class LNGLongPriorityQueueTest {

    @Test
    public void testCreation() {
        final LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
        assertThat(q1.empty()).isTrue();
        assertThat(q1.size()).isEqualTo(0);
    }

    @Test
    public void testPushPopAndContains() {
        final LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
        q1.push(0);
        q1.push(1);
        q1.push(2);
        q1.push(3);
        q1.push(4);
        assertThat(q1.size()).isEqualTo(5);
        assertThat(q1.contains(0)).isTrue();
        assertThat(q1.contains(1)).isTrue();
        assertThat(q1.contains(2)).isTrue();
        assertThat(q1.contains(3)).isTrue();
        assertThat(q1.contains(4)).isTrue();
        assertThat(q1.contains(5)).isFalse();
        assertThat(q1.contains(6)).isFalse();
        assertThat(q1.contains(-2)).isFalse();
        assertThat(q1.top()).isEqualTo(0);
        q1.pop();
        assertThat(q1.size()).isEqualTo(4);
        assertThat(q1.top()).isEqualTo(4);
        q1.pop(2);
        assertThat(q1.size()).isEqualTo(3);
        assertThat(q1.top()).isEqualTo(4);
        q1.pop();
        assertThat(q1.size()).isEqualTo(2);
        assertThat(q1.top()).isEqualTo(3);
        q1.pop(3);
        assertThat(q1.size()).isEqualTo(1);
        assertThat(q1.top()).isEqualTo(1);
        q1.pop();
        assertThat(q1.empty()).isTrue();
    }

    @Test
    public void testIllegalPush() {
        final LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
        assertThatThrownBy(() -> q1.push(-2)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testUpdateAndPriorities() {
        final LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
        q1.push(0);
        q1.push(1);
        q1.push(2);
        q1.push(3);
        q1.push(4);
        q1.update(0, 6);
        q1.update(1, 7);
        assertThat(q1.size()).isEqualTo(5);
        assertThat(q1.top()).isEqualTo(1);
        q1.update(1, 2);
        assertThat(q1.top()).isEqualTo(0);
        q1.update(4, 8);
        assertThat(q1.top()).isEqualTo(4);
        assertThat(q1.priority(0)).isEqualTo(6);
        assertThat(q1.priority(1)).isEqualTo(2);
        assertThat(q1.priority(2)).isEqualTo(0);
        assertThat(q1.priority(3)).isEqualTo(0);
        assertThat(q1.priority(4)).isEqualTo(8);
        q1.pop(4);
        assertThat(q1.top()).isEqualTo(0);
    }

    @Test
    public void testToString() {
        final LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
        assertThat(q1.toString()).isEqualTo("LNGLongPriorityQueue{}");
        q1.push(0);
        assertThat(q1.toString()).isEqualTo("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>}");
        q1.push(1);
        assertThat(q1.toString()).isEqualTo("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>, <elem=1, pos=1, prio=0>}");
        q1.push(2);
        assertThat(q1.toString()).isEqualTo("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>, <elem=1, pos=1, prio=0>, <elem=2, pos=2, prio=0>}");
        q1.push(3);
        q1.push(4);
        q1.update(0, 6);
        q1.update(1, 7);
        q1.update(1, 2);
        q1.update(4, 8);
        assertThat(q1.toString()).isEqualTo("LNGLongPriorityQueue{<elem=4, pos=1, prio=6>, <elem=0, pos=4, prio=2>, <elem=2, pos=2, prio=0>, <elem=3, pos=3, prio=0>, <elem=1, pos=0, prio=8>}");
    }
}
