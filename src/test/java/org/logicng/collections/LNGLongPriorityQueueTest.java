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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests for {@link LNGLongPriorityQueue}.
 * @version 1.0
 * @since 1.0
 */
public class LNGLongPriorityQueueTest {

  @Test
  public void testCreation() {
    LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
    Assert.assertTrue(q1.empty());
    Assert.assertEquals(0, q1.size());
  }

  @Test
  public void testPushPopAndContains() {
    LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
    q1.push(0);
    q1.push(1);
    q1.push(2);
    q1.push(3);
    q1.push(4);
    Assert.assertEquals(5, q1.size());
    Assert.assertTrue(q1.contains(0));
    Assert.assertTrue(q1.contains(1));
    Assert.assertTrue(q1.contains(2));
    Assert.assertTrue(q1.contains(3));
    Assert.assertTrue(q1.contains(4));
    Assert.assertFalse(q1.contains(5));
    Assert.assertFalse(q1.contains(6));
    Assert.assertFalse(q1.contains(-2));
    Assert.assertEquals(0, q1.top());
    q1.pop();
    Assert.assertEquals(4, q1.size());
    Assert.assertEquals(4, q1.top());
    q1.pop(2);
    Assert.assertEquals(3, q1.size());
    Assert.assertEquals(4, q1.top());
    q1.pop();
    Assert.assertEquals(2, q1.size());
    Assert.assertEquals(3, q1.top());
    q1.pop(3);
    Assert.assertEquals(1, q1.size());
    Assert.assertEquals(1, q1.top());
    q1.pop();
    Assert.assertTrue(q1.empty());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalPush() {
    LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
    q1.push(-2);
  }

  @Test
  public void testUpdateAndPriorities() {
    LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
    q1.push(0);
    q1.push(1);
    q1.push(2);
    q1.push(3);
    q1.push(4);
    q1.update(0, 6);
    q1.update(1, 7);
    Assert.assertEquals(5, q1.size());
    Assert.assertEquals(1, q1.top());
    q1.update(1, 2);
    Assert.assertEquals(0, q1.top());
    q1.update(4, 8);
    Assert.assertEquals(4, q1.top());
    Assert.assertEquals(6, q1.priority(0));
    Assert.assertEquals(2, q1.priority(1));
    Assert.assertEquals(0, q1.priority(2));
    Assert.assertEquals(0, q1.priority(3));
    Assert.assertEquals(8, q1.priority(4));
    q1.pop(4);
    Assert.assertEquals(0, q1.top());
  }

  @Test
  public void testToString() {
    LNGLongPriorityQueue q1 = new LNGLongPriorityQueue();
    Assert.assertEquals("LNGLongPriorityQueue{}", q1.toString());
    q1.push(0);
    Assert.assertEquals("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>}", q1.toString());
    q1.push(1);
    Assert.assertEquals("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>, <elem=1, pos=1, prio=0>}", q1.toString());
    q1.push(2);
    Assert.assertEquals("LNGLongPriorityQueue{<elem=0, pos=0, prio=0>, <elem=1, pos=1, prio=0>, " +
            "<elem=2, pos=2, prio=0>}", q1.toString());
    q1.push(3);
    q1.push(4);
    q1.update(0, 6);
    q1.update(1, 7);
    q1.update(1, 2);
    q1.update(4, 8);
    Assert.assertEquals("LNGLongPriorityQueue{<elem=4, pos=1, prio=6>, <elem=0, pos=4, prio=2>, <elem=2, pos=2, prio=0>, " +
            "<elem=3, pos=3, prio=0>, <elem=1, pos=0, prio=8>}", q1.toString());
  }

}
