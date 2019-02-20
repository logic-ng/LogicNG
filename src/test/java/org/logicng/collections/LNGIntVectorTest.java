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
 * Unit tests for {@link LNGIntVector}.
 * @version 1.0
 * @since 1.0
 */
public class LNGIntVectorTest {

  @Test
  public void testVectorCreation() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertEquals(0, v1.size());
    Assert.assertTrue(v1.empty());
    LNGIntVector v2 = new LNGIntVector(10);
    Assert.assertEquals(0, v2.size());
    Assert.assertTrue(v2.empty());
    LNGIntVector v3 = new LNGIntVector(10, 42);
    Assert.assertEquals(10, v3.size());
    for (int i = 0; i < v3.size(); i++)
      Assert.assertEquals(42, v3.get(i));
    Assert.assertFalse(v3.empty());
    LNGIntVector v4 = new LNGIntVector(v3);
    Assert.assertEquals(10, v4.size());
    for (int i = 0; i < v4.size(); i++)
      Assert.assertEquals(42, v4.get(i));
    Assert.assertFalse(v4.empty());
    LNGIntVector v5 = new LNGIntVector(0, 1, 2, 3, 4);
    Assert.assertEquals(5, v5.size());
    for (int i = 0; i < 5; i++)
      Assert.assertEquals(i, v5.get(i));
  }

  @Test
  public void testVectorAddElements() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.push(i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i, v1.back());
      Assert.assertEquals(i, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void legalUnsafePush() {
    LNGIntVector v1 = new LNGIntVector(1000);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.unsafePush(i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i, v1.back());
      Assert.assertEquals(i, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void illegalUnsafePush() {
    LNGIntVector v1 = new LNGIntVector(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.unsafePush(i);
  }

  @Test
  public void testGettingSettingAndPopping() {
    LNGIntVector v1 = new LNGIntVector();
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    for (int i = 999; i >= 0; i--) {
      v1.set(i, 42);
      Assert.assertEquals(42, v1.get(i));
    }
    for (int i = 999; i >= 0; i--) {
      v1.pop();
      Assert.assertEquals(i, v1.size());
    }
  }

  @Test
  public void testVectorShrink() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 500; i > 0; i--) {
      v1.shrinkTo(i);
      Assert.assertEquals((i - 1), v1.back());
    }
  }

  @Test
  public void testGrowTo() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 1001; i += 10) {
      v1.growTo(1000 + i, 1001);
      Assert.assertEquals(1000 + i, v1.size());
      for (int j = 0; j < 1000; j++)
        Assert.assertEquals(j, v1.get(j));
      for (int j = 1000; j < 1000 + i; j++)
        Assert.assertEquals(1001, v1.get(j));
    }
    Assert.assertEquals(2000, v1.size());
    v1.growTo(100, 1001);
    Assert.assertEquals(2000, v1.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals(i, v1.get(i));
    for (int i = 1000; i < 2000; i++)
      Assert.assertEquals(1001, v1.get(i));
  }

  @Test
  public void testRemoveElements() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 9; i++) {
      v1.removeElements(100);
      Assert.assertEquals(1000 - (i + 1) * 100, v1.size());
      Assert.assertEquals(1000 - (i + 1) * 100 - 1, v1.back());
    }
    Assert.assertEquals(100, v1.size());
    v1.removeElements(100);
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testSort() {
    LNGIntVector v1 = new LNGIntVector(1000);
    LNGIntVector v2 = new LNGIntVector(1000);
    for (int i = 999; i >= 0; i--)
      v1.push(i);
    for (int i = 0; i < 1000; i++)
      v2.push(i);
    v1.sort();
    v2.sort();
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 999)
        Assert.assertTrue(v1.get(i) < v1.get(i + 1));
    }
    LNGIntVector v3 = new LNGIntVector(1000);
    v3.sort();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testSortReverse() {
    LNGIntVector v1 = new LNGIntVector(1000);
    LNGIntVector v2 = new LNGIntVector(1000);
    for (int i = 999; i >= 0; i--)
      v1.push(i);
    for (int i = 0; i < 1000; i++)
      v2.push(i);
    v1.sortReverse();
    v2.sortReverse();
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 999)
        Assert.assertTrue(v1.get(i) > v1.get(i + 1));
    }
    LNGIntVector v3 = new LNGIntVector(1000);
    v3.sortReverse();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testToArray() {
    LNGIntVector v1 = new LNGIntVector(1000);
    int[] expected = new int[500];
    for (int i = 0; i < 1000; i++) {
      v1.push(i);
      if (i < 500)
        expected[i] = i;
    }
    v1.shrinkTo(500);
    Assert.assertArrayEquals(expected, v1.toArray());
  }

  @Test
  public void testToString() {
    LNGIntVector v1 = new LNGIntVector();
    Assert.assertEquals("[]", v1.toString());
    v1.push(1);
    Assert.assertEquals("[1]", v1.toString());
    v1.push(2);
    Assert.assertEquals("[1, 2]", v1.toString());
    v1.push(3);
    Assert.assertEquals("[1, 2, 3]", v1.toString());
    v1.push(4);
    Assert.assertEquals("[1, 2, 3, 4]", v1.toString());
  }
}
