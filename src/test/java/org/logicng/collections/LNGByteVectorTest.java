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
 * Unit tests for {@link LNGByteVector}.
 * @version 1.1
 * @since 1.0
 */
public class LNGByteVectorTest {

  @Test
  public void testVectorCreation() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertEquals(0, v1.size());
    Assert.assertTrue(v1.empty());
    LNGByteVector v2 = new LNGByteVector(10);
    Assert.assertEquals(0, v2.size());
    Assert.assertTrue(v2.empty());
    LNGByteVector v3 = new LNGByteVector(10, (byte) 42);
    Assert.assertEquals(10, v3.size());
    for (int i = 0; i < v3.size(); i++)
      Assert.assertEquals(42, v3.get(i));
    Assert.assertFalse(v3.empty());
    LNGByteVector v4 = new LNGByteVector(v3);
    Assert.assertEquals(10, v4.size());
    for (int i = 0; i < v4.size(); i++)
      Assert.assertEquals(42, v4.get(i));
    Assert.assertFalse(v4.empty());
    LNGByteVector v5 = new LNGByteVector((byte) 0, (byte) 1, (byte) 2, (byte) 3, (byte) 4);
    Assert.assertEquals(5, v5.size());
    for (int i = 0; i < 5; i++)
      Assert.assertEquals(i, v5.get(i));
  }

  @Test
  public void testVectorAddElements() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 100; i++) {
      v1.push((byte) i);
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
    LNGByteVector v1 = new LNGByteVector(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 100; i++) {
      v1.unsafePush((byte) i);
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
    LNGByteVector v1 = new LNGByteVector(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.unsafePush((byte) i);
  }

  @Test
  public void testGettingSettingAndPopping() {
    LNGByteVector v1 = new LNGByteVector();
    for (int i = 0; i < 100; i++)
      v1.push((byte) i);
    for (int i = 99; i >= 0; i--) {
      v1.set(i, (byte) 42);
      Assert.assertEquals(42, v1.get(i));
    }
    for (int i = 99; i >= 0; i--) {
      v1.pop();
      Assert.assertEquals(i, v1.size());
    }
  }

  @Test
  public void testVectorShrink() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 100; i++)
      v1.push((byte) i);
    Assert.assertFalse(v1.empty());
    int beforeSize = v1.size();
    v1.shrinkTo(v1.size() + 50);
    Assert.assertEquals(v1.size(), beforeSize);
    for (int i = 50; i > 0; i--) {
      v1.shrinkTo(i);
      Assert.assertEquals((i - 1), v1.back());
    }
  }

  @Test
  public void testGrowTo() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 50; i++)
      v1.push((byte) i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 51; i += 10) {
      v1.growTo(50 + i, (byte) 51);
      Assert.assertEquals(50 + i, v1.size());
      for (int j = 0; j < 50; j++)
        Assert.assertEquals(j, v1.get(j));
      for (int j = 50; j < 50 + i; j++)
        Assert.assertEquals(51, v1.get(j));
    }
    Assert.assertEquals(100, v1.size());
    v1.growTo(100, (byte) 51);
    Assert.assertEquals(100, v1.size());
    for (int i = 0; i < 50; i++)
      Assert.assertEquals(i, v1.get(i));
    for (int i = 50; i < 100; i++)
      Assert.assertEquals(51, v1.get(i));
  }

  @Test
  public void testRemoveElements() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 100; i++)
      v1.push((byte) i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 9; i++) {
      v1.removeElements(10);
      Assert.assertEquals(100 - (i + 1) * 10, v1.size());
      Assert.assertEquals(100 - (i + 1) * 10 - 1, v1.back());
    }
    Assert.assertEquals(10, v1.size());
    v1.removeElements(10);
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testSort() {
    LNGByteVector v1 = new LNGByteVector(100);
    LNGByteVector v2 = new LNGByteVector(100);
    for (int i = 99; i >= 0; i--)
      v1.push((byte) i);
    for (int i = 0; i < 100; i++)
      v2.push((byte) i);
    v1.sort();
    v2.sort();
    for (int i = 0; i < 100; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 99)
        Assert.assertTrue(v1.get(i) < v1.get(i + 1));
    }
    LNGByteVector v3 = new LNGByteVector(100);
    v3.sort();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testSortReverse() {
    LNGByteVector v1 = new LNGByteVector(100);
    LNGByteVector v2 = new LNGByteVector(100);
    for (int i = 99; i >= 0; i--)
      v1.push((byte) i);
    for (int i = 0; i < 100; i++)
      v2.push((byte) i);
    v1.sortReverse();
    v2.sortReverse();
    for (int i = 0; i < 100; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 99)
        Assert.assertTrue(v1.get(i) > v1.get(i + 1));
    }
    LNGByteVector v3 = new LNGByteVector(100);
    v3.sortReverse();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testToArray() {
    LNGByteVector v1 = new LNGByteVector(100);
    byte[] expected = new byte[50];
    for (int i = 0; i < 100; i++) {
      v1.push((byte) i);
      if (i < 50)
        expected[i] = (byte) i;
    }
    v1.shrinkTo(50);
    Assert.assertArrayEquals(expected, v1.toArray());
  }

  @Test
  public void testToString() {
    LNGByteVector v1 = new LNGByteVector();
    Assert.assertEquals("[]", v1.toString());
    v1.push((byte) 1);
    Assert.assertEquals("[1]", v1.toString());
    v1.push((byte) 2);
    Assert.assertEquals("[1, 2]", v1.toString());
    v1.push((byte) 3);
    Assert.assertEquals("[1, 2, 3]", v1.toString());
    v1.push((byte) 4);
    Assert.assertEquals("[1, 2, 3, 4]", v1.toString());
  }
}
