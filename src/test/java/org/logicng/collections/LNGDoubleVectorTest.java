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
 * Unit tests for {@link LNGDoubleVector}.
 * @version 1.0
 * @since 1.0
 */
public class LNGDoubleVectorTest {

  @Test
  public void testVectorCreation() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertEquals(0, v1.size());
    Assert.assertTrue(v1.empty());
    LNGDoubleVector v2 = new LNGDoubleVector(10);
    Assert.assertEquals(0, v2.size());
    Assert.assertTrue(v2.empty());
    LNGDoubleVector v3 = new LNGDoubleVector(10, 42.0);
    Assert.assertEquals(10, v3.size());
    for (int i = 0; i < v3.size(); i++)
      Assert.assertEquals(42.0, v3.get(i), 0.1);
    Assert.assertFalse(v3.empty());
    LNGDoubleVector v4 = new LNGDoubleVector(v3);
    Assert.assertEquals(10, v4.size());
    for (int i = 0; i < v4.size(); i++)
      Assert.assertEquals(42.0, v4.get(i), 0.1);
    Assert.assertFalse(v4.empty());
    LNGDoubleVector v5 = new LNGDoubleVector(0.0, 1.0, 2.0, 3.0, 4.0);
    Assert.assertEquals(5, v5.size());
    for (int i = 0; i < 5; i++)
      Assert.assertEquals(i, v5.get(i), 0.1);
  }

  @Test
  public void testVectorAddElements() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.push(i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i, v1.back(), 0.1);
      Assert.assertEquals(i, v1.get(i), 0.1);
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void legalUnsafePush() {
    LNGDoubleVector v1 = new LNGDoubleVector(1000);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.unsafePush(i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i, v1.back(), 0.1);
      Assert.assertEquals(i, v1.get(i), 0.1);
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void illegalUnsafePush() {
    LNGDoubleVector v1 = new LNGDoubleVector(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.unsafePush(i);
  }

  @Test
  public void testGettingSettingAndPopping() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    for (int i = 999; i >= 0; i--) {
      v1.set(i, 42.0);
      Assert.assertEquals(42.0, v1.get(i), 0.1);
    }
    for (int i = 999; i >= 0; i--) {
      v1.pop();
      Assert.assertEquals(i, v1.size());
    }
  }

  @Test
  public void testVectorShrink() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 500; i > 0; i--) {
      v1.shrinkTo(i);
      Assert.assertEquals((i - 1), v1.back(), 0.1);
    }
  }

  @Test
  public void testGrowTo() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 1001; i += 10) {
      v1.growTo(1000 + i, 1001.0);
      Assert.assertEquals(1000 + i, v1.size());
      for (int j = 0; j < 1000; j++)
        Assert.assertEquals(j, v1.get(j), 0.1);
      for (int j = 1000; j < 1000 + i; j++)
        Assert.assertEquals(1001.0, v1.get(j), 0.1);
    }
    Assert.assertEquals(2000, v1.size());
    v1.growTo(100, 1001.0);
    Assert.assertEquals(2000, v1.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals(i, v1.get(i), 0.1);
    for (int i = 1000; i < 2000; i++)
      Assert.assertEquals(1001.0, v1.get(i), 0.1);
  }

  @Test
  public void testRemoveElements() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 9; i++) {
      v1.removeElements(100);
      Assert.assertEquals(1000 - (i + 1) * 100, v1.size());
      Assert.assertEquals(1000 - (i + 1) * 100 - 1, v1.back(), 0.1);
    }
    Assert.assertEquals(100, v1.size());
    v1.removeElements(100);
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testSort() {
    LNGDoubleVector v1 = new LNGDoubleVector(1000);
    LNGDoubleVector v2 = new LNGDoubleVector(1000);
    for (int i = 999; i >= 0; i--)
      v1.push(i);
    for (int i = 0; i < 1000; i++)
      v2.push(i);
    v1.sort();
    v2.sort();
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i), 0.1);
      if (i != 999)
        Assert.assertTrue(v1.get(i) < v1.get(i + 1));
    }
    LNGDoubleVector v3 = new LNGDoubleVector(1000);
    v3.sort();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testSortReverse() {
    LNGDoubleVector v1 = new LNGDoubleVector(1000);
    LNGDoubleVector v2 = new LNGDoubleVector(1000);
    for (int i = 999; i >= 0; i--)
      v1.push(i);
    for (int i = 0; i < 1000; i++)
      v2.push(i);
    v1.sortReverse();
    v2.sortReverse();
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i), 0.1);
      if (i != 999)
        Assert.assertTrue(v1.get(i) > v1.get(i + 1));
    }
    LNGDoubleVector v3 = new LNGDoubleVector(1000);
    v3.sortReverse();
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testToArray() {
    LNGDoubleVector v1 = new LNGDoubleVector(1000);
    double[] expected = new double[500];
    for (int i = 0; i < 1000; i++) {
      v1.push(i);
      if (i < 500)
        expected[i] = i;
    }
    v1.shrinkTo(500);
    Assert.assertArrayEquals(expected, v1.toArray(), 0.1);
  }

  @Test
  public void testToString() {
    LNGDoubleVector v1 = new LNGDoubleVector();
    Assert.assertEquals("[]", v1.toString());
    v1.push(1);
    Assert.assertEquals("[1.0]", v1.toString());
    v1.push(2);
    Assert.assertEquals("[1.0, 2.0]", v1.toString());
    v1.push(3);
    Assert.assertEquals("[1.0, 2.0, 3.0]", v1.toString());
    v1.push(4);
    Assert.assertEquals("[1.0, 2.0, 3.0, 4.0]", v1.toString());
  }
}
