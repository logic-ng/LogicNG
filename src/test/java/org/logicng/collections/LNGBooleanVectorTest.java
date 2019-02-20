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
 * Unit tests for {@link LNGBooleanVector}.
 * @version 1.1
 * @since 1.0
 */
public class LNGBooleanVectorTest {

  @Test
  public void testVectorCreation() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertEquals(0, v1.size());
    Assert.assertTrue(v1.empty());
    LNGBooleanVector v2 = new LNGBooleanVector(10);
    Assert.assertEquals(0, v2.size());
    Assert.assertTrue(v2.empty());
    LNGBooleanVector v3 = new LNGBooleanVector(10, true);
    Assert.assertEquals(10, v3.size());
    for (int i = 0; i < v3.size(); i++)
      Assert.assertEquals(true, v3.get(i));
    Assert.assertFalse(v3.empty());
    LNGBooleanVector v4 = new LNGBooleanVector(v3);
    Assert.assertEquals(10, v4.size());
    for (int i = 0; i < v4.size(); i++)
      Assert.assertEquals(true, v4.get(i));
    Assert.assertFalse(v4.empty());
    LNGBooleanVector v5 = new LNGBooleanVector(true, true, true, false, false);
    Assert.assertEquals(5, v5.size());
    for (int i = 0; i < 5; i++)
      if (i < 3)
        Assert.assertEquals(true, v5.get(i));
      else
        Assert.assertEquals(false, v5.get(i));
  }

  @Test
  public void testVectorAddElements() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.push(i % 2 == 0);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i % 2 == 0, v1.back());
      Assert.assertEquals(i % 2 == 0, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void legalUnsafePush() {
    LNGBooleanVector v1 = new LNGBooleanVector(1000);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.unsafePush(i % 2 == 0);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals(i % 2 == 0, v1.back());
      Assert.assertEquals(i % 2 == 0, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void illegalUnsafePush() {
    LNGBooleanVector v1 = new LNGBooleanVector(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.unsafePush(i % 2 == 0);
  }

  @Test
  public void testGettingSettingAndPopping() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    for (int i = 0; i < 1000; i++)
      v1.push(i % 2 == 0);
    for (int i = 999; i >= 0; i--) {
      v1.set(i, true);
      Assert.assertEquals(true, v1.get(i));
    }
    for (int i = 999; i >= 0; i--) {
      v1.pop();
      Assert.assertEquals(i, v1.size());
    }
  }

  @Test
  public void testVectorShrink() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i % 2 == 0);
    Assert.assertFalse(v1.empty());
    int beforeSize = v1.size();
    v1.shrinkTo(v1.size() + 50);
    Assert.assertEquals(v1.size(), beforeSize);
    for (int i = 500; i > 0; i--) {
      v1.shrinkTo(i);
      Assert.assertEquals((i - 1) % 2 == 0, v1.back());
    }
  }

  @Test
  public void testGrowTo() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i % 2 == 0);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 1001; i += 10) {
      v1.growTo(1000 + i, true);
      Assert.assertEquals(1000 + i, v1.size());
      for (int j = 0; j < 1000; j++)
        Assert.assertEquals(j % 2 == 0, v1.get(j));
      for (int j = 1000; j < 1000 + i; j++)
        Assert.assertEquals(true, v1.get(j));
    }
    Assert.assertEquals(2000, v1.size());
    v1.growTo(100, true);
    Assert.assertEquals(2000, v1.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals(i % 2 == 0, v1.get(i));
    for (int i = 1000; i < 2000; i++)
      Assert.assertEquals(true, v1.get(i));
  }

  @Test
  public void testRemoveElements() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push(i % 2 == 0);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 9; i++) {
      v1.removeElements(100);
      Assert.assertEquals(1000 - (i + 1) * 100, v1.size());
      Assert.assertEquals((1000 - (i + 1) * 100 - 1) % 2 == 0, v1.back());
    }
    Assert.assertEquals(100, v1.size());
    v1.removeElements(100);
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testReverseInplace() {
    LNGBooleanVector v1 = new LNGBooleanVector(true, true, false, true, false, false, true, true);
    v1.shrinkTo(7);
    v1.reverseInplace();
    Assert.assertEquals(true, v1.get(0));
    Assert.assertEquals(false, v1.get(1));
    Assert.assertEquals(false, v1.get(2));
    Assert.assertEquals(true, v1.get(3));
    Assert.assertEquals(false, v1.get(4));
    Assert.assertEquals(true, v1.get(5));
    Assert.assertEquals(true, v1.get(6));
  }

  @Test
  public void testToArray() {
    LNGBooleanVector v1 = new LNGBooleanVector(1000);
    boolean[] expected = new boolean[500];
    for (int i = 0; i < 1000; i++) {
      v1.push(i % 2 == 0);
      if (i < 500)
        expected[i] = i % 2 == 0;
    }
    v1.shrinkTo(500);
    Assert.assertArrayEquals(expected, v1.toArray());
  }

  @Test
  public void testToString() {
    LNGBooleanVector v1 = new LNGBooleanVector();
    Assert.assertEquals("[]", v1.toString());
    v1.push(true);
    Assert.assertEquals("[true]", v1.toString());
    v1.push(false);
    Assert.assertEquals("[true, false]", v1.toString());
    v1.push(false);
    Assert.assertEquals("[true, false, false]", v1.toString());
    v1.push(true);
    Assert.assertEquals("[true, false, false, true]", v1.toString());
  }
}
