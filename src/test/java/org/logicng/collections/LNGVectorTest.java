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

import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Unit tests for {@link LNGVector}.
 * @version 1.1
 * @since 1.0
 */
public class LNGVectorTest {

  private Comparator<String> stringComparator;

  public LNGVectorTest() {
    stringComparator = new Comparator<String>() {
      @Override
      public int compare(String o1, String o2) {
        return o1.compareTo(o2);
      }
    };
  }

  @Test
  public void testVectorCreation() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertEquals(0, v1.size());
    Assert.assertTrue(v1.empty());
    LNGVector<String> v2 = new LNGVector<>(10);
    Assert.assertEquals(0, v2.size());
    Assert.assertTrue(v2.empty());
    LNGVector<String> v3 = new LNGVector<>(10, "string");
    Assert.assertEquals(10, v3.size());
    for (int i = 0; i < v3.size(); i++)
      Assert.assertEquals("string", v3.get(i));
    Assert.assertFalse(v3.empty());
    LNGVector<String> v4 = new LNGVector<>("s1", "s2", "s3", "s4", "s5");
    Assert.assertEquals(5, v4.size());
    int count = 1;
    for (final String s : v4)
      Assert.assertEquals("s" + count++, s);
    List<String> list = new LinkedList<>();
    for (int i = 0; i < 1000; i++)
      list.add("s" + i);
    LNGVector<String> v5 = new LNGVector<>(list);
    Assert.assertEquals(1000, v5.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals("s" + i, v5.get(i));
  }

  @Test
  public void testVectorAddElements() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.push("s" + i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals("s" + i, v1.back());
      Assert.assertEquals("s" + i, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testRelease() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.push("s" + i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals("s" + i, v1.back());
      Assert.assertEquals("s" + i, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.release();
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void legalUnsafePush() {
    LNGVector<String> v1 = new LNGVector<>(1000);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++) {
      v1.unsafePush("s" + i);
      Assert.assertEquals(i + 1, v1.size());
      Assert.assertEquals("s" + i, v1.back());
      Assert.assertEquals("s" + i, v1.get(i));
    }
    Assert.assertFalse(v1.empty());
    v1.clear();
    Assert.assertTrue(v1.empty());
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void illegalUnsafePush() {
    LNGVector<String> v1 = new LNGVector<>(100);
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.unsafePush("s" + i);
  }

  @Test
  public void testGettingSettingAndPopping() {
    LNGVector<String> v1 = new LNGVector<>();
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    for (int i = 999; i >= 0; i--) {
      v1.set(i, "string");
      Assert.assertEquals("string", v1.get(i));
    }
    for (int i = 999; i >= 0; i--) {
      v1.pop();
      Assert.assertEquals(i, v1.size());
    }
  }

  @Test
  public void testVectorShrink() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    Assert.assertFalse(v1.empty());
    int beforeSize = v1.size();
    v1.shrinkTo(v1.size() + 50);
    Assert.assertEquals(v1.size(), beforeSize);
    for (int i = 500; i > 0; i--) {
      v1.shrinkTo(i);
      Assert.assertEquals("s" + (i - 1), v1.back());
    }
  }

  @Test
  public void testGrowToWithPad() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 1001; i += 10) {
      v1.growTo(1000 + i, "string");
      Assert.assertEquals(1000 + i, v1.size());
      for (int j = 0; j < 1000; j++)
        Assert.assertEquals("s" + j, v1.get(j));
      for (int j = 1000; j < 1000 + i; j++)
        Assert.assertEquals("string", v1.get(j));
    }
    Assert.assertEquals(2000, v1.size());
    v1.growTo(100, "string");
    Assert.assertEquals(2000, v1.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals("s" + i, v1.get(i));
    for (int i = 1000; i < 2000; i++)
      Assert.assertEquals("string", v1.get(i));
  }

  @Test
  public void testGrowTo() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 2000; i++)
      v1.push("s" + i);
    v1.shrinkTo(1000);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 1001; i += 10) {
      v1.growTo(1000 + i);
      Assert.assertEquals(1000 + i, v1.size());
      for (int j = 0; j < 1000; j++)
        Assert.assertEquals("s" + j, v1.get(j));
      for (int j = 1000; j < 1000 + i; j++)
        Assert.assertEquals(null, v1.get(j));
    }
    Assert.assertEquals(2000, v1.size());
    v1.growTo(100, "string");
    Assert.assertEquals(2000, v1.size());
    for (int i = 0; i < 1000; i++)
      Assert.assertEquals("s" + i, v1.get(i));
    for (int i = 1000; i < 2000; i++)
      Assert.assertEquals(null, v1.get(i));
  }

  @Test
  public void testRemoveElements() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    Assert.assertFalse(v1.empty());
    for (int i = 0; i < 9; i++) {
      v1.removeElements(100);
      Assert.assertEquals(1000 - (i + 1) * 100, v1.size());
      Assert.assertEquals("s" + (1000 - (i + 1) * 100 - 1), v1.back());
    }
    Assert.assertEquals(100, v1.size());
    v1.removeElements(100);
    Assert.assertTrue(v1.empty());
  }

  @Test
  public void testInplaceReplace() {
    LNGVector<String> v1 = new LNGVector<>();
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    LNGVector<String> v2 = new LNGVector<>();
    for (int i = 0; i < 500; i++)
      v2.push("str" + i);
    LNGVector<String> v3 = new LNGVector<>();
    for (int i = 0; i < 2000; i++)
      v3.push("string" + i);
    v1.replaceInplace(v2);
    Assert.assertEquals(500, v1.size());
    for (int i = 0; i < 500; i++)
      Assert.assertEquals("str" + i, v1.get(i));
    v2.replaceInplace(v3);
    Assert.assertEquals(2000, v2.size());
    for (int i = 0; i < 2000; i++)
      Assert.assertEquals("string" + i, v2.get(i));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalInplaceReplace() {
    LNGVector<String> v1 = new LNGVector<>();
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    v1.replaceInplace(v1);
  }

  @Test
  public void testManualSort() {
    LNGVector<String> v1 = new LNGVector<>(1000);
    LNGVector<String> v2 = new LNGVector<>(1000);
    for (int i = 999; i >= 0; i--)
      v1.push("s" + i);
    for (int i = 0; i < 1000; i++)
      v2.push("s" + i);
    v1.manualSort(stringComparator);
    v2.manualSort(stringComparator);
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 999)
        Assert.assertTrue(v1.get(i).compareTo(v1.get(i + 1)) < 0);
    }
    LNGVector<String> v3 = new LNGVector<>(1000);
    v3.manualSort(stringComparator);
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testSort() {
    LNGVector<String> v1 = new LNGVector<>(1000);
    LNGVector<String> v2 = new LNGVector<>(1000);
    for (int i = 999; i >= 0; i--)
      v1.push("s" + i);
    for (int i = 0; i < 1000; i++)
      v2.push("s" + i);
    v1.sort(stringComparator);
    v2.sort(stringComparator);
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 999)
        Assert.assertTrue(v1.get(i).compareTo(v1.get(i + 1)) < 0);
    }
    LNGVector<String> v3 = new LNGVector<>(1000);
    v3.sort(stringComparator);
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testSortReverse() {
    LNGVector<String> v1 = new LNGVector<>(1000);
    LNGVector<String> v2 = new LNGVector<>(1000);
    for (int i = 999; i >= 0; i--)
      v1.push("s" + i);
    for (int i = 0; i < 1000; i++)
      v2.push("s" + i);
    v1.sortReverse(stringComparator);
    v2.sortReverse(stringComparator);
    for (int i = 0; i < 1000; i++) {
      Assert.assertEquals(v2.get(i), v1.get(i));
      if (i != 999)
        Assert.assertTrue(v1.get(i).compareTo(v1.get(i + 1)) > 0);
    }
    LNGVector<String> v3 = new LNGVector<>(1000);
    v3.sortReverse(stringComparator);
    Assert.assertTrue(v3.empty());
  }

  @Test
  public void testRemove() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    v1.remove("s500");
    Assert.assertEquals(999, v1.size());
    Assert.assertEquals("s499", v1.get(499));
    Assert.assertEquals("s501", v1.get(500));
    v1.remove("s0");
    Assert.assertEquals(998, v1.size());
    Assert.assertEquals("s1", v1.get(0));
    Assert.assertEquals("s499", v1.get(498));
    Assert.assertEquals("s501", v1.get(499));
    v1.remove("s999");
    Assert.assertEquals(997, v1.size());
    Assert.assertEquals("s1", v1.get(0));
    Assert.assertEquals("s499", v1.get(498));
    Assert.assertEquals("s501", v1.get(499));
    Assert.assertEquals("s998", v1.get(996));
    v1.remove("s1001");
    Assert.assertEquals(997, v1.size());
    Assert.assertEquals("s1", v1.get(0));
    Assert.assertEquals("s499", v1.get(498));
    Assert.assertEquals("s501", v1.get(499));
    Assert.assertEquals("s998", v1.get(996));
    LNGVector<String> v2 = new LNGVector<>("s1", "s1", "s2", "s5", "s8");
    v2.remove("s1");
    Assert.assertEquals(4, v2.size());
    Assert.assertEquals("[s1, s2, s5, s8]", v2.toString());
  }

  @Test
  public void testToArray() {
    LNGVector<String> v1 = new LNGVector<>(1000);
    String[] expected = new String[500];
    for (int i = 0; i < 1000; i++) {
      v1.push("s" + i);
      if (i < 500)
        expected[i] = "s" + i;
    }
    v1.shrinkTo(500);
    Assert.assertArrayEquals(expected, v1.toArray());
  }

  @Test
  public void testIterator() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    int count = 0;
    for (final String s : v1)
      Assert.assertEquals("s" + count++, s);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIllegalIteratorRemoval() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    final Iterator<String> it = v1.iterator();
    Assert.assertTrue(it.hasNext());
    it.next();
    it.remove();
  }

  @Test(expected = NoSuchElementException.class)
  public void testIllegalIteratorTraversal() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertTrue(v1.empty());
    for (int i = 0; i < 1000; i++)
      v1.push("s" + i);
    final Iterator<String> it = v1.iterator();
    Assert.assertTrue(it.hasNext());
    for (int i = 0; i < 1001; i++)
      it.next();
  }

  @Test
  public void testToString() {
    LNGVector<String> v1 = new LNGVector<>();
    Assert.assertEquals("[]", v1.toString());
    v1.push("s1");
    Assert.assertEquals("[s1]", v1.toString());
    v1.push("s2");
    Assert.assertEquals("[s1, s2]", v1.toString());
    v1.push("s3");
    Assert.assertEquals("[s1, s2, s3]", v1.toString());
    v1.push("s4");
    Assert.assertEquals("[s1, s2, s3, s4]", v1.toString());
  }
}
