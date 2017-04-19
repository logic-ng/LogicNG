package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;

import static org.logicng.formulas.ExtendedFormulaFactory.shrinkMap;
import static org.logicng.formulas.ExtendedFormulaFactory.shrinkSet;

public class ExtendedFormulaFactoryTest {

  @Test
  public void testMapShrink() {
    LinkedHashMap<String, Integer> map = new LinkedHashMap<>();
    shrinkMap(map, 0);
    Assert.assertTrue(map.isEmpty());

    map.put("s1", 1);
    shrinkMap(map, 1);
    Assert.assertEquals(1, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));
    shrinkMap(map, 0);
    Assert.assertTrue(map.isEmpty());
    map.put("s1", 1);

    map.put("s2", 2);
    shrinkMap(map, 2);
    Assert.assertEquals(2, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));
    Assert.assertEquals(Integer.valueOf(2), map.get("s2"));

    shrinkMap(map, 1);
    Assert.assertEquals(1, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));

    map.put("s2", 2);
    shrinkMap(map, 0);
    Assert.assertTrue(map.isEmpty());

    map.put("s1", 1);
    map.put("s2", 2);
    map.put("s3", 3);
    map.put("s4", 4);

    shrinkMap(map, 3);
    Assert.assertEquals(3, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));
    Assert.assertEquals(Integer.valueOf(2), map.get("s2"));
    Assert.assertEquals(Integer.valueOf(3), map.get("s3"));
    map.put("s4", 4);

    shrinkMap(map, 2);
    Assert.assertEquals(2, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));
    Assert.assertEquals(Integer.valueOf(2), map.get("s2"));
    map.put("s3", 3);
    map.put("s4", 4);

    shrinkMap(map, 1);
    Assert.assertEquals(1, map.size());
    Assert.assertEquals(Integer.valueOf(1), map.get("s1"));
    map.put("s2", 2);
    map.put("s3", 3);
    map.put("s4", 4);

    shrinkMap(map, 0);
    Assert.assertTrue(map.isEmpty());
  }

  @Test
  public void testSetShrink() {
    LinkedHashSet<String> set = new LinkedHashSet<>();
    shrinkSet(set, 0);
    Assert.assertTrue(set.isEmpty());

    set.add("s1");
    shrinkSet(set, 1);
    Assert.assertEquals(1, set.size());
    Assert.assertTrue(set.contains("s1"));
    shrinkSet(set, 0);
    Assert.assertTrue(set.isEmpty());
    set.add("s1");

    set.add("s2");
    shrinkSet(set, 2);
    Assert.assertEquals(2, set.size());
    Assert.assertTrue(set.contains("s1"));
    Assert.assertTrue(set.contains("s2"));

    shrinkSet(set, 1);
    Assert.assertEquals(1, set.size());
    Assert.assertTrue(set.contains("s1"));

    set.add("s2");
    shrinkSet(set, 0);
    Assert.assertTrue(set.isEmpty());

    set.add("s1");
    set.add("s2");
    set.add("s3");
    set.add("s4");

    shrinkSet(set, 3);
    Assert.assertEquals(3, set.size());
    Assert.assertTrue(set.contains("s1"));
    Assert.assertTrue(set.contains("s2"));
    Assert.assertTrue(set.contains("s3"));
    set.add("s4");

    shrinkSet(set, 2);
    Assert.assertEquals(2, set.size());
    Assert.assertTrue(set.contains("s1"));
    Assert.assertTrue(set.contains("s2"));
    set.add("s3");
    set.add("s4");

    shrinkSet(set, 1);
    Assert.assertEquals(1, set.size());
    Assert.assertTrue(set.contains("s1"));
    set.add("s2");
    set.add("s3");
    set.add("s4");

    shrinkSet(set, 0);
    Assert.assertTrue(set.isEmpty());
  }

}
