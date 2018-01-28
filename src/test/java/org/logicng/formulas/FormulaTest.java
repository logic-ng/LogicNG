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

package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.cache.CacheEntry;

import java.util.Arrays;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_CNF;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_DNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.FACTORIZED_CNF;

/**
 * Test some common formula functionality.
 * @version 1.3
 * @since 1.0
 */
public class FormulaTest {

  private enum MyOwnCacheKey implements CacheEntry {
    MYKEY1("My Key 1"),
    MYKEY2("My Key 2");

    private String description;

    MyOwnCacheKey(final String description) {
      this.description = description;
    }

    @Override
    public String description() {
      return "MyOwnCacheKey{description=" + description + "}";
    }
  }

  @Test
  public void testStringContains() {
    final FormulaFactory f = new FormulaFactory();
    final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
    Assert.assertTrue(formula.containsVariable("a"));
    Assert.assertTrue(formula.containsVariable("b"));
    Assert.assertFalse(formula.containsVariable("x"));
    Assert.assertFalse(formula.containsVariable("y"));
  }

  @Test
  public void testTransformationCache() {
    final FormulaFactory f = new FormulaFactory();
    final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
    formula.setTransformationCacheEntry(FACTORIZED_CNF, f.or(f.literal("a", false), f.literal("b", false)));
    Assert.assertEquals(f.or(f.literal("a", false), f.literal("b", false)), formula.transformationCacheEntry(FACTORIZED_CNF));
  }

  @Test
  public void testPredicateCache() {
    final FormulaFactory f = new FormulaFactory();
    final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
    formula.setPredicateCacheEntry(IS_CNF, false);
    formula.setPredicateCacheEntry(IS_DNF, Tristate.UNDEF);
    Assert.assertEquals(Tristate.FALSE, formula.predicateCacheEntry(IS_CNF));
    Assert.assertEquals(Tristate.UNDEF, formula.predicateCacheEntry(IS_DNF));
  }

  @Test
  public void testFunctionCache() {
    final FormulaFactory f = new FormulaFactory();
    final Formula formula = f.not(f.and(f.variable("a"), f.variable("b")));
    formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY1, "key1");
    formula.setFunctionCacheEntry(MyOwnCacheKey.MYKEY2, "key2");
    Assert.assertEquals("My Key 1", MyOwnCacheKey.MYKEY1.description);
    Assert.assertEquals("key1", formula.functionCacheEntry(MyOwnCacheKey.MYKEY1));
    Assert.assertEquals("key2", formula.functionCacheEntry(MyOwnCacheKey.MYKEY2));
  }

  @Test
  public void testFType() {
    Assert.assertEquals(FType.AND, FType.valueOf("AND"));
    Assert.assertEquals(FType.NONE, FType.valueOf("NONE"));
    Assert.assertTrue(Arrays.asList(FType.values()).contains(FType.valueOf("PBC")));
    Assert.assertEquals(10, FType.values().length);
  }

  @Test
  public void testCType() {
    Assert.assertEquals(CType.EQ, CType.valueOf("EQ"));
    Assert.assertEquals(CType.LE, CType.valueOf("LE"));
    Assert.assertTrue(Arrays.asList(CType.values()).contains(CType.valueOf("GT")));
    Assert.assertEquals(5, CType.values().length);
  }
}
