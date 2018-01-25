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

package org.logicng.transformations;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.AIGPredicate;


/**
 * Unit Tests for AIG conversion.
 * @version 1.1
 * @since 1.0
 */
public class AIGTest {

  private final AIGTransformation aigTrans = new AIGTransformation();
  private final AIGPredicate aigPred = new AIGPredicate();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(aigTrans));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(aigTrans));
    Assert.assertTrue(F.TRUE.holds(aigPred));
    Assert.assertTrue(F.FALSE.holds(aigPred));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(aigTrans));
    Assert.assertEquals(F.NA, F.NA.transform(aigTrans));
    Assert.assertTrue(F.A.holds(aigPred));
    Assert.assertTrue(F.NA.holds(aigPred));
  }

  @Test
  public void testBinaryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~(a & ~b)"), F.IMP1.transform(aigTrans));
    Assert.assertEquals(p.parse("~(~a & b)"), F.IMP2.transform(aigTrans));
    Assert.assertEquals(p.parse("~((a & b) & (~x & ~y))"), F.IMP3.transform(aigTrans));
    Assert.assertEquals(p.parse("~(a & ~b) & ~(~a & b)"), F.EQ1.transform(aigTrans));
    Assert.assertEquals(p.parse("~(a & ~b) & ~(~a & b)"), F.EQ2.transform(aigTrans));
    Assert.assertTrue(F.IMP1.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.IMP2.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.IMP3.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.EQ1.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.EQ2.transform(aigTrans).holds(aigPred));
    Assert.assertFalse(F.IMP1.holds(aigPred));
    Assert.assertFalse(F.IMP2.holds(aigPred));
    Assert.assertFalse(F.IMP3.holds(aigPred));
    Assert.assertFalse(F.EQ1.holds(aigPred));
    Assert.assertFalse(F.EQ2.holds(aigPred));
    Formula impl = p.parse("m => n");
    impl.transform(aigTrans, false);
    Formula aigIMPL = impl.transformationCacheEntry(TransformationCacheEntry.AIG);
    Assert.assertNull(aigIMPL);
    Formula equi = p.parse("m <=> n");
    equi.transform(aigTrans, false);
    Formula aigEQUI = impl.transformationCacheEntry(TransformationCacheEntry.AIG);
    Assert.assertNull(aigEQUI);
  }

  @Test
  public void testNAryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.AND1.transform(aigTrans));
    Assert.assertEquals(p.parse("~(~x & ~y)"), F.OR1.transform(aigTrans));
    Assert.assertEquals(p.parse("(~a & ~b) & c & ~(x & ~y) & ~(w & ~z)"), p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(aigTrans));
    Assert.assertEquals(p.parse("~(a & b & ~c & ~(~x & y))"), p.parse("~(a & b) | c | ~(x | ~y)").transform(aigTrans));
    Assert.assertEquals(p.parse("~(~a & ~b & ~(~x & ~y))"), p.parse("a | b | (~x & ~y)").transform(aigTrans));
    Assert.assertTrue(F.AND1.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.OR1.transform(aigTrans).holds(aigPred));
    Assert.assertTrue(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(aigTrans).holds(aigPred));
    Assert.assertTrue(p.parse("~(a & b) | c | ~(x | ~y)").transform(aigTrans).holds(aigPred));
    Assert.assertTrue(p.parse("a | b | (~x & ~y)").transform(aigTrans).holds(aigPred));
    Assert.assertTrue(F.AND1.holds(aigPred));
    Assert.assertFalse(F.OR1.holds(aigPred));
    Assert.assertFalse(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").holds(aigPred));
    Assert.assertFalse(p.parse("~(a & b) | c | ~(x | ~y)").holds(aigPred));
    Assert.assertFalse(p.parse("a | b | (~x & ~y)").holds(aigPred));
    Formula or = p.parse("m | n | o");
    or.transform(aigTrans, false);
    Formula aigOR = or.transformationCacheEntry(TransformationCacheEntry.AIG);
    Assert.assertNull(aigOR);
    Formula and = p.parse("m & n & o");
    and.transform(aigTrans, false);
    Formula aigAND = and.transformationCacheEntry(TransformationCacheEntry.AIG);
    Assert.assertNull(aigAND);
  }

  @Test
  public void testNot() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a"), p.parse("~a").transform(aigTrans));
    Assert.assertEquals(p.parse("a"), p.parse("~~a").transform(aigTrans));
    Assert.assertEquals(p.parse("a & ~b"), p.parse("~(a => b)").transform(aigTrans));
    Assert.assertEquals(p.parse("(~a & ~b) & ~(~x & ~y)"), p.parse("~(~(a | b) => ~(x | y))").transform(aigTrans));
    Assert.assertEquals(p.parse("~(~(a & ~b) & ~(~a & b))"), p.parse("~(a <=> b)").transform(aigTrans));
    Assert.assertEquals(p.parse("~(~(~a & ~b & ~(~x & ~y)) & ~((a | b) & ~(x | y)))"), p.parse("~(~(a | b) <=> ~(x | y))").transform(aigTrans));
    Assert.assertEquals(p.parse("~(a & b & ~x & ~y)"), p.parse("~(a & b & ~x & ~y)").transform(aigTrans));
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").transform(aigTrans));
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").transform(aigTrans)); // test caching
    Formula not = p.parse("~(m | n)");
    not.transform(aigTrans, false);
    Formula aig = not.transformationCacheEntry(TransformationCacheEntry.AIG);
    Assert.assertNull(aig);
  }

  @Test
  public void testPBC() {
    Assert.assertTrue(F.PBC1.transform(aigTrans).holds(aigPred));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("AIGTransformation", aigTrans.toString());
    Assert.assertEquals("AIGPredicate", aigPred.toString());
  }
}
