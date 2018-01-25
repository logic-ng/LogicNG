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

package org.logicng.functions;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.LinkedHashSet;

import static org.logicng.formulas.cache.FunctionCacheEntry.SUBFORMULAS;

/**
 * Unit tests for {@link SubNodeFunction}.
 * @version 1.1
 * @since 1.0
 */
public class SubformulaTest {

  @Test
  public void testConstants() {
    Assert.assertTrue(F.TRUE.containsNode(F.TRUE));
    Assert.assertTrue(F.FALSE.containsNode(F.FALSE));
    Assert.assertFalse(F.TRUE.containsNode(F.FALSE));
    Assert.assertFalse(F.FALSE.containsNode(F.TRUE));
    Assert.assertFalse(F.FALSE.containsNode(F.A));
  }

  @Test
  public void testLiterals() {
    Assert.assertTrue(F.A.containsNode(F.A));
    Assert.assertTrue(F.A.containsNode(F.f.variable("a")));
    Assert.assertFalse(F.NA.containsNode(F.A));
    Assert.assertTrue(F.NA.containsNode(F.f.literal("a", false)));
    Assert.assertFalse(F.A.containsNode(F.NA));
    Assert.assertFalse(F.A.containsNode(F.B));
    Assert.assertFalse(F.NA.containsNode(F.NB));
    Assert.assertFalse(F.A.containsNode(F.FALSE));
    Assert.assertFalse(F.NA.containsNode(F.TRUE));
  }

  @Test
  public void testNot() {
    Assert.assertTrue(F.NOT1.containsNode(F.NOT1));
    Assert.assertTrue(F.NOT1.containsNode(F.f.not(F.AND1)));
    Assert.assertTrue(F.NOT1.containsNode(F.AND1));
    Assert.assertTrue(F.NOT1.containsNode(F.A));
    Assert.assertTrue(F.NOT1.containsNode(F.f.variable("b")));
    Assert.assertTrue(F.NOT2.containsNode(F.NOT2));
    Assert.assertTrue(F.NOT2.containsNode(F.OR1));
    Assert.assertTrue(F.NOT2.containsNode(F.X));
    Assert.assertTrue(F.NOT2.containsNode(F.Y));

    Assert.assertFalse(F.NOT1.containsNode(F.OR1));
    Assert.assertFalse(F.NOT1.containsNode(F.X));
    Assert.assertFalse(F.NOT2.containsNode(F.NOT1));
    Assert.assertFalse(F.NOT2.containsNode(F.AND1));
  }

  @Test
  public void testImplication() {
    Assert.assertTrue(F.IMP1.containsNode(F.IMP1));
    Assert.assertTrue(F.IMP1.containsNode(F.f.implication(F.A, F.B)));
    Assert.assertTrue(F.IMP2.containsNode(F.IMP2));
    Assert.assertTrue(F.IMP3.containsNode(F.IMP3));
    Assert.assertTrue(F.IMP4.containsNode(F.IMP4));
    Assert.assertTrue(F.IMP1.containsNode(F.A));
    Assert.assertTrue(F.IMP1.containsNode(F.B));
    Assert.assertTrue(F.IMP2.containsNode(F.NA));
    Assert.assertTrue(F.IMP2.containsNode(F.NB));
    Assert.assertFalse(F.IMP2.containsNode(F.A));
    Assert.assertFalse(F.IMP2.containsNode(F.B));
    Assert.assertTrue(F.IMP3.containsNode(F.AND1));
    Assert.assertTrue(F.IMP3.containsNode(F.OR1));
    Assert.assertTrue(F.IMP3.containsNode(F.A));
    Assert.assertTrue(F.IMP3.containsNode(F.B));
    Assert.assertTrue(F.IMP3.containsNode(F.X));
    Assert.assertTrue(F.IMP3.containsNode(F.Y));
    Assert.assertTrue(F.IMP4.containsNode(F.f.equivalence(F.A, F.B)));
    Assert.assertTrue(F.IMP4.containsNode(F.f.equivalence(F.NX, F.NY)));

    Assert.assertFalse(F.IMP4.containsNode(F.C));
    Assert.assertFalse(F.IMP4.containsNode(F.NOT1));
    Assert.assertFalse(F.IMP4.containsNode(F.f.equivalence(F.X, F.NY)));
    Assert.assertFalse(F.IMP4.containsNode(F.f.equivalence(F.NY, F.X)));
  }

  @Test
  public void testEquivalence() {
    Assert.assertTrue(F.EQ1.containsNode(F.EQ1));
    Assert.assertTrue(F.EQ1.containsNode(F.f.equivalence(F.A, F.B)));
    Assert.assertTrue(F.EQ4.containsNode(F.IMP1));
    Assert.assertTrue(F.EQ4.containsNode(F.IMP2));
    Assert.assertTrue(F.EQ4.containsNode(F.A));
    Assert.assertTrue(F.EQ4.containsNode(F.B));

    Assert.assertFalse(F.EQ2.containsNode(F.C));
    Assert.assertFalse(F.EQ2.containsNode(F.NOT1));
  }

  @Test
  public void testOr() {
    Assert.assertTrue(F.OR1.containsNode(F.f.or(F.X, F.Y)));
    Assert.assertTrue(F.OR1.containsNode(F.X));
    Assert.assertTrue(F.OR1.containsNode(F.f.variable("y")));
    Assert.assertTrue(F.OR3.containsNode(F.AND1));
    Assert.assertTrue(F.OR3.containsNode(F.AND2));
    Assert.assertTrue(F.OR3.containsNode(F.NA));
    Assert.assertTrue(F.OR3.containsNode(F.NB));
    Assert.assertTrue(F.OR3.containsNode(F.A));
    Assert.assertTrue(F.OR3.containsNode(F.B));
    Assert.assertTrue(F.f.or(F.A, F.B, F.NX, F.NY, F.C).containsNode(F.f.or(F.A, F.NX, F.C)));
    Assert.assertTrue(F.f.or(F.A, F.B, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.X, F.Y, F.AND1)));
    Assert.assertTrue(F.f.or(F.A, F.B, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.AND1, F.X)));

    Assert.assertFalse(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.B)));
    Assert.assertFalse(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.NY));
    Assert.assertFalse(F.f.or(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.C)));
  }

  @Test
  public void testAnd() {
    Assert.assertTrue(F.AND1.containsNode(F.f.and(F.A, F.B)));
    Assert.assertTrue(F.AND1.containsNode(F.A));
    Assert.assertTrue(F.AND1.containsNode(F.f.variable("b")));
    Assert.assertTrue(F.AND3.containsNode(F.OR1));
    Assert.assertTrue(F.AND3.containsNode(F.OR2));
    Assert.assertTrue(F.AND3.containsNode(F.NX));
    Assert.assertTrue(F.AND3.containsNode(F.NY));
    Assert.assertTrue(F.AND3.containsNode(F.X));
    Assert.assertTrue(F.AND3.containsNode(F.Y));
    Assert.assertTrue(F.f.and(F.A, F.B, F.NX, F.NY, F.C).containsNode(F.f.and(F.A, F.NX, F.C)));
    Assert.assertTrue(F.f.and(F.X, F.Y, F.OR1, F.C, F.AND1).containsNode(F.f.and(F.A, F.B, F.C)));
    Assert.assertTrue(F.f.and(F.A, F.B, F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.and(F.A, F.OR1, F.NX)));
    Assert.assertTrue(F.f.and(F.A, F.B, F.NX, F.IMP1, F.C).containsNode(F.IMP1));

    Assert.assertFalse(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.B)));
    Assert.assertFalse(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.NY));
    Assert.assertFalse(F.f.and(F.NX, F.OR1, F.C, F.AND1).containsNode(F.f.or(F.A, F.C)));
  }

  @Test
  public void subformulasTest() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    final Formula f1 = p.parse("((a & ~b & c) | (d & (~e | c))) & (a => (~x | y) & (x | ~z))");
    final LinkedHashSet<Formula> expected = new LinkedHashSet<>();
    expected.add(p.parse("a"));
    expected.add(p.parse("~b"));
    expected.add(p.parse("c"));
    expected.add(p.parse("a & ~b & c"));
    expected.add(p.parse("d"));
    expected.add(p.parse("~e"));
    expected.add(p.parse("~e | c"));
    expected.add(p.parse("d & (~e | c)"));
    expected.add(p.parse("(a & ~b & c) | (d & (~e | c))"));
    expected.add(p.parse("~x"));
    expected.add(p.parse("y"));
    expected.add(p.parse("~x | y"));
    expected.add(p.parse("x"));
    expected.add(p.parse("~z"));
    expected.add(p.parse("x | ~z"));
    expected.add(p.parse("(~x | y) & (x | ~z)"));
    expected.add(p.parse("a => (~x | y) & (x | ~z)"));
    expected.add(p.parse("((a & ~b & c) | (d & (~e | c))) & (a => (~x | y) & (x | ~z))"));
    Assert.assertEquals(expected, f1.apply(new SubNodeFunction()));
  }

  @Test
  public void testNotCache() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    final Formula f1 = p.parse("(d | (a & b)) & (c | (a & b)) | (a & b )");
    f1.apply(new SubNodeFunction(), false);
    Assert.assertNull(f1.functionCacheEntry(SUBFORMULAS));
  }
}
