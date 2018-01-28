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
import org.logicng.datastructures.Assignment;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for formula evaluation.
 * @version 1.0
 * @since 1.0
 */
public class EvaluationTest {

  private Assignment ass = new Assignment(Arrays.asList(F.A, F.B, F.C, F.NX, F.NY));

  @Test
  public void testConstantEval() {
    Assert.assertTrue(F.TRUE.evaluate(ass));
    Assert.assertFalse(F.FALSE.evaluate(ass));
  }

  @Test
  public void testLiteralEval() {
    Assert.assertTrue(F.A.evaluate(ass));
    Assert.assertFalse(F.NA.evaluate(ass));
    Assert.assertFalse(F.X.evaluate(ass));
    Assert.assertTrue(F.NX.evaluate(ass));
  }

  @Test
  public void testNotEval() {
    Assert.assertFalse(F.NOT1.evaluate(ass));
    Assert.assertTrue(F.NOT2.evaluate(ass));
  }

  @Test
  public void testBinaryEval() {
    Assert.assertTrue(F.IMP1.evaluate(ass));
    Assert.assertTrue(F.IMP2.evaluate(ass));
    Assert.assertFalse(F.IMP3.evaluate(ass));
    Assert.assertTrue(F.IMP4.evaluate(ass));

    Assert.assertTrue(F.EQ1.evaluate(ass));
    Assert.assertTrue(F.EQ2.evaluate(ass));
    Assert.assertFalse(F.EQ3.evaluate(ass));
    Assert.assertTrue(F.EQ4.evaluate(ass));
  }

  @Test
  public void testNAryEval() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertFalse(F.OR1.evaluate(ass));
    Assert.assertTrue(F.OR2.evaluate(ass));
    Assert.assertTrue(F.OR3.evaluate(ass));
    Assert.assertFalse(p.parse("~a | ~b | ~c | x | y").evaluate(ass));
    Assert.assertTrue(p.parse("~a | ~b | ~c | x | ~y").evaluate(ass));

    Assert.assertTrue(F.AND1.evaluate(ass));
    Assert.assertFalse(F.AND2.evaluate(ass));
    Assert.assertFalse(F.AND3.evaluate(ass));
    Assert.assertTrue(p.parse("a & b & c & ~x & ~y").evaluate(ass));
    Assert.assertFalse(p.parse("a & b & c & ~x & y").evaluate(ass));
  }
}
