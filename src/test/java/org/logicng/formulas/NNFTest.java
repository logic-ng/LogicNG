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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit Tests for NNF conversion.
 * @version 1.3
 * @since 1.0
 */
public class NNFTest {

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.nnf());
    Assert.assertEquals(F.FALSE, F.FALSE.nnf());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.nnf());
    Assert.assertEquals(F.NA, F.NA.nnf());
  }

  @Test
  public void testBinaryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a | b"), F.IMP1.nnf());
    Assert.assertEquals(p.parse("a | ~b"), F.IMP2.nnf());
    Assert.assertEquals(p.parse("~a | ~b | x | y"), F.IMP3.nnf());
    Assert.assertEquals(p.parse("(~a | ~b) & (a | b) | (~x & ~y) | (x & y)"), F.IMP4.nnf());
    Assert.assertEquals(p.parse("(a & b) | (~a & ~b)"), F.EQ1.nnf());
    Assert.assertEquals(p.parse("(~a & ~b) | (a & b)"), F.EQ2.nnf());
    Assert.assertEquals(p.parse("(a & b & (x | y)) | ((~a | ~b) & ~x & ~y)"), F.EQ3.nnf());
  }

  @Test
  public void testNAryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.AND1.nnf());
    Assert.assertEquals(F.OR1, F.OR1.nnf());
    Assert.assertEquals(p.parse("~a & ~b & c & (~x | y) & (~w | z)"), p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").nnf());
    Assert.assertEquals(p.parse("~a  | ~b | c | (~x & y) | (~w | z)"), p.parse("~(a & b) | c | ~(x | ~y) | (w => z)").nnf());
  }

  @Test
  public void testNot() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a"), p.parse("~a").nnf());
    Assert.assertEquals(p.parse("a"), p.parse("~~a").nnf());
    Assert.assertEquals(p.parse("a & ~b"), p.parse("~(a => b)").nnf());
    Assert.assertEquals(p.parse("~a & ~b & (x | y)"), p.parse("~(~(a | b) => ~(x | y))").nnf());
    Assert.assertEquals(p.parse("(~a | ~b) & (a | b)"), p.parse("~(a <=> b)").nnf());
    Assert.assertEquals(p.parse("((a | b) | (x | y)) & ((~a & ~b) | (~x & ~y))"), p.parse("~(~(a | b) <=> ~(x | y))").nnf());
    Assert.assertEquals(p.parse("~a | ~b | x | y"), p.parse("~(a & b & ~x & ~y)").nnf());
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").nnf());
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").nnf());
  }
}
