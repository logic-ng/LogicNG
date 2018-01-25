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
 * Unit tests for formula restriction.
 * @version 1.3
 * @since 1.0
 */
public class RestrictionTest {

  private Assignment ass = new Assignment(Arrays.asList(F.A, F.NB, F.NX));

  @Test
  public void testConstantRestrict() {
    Assert.assertEquals(F.TRUE, F.TRUE.restrict(ass));
    Assert.assertEquals(F.FALSE, F.FALSE.restrict(ass));
  }

  @Test
  public void testLiteralRestrict() {
    Assert.assertEquals(F.TRUE, F.A.restrict(ass));
    Assert.assertEquals(F.FALSE, F.NA.restrict(ass));
    Assert.assertEquals(F.FALSE, F.X.restrict(ass));
    Assert.assertEquals(F.TRUE, F.NX.restrict(ass));
    Assert.assertEquals(F.C, F.C.restrict(ass));
    Assert.assertEquals(F.NY, F.NY.restrict(ass));
  }

  @Test
  public void testNotRestrict() {
    Assert.assertEquals(F.TRUE, F.NOT1.restrict(ass));
    Assert.assertEquals(F.NY, F.NOT2.restrict(ass));
  }

  @Test
  public void testBinaryRestrict() {
    Assert.assertEquals(F.FALSE, F.IMP1.restrict(ass));
    Assert.assertEquals(F.TRUE, F.IMP2.restrict(ass));
    Assert.assertEquals(F.TRUE, F.f.implication(F.NA, F.C).restrict(ass));
    Assert.assertEquals(F.TRUE, F.IMP3.restrict(ass));
    Assert.assertEquals(F.C, F.f.implication(F.A, F.C).restrict(ass));

    Assert.assertEquals(F.FALSE, F.EQ1.restrict(ass));
    Assert.assertEquals(F.FALSE, F.EQ2.restrict(ass));
    Assert.assertEquals(F.NY, F.EQ3.restrict(ass));
    Assert.assertEquals(F.FALSE, F.EQ4.restrict(ass));
  }

  @Test
  public void testNAryRestrict() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.Y, F.OR1.restrict(ass));
    Assert.assertEquals(F.TRUE, F.OR2.restrict(ass));
    Assert.assertEquals(F.FALSE, F.OR3.restrict(ass));
    Assert.assertEquals(p.parse("~c | y"), p.parse("~a | b | ~c | x | y").restrict(ass));
    Assert.assertEquals(F.TRUE, p.parse("~a | b | ~c | ~x | ~y").restrict(ass));

    Assert.assertEquals(F.FALSE, F.AND1.restrict(ass));
    Assert.assertEquals(F.FALSE, F.AND2.restrict(ass));
    Assert.assertEquals(F.Y, F.AND3.restrict(ass));
    Assert.assertEquals(p.parse("c & ~y"), p.parse("a & ~b & c & ~x & ~y").restrict(ass));
    Assert.assertEquals(F.FALSE, p.parse("a & b & c & ~x & y").restrict(ass));
  }
}
