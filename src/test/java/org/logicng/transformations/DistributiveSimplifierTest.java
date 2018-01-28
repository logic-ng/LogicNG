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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link DistributiveSimplifier}.
 * @version 1.3
 * @since 1.3
 */
public class DistributiveSimplifierTest {

  private DistributiveSimplifier distributiveSimplifier = new DistributiveSimplifier();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(distributiveSimplifier));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(distributiveSimplifier));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(distributiveSimplifier));
    Assert.assertEquals(F.NA, F.NA.transform(distributiveSimplifier));
  }

  @Test
  public void testNoPropagation() {
    Assert.assertEquals(F.AND1, F.AND1.transform(distributiveSimplifier));
    Assert.assertEquals(F.AND2, F.AND2.transform(distributiveSimplifier));
    Assert.assertEquals(F.OR1, F.OR1.transform(distributiveSimplifier));
    Assert.assertEquals(F.OR2, F.OR2.transform(distributiveSimplifier));
  }

  @Test
  public void testPropagations() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.f.and(F.AND1, F.A).transform(distributiveSimplifier));
    Assert.assertEquals(F.FALSE, F.f.and(F.AND2, F.A).transform(distributiveSimplifier));
    Assert.assertEquals(F.f.and(F.OR1, F.X), F.f.and(F.OR1, F.X).transform(distributiveSimplifier));
    Assert.assertEquals(F.f.and(F.OR2, F.X), F.f.and(F.OR2, F.X).transform(distributiveSimplifier));
    Assert.assertEquals(F.f.or(F.AND1, F.A), F.f.or(F.AND1, F.A).transform(distributiveSimplifier));
    Assert.assertEquals(F.f.or(F.AND2, F.A), F.f.or(F.AND2, F.A).transform(distributiveSimplifier));
    Assert.assertEquals(F.OR1, F.f.or(F.OR1, F.X).transform(distributiveSimplifier));
    Assert.assertEquals(p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & f & c & (e | (~b | ~f | g) & (f | g | h) & (~f | ~g | h))"),
            p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(distributiveSimplifier));
  }

  @Test
  public void testFormulaTypes() {
    Assert.assertEquals(F.IMP1, F.IMP1.transform(distributiveSimplifier));
    Assert.assertEquals(F.EQ1, F.EQ1.transform(distributiveSimplifier));
    Assert.assertEquals(F.NOT1, F.NOT1.transform(distributiveSimplifier));
  }

  @Test
  public void testComplexExamples() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Formula cAnd = p.parse("(a | b | ~c) & (~a | ~d) & (~c | d | b) & (~c | ~b)");
    Formula cAndD1 = cAnd.transform(distributiveSimplifier);
    Assert.assertEquals(p.parse("(~a | ~d) & (~c | (a | b) & (d | b) & ~b)"), cAndD1);
    Assert.assertEquals(p.parse("(~a | ~d) & (~c | ~b & (b | a & d))"), cAndD1.transform(distributiveSimplifier));

    Assert.assertEquals(F.f.not(cAndD1), F.f.not(cAnd).transform(distributiveSimplifier));

    Formula cOr = p.parse("(x & y & z) | (x & y & ~z) | (x & ~y & z)");
    Formula cOrD1 = cOr.transform(distributiveSimplifier);
    Assert.assertEquals(p.parse("x & (y & z | y & ~z | ~y & z)"), cOrD1);
    Assert.assertEquals(p.parse("x & (~y & z | y)"), cOrD1.transform(distributiveSimplifier));

    Assert.assertEquals(F.f.equivalence(cOrD1, cAndD1), F.f.equivalence(cOr, cAnd).transform(distributiveSimplifier));
    Assert.assertEquals(F.f.implication(cOrD1, cAndD1), F.f.implication(cOr, cAnd).transform(distributiveSimplifier));

  }
}
