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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link UnitPropagation}.
 * @version 1.2
 * @since 1.2
 */
public class UnitPropagationTest {

  private UnitPropagation unitPropagation = new UnitPropagation();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(unitPropagation));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(unitPropagation));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(unitPropagation));
    Assert.assertEquals(F.NA, F.NA.transform(unitPropagation));
  }

  @Test
  public void testNoPropagation() {
    Assert.assertEquals(F.AND1, F.AND1.transform(unitPropagation));
    Assert.assertEquals(F.AND2, F.AND2.transform(unitPropagation));
    Assert.assertEquals(F.OR1, F.OR1.transform(unitPropagation));
    Assert.assertEquals(F.OR2, F.OR2.transform(unitPropagation));
  }

  @Test
  public void testPropagations() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.f.and(F.AND1, F.A).transform(unitPropagation));
    Assert.assertEquals(F.FALSE, F.f.and(F.AND2, F.A).transform(unitPropagation));
    Assert.assertEquals(F.X, F.f.and(F.OR1, F.X).transform(unitPropagation));
    Assert.assertEquals(F.f.and(F.X, F.NY), F.f.and(F.OR2, F.X).transform(unitPropagation));
    Assert.assertEquals(F.A, F.f.or(F.AND1, F.A).transform(unitPropagation));
    Assert.assertEquals(F.f.or(F.A, F.NB), F.f.or(F.AND2, F.A).transform(unitPropagation));
    Assert.assertEquals(F.OR1, F.f.or(F.OR1, F.X).transform(unitPropagation));
    Assert.assertEquals(p.parse("(e | g) & (e | ~g | h) & f & c & d & ~a & b"),
            p.parse("(a | b | ~c) & (~a | ~d) & (~c | d) & (~b | e | ~f | g) & (e | f | g | h) & (e | ~f | ~g | h) & f & c").transform(unitPropagation));
  }
}
