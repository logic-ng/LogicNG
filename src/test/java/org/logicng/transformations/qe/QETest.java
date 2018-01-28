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

package org.logicng.transformations.qe;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for {@link UniversalQuantifierElimination} and {@link ExistentialQuantifierElimination}.
 * @version 1.0
 * @since 1.0
 */
public class QETest {

  private final FormulaFactory f = new FormulaFactory();
  private final PropositionalParser p = new PropositionalParser(f);
  private final ExistentialQuantifierElimination ex1 = new ExistentialQuantifierElimination();
  private final ExistentialQuantifierElimination ex2 = new ExistentialQuantifierElimination(f.variable("x"));
  private final ExistentialQuantifierElimination ex3 = new ExistentialQuantifierElimination(Arrays.asList(f.variable("x"), f.variable("y")));
  private final UniversalQuantifierElimination uni1 = new UniversalQuantifierElimination();
  private final UniversalQuantifierElimination uni2 = new UniversalQuantifierElimination(f.variable("x"));
  private final UniversalQuantifierElimination uni3 = new UniversalQuantifierElimination(Arrays.asList(f.variable("x"), f.variable("y")));

  @Test
  public void testConstants() {
    Assert.assertEquals(f.verum(), f.verum().transform(ex1));
    Assert.assertEquals(f.verum(), f.verum().transform(ex2));
    Assert.assertEquals(f.verum(), f.verum().transform(ex3));
    Assert.assertEquals(f.verum(), f.verum().transform(uni1));
    Assert.assertEquals(f.verum(), f.verum().transform(uni2));
    Assert.assertEquals(f.verum(), f.verum().transform(uni3));
    Assert.assertEquals(f.falsum(), f.falsum().transform(ex1));
    Assert.assertEquals(f.falsum(), f.falsum().transform(ex2));
    Assert.assertEquals(f.falsum(), f.falsum().transform(ex3));
    Assert.assertEquals(f.falsum(), f.falsum().transform(uni1));
    Assert.assertEquals(f.falsum(), f.falsum().transform(uni2));
    Assert.assertEquals(f.falsum(), f.falsum().transform(uni3));
  }

  @Test
  public void testLiterals() {
    final Formula x = f.variable("x");
    final Formula y = f.literal("y", false);
    final Formula z = f.variable("z");
    Assert.assertEquals(x, x.transform(ex1));
    Assert.assertEquals(f.verum(), x.transform(ex2));
    Assert.assertEquals(f.verum(), x.transform(ex3));
    Assert.assertEquals(x, x.transform(uni1));
    Assert.assertEquals(f.falsum(), x.transform(uni2));
    Assert.assertEquals(f.falsum(), x.transform(uni3));
    Assert.assertEquals(y, y.transform(ex1));
    Assert.assertEquals(y, y.transform(ex2));
    Assert.assertEquals(f.verum(), y.transform(ex3));
    Assert.assertEquals(y, y.transform(uni1));
    Assert.assertEquals(y, y.transform(uni2));
    Assert.assertEquals(f.falsum(), y.transform(uni3));
    Assert.assertEquals(z, z.transform(ex1));
    Assert.assertEquals(z, z.transform(ex2));
    Assert.assertEquals(z, z.transform(ex3));
    Assert.assertEquals(z, z.transform(uni1));
    Assert.assertEquals(z, z.transform(uni2));
    Assert.assertEquals(z, z.transform(uni3));
  }

  @Test
  public void testFormulas() throws ParserException {
    final Formula f1 = p.parse("a & (b | ~c)");
    final Formula f2 = p.parse("x & (b | ~c)");
    final Formula f3 = p.parse("x & (y | ~c)");
    Assert.assertEquals(f1, f1.transform(ex1));
    Assert.assertEquals(f1, f1.transform(ex2));
    Assert.assertEquals(f1, f1.transform(ex3));
    Assert.assertEquals(f1, f1.transform(uni1));
    Assert.assertEquals(f1, f1.transform(uni2));
    Assert.assertEquals(f1, f1.transform(uni3));
    Assert.assertEquals(f2, f2.transform(ex1));
    Assert.assertEquals(p.parse("b | ~c"), f2.transform(ex2));
    Assert.assertEquals(p.parse("b | ~c"), f2.transform(ex3));
    Assert.assertEquals(f2, f2.transform(uni1));
    Assert.assertEquals(f.falsum(), f2.transform(uni2));
    Assert.assertEquals(f.falsum(), f2.transform(uni3));
    Assert.assertEquals(f3, f3.transform(ex1));
    Assert.assertEquals(p.parse("y | ~c"), f3.transform(ex2));
    Assert.assertEquals(f.verum(), f3.transform(ex3));
    Assert.assertEquals(f3, f3.transform(uni1));
    Assert.assertEquals(f.falsum(), f3.transform(uni2));
    Assert.assertEquals(f.falsum(), f3.transform(uni3));
  }

}
