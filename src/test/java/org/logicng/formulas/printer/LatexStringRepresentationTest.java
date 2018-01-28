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

package org.logicng.formulas.printer;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Variable;

/**
 * Unit tests for {@link LatexStringRepresentation}
 * @version 1.1
 * @since 1.0
 */
public class LatexStringRepresentationTest {
  private final FormulaStringRepresentation sr = new LatexStringRepresentation();

  @Test
  public void testLatexPrinter() {
    Assert.assertEquals("\\bottom", F.f.string(F.FALSE, sr));
    Assert.assertEquals("\\top", F.f.string(F.TRUE, sr));
    Assert.assertEquals("x", F.f.string(F.X, sr));
    Assert.assertEquals("\\lnot a", F.f.string(F.NA, sr));
    Assert.assertEquals("x_{1}", F.f.string(F.f.variable("x1"), sr));
    Assert.assertEquals("x_{190}", F.f.string(F.f.variable("x190"), sr));
    Assert.assertEquals("x_{234}", F.f.string(F.f.variable("x234"), sr));
    Assert.assertEquals("x_{567}", F.f.string(F.f.variable("x567"), sr));
    Assert.assertEquals("abc_{8}", F.f.string(F.f.variable("abc8"), sr));
    Assert.assertEquals("\\lnot a \\rightarrow \\lnot b", F.f.string(F.IMP2, sr));
    Assert.assertEquals("a \\land b \\rightarrow x \\lor y", F.f.string(F.IMP3, sr));
    Assert.assertEquals("a \\rightarrow b \\leftrightarrow \\lnot a \\rightarrow \\lnot b", F.f.string(F.EQ4, sr));
    Assert.assertEquals("\\left(x \\lor y\\right) \\land \\left(\\lnot x \\lor \\lnot y\\right)", F.f.string(F.AND3, sr));
    Assert.assertEquals("a \\land b \\land c \\land x", F.f.string(F.f.and(F.A, F.B, F.C, F.X), sr));
    Assert.assertEquals("a \\lor b \\lor c \\lor x", F.f.string(F.f.or(F.A, F.B, F.C, F.X), sr));
    Assert.assertEquals("2\\cdot a + -4\\cdot b + 3\\cdot x = 2", F.f.string(F.PBC1, sr));
    Assert.assertEquals("2\\cdot a + -4\\cdot b + 3\\cdot x > 2", F.f.string(F.PBC2, sr));
    Assert.assertEquals("2\\cdot a + -4\\cdot b + 3\\cdot x \\geq 2", F.f.string(F.PBC3, sr));
    Assert.assertEquals("2\\cdot a + -4\\cdot b + 3\\cdot x < 2", F.f.string(F.PBC4, sr));
    Assert.assertEquals("2\\cdot a + -4\\cdot b + 3\\cdot x \\leq 2", F.f.string(F.PBC5, sr));
  }

  @Test
  public void testSpecialCases() {
    Variable var = F.f.variable("\ntest9t");
    Assert.assertEquals("\ntest9t", F.f.string(var, sr));
    Assert.assertEquals("LatexStringRepresentation", sr.toString());
  }
}
