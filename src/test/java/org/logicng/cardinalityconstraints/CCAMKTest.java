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

package org.logicng.cardinalityconstraints;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the at-most-k encoders.
 * @version 1.1
 * @since 1.0
 */
public class CCAMKTest {

  private CCConfig[] configs;

  public CCAMKTest() {
    configs = new CCConfig[3];
    configs[0] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).build();
    configs[1] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).build();
    configs[2] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).build();
  }

  @Test
  public void testAMK() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs) {
      f.putConfiguration(config);
      testCC(10, 0, 1, f, false);
      testCC(10, 1, 11, f, false);
      testCC(10, 2, 56, f, false);
      testCC(10, 3, 176, f, false);
      testCC(10, 4, 386, f, false);
      testCC(10, 5, 638, f, false);
      testCC(10, 6, 848, f, false);
      testCC(10, 7, 968, f, false);
      testCC(10, 8, 1013, f, false);
      testCC(10, 9, 1023, f, false);
      testCC(10, 10, 1, f, false);
      testCC(10, 15, 1, f, false);
      Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
    }
  }

  @Test
  public void testAMKMiniCard() {
    final FormulaFactory f = new FormulaFactory();
    testCC(10, 0, 1, f, true);
    testCC(10, 1, 11, f, true);
    testCC(10, 2, 56, f, true);
    testCC(10, 3, 176, f, true);
    testCC(10, 4, 386, f, true);
    testCC(10, 5, 638, f, true);
    testCC(10, 6, 848, f, true);
    testCC(10, 7, 968, f, true);
    testCC(10, 8, 1013, f, true);
    testCC(10, 9, 1023, f, true);
    testCC(10, 10, 1024, f, true);
    testCC(10, 15, 1024, f, true);
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testLargeAMK() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs) {
      f.putConfiguration(config);
      testCC(150, 2, 1 + 150 + 11175, f, false);
      Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
    }
  }

  @Test
  public void testLargeAMKMiniCard() {
    final FormulaFactory f = new FormulaFactory();
    testCC(150, 2, 1 + 150 + 11175, f, true);
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  private void testCC(int numLits, int rhs, int expected, final FormulaFactory f, boolean miniCard) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final SATSolver solver = miniCard ? MiniSat.miniCard(f) : MiniSat.miniSat(f);
    solver.add(f.cc(CType.LE, rhs, problemLits));
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= rhs);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC1() {
    final FormulaFactory f = new FormulaFactory();
    final CCEncoder encoder = new CCEncoder(f);
    final int numLits = 100;
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    encoder.encode(f.cc(CType.LE, -1, problemLits));
  }

  @Test
  public void testToString() {
    FormulaFactory f = new FormulaFactory();
    Assert.assertEquals("TOTALIZER", configs[0].amkEncoder.toString());
    Assert.assertEquals("MODULAR_TOTALIZER", configs[1].amkEncoder.toString());
    Assert.assertEquals("CARDINALITY_NETWORK", configs[2].amkEncoder.toString());

    Assert.assertEquals("CCAMKTotalizer", new CCAMKTotalizer().toString());
    Assert.assertEquals("CCAMKModularTotalizer", new CCAMKModularTotalizer(f).toString());
    Assert.assertEquals("CCAMKCardinalityNetwork", new CCAMKCardinalityNetwork().toString());

    Assert.assertTrue(Arrays.asList(CCConfig.AMK_ENCODER.values()).contains(CCConfig.AMK_ENCODER.valueOf("TOTALIZER")));
  }

  @Test
  public void testCCSorting() {
    Assert.assertTrue(Arrays.asList(CCSorting.ImplicationDirection.values()).contains(CCSorting.ImplicationDirection.valueOf("INPUT_TO_OUTPUT")));
  }
}
