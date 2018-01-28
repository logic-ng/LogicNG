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
import org.logicng.datastructures.EncodingResult;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the exactly-k encoders.
 * @version 1.1
 * @since 1.1
 */
public class CCEXKTest {

  private CCConfig[] configs;

  public CCEXKTest() {
    configs = new CCConfig[2];
    configs[0] = new CCConfig.Builder().exkEncoding(CCConfig.EXK_ENCODER.TOTALIZER).build();
    configs[1] = new CCConfig.Builder().exkEncoding(CCConfig.EXK_ENCODER.CARDINALITY_NETWORK).build();
  }

  @Test
  public void testEXK() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs) {
      f.putConfiguration(config);
      testCC(10, 1, 10, f);
      testCC(10, 2, 45, f);
      testCC(10, 3, 120, f);
      testCC(10, 4, 210, f);
      testCC(10, 5, 252, f);
      testCC(10, 6, 210, f);
      testCC(10, 7, 120, f);
      testCC(10, 8, 45, f);
      testCC(10, 9, 10, f);
      testCC(10, 10, 1, f);
      testCC(10, 12, 0, f);
      Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
    }
  }

  private void testCC(int numLits, int rhs, int expected, final FormulaFactory f) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(f.cc(CType.EQ, rhs, problemLits));
    if (expected != 0)
      Assert.assertEquals(Tristate.TRUE, solver.sat());
    else
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() == rhs);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC1() {
    final FormulaFactory f = new FormulaFactory();
    final CCEncoder encoder = new CCEncoder(f);
    final int numLits = 100;
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    encoder.encode(f.cc(CType.EQ, -1, problemLits));
  }

  @Test
  public void testCCEXKTotalizer() {
    FormulaFactory f = new FormulaFactory();
    CCEXKTotalizer totalizer = new CCEXKTotalizer();
    totalizer.build(EncodingResult.resultForFormula(f), new Variable[]{f.variable("A"), f.variable("B"), f.variable("C")}, 2);
    Assert.assertNull(totalizer.incrementalData());
    Assert.assertEquals("CCEXKTotalizer", totalizer.toString());

    CCEXKCardinalityNetwork cNetwork = new CCEXKCardinalityNetwork();
    cNetwork.build(EncodingResult.resultForFormula(f), new Variable[]{f.variable("A"), f.variable("B"), f.variable("C")}, 2);
    Assert.assertNull(cNetwork.incrementalData());
    Assert.assertEquals("CCEXKCardinalityNetwork", cNetwork.toString());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("TOTALIZER", configs[0].exkEncoder.toString());
    Assert.assertEquals("CARDINALITY_NETWORK", configs[1].exkEncoder.toString());

    Assert.assertTrue(Arrays.asList(CCConfig.EXK_ENCODER.values()).contains(CCConfig.EXK_ENCODER.valueOf("CARDINALITY_NETWORK")));
  }
}
