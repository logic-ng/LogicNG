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
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.List;

import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BEST;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BIMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BINARY;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.COMMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.LADDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.NESTED;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PRODUCT;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PURE;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.FIXED;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.HALF;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.SQRT;

/**
 * Unit tests for the at-most-one configs.
 * @version 1.1
 * @since 1.0
 */
public class CCAMOTest {

  private CCConfig[] configs;

  public CCAMOTest() {
    configs = new CCConfig[14];
    configs[0] = new CCConfig.Builder().amoEncoding(PURE).build();
    configs[1] = new CCConfig.Builder().amoEncoding(LADDER).build();
    configs[2] = new CCConfig.Builder().amoEncoding(PRODUCT).build();
    configs[3] = new CCConfig.Builder().amoEncoding(BINARY).build();
    configs[4] = new CCConfig.Builder().amoEncoding(NESTED).build();
    configs[5] = new CCConfig.Builder().amoEncoding(COMMANDER).commanderGroupSize(3).build();
    configs[6] = new CCConfig.Builder().amoEncoding(COMMANDER).commanderGroupSize(7).build();
    configs[7] = new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(FIXED).build();
    configs[8] = new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(HALF).build();
    configs[9] = new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(SQRT).build();
    configs[10] = new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(FIXED).bimanderFixedGroupSize(2).build();
    configs[11] = new CCConfig.Builder().amoEncoding(NESTED).nestingGroupSize(5).build();
    configs[12] = new CCConfig.Builder().amoEncoding(PRODUCT).productRecursiveBound(10).build();
    configs[13] = new CCConfig.Builder().amoEncoding(BEST).build();
  }

  @Test
  public void testAMO0() {
    final FormulaFactory f = new FormulaFactory();
    final PBConstraint cc = f.amo();
    for (final CCConfig config : this.configs)
      Assert.assertTrue(new CCEncoder(f, config).encode(cc).empty());
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testAMO1() {
    final FormulaFactory f = new FormulaFactory();
    final PBConstraint cc = f.amo(f.variable("v0"));
    for (final CCConfig config : this.configs)
      Assert.assertTrue(new CCEncoder(f, config).encode(cc).empty());
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testAMOK() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs)
      if (config != null) {
        f.putConfiguration(config);
        testAMO(2, f, false);
        testAMO(10, f, false);
        testAMO(100, f, false);
        testAMO(250, f, false);
        testAMO(500, f, false);
        Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
      }
  }

  @Test
  public void testAMOKMiniCard() {
    final FormulaFactory f = new FormulaFactory();
    testAMO(2, f, true);
    testAMO(10, f, true);
    testAMO(100, f, true);
    testAMO(250, f, true);
    testAMO(500, f, true);
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("PURE", configs[0].amoEncoder.toString());
    Assert.assertEquals("LADDER", configs[1].amoEncoder.toString());
    Assert.assertEquals("PRODUCT", configs[2].amoEncoder.toString());
    Assert.assertEquals("BINARY", configs[3].amoEncoder.toString());
    Assert.assertEquals("NESTED", configs[4].amoEncoder.toString());
    Assert.assertEquals("COMMANDER", configs[5].amoEncoder.toString());
    Assert.assertEquals("BIMANDER", configs[7].amoEncoder.toString());
    Assert.assertEquals("BEST", configs[13].amoEncoder.toString());

    Assert.assertEquals("CCAMOPure", new CCAMOPure().toString());
    Assert.assertEquals("CCAMOLadder", new CCAMOLadder().toString());
    Assert.assertEquals("CCAMOProduct", new CCAMOProduct(2).toString());
    Assert.assertEquals("CCAMOBinary", new CCAMOBinary().toString());
    Assert.assertEquals("CCAMONested", new CCAMONested(2).toString());
    Assert.assertEquals("CCAMOCommander", new CCAMOCommander(2).toString());
    Assert.assertEquals("CCAMOBimander", new CCAMOBimander(2).toString());

    Assert.assertTrue(Arrays.asList(CCConfig.AMO_ENCODER.values()).contains(CCConfig.AMO_ENCODER.valueOf("LADDER")));
    Assert.assertTrue(Arrays.asList(CCConfig.BIMANDER_GROUP_SIZE.values()).contains(CCConfig.BIMANDER_GROUP_SIZE.valueOf("SQRT")));
  }

  private void testAMO(int numLits, final FormulaFactory f, boolean miniCard) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final SATSolver solver = miniCard ? MiniSat.miniCard(f) : MiniSat.miniSat(f);
    solver.add(f.amo(problemLits));
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(numLits + 1, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= 1);
  }
}
