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
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

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
 * Unit tests for the exactly-one encoders.
 * @version 1.1
 * @since 1.0
 */
public class CCEXOTest {

  private CCConfig[] configs;

  public CCEXOTest() {
    configs = new CCConfig[11];
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
    configs[10] = new CCConfig.Builder().amoEncoding(BEST).build();
  }

  @Test
  public void testEXO0() {
    final FormulaFactory f = new FormulaFactory();
    final PBConstraint cc = f.exo();
    for (final CCConfig config : this.configs)
      Assert.assertEquals(f.falsum(), new CCEncoder(f, config).encode(cc).formula(f).cnf());
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testEXO1() {
    final FormulaFactory f = new FormulaFactory();
    final PBConstraint cc = f.exo(f.variable("v0"));
    final ImmutableFormulaList expected = new ImmutableFormulaList(FType.AND, f.variable("v0"));
    for (final CCConfig config : this.configs)
      Assert.assertEquals(expected, new CCEncoder(f, config).encode(cc));
    Assert.assertTrue(f.newCCVariable().name().endsWith("_0"));
  }

  @Test
  public void testEXOK() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs)
      if (config != null) {
        f.putConfiguration(config);
        testEXO(2, f);
        testEXO(10, f);
        testEXO(100, f);
        testEXO(250, f);
        testEXO(500, f);
        Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
      }
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
  }

  private void testEXO(int numLits, final FormulaFactory f) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(f.exo(problemLits));
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(numLits, models.size());
    for (final Assignment model : models)
      Assert.assertEquals(1, model.positiveLiterals().size());
  }
}
