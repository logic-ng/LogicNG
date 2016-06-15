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
//  Copyright 2015-2016 Christoph Zengler                                //
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
import org.junit.Ignore;
import org.junit.Test;
import org.logicng.configurations.ConfigurationType;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

/**
 * Performance tests for cardinality constraints.
 * @version 1.1
 * @since 1.1
 */
public class CCPerformanceTest {

  private CCConfig[] configs;

  public CCPerformanceTest() {
    configs = new CCConfig[3];
    configs[0] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).build();
    configs[1] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).build();
    configs[2] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).build();
  }

  @Ignore
  @Test
  public void testAMKPerformance() {
    final FormulaFactory f = new FormulaFactory();
    int counter = 0;
    for (final CCConfig config : this.configs) {
      f.putConfiguration(config);
      buildAMK(10_000, f);
      Assert.assertTrue(f.newCCVariable().name().endsWith("_" + counter++));
    }
  }

  private void buildAMK(int numLits, final FormulaFactory f) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    for (int i = 10; i < 100; i = i + 10) {
      System.out.println(((CCConfig) f.configurationFor(ConfigurationType.CC_ENCODER)).amkEncoder + ": " + i);
      final PBConstraint pbc = f.cc(CType.LE, i, problemLits);
      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(pbc);
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      final Assignment model = solver.model();
      Assert.assertTrue(pbc.evaluate(model));
    }
  }
}
