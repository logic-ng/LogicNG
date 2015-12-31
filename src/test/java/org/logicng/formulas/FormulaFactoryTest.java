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
//  Copyright 2015 Christoph Zengler                                     //
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
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;

/**
 * Test some basic formula factory functionality.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class FormulaFactoryTest {

  @Test
  public void testToString() {
    final FormulaFactory f = new FormulaFactory();
    f.literal("a");
    f.literal("b", false);
    f.and(f.literal("a"), f.literal("b", false));
    f.or(f.literal("a"), f.literal("b", false), f.literal("x"), f.implication(f.literal("a"), f.literal("x")));
    final String expected = "Positive Literals: 3\n" +
            "Negative Literals: 3\n" +
            "Negations:         1\n" +
            "Implications:      1\n" +
            "Equivalences:      0\n" +
            "Conjunctions (2):  1\n" +
            "Conjunctions (3):  0\n" +
            "Conjunctions (4):  0\n" +
            "Conjunctions (>4): 0\n" +
            "Disjunctions (2):  0\n" +
            "Disjunctions (3):  0\n" +
            "Disjunctions (4):  1\n" +
            "Disjunctions (>4): 0\n";
    Assert.assertEquals(expected, f.toString());
  }

  @Test
  public void testConfigurations() {
    final FormulaFactory f = new FormulaFactory();
    final Configuration configMaxSat = new MaxSATConfig.Builder().build();
    final Configuration configMiniSat = new MiniSatConfig.Builder().build();
    final Configuration configGlucose = new GlucoseConfig.Builder().build();
    f.putConfiguration(configMaxSat);
    f.putConfiguration(configMiniSat);
    f.putConfiguration(configGlucose);
    Assert.assertEquals(configMaxSat, f.configurationFor(ConfigurationType.MAXSAT));
    Assert.assertEquals(configMiniSat, f.configurationFor(ConfigurationType.MINISAT));
    Assert.assertEquals(configGlucose, f.configurationFor(ConfigurationType.GLUCOSE));
    Assert.assertNull(f.configurationFor(ConfigurationType.CLEANELING));
  }

  @Test
  public void testGeneratedLiterals() {
    final FormulaFactory f = new FormulaFactory();
    final Literal ccVar = f.newCCLiteral();
    final Literal cnfVar = f.newCNFLiteral();
    final Literal pbVar = f.newPBLiteral();
    final Literal var = f.literal("x");
    Assert.assertTrue(f.isGeneratedLiteral(ccVar));
    Assert.assertTrue(f.isGeneratedLiteral(cnfVar));
    Assert.assertTrue(f.isGeneratedLiteral(pbVar));
    Assert.assertFalse(f.isGeneratedLiteral(var));


  }

}
