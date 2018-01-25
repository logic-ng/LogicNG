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

package org.logicng.solvers.maxsat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.maxsat.algorithms.IncWBO;
import org.logicng.solvers.maxsat.algorithms.LinearSU;
import org.logicng.solvers.maxsat.algorithms.LinearUS;
import org.logicng.solvers.maxsat.algorithms.MSU3;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.maxsat.algorithms.WBO;
import org.logicng.solvers.maxsat.algorithms.WMSU3;

import java.util.Arrays;

/**
 * Unit tests for the package {@link org.logicng.solvers.maxsat}.
 * @version 1.1
 * @since 1.1
 */
public class MaxSATClassTest {

  @Test
  public void testMaxSATConfig() {
    Assert.assertTrue(Arrays.asList(MaxSATConfig.SolverType.values()).contains(MaxSATConfig.SolverType.valueOf("GLUCOSE")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.IncrementalStrategy.values()).contains(MaxSATConfig.IncrementalStrategy.valueOf("ITERATIVE")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.AMOEncoding.values()).contains(MaxSATConfig.AMOEncoding.valueOf("LADDER")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.PBEncoding.values()).contains(MaxSATConfig.PBEncoding.valueOf("SWC")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.CardinalityEncoding.values()).contains(MaxSATConfig.CardinalityEncoding.valueOf("TOTALIZER")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.WeightStrategy.values()).contains(MaxSATConfig.WeightStrategy.valueOf("DIVERSIFY")));
    Assert.assertTrue(Arrays.asList(MaxSATConfig.Verbosity.values()).contains(MaxSATConfig.Verbosity.valueOf("SOME")));
  }

  @Test
  public void testMaxSATenum() {
    Assert.assertTrue(Arrays.asList(MaxSAT.ProblemType.values()).contains(MaxSAT.ProblemType.valueOf("UNWEIGHTED")));
    Assert.assertTrue(Arrays.asList(MaxSAT.MaxSATResult.values()).contains(MaxSAT.MaxSATResult.valueOf("OPTIMUM")));
  }

  @Test
  public void testMaxSATtoString() {
    MaxSAT wmsu3 = new WMSU3();
    Assert.assertEquals("WMSU3", wmsu3.toString());
    MaxSAT wbo = new WBO();
    Assert.assertEquals("WBO", wbo.toString());
    MaxSAT incWbo = new IncWBO();
    Assert.assertEquals("IncWBO", incWbo.toString());
    MaxSAT msu3 = new MSU3();
    Assert.assertEquals("MSU3", msu3.toString());
    MaxSAT linearSu = new LinearSU();
    Assert.assertEquals("LinearSU", linearSu.toString());
    MaxSAT linearUs = new LinearUS();
    Assert.assertEquals("LinearUS", linearUs.toString());
  }
}
