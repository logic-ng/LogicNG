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

package org.logicng.solvers.sat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.collections.LNGIntVector;

import java.util.Arrays;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

/**
 * Some MiniSat specific unit tests.
 * @version 1.3
 * @since 1.0
 */
public class MiniSatTest {

  @Test
  public void testAnalyzeFinal() {
    final MiniSat2Solver solver = new MiniSat2Solver();
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.addClause(clause(1, 2, 3), null);
    solver.addClause(clause(-1, -2), null);
    solver.addClause(clause(-1, -3), null);
    solver.addClause(clause(-2, -3), null);
    Assert.assertEquals(TRUE, solver.solve(null));
    Assert.assertEquals(FALSE, solver.solve(null, clause(1, 2)));
  }

  @Test(expected = IllegalStateException.class)
  public void testInvalidSaveState() {
    final MiniSat2Solver solver = new MiniSat2Solver(new MiniSatConfig.Builder().incremental(false).build());
    solver.saveState();
  }

  @Test(expected = IllegalStateException.class)
  public void testInvalidLoadState() {
    final MiniSat2Solver solver = new MiniSat2Solver(new MiniSatConfig.Builder().incremental(false).build());
    solver.loadState(null);
  }

  @Test
  public void testConfig() {
    Assert.assertEquals("MINISAT", new MiniSatConfig.Builder().build().type().toString());
    Assert.assertTrue(Arrays.asList(MiniSatConfig.ClauseMinimization.values()).contains(MiniSatConfig.ClauseMinimization.valueOf("DEEP")));
  }

  private LNGIntVector clause(int... lits) {
    final LNGIntVector c = new LNGIntVector(lits.length);
    for (int l : lits)
      c.push(literal(l));
    return c;
  }

  private int literal(int l) {
    return l < 0 ? (-l * 2) ^ 1 : l * 2;
  }
}
