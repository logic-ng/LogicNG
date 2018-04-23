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
import org.logicng.formulas.FormulaFactory;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.testutils.PigeonHoleGenerator;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

/**
 * Tests for the incremental/decremental interface of the SAT solvers.
 * @version 1.3
 * @since 1.0
 */
public class IncDecTest {

  private final FormulaFactory f;
  private final MiniSat[] solvers;
  private final PigeonHoleGenerator pg;

  public IncDecTest() {
    this.f = new FormulaFactory();
    this.pg = new PigeonHoleGenerator(f);
    this.solvers = new MiniSat[2];
    this.solvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[1] = MiniSat.miniCard(f, new MiniSatConfig.Builder().incremental(true).build());
  }

  @Test
  public void testIncDec() {
    for (final MiniSat s : this.solvers) {
      s.add(f.variable("a"));
      final SolverState state1 = s.saveState();
      if (s.underlyingSolver() instanceof MiniCard)
        Assert.assertEquals("SolverState{id=0, state=[1, 1, 0, 0, 1]}", state1.toString());
      else
        Assert.assertEquals("SolverState{id=0, state=[1, 1, 0, 0, 1, 0, 0]}", state1.toString());
      Assert.assertEquals(TRUE, s.sat());
      s.add(pg.generate(5));
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state1);
      Assert.assertEquals(TRUE, s.sat());
      s.add(f.literal("a", false));
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state1);
      Assert.assertEquals(TRUE, s.sat());
      s.add(pg.generate(5));
      final SolverState state2 = s.saveState();
      if (s.underlyingSolver() instanceof MiniCard)
        Assert.assertEquals("SolverState{id=1, state=[1, 31, 81, 0, 1]}", state2.toString());
      else
        Assert.assertEquals("SolverState{id=1, state=[1, 31, 81, 0, 1, 0, 0]}", state2.toString());
      s.add(pg.generate(4));
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state2);
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state1);
      Assert.assertEquals(TRUE, s.sat());
    }
  }

  @Test
  public void testIncDecDeep() {
    for (final SATSolver s : this.solvers) {
      s.add(f.variable("a"));
      final SolverState state1 = s.saveState();
      s.add(f.variable("b"));
      Assert.assertEquals(TRUE, s.sat());
      final SolverState state2 = s.saveState();
      s.add(f.literal("a", false));
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state1);
      try {
        s.loadState(state2);
        Assert.fail("Returned to invalid state.");
      } catch (IllegalArgumentException e) {
        // fine
      }
      s.add(f.literal("b", false));
      Assert.assertEquals(TRUE, s.sat());
      final SolverState state3 = s.saveState();
      s.add(f.literal("a", false));
      Assert.assertEquals(FALSE, s.sat());
      s.loadState(state3);
      s.add(f.variable("c"));
      final SolverState state4 = s.saveState();
      final SolverState state5 = s.saveState();
      s.loadState(state4);
      try {
        s.loadState(state5);
        Assert.fail("Returned to invalid state.");
      } catch (IllegalArgumentException e) {
        // fine
      }
      Assert.assertEquals(TRUE, s.sat());
      s.loadState(state1);
      Assert.assertEquals(TRUE, s.sat());
      try {
        s.loadState(state3);
        Assert.fail("Returned to invalid state.");
      } catch (IllegalArgumentException e) {
        // fine
      }
    }
  }
}
