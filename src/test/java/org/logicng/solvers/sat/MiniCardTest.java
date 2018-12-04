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

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

/**
 * Some MiniCard specific unit tests.
 * @version 1.3
 * @since 1.0
 */
public class MiniCardTest {

  @Test
  public void testAnalyzeFinalWithClauses() {
    final MiniCard solver = new MiniCard();
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

  @Test
  public void testAnalyzeFinalWithAtMost() {
    final MiniCard solver = new MiniCard();
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.addClause(clause(1, 2, 3), null);
    solver.addAtMost(clause(1, 2, 3), 2);
    Assert.assertEquals(TRUE, solver.solve(null));
    Assert.assertEquals(TRUE, solver.solve(null, clause(1, 2)));
    Assert.assertEquals(FALSE, solver.solve(null, clause(1, 2, 3)));
  }

  @Test(expected = IllegalStateException.class)
  public void testInvalidSaveState() {
    final MiniCard solver = new MiniCard(new MiniSatConfig.Builder().incremental(false).build());
    solver.saveState();
  }

  @Test(expected = IllegalStateException.class)
  public void testInvalidLoadState() {
    final MiniCard solver = new MiniCard(new MiniSatConfig.Builder().incremental(false).build());
    solver.loadState(null);
  }

  @Test
  public void testIncDecWithAtMost() {
    final MiniCard solver = new MiniCard();
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.addClause(clause(1, 2, 3), null);
    solver.addAtMost(clause(1, 2, 3), 2);
    Assert.assertEquals(TRUE, solver.solve(null));
    final int[] original = solver.saveState();
    solver.addClause(clause(1), null);
    Assert.assertEquals(TRUE, solver.solve(null));
    solver.addClause(clause(2), null);
    Assert.assertEquals(TRUE, solver.solve(null));
    solver.addClause(clause(3), null);
    Assert.assertEquals(FALSE, solver.solve(null));
    solver.loadState(original);
    Assert.assertEquals(TRUE, solver.solve(null));
    solver.addAtMost(clause(1, 2, 3), 1);
    Assert.assertEquals(TRUE, solver.solve(null));
    solver.addClause(clause(2), null);
    Assert.assertEquals(TRUE, solver.solve(null));
    solver.addClause(clause(3), null);
    Assert.assertEquals(FALSE, solver.solve(null));
    solver.loadState(original);
    Assert.assertEquals(TRUE, solver.solve(null));
  }

  @Test
  public void testToString() {
    final MiniCard solver = new MiniCard();
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.newVar(true, true);
    solver.addClause(clause(1, 2, 3), null);
    solver.addAtMost(clause(1, 2, 3), 2);

    String expected = String.format("ok            true%n" +
            "qhead         0%n" +
            "#clauses      2%n" +
            "#learnts      0%n" +
            "#watches      8%n" +
            "#vars         4%n" +
            "#orderheap    4%n" +
            "#trail        0%n" +
            "#trailLim     0%n" +
            "model         []%n" +
            "conflict      []%n" +
            "assumptions   []%n" +
            "#seen         4%n" +
            "#stack        0%n" +
            "#toclear      0%n" +
            "claInc        1.0%n" +
            "simpDBAssigns -1%n" +
            "simpDBProps   0%n" +
            "#clause lits  6%n" +
            "#learnts lits 0%n");
    Assert.assertEquals(expected, solver.toString());
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
