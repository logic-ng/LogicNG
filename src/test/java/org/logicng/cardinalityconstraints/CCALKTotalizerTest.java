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

package org.logicng.cardinalityconstraints;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.LinkedList;
import java.util.List;

/**
 * Unit tests for the {@link CCALKTotalizer}.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class CCALKTotalizerTest {

  private static final FormulaFactory f = new FormulaFactory();

  @Test
  public void testCC0() {
    final CCALKTotalizer totalizer = new CCALKTotalizer(f);
    final ImmutableFormulaList clauses = totalizer.build(new LinkedList<Variable>(), 0);
    Assert.assertTrue(clauses.empty());
  }

  @Test
  public void testCCALL() {
    final CCAtLeastK totalizer = new CCALKTotalizer(f);
    final int numLits = 100;
    final List<Variable> lits = new LinkedList<>();
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++) {
      final Variable var = f.variable("v" + i);
      lits.add(var);
      problemLits[i] = var;
    }
    final ImmutableFormulaList clauses = totalizer.build(lits, 100);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(1, models.size());
    Assert.assertEquals(100, models.get(0).positiveLiterals().size());
  }

  @Test
  public void testCCs() {
    final CCAtLeastK totalizer = new CCALKTotalizer(f);
    testCC(10, 1, 1023, totalizer);
    testCC(10, 2, 1013, totalizer);
    testCC(10, 3, 968, totalizer);
    testCC(10, 4, 848, totalizer);
    testCC(10, 5, 638, totalizer);
    testCC(10, 6, 386, totalizer);
    testCC(10, 7, 176, totalizer);
    testCC(10, 8, 56, totalizer);
    testCC(10, 9, 11, totalizer);
    testCC(10, 10, 1, totalizer);
    testCC(10, 12, 0, totalizer);
  }

  private void testCC(int numLits, int rhs, int expected, final CCAtLeastK totalizer) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final ImmutableFormulaList clauses = totalizer.build(problemLits, rhs);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    if (expected != 0)
      Assert.assertEquals(Tristate.TRUE, solver.sat());
    else
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() >= rhs);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC1() {
    final CCAtLeastK totalizer = new CCALKTotalizer(f);
    final int numLits = 100;
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    totalizer.build(problemLits, -1);
  }
}
