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
 * Unit tests for the {@link CCAMKModularTotalizer}.
 * @version 1.0
 * @since 1.0
 */
public class CCAMKModularTotalizerTest {

  private static final FormulaFactory f = new FormulaFactory();

  @Test
  public void testCC0() {
    final CCAMKModularTotalizer totalizer = new CCAMKModularTotalizer(f);
    final int numLits = 100;
    final List<Variable> lits = new LinkedList<>();
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++) {
      final Variable lit = f.variable("v" + i);
      lits.add(lit);
      problemLits[i] = lit;
    }
    final ImmutableFormulaList clauses = totalizer.build(lits, 0);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(1, models.size());
    Assert.assertEquals(100, models.get(0).negativeVariables().size());
  }

  @Test
  public void testCC1() {
    final CCAMKModularTotalizer totalizer = new CCAMKModularTotalizer(f);
    final int numLits = 100;
    final int rhs = 1;
    final List<Variable> lits = new LinkedList<>();
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++) {
      final Variable lit = f.variable("v" + i);
      lits.add(lit);
      problemLits[i] = lit;
    }
    final ImmutableFormulaList clauses = totalizer.build(lits, rhs);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(numLits + 1, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= rhs);
  }

  @Test
  public void testCCs() {
    testCC(10, 0, 1);
    testCC(10, 1, 11);
    testCC(10, 2, 56);
    testCC(10, 3, 176);
    testCC(10, 4, 386);
    testCC(10, 5, 638);
    testCC(10, 6, 848);
    testCC(10, 7, 968);
    testCC(10, 8, 1013);
    testCC(10, 9, 1023);
    testCC(10, 10, 1);
    testCC(10, 15, 1);
  }

  private void testCC(int numLits, int rhs, int expected) {
    final CCAMKModularTotalizer totalizer = new CCAMKModularTotalizer(f);
    final List<Variable> lits = new LinkedList<>();
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++) {
      final Variable lit = f.variable("v" + i);
      lits.add(lit);
      problemLits[i] = lit;
    }
    final ImmutableFormulaList clauses = totalizer.build(lits, rhs);
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= rhs);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC2() {
    final CCAtMostK totalizer = new CCAMKModularTotalizer(f);
    final int numLits = 100;
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    totalizer.build(problemLits, -1);
  }
}
