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
import org.junit.Ignore;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.CleaneLing;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

/**
 * Tests for incremental cardinality constraints generated on the solver and {@link CCIncrementalData}.
 * @version 1.3
 * @since 1.1
 */
public class CCIncrementalSolverTest {

  private final FormulaFactory f = new FormulaFactory();
  private final SATSolver[] solvers;
  private CCConfig[] configs;

  public CCIncrementalSolverTest() {
    configs = new CCConfig[3];
    configs[0] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build();
    configs[1] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build();
    configs[2] = new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build();
    this.solvers = new SATSolver[5];
    solvers[0] = MiniSat.miniSat(f);
    solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(false).build());
    solvers[2] = MiniSat.miniCard(f);
    solvers[3] = MiniSat.glucose(f);
    solvers[4] = CleaneLing.minimalistic(f);
  }

  @Test
  public void testSimpleIncrementalAMK() {
    for (final CCConfig config : this.configs) {
      f.putConfiguration(configs[2]);
      int numLits = 10;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(f.cc(CType.GE, 4, vars)); // >= 4
      solver.add(f.cc(CType.LE, 7, vars)); // <= 7

      f.putConfiguration(config);

      final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.LE, 9, vars));
      Assert.assertEquals(Tristate.TRUE, solver.sat()); // <= 9
      incData.newUpperBoundForSolver(8); // <= 8
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newUpperBoundForSolver(7); // <= 7
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newUpperBoundForSolver(6); // <= 6
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newUpperBoundForSolver(5); // <= 5
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newUpperBoundForSolver(4); // <= 4
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      final SolverState state = solver.saveState();
      incData.newUpperBoundForSolver(3); // <= 3
      Assert.assertEquals(Tristate.FALSE, solver.sat());
      solver.loadState(state);
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      incData.newUpperBoundForSolver(2); // <= 2
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    }
  }

  @Test
  public void testSimpleIncrementalALK() {
    for (final CCConfig config : this.configs) {
      f.putConfiguration(configs[2]);
      int numLits = 10;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      final SATSolver solver = this.solvers[2];
      solver.reset();
      solver.add(f.cc(CType.GE, 4, vars)); // >= 4
      solver.add(f.cc(CType.LE, 7, vars)); // <= 7

      f.putConfiguration(config);

      final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.GE, 2, vars));
      Assert.assertEquals(Tristate.TRUE, solver.sat()); // >=2
      incData.newLowerBoundForSolver(3); // >= 3
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newLowerBoundForSolver(4); // >= 4
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newLowerBoundForSolver(5); // >= 5
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newLowerBoundForSolver(6); // >= 6
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      incData.newLowerBoundForSolver(7); // >= 7
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      final SolverState state = solver.saveState();
      incData.newLowerBoundForSolver(8); // >= 8
      Assert.assertEquals(Tristate.FALSE, solver.sat());
      solver.loadState(state);
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      incData.newLowerBoundForSolver(9); // <= 9
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    }
  }

  @Test
  public void testLargeTotalizerUpperBoundAMK() {
    f.putConfiguration(configs[2]);
    int numLits = 100;
    int currentBound = numLits - 1;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final SATSolver solver = this.solvers[3];
    solver.reset();
    solver.add(f.cc(CType.GE, 42, vars)); // >= 42
    f.putConfiguration(configs[0]);
    final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.LE, currentBound, vars));
    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      incData.newUpperBoundForSolver(--currentBound); // <= currentBound - 1
    Assert.assertEquals(41, currentBound);
  }

  @Test
  public void testLargeTotalizerLowerBoundALK() {
    f.putConfiguration(configs[2]);
    int numLits = 100;
    int currentBound = 2;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final SATSolver solver = this.solvers[0];
    solver.reset();
    solver.add(f.cc(CType.LE, 87, vars));
    f.putConfiguration(configs[0]);
    final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.GE, currentBound, vars));
    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      incData.newLowerBoundForSolver(++currentBound); // <= currentBound + 1
    Assert.assertEquals(88, currentBound);
  }

  @Test
  public void testLargeModularTotalizerAMK() {
    for (final SATSolver solver : this.solvers) {
      if (solver != null) {
        f.putConfiguration(configs[2]);
        int numLits = 100;
        int currentBound = numLits - 1;
        Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++)
          vars[i] = f.variable("v" + i);
        solver.reset();
        solver.add(f.cc(CType.GE, 42, vars)); // >= 42
        final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.LE, currentBound, vars));
        // search the lower bound
        while (solver.sat() == Tristate.TRUE)
          incData.newUpperBoundForSolver(--currentBound); // <= currentBound - 1
        Assert.assertEquals(41, currentBound);
      }
    }
  }

  @Ignore
  @Test
  public void testVeryLargeModularTotalizerAMK() {
    f.putConfiguration(configs[2]);
    int numLits = 300;
    int currentBound = numLits - 1;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final SATSolver solver = this.solvers[3];
    solver.reset();
    solver.add(f.cc(CType.GE, 234, vars));
    final CCIncrementalData incData = solver.addIncrementalCC(f.cc(CType.LE, currentBound, vars));
    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      incData.newUpperBoundForSolver(--currentBound);
    Assert.assertEquals(233, currentBound);
  }
}
