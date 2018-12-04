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
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.CleaneLing;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.Pair;

/**
 * Tests for incremental cardinality constraints generated as formulas and {@link CCIncrementalData}.
 * @version 1.3
 * @since 1.1
 */
public class CCIncrementalFormulaTest {

  private final FormulaFactory f = new FormulaFactory();
  private final SATSolver[] solvers;
  private CCEncoder[] encoders;

  public CCIncrementalFormulaTest() {
    encoders = new CCEncoder[3];
    encoders[0] = new CCEncoder(f, new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build());
    encoders[1] = new CCEncoder(f, new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build());
    encoders[2] = new CCEncoder(f, new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build());
    this.solvers = new SATSolver[5];
    solvers[0] = MiniSat.miniSat(f);
    solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(false).build());
    solvers[2] = MiniSat.miniCard(f);
    solvers[3] = MiniSat.glucose(f);
    solvers[4] = CleaneLing.minimalistic(f);
  }

  @Test
  public void testSimpleIncrementalAMK() {
    for (final CCEncoder encoder : this.encoders) {
      CCEncoder initialEncoder = new CCEncoder(f);
      int numLits = 10;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.LE, 9, vars));
      final CCIncrementalData incData = cc.second();

      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(initialEncoder.encode(f.cc(CType.GE, 4, vars))); // >= 4
      solver.add(initialEncoder.encode(f.cc(CType.LE, 7, vars))); // <= 7

      solver.add(cc.first());
      Assert.assertEquals(Tristate.TRUE, solver.sat()); // <= 9
      solver.add(incData.newUpperBound(8)); // <= 8
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      Assert.assertEquals(8, incData.currentRHS());
      solver.add(incData.newUpperBound(7)); // <= 7
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newUpperBound(6)); // <= 6
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newUpperBound(5)); // <= 5
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newUpperBound(4)); // <= 4
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      final SolverState state = solver.saveState();
      solver.add(incData.newUpperBound(3)); // <= 3
      Assert.assertEquals(Tristate.FALSE, solver.sat());
      solver.loadState(state);
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      solver.add(incData.newUpperBound(2)); // <= 2
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    }
  }

  @Test
  public void testIncrementalData() {
    for (final CCEncoder encoder : this.encoders) {
      int numLits = 10;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.LT, 10, vars));
      CCIncrementalData incData = cc.second();
      Assert.assertTrue(incData.toString().contains("currentRHS=9"));

      cc = encoder.encodeIncremental(f.cc(CType.GT, 1, vars));
      incData = cc.second();
      Assert.assertTrue(incData.toString().contains("currentRHS=2"));

      cc = encoder.encodeIncremental(f.cc(CType.LT, 1, vars));
      incData = cc.second();
      Assert.assertNull(incData);
      Assert.assertTrue(cc.first().containsFormula(vars[0].negate()));

      cc = encoder.encodeIncremental(f.cc(CType.LE, numLits + 1, vars));
      incData = cc.second();
      Assert.assertNull(incData);

      cc = encoder.encodeIncremental(f.cc(CType.GE, numLits + 1, vars));
      incData = cc.second();
      Assert.assertNull(incData);

      cc = encoder.encodeIncremental(f.cc(CType.GE, numLits, vars));
      incData = cc.second();
      Assert.assertNull(incData);

      cc = encoder.encodeIncremental(f.cc(CType.GE, 0, vars));
      incData = cc.second();
      Assert.assertNull(incData);

      cc = encoder.encodeIncremental(f.cc(CType.GE, 1, vars));
      incData = cc.second();
      Assert.assertNull(incData);
    }
  }

  @Test
  public void testSimpleIncrementalALK() {
    for (final CCEncoder encoder : this.encoders) {
      CCEncoder initialEncoder = new CCEncoder(f);
      int numLits = 10;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.GE, 2, vars));
      final CCIncrementalData incData = cc.second();

      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(initialEncoder.encode(f.cc(CType.GE, 4, vars))); // >= 4
      solver.add(initialEncoder.encode(f.cc(CType.LE, 7, vars))); // <= 7

      solver.add(cc.first());
      Assert.assertEquals(Tristate.TRUE, solver.sat()); // >=2
      solver.add(incData.newLowerBound(3)); // >= 3
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newLowerBound(4)); // >= 4
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newLowerBound(5)); // >= 5
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newLowerBound(6)); // >= 6
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newLowerBound(7)); // >= 7
      Assert.assertEquals(Tristate.TRUE, solver.sat());

      final SolverState state = solver.saveState();
      solver.add(incData.newLowerBound(8)); // >= 8
      Assert.assertEquals(Tristate.FALSE, solver.sat());
      solver.loadState(state);
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      solver.add(incData.newLowerBound(9)); // <= 9
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    }
  }

  @Test
  public void testLargeTotalizerUpperBoundAMK() {
    final CCEncoder encoder = this.encoders[0];
    final CCEncoder initivalEncoder = new CCEncoder(f);
    int numLits = 100;
    int currentBound = numLits - 1;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.LE, currentBound, vars));
    final CCIncrementalData incData = cc.second();

    final SATSolver solver = this.solvers[3];
    solver.reset();
    solver.add(initivalEncoder.encode(f.cc(CType.GE, 42, vars))); // >= 42
    solver.add(cc.first());

    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      solver.add(incData.newUpperBound(--currentBound)); // <= currentBound - 1
    Assert.assertEquals(41, currentBound);
  }

  @Test
  public void testLargeTotalizerLowerBoundALK() {
    final CCEncoder encoder = this.encoders[0];
    final CCEncoder initivalEncoder = new CCEncoder(f);
    int numLits = 100;
    int currentBound = 2;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.GE, currentBound, vars));
    final CCIncrementalData incData = cc.second();

    final SATSolver solver = this.solvers[3];
    solver.reset();
    solver.add(initivalEncoder.encode(f.cc(CType.LE, 87, vars))); // <= 42
    solver.add(cc.first());

    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      solver.add(incData.newLowerBound(++currentBound)); // <= currentBound + 1
    Assert.assertEquals(88, currentBound);
  }

  @Test
  public void testLargeModularTotalizerAMK() {
    for (final SATSolver solver : this.solvers) {
      final CCEncoder encoder = this.encoders[2];
      final CCEncoder initialEncoder = new CCEncoder(f);
      int numLits = 100;
      int currentBound = numLits - 1;
      Variable[] vars = new Variable[numLits];
      for (int i = 0; i < numLits; i++)
        vars[i] = f.variable("v" + i);
      final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.LE, currentBound, vars));
      final CCIncrementalData incData = cc.second();

      solver.reset();
      solver.add(initialEncoder.encode(f.cc(CType.GE, 42, vars))); // >= 42
      solver.add(cc.first());

      // search the lower bound
      while (solver.sat() == Tristate.TRUE)
        solver.add(incData.newUpperBound(--currentBound)); // <= currentBound - 1
      Assert.assertEquals(41, currentBound);
    }
  }

  @Test
  public void testToString() {
    String expected = String.format("CCConfig{%n" +
            "amoEncoder=BEST%n" +
            "amkEncoder=TOTALIZER%n" +
            "alkEncoder=TOTALIZER%n" +
            "exkEncoder=BEST%n" +
            "bimanderGroupSize=SQRT%n" +
            "bimanderFixedGroupSize=3%n" +
            "nestingGroupSize=4%n" +
            "productRecursiveBound=20%n" +
            "commanderGroupSize=3%n" +
            "}%n");
    Assert.assertEquals(expected, encoders[0].config().toString());
    Assert.assertEquals(expected, encoders[0].toString());
  }

  @Ignore
  @Test
  public void testVeryLargeModularTotalizerAMK() {
    final CCEncoder encoder = this.encoders[2];
    final CCEncoder initivalEncoder = new CCEncoder(f);
    int numLits = 300;
    int currentBound = numLits - 1;
    Variable[] vars = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      vars[i] = f.variable("v" + i);
    final Pair<ImmutableFormulaList, CCIncrementalData> cc = encoder.encodeIncremental(f.cc(CType.LE, currentBound, vars));
    final CCIncrementalData incData = cc.second();

    final SATSolver solver = this.solvers[3];
    solver.reset();
    solver.add(initivalEncoder.encode(f.cc(CType.GE, 234, vars)));
    solver.add(cc.first());

    // search the lower bound
    while (solver.sat() == Tristate.TRUE)
      solver.add(incData.newUpperBound(--currentBound));
    Assert.assertEquals(233, currentBound);
  }
}
