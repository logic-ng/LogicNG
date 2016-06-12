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

  private CCEncoder[] amkEncoders;

  public CCPerformanceTest() {
    amkEncoders = new CCEncoder[3];
    amkEncoders[0] = new CCEncoder(new FormulaFactory(), new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).build());
    amkEncoders[1] = new CCEncoder(new FormulaFactory(), new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).build());
    amkEncoders[2] = new CCEncoder(new FormulaFactory(), new CCConfig.Builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).build());
  }

  @Test
  public void testAMKPerformance() {
    for (final CCEncoder encoder : this.amkEncoders) {
      buildAMK(10_000, encoder);
    }
  }

  private void buildAMK(int numLits, final CCEncoder encoder) {
    final FormulaFactory f = new FormulaFactory();
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);

    for (int i = 10; i < 100; i = i + 10) {
      f.clear();
      final PBConstraint pbc = f.cc(CType.LE, i, problemLits);

      final long time1 = System.currentTimeMillis();
      final ImmutableFormulaList clauses = encoder.encode(pbc);
      final long time2 = System.currentTimeMillis();
      final SATSolver solver = MiniSat.miniSat(f);
      final long time3 = System.currentTimeMillis();
      solver.add(clauses);
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      final long time4 = System.currentTimeMillis();

      final int numVars = clauses.variables().size() - numLits;
      final int numClauses = clauses.size();
      final long encodingTime = time2 - time1;
      final long solvingTime = time4 - time3;
      System.out.println(String.format("%s;%s;%s;%s;%s;%s", encoder.config().amkEncoder, i, numVars, numClauses, encodingTime, solvingTime));

      final Assignment model = solver.model();
      Assert.assertTrue(pbc.evaluate(model));
      System.out.println(f.newCCVariable());
    }
  }

}

