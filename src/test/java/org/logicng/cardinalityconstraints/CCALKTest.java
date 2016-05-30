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
import org.logicng.formulas.Variable;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;

/**
 * Unit tests for the at-least-k encoders.
 * @version 1.1
 * @since 1.0
 */
public class CCALKTest {

  private static final FormulaFactory f = new FormulaFactory();
  private CCEncoder[] encoders;

  public CCALKTest() {
    encoders = new CCEncoder[3];
    encoders[0] = new CCEncoder(f, new CCConfig.Builder().alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build());
    encoders[1] = new CCEncoder(f, new CCConfig.Builder().alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build());
    encoders[2] = new CCEncoder(f, new CCConfig.Builder().alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build());
  }

  @Test
  public void testALK() {
    for (final CCEncoder encoder : this.encoders) {
      testCC(10, 1, 1023, encoder);
      testCC(10, 2, 1013, encoder);
      testCC(10, 3, 968, encoder);
      testCC(10, 4, 848, encoder);
      testCC(10, 5, 638, encoder);
      testCC(10, 6, 386, encoder);
      testCC(10, 7, 176, encoder);
      testCC(10, 8, 56, encoder);
      testCC(10, 9, 11, encoder);
      testCC(10, 10, 1, encoder);
      testCC(10, 12, 0, encoder);
    }
  }

  private void testCC(int numLits, int rhs, int expected, final CCEncoder encoder) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final ImmutableFormulaList clauses = encoder.encode(f.cc(CType.GE, rhs, problemLits));
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    if (expected != 0)
      Assert.assertEquals(Tristate.TRUE, solver.sat());
    else
      Assert.assertEquals(Tristate.FALSE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits, new NumberOfModelsHandler(12000));
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() >= rhs);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCC1() {
    final CCEncoder encoder = new CCEncoder(f);
    final int numLits = 100;
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    encoder.encode(f.cc(CType.GE, -1, problemLits));
  }
}
