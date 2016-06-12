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
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;

import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BEST;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BIMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.BINARY;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.COMMANDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.LADDER;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.NESTED;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PRODUCT;
import static org.logicng.cardinalityconstraints.CCConfig.AMO_ENCODER.PURE;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.FIXED;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.HALF;
import static org.logicng.cardinalityconstraints.CCConfig.BIMANDER_GROUP_SIZE.SQRT;

/**
 * Unit tests for the at-most-one encoders.
 * @version 1.1
 * @since 1.0
 */
public class CCAMOTest {

  private final FormulaFactory f = new FormulaFactory();
  private CCEncoder[] encoders;

  public CCAMOTest() {
    encoders = new CCEncoder[11];
    encoders[0] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(PURE).build());
    encoders[1] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(LADDER).build());
    encoders[2] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(PRODUCT).build());
    encoders[3] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(BINARY).build());
    encoders[4] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(NESTED).build());
    encoders[5] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(COMMANDER).commanderGroupSize(3).build());
    encoders[6] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(COMMANDER).commanderGroupSize(7).build());
    encoders[7] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(FIXED).build());
    encoders[8] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(HALF).build());
    encoders[9] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(BIMANDER).bimanderGroupSize(SQRT).build());
    encoders[10] = new CCEncoder(f, new CCConfig.Builder().amoEncoding(BEST).build());
  }

  @Test
  public void testAMO0() {
    final PBConstraint cc = this.f.amo();
    for (final CCEncoder encoder : this.encoders)
      Assert.assertTrue(encoder.encode(cc).empty());
  }

  @Test
  public void testAMO1() {
    final PBConstraint cc = this.f.amo(f.variable("v0"));
    for (final CCEncoder encoder : this.encoders)
      Assert.assertTrue(encoder.encode(cc).empty());
  }

  @Test
  public void testAMOK() {
    for (final CCEncoder encoder : this.encoders) {
      if (encoder != null) {
        testAMO(2, encoder);
        testAMO(10, encoder);
        testAMO(100, encoder);
        testAMO(250, encoder);
        testAMO(500, encoder);
      }
    }
  }

  private void testAMO(int numLits, final CCEncoder encoder) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final ImmutableFormulaList clauses = encoder.encode(f.amo(problemLits));
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(numLits + 1, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= 1);
  }
}
