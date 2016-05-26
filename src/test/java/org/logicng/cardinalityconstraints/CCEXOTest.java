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
import org.logicng.formulas.FType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;

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
 * Unit tests for the exactly-one encoders.
 * @version 1.1
 * @since 1.0
 */
public class CCEXOTest {

  private final FormulaFactory f = new FormulaFactory();
  private CCEncoder[] encoders;

  public CCEXOTest() {
    encoders = new CCEncoder[10];
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
  }

  @Test
  public void testEXO0() {
    final PBConstraint cc = this.f.exo();
    for (final CCEncoder encoder : this.encoders)
      Assert.assertTrue(encoder.encode(cc).empty());
  }

  @Test
  public void testEXO1() {
    final PBConstraint cc = this.f.exo(f.variable("v0"));
    final ImmutableFormulaList expected = new ImmutableFormulaList(FType.AND, f.variable("v0"));
    for (final CCEncoder encoder : this.encoders)
      Assert.assertEquals(expected, encoder.encode(cc));
  }

  @Test
  public void testEXOK() {
    for (final CCEncoder encoder : this.encoders) {
      testEXO(2, encoder);
      testEXO(10, encoder);
      testEXO(100, encoder);
      testEXO(250, encoder);
      testEXO(500, encoder);
    }
  }

  private void testEXO(int numLits, final CCEncoder encoder) {
    final Variable[] problemLits = new Variable[numLits];
    for (int i = 0; i < numLits; i++)
      problemLits[i] = f.variable("v" + i);
    final ImmutableFormulaList clauses = encoder.encode(f.exo(problemLits));
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(numLits, models.size());
    for (final Assignment model : models)
      Assert.assertEquals(1, model.positiveLiterals().size());
  }
}
