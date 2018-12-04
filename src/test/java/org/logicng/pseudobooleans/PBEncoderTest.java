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
package org.logicng.pseudobooleans;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.configurations.ConfigurationType;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Unit tests for {@link PBEncoder}.
 * @version 1.1
 * @since 1.0
 */
public class PBEncoderTest {

  private final FormulaFactory f = new FormulaFactory();
  private PBEncoder[] encoders;

  public PBEncoderTest() {
    this.encoders = new PBEncoder[3];
    this.encoders[0] = new PBEncoder(this.f, new PBConfig.Builder().pbEncoding(PBConfig.PB_ENCODER.SWC).build());
    this.encoders[1] = new PBEncoder(this.f, new PBConfig.Builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).build(), new CCConfig.Builder().amoEncoding(CCConfig.AMO_ENCODER.NESTED).build());
    this.encoders[2] = new PBEncoder(this.f, null);
  }

  @Test
  public void testCC0() {
    for (final PBEncoder encoder : this.encoders) {
      final int numLits = 100;
      final List<Literal> lits = new LinkedList<>();
      final List<Integer> coeffs = new LinkedList<>();
      final Variable[] problemLits = new Variable[numLits];
      for (int i = 0; i < numLits; i++) {
        final Variable var = f.variable("v" + i);
        lits.add(var);
        problemLits[i] = var;
        coeffs.add(1);
      }
      final ImmutableFormulaList clauses = encoder.encode(f.pbc(CType.LE, 0, lits, coeffs));
      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(clauses);
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      final List<Assignment> models = solver.enumerateAllModels(problemLits);
      Assert.assertEquals(1, models.size());
      Assert.assertEquals(100, models.get(0).negativeLiterals().size());
    }
  }

  @Test
  public void testCC1() {
    for (final PBEncoder encoder : this.encoders) {
      final int numLits = 100;
      final int rhs = 1;
      final List<Literal> lits = new LinkedList<>();
      final List<Integer> coeffs = new LinkedList<>();
      final Variable[] problemLits = new Variable[numLits];
      for (int i = 0; i < numLits; i++) {
        final Variable var = f.variable("v" + i);
        lits.add(var);
        problemLits[i] = var;
        coeffs.add(1);
      }
      final ImmutableFormulaList clauses = encoder.encode(f.pbc(CType.LE, rhs, lits, coeffs));
      final SATSolver solver = MiniSat.miniSat(f);
      solver.add(clauses);
      Assert.assertEquals(Tristate.TRUE, solver.sat());
      final List<Assignment> models = solver.enumerateAllModels(problemLits);
      Assert.assertEquals(numLits + 1, models.size());
      for (final Assignment model : models)
        Assert.assertTrue(model.positiveLiterals().size() <= rhs);
    }
  }

  @Test
  public void testCCs() {
    for (final PBEncoder encoder : this.encoders) {
      testCC(10, 0, 1, encoder);
      testCC(10, 1, 11, encoder);
      testCC(10, 2, 56, encoder);
      testCC(10, 3, 176, encoder);
      testCC(10, 4, 386, encoder);
      testCC(10, 5, 638, encoder);
      testCC(10, 6, 848, encoder);
      testCC(10, 7, 968, encoder);
      testCC(10, 8, 1013, encoder);
      testCC(10, 9, 1023, encoder);
    }
  }

  private void testCC(int numLits, int rhs, int expected, final PBEncoder encoder) {
    final Variable[] problemLits = new Variable[numLits];
    final int[] coeffs = new int[numLits];
    for (int i = 0; i < numLits; i++) {
      problemLits[i] = f.variable("v" + i);
      coeffs[i] = 1;
    }
    final ImmutableFormulaList clauses = encoder.encode(f.pbc(CType.LE, rhs, problemLits, coeffs));
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(clauses);
    Assert.assertEquals(Tristate.TRUE, solver.sat());
    final List<Assignment> models = solver.enumerateAllModels(problemLits);
    Assert.assertEquals(expected, models.size());
    for (final Assignment model : models)
      Assert.assertTrue(model.positiveLiterals().size() <= rhs);
  }

  @Test
  public void testNotNullConfig() {
    for (final PBEncoder encoder : this.encoders) {
      Assert.assertNotNull(encoder.config());
    }
  }

  @Test
  public void testSpecialCases() {
    List<Literal> lits = new ArrayList<>();
    lits.add(f.literal("m", true));
    lits.add(f.literal("n", true));
    List<Integer> coeffs = new ArrayList<>();
    coeffs.add(2);
    coeffs.add(1);
    PBConstraint truePBC = f.pbc(CType.GE, 0, lits, coeffs);
    for (final PBEncoder encoder : this.encoders) {
      Assert.assertEquals(new ImmutableFormulaList(FType.AND), encoder.encode(truePBC));
    }
  }

  @Test
  public void testCCNormalized() {
    List<Literal> lits = new ArrayList<>();
    lits.add(f.literal("m", true));
    lits.add(f.literal("n", true));
    List<Integer> coeffs2 = new ArrayList<>();
    coeffs2.add(2);
    coeffs2.add(2);
    PBConstraint normCC = f.pbc(CType.LE, 2, lits, coeffs2);
    Assert.assertEquals("AND[~m | ~n]", encoders[0].encode(normCC).toString());
  }

  @Test
  public void testConfigToString() {
    Assert.assertEquals(String.format("PBConfig{%n" +
            "pbEncoder=SWC%n" +
            "binaryMergeUseGAC=true%n" +
            "binaryMergeNoSupportForSingleBit=false%n" +
            "binaryMergeUseWatchDog=true%n" +
            "}%n"), encoders[0].config().toString());
    Assert.assertTrue(encoders[0].config().toString().contains("pbEncoder=" + PBConfig.PB_ENCODER.valueOf("SWC")));
    Assert.assertEquals(ConfigurationType.PB_ENCODER, encoders[0].config().type());
    Assert.assertEquals("PBSWC", new PBSWC(f).toString());
    Assert.assertTrue(Arrays.asList(PBConfig.PB_ENCODER.values()).contains(PBConfig.PB_ENCODER.valueOf("ADDER_NETWORKS")));
  }
}
