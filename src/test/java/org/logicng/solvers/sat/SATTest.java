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

package org.logicng.solvers.sat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.cardinalityconstraints.CCEXOProduct;
import org.logicng.cardinalityconstraints.CCExactlyOne;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.handlers.TimeoutSATHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.CleaneLing;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.BASIC;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.NONE;

/**
 * Unit tests for the SAT solvers.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class SATTest {

  private final FormulaFactory f;
  private final SATSolver[] solvers;
  private final PigeonHoleGenerator pg;
  final PropositionalParser parser;

  public SATTest() {
    this.f = new FormulaFactory();
    this.pg = new PigeonHoleGenerator(f);
    this.parser = new PropositionalParser(f);
    this.solvers = new SATSolver[9];
    this.solvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(false).build());
    this.solvers[2] = MiniSat.glucose(f, new MiniSatConfig.Builder().incremental(false).build(),
            new GlucoseConfig.Builder().build());
    this.solvers[3] = MiniSat.glucose(f, new MiniSatConfig.Builder().incremental(false).build(),
            new GlucoseConfig.Builder().useUnaryWatched(true).build());
    this.solvers[4] = MiniSat.miniCard(f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[5] = MiniSat.miniCard(f, new MiniSatConfig.Builder().incremental(false).build());
    this.solvers[6] = CleaneLing.minimalistic(f);
    this.solvers[7] = CleaneLing.full(f, new CleaneLingConfig.Builder().plain(true).build());
    this.solvers[8] = CleaneLing.full(f);
  }

  @Test
  public void testTrue() {
    for (final SATSolver s : this.solvers) {
      s.add(F.TRUE);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(0, s.model().size());
      s.reset();
    }
  }

  @Test
  public void testFalse() {
    for (final SATSolver s : this.solvers) {
      s.add(F.FALSE);
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testLiterals() {
    for (final SATSolver s : this.solvers) {
      s.add(F.A);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(1, s.model().size());
      Assert.assertTrue(s.model().evaluateLit(F.A));
      s.add(F.NA);
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(F.NA);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(1, s.model().size());
      Assert.assertTrue(s.model().evaluateLit(F.NA));
      s.reset();
    }
  }

  @Test
  public void testAnd1() {
    for (final SATSolver s : this.solvers) {
      s.add(F.AND1);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(2, s.model().size());
      Assert.assertTrue(s.model().evaluateLit(F.A));
      Assert.assertTrue(s.model().evaluateLit(F.B));
      s.add(F.NOT1);
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testAnd2() {
    for (final SATSolver s : this.solvers) {
      final StandardProposition prop = new StandardProposition(f.and(f.literal("a", true), f.literal("b", false), f.literal("c", true), f.literal("d", false)));
      s.add(prop);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(4, s.model().size());
      Assert.assertTrue(s.model().evaluateLit(f.literal("a")));
      Assert.assertFalse(s.model().evaluateLit(f.literal("b")));
      Assert.assertTrue(s.model().evaluateLit(f.literal("c")));
      Assert.assertFalse(s.model().evaluateLit(f.literal("d")));
      s.reset();
    }
  }

  @Test
  public void testAnd3() {
    for (final SATSolver s : this.solvers) {
      final List<Formula> formulas = new ArrayList<>(3);
      formulas.add(f.literal("a", true));
      formulas.add(f.literal("b", false));
      formulas.add(f.literal("a", false));
      formulas.add(f.literal("d", false));
      s.add(formulas);
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testFormula1() throws ParserException {
    for (final SATSolver s : this.solvers) {
      s.add(parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(3, s.model().size());
      Assert.assertFalse(s.model().evaluateLit(f.literal("x")));
      Assert.assertTrue(s.model().evaluateLit(f.literal("y")));
      Assert.assertTrue(s.model().evaluateLit(f.literal("z")));
      s.add(f.literal("x"));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testFormula2() throws ParserException {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      s.add(parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
      final List<Assignment> models = s.enumerateAllModels();
      Assert.assertEquals(1, models.size());
      Assert.assertEquals(3, models.get(0).size());
      Assert.assertFalse(models.get(0).evaluateLit(f.literal("x")));
      Assert.assertTrue(models.get(0).evaluateLit(f.literal("y")));
      Assert.assertTrue(models.get(0).evaluateLit(f.literal("z")));
      s.add(f.literal("x"));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testCC1() throws InterruptedException {
    final CCExactlyOne c = new CCEXOProduct(f);
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final Literal[] lits = new Literal[100];
      for (int j = 0; j < lits.length; j++)
        lits[j] = f.literal("x" + j);
      final ImmutableFormulaList cc = c.build(lits);
      s.add(cc);
      final List<Assignment> models = s.enumerateAllModels(lits);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models)
        Assert.assertEquals(1, m.positiveLiterals().size());
      s.reset();
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIllegalEnumeration() {
    final SATSolver s = this.solvers[8];
    final Literal[] lits = new Literal[100];
    final CCExactlyOne c = new CCEXOProduct(f);
    for (int j = 0; j < lits.length; j++)
      lits[j] = f.literal("x" + j);
    final ImmutableFormulaList cc = c.build(lits);
    s.add(cc);
    s.enumerateAllModels(lits);
  }

  @Test
  public void testPigeonHole1() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(1));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole2() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(2));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole3() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(3));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole4() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(4));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole5() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(5));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole6() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(6));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole7() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testDifferentClauseMinimizations() {
    final SATSolver[] moreSolvers = new SATSolver[6];
    moreSolvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().clMinimization(NONE).build());
    moreSolvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().clMinimization(BASIC).build());
    moreSolvers[2] = MiniSat.glucose(f, new MiniSatConfig.Builder().clMinimization(NONE).build(), new GlucoseConfig.Builder().build());
    moreSolvers[3] = MiniSat.glucose(f, new MiniSatConfig.Builder().clMinimization(BASIC).build(), new GlucoseConfig.Builder().build());
    moreSolvers[4] = MiniSat.miniCard(f, new MiniSatConfig.Builder().clMinimization(NONE).build());
    moreSolvers[5] = MiniSat.miniCard(f, new MiniSatConfig.Builder().clMinimization(BASIC).build());
    for (final SATSolver s : moreSolvers) {
      s.add(pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
    }
  }

  @Test
  public void testTimeoutSATHandler() throws IOException {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(10));
      final Tristate result = s.sat(new TimeoutSATHandler(1000));
      Assert.assertEquals(UNDEF, result);
      s.reset();
    }
  }

  @Test
  public void testDimacsFiles() throws IOException {
    final Map<String, Boolean> expectedResults = new HashMap<>();
    final BufferedReader reader = new BufferedReader(new FileReader("tests/sat/results.txt"));
    while (reader.ready()) {
      final String[] tokens = reader.readLine().split(";");
      expectedResults.put(tokens[0], Boolean.valueOf(tokens[1]));
    }
    final File testFolder = new File("tests/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    for (final SATSolver solver : this.solvers) {
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          readCNF(solver, file);
          boolean res = solver.sat() == TRUE;
          Assert.assertEquals(expectedResults.get(fileName), res);
        }
      }
      solver.reset();
    }
  }

  private void readCNF(final SATSolver solver, final File file) throws IOException {
    solver.reset();
    final BufferedReader reader = new BufferedReader(new FileReader(file));
    while (reader.ready()) {
      final String line = reader.readLine();
      if (line.startsWith("p cnf"))
        break;
    }
    String[] tokens;
    final List<Literal> literals = new ArrayList<>();
    while (reader.ready()) {
      tokens = reader.readLine().split("\\s+");
      if (tokens.length >= 2) {
        assert "0".equals(tokens[tokens.length - 1]);
        literals.clear();
        for (int i = 0; i < tokens.length - 1; i++) {
          if (!tokens[i].isEmpty()) {
            int parsedLit = Integer.parseInt(tokens[i]);
            String var = "v" + Math.abs(parsedLit);
            literals.add(parsedLit > 0 ? f.literal(var, true) : f.literal(var, false));
          }
        }
        if (!literals.isEmpty())
          solver.add(f.or(literals));
      }
    }
  }

  @Test
  public void testPigeonHoleWithReset() {
    for (final SATSolver s : this.solvers) {
      s.add(pg.generate(4));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(pg.generate(5));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(pg.generate(6));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testNumberOfModelHandler() {
    final CCExactlyOne c = new CCEXOProduct(f);
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final Literal[] lits = new Literal[100];
      for (int j = 0; j < lits.length; j++)
        lits[j] = f.literal("x" + j);
      final ImmutableFormulaList cc = c.build(lits);

      s.add(cc);
      NumberOfModelsHandler handler = new NumberOfModelsHandler(100);
      List<Assignment> models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models)
        Assert.assertEquals(1, m.positiveLiterals().size());
      s.reset();

      s.add(cc);
      handler = new NumberOfModelsHandler(200);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models)
        Assert.assertEquals(1, m.positiveLiterals().size());
      s.reset();

      s.add(cc);
      handler = new NumberOfModelsHandler(50);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(50, models.size());
      for (final Assignment m : models)
        Assert.assertEquals(1, m.positiveLiterals().size());
      s.reset();

      s.add(cc);
      handler = new NumberOfModelsHandler(1);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(1, models.size());
      for (final Assignment m : models)
        Assert.assertEquals(1, m.positiveLiterals().size());
      s.reset();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalHandler() {
    new NumberOfModelsHandler(0);
  }
}
