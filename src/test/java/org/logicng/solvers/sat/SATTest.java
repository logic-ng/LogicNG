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
//  Copyright 2015-20xx Christoph Zengler                                //
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.BASIC;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.NONE;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.handlers.TimeoutSATHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.CleaneLing;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.testutils.PigeonHoleGenerator;
import org.logicng.util.FormulaHelper;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for the SAT solvers.
 * @version 1.6
 * @since 1.0
 */
public class SATTest {

  private final FormulaFactory f;
  private final SATSolver[] solvers;
  private final PigeonHoleGenerator pg;
  private final PropositionalParser parser;
  private final String[] testStrings;

  public SATTest() {
    this.f = new FormulaFactory();
    this.pg = new PigeonHoleGenerator(this.f);
    this.parser = new PropositionalParser(this.f);
    this.solvers = new SATSolver[10];
    this.solvers[0] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[1] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().incremental(false).build());
    this.solvers[2] = MiniSat.glucose(this.f, new MiniSatConfig.Builder().incremental(false).build(),
            new GlucoseConfig.Builder().build());
    this.solvers[3] = MiniSat.miniCard(this.f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[4] = MiniSat.miniCard(this.f, new MiniSatConfig.Builder().incremental(false).build());
    this.solvers[5] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
    this.solvers[6] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
    this.solvers[7] = CleaneLing.minimalistic(this.f);
    this.solvers[8] = CleaneLing.full(this.f, new CleaneLingConfig.Builder().plain(true).glueUpdate(true).gluered(true).build());
    this.solvers[9] = CleaneLing.full(this.f);

    this.testStrings = new String[10];
    this.testStrings[0] = "MiniSat2Solver{result=UNDEF, incremental=true}";
    this.testStrings[1] = "MiniSat2Solver{result=UNDEF, incremental=false}";
    this.testStrings[2] = "GlucoseSyrup{result=UNDEF, incremental=false}";
    this.testStrings[3] = "MiniCard{result=UNDEF, incremental=true}";
    this.testStrings[4] = "MiniCard{result=UNDEF, incremental=false}";
    this.testStrings[5] = "MiniSat2Solver{result=UNDEF, incremental=true}";
    this.testStrings[6] = "MiniSat2Solver{result=UNDEF, incremental=true}";
    this.testStrings[7] = "CleaneLing{result=UNDEF, idx2name={}}";
    this.testStrings[8] = "CleaneLing{result=UNDEF, idx2name={}}";
    this.testStrings[9] = "CleaneLing{result=UNDEF, idx2name={}}";
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
      final StandardProposition prop = new StandardProposition(this.f.and(this.f.literal("a", true), this.f.literal("b", false), this.f.literal("c", true), this.f.literal("d", false)));
      s.add(prop);
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(4, s.model().size());
      Assert.assertTrue(s.model().evaluateLit(this.f.variable("a")));
      Assert.assertFalse(s.model().evaluateLit(this.f.variable("b")));
      Assert.assertTrue(s.model().evaluateLit(this.f.variable("c")));
      Assert.assertFalse(s.model().evaluateLit(this.f.variable("d")));
      s.reset();
    }
  }

  @Test
  public void testAnd3() {
    for (final SATSolver s : this.solvers) {
      final List<Formula> formulas = new ArrayList<>(3);
      formulas.add(this.f.literal("a", true));
      formulas.add(this.f.literal("b", false));
      formulas.add(this.f.literal("a", false));
      formulas.add(this.f.literal("d", false));
      s.add(formulas);
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testFormula1() throws ParserException {
    for (final SATSolver s : this.solvers) {
      s.add(this.parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
      Assert.assertEquals(TRUE, s.sat());
      Assert.assertEquals(3, s.model().size());
      Assert.assertFalse(s.model().evaluateLit(this.f.variable("x")));
      Assert.assertTrue(s.model().evaluateLit(this.f.variable("y")));
      Assert.assertTrue(s.model().evaluateLit(this.f.variable("z")));
      s.add(this.f.variable("x"));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testFormula2() throws ParserException {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      s.add(this.parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
      final List<Assignment> models = s.enumerateAllModels();
      Assert.assertEquals(1, models.size());
      Assert.assertEquals(3, models.get(0).size());
      Assert.assertFalse(models.get(0).evaluateLit(this.f.variable("x")));
      Assert.assertTrue(models.get(0).evaluateLit(this.f.variable("y")));
      Assert.assertTrue(models.get(0).evaluateLit(this.f.variable("z")));
      s.add(this.f.variable("x"));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testCC1() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final Variable[] lits = new Variable[100];
      for (int j = 0; j < lits.length; j++) {
        lits[j] = this.f.variable("x" + j);
      }
      s.add(this.f.exo(lits));
      final List<Assignment> models = s.enumerateAllModels(lits);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models) {
        Assert.assertEquals(1, m.positiveLiterals().size());
      }
      s.reset();
    }
  }

  @Test
  public void testPBC() {
    for (final SATSolver s : this.solvers) {
      final List<Literal> lits = new ArrayList<>();
      final List<Integer> coeffs = new ArrayList<>();
      for (int i = 0; i < 5; i++) {
        lits.add(this.f.literal("x" + i, i % 2 == 0));
        coeffs.add(i + 1);
      }
      s.add(this.f.pbc(CType.GE, 10, lits, coeffs));
      Assert.assertEquals(Tristate.TRUE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testPartialModel() {
    for (final SATSolver s : this.solvers) {
      s.add(F.A);
      s.add(F.B);
      s.add(F.C);
      final Variable[] relevantVars = new Variable[2];
      relevantVars[0] = F.A;
      relevantVars[1] = F.B;
      Assert.assertEquals(Tristate.TRUE, s.sat());
      final Assignment relModel = s.model(relevantVars);
      Assert.assertTrue(relModel.negativeLiterals().isEmpty());
      Assert.assertFalse(relModel.literals().contains(F.C));
      s.reset();
    }
  }

  @Test
  public void testModelEnumerationHandler() {
    for (final SATSolver s : this.solvers) {
      s.add(F.IMP3);
      try {
        final List<Assignment> models = s.enumerateAllModels(new ModelEnumerationHandler() {
          @Override
          public boolean foundModel(final Assignment assignment) {
            return !assignment.negativeLiterals().isEmpty();
          }
        });
        Assert.assertFalse(models.isEmpty());
        Assert.assertTrue(models.get(models.size() - 1).negativeLiterals().isEmpty());
        models.remove(models.size() - 1);
        for (final Assignment model : models) {
          Assert.assertFalse(model.negativeLiterals().isEmpty());
        }
      } catch (final Exception e) {
        Assert.assertTrue(e instanceof UnsupportedOperationException);
      }

      s.reset();
    }
  }

  @Test
  public void testWithRelaxation() throws ParserException {
    final PropositionalParser parser = new PropositionalParser(this.f);
    final Formula one = parser.parse("a & b & (c | ~d)");
    final Formula two = parser.parse("~a | ~c");

    for (final SATSolver s : this.solvers) {
      s.add(one);
      s.addWithRelaxation(this.f.variable("d"), two);
      Assert.assertEquals(Tristate.TRUE, s.sat());
      try {
        Assert.assertEquals(2, s.enumerateAllModels().size());
      } catch (final Exception e) {
        Assert.assertTrue(e instanceof UnsupportedOperationException);
      }
      s.reset();

      s.add(one);
      s.addWithRelaxation(this.f.variable("d"), new StandardProposition(two));
      Assert.assertEquals(Tristate.TRUE, s.sat());
      try {
        Assert.assertEquals(2, s.enumerateAllModels().size());
      } catch (final Exception e) {
        Assert.assertTrue(e instanceof UnsupportedOperationException);
      }
      s.reset();

      s.add(one);
      s.addWithRelaxation(this.f.variable("d"), new ImmutableFormulaList(two));
      Assert.assertEquals(Tristate.TRUE, s.sat());
      try {
        Assert.assertEquals(2, s.enumerateAllModels().size());
      } catch (final Exception e) {
        Assert.assertTrue(e instanceof UnsupportedOperationException);
      }
      s.reset();

      s.add(one);
      s.addWithRelaxation(this.f.variable("d"), Arrays.asList(two, this.f.verum()));
      Assert.assertEquals(Tristate.TRUE, s.sat());
      try {
        Assert.assertEquals(2, s.enumerateAllModels().size());
      } catch (final Exception e) {
        Assert.assertTrue(e instanceof UnsupportedOperationException);
      }
      s.reset();
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIllegalEnumeration() {
    final SATSolver s = this.solvers[9];
    final Variable[] lits = new Variable[100];
    for (int j = 0; j < lits.length; j++) {
      lits[j] = this.f.variable("x" + j);
    }
    s.add(this.f.exo(lits));
    s.enumerateAllModels(lits);
  }

  @Test
  public void testPigeonHole1() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(1));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole2() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(2));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole3() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(3));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole4() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(4));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole5() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(5));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole6() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(6));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testPigeonHole7() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
      s.reset();
    }
  }

  @Test
  public void testDifferentClauseMinimizations() {
    final SATSolver[] moreSolvers = new SATSolver[6];
    moreSolvers[0] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().clMinimization(NONE).build());
    moreSolvers[1] = MiniSat.miniSat(this.f, new MiniSatConfig.Builder().clMinimization(BASIC).build());
    moreSolvers[2] = MiniSat.glucose(this.f, new MiniSatConfig.Builder().clMinimization(NONE).build(), new GlucoseConfig.Builder().build());
    moreSolvers[3] = MiniSat.glucose(this.f, new MiniSatConfig.Builder().clMinimization(BASIC).build(), new GlucoseConfig.Builder().build());
    moreSolvers[4] = MiniSat.miniCard(this.f, new MiniSatConfig.Builder().clMinimization(NONE).build());
    moreSolvers[5] = MiniSat.miniCard(this.f, new MiniSatConfig.Builder().clMinimization(BASIC).build());
    for (final SATSolver s : moreSolvers) {
      s.add(this.pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      Assert.assertNull(s.model());
    }
  }

  @Test
  public void testTimeoutSATHandler() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(10));
      final Tristate result = s.sat(new TimeoutSATHandler(1000));
      Assert.assertEquals(UNDEF, result);
      s.reset();
    }
  }

  @Test
  public void testDimacsFiles() throws IOException {
    final Map<String, Boolean> expectedResults = new HashMap<>();
    final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/sat/results.txt"));
    while (reader.ready()) {
      final String[] tokens = reader.readLine().split(";");
      expectedResults.put(tokens[0], Boolean.valueOf(tokens[1]));
    }
    final File testFolder = new File("src/test/resources/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    for (final SATSolver solver : this.solvers) {
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          readCNF(solver, file);
          final boolean res = solver.sat() == TRUE;
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
      if (line.startsWith("p cnf")) {
        break;
      }
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
            final int parsedLit = Integer.parseInt(tokens[i]);
            final String var = "v" + Math.abs(parsedLit);
            literals.add(parsedLit > 0 ? this.f.literal(var, true) : this.f.literal(var, false));
          }
        }
        if (!literals.isEmpty()) {
          solver.add(this.f.or(literals));
        }
      }
    }
  }

  @Test
  public void testPigeonHoleWithReset() {
    for (final SATSolver s : this.solvers) {
      s.add(this.pg.generate(4));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(this.pg.generate(5));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(this.pg.generate(6));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
      s.add(this.pg.generate(7));
      Assert.assertEquals(FALSE, s.sat());
      s.reset();
    }
  }

  @Test
  public void testModelEnumeration() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final SortedSet<Variable> lits = new TreeSet<>();
      final SortedSet<Variable> firstFive = new TreeSet<>();
      for (int j = 0; j < 20; j++) {
        final Variable lit = this.f.variable("x" + j);
        lits.add(lit);
        if (j < 5) {
          firstFive.add(lit);
        }
      }
      s.add(this.f.cc(CType.GE, 1, lits));

      final List<Assignment> models = s.enumerateAllModels(firstFive, lits);
      Assert.assertEquals(32, models.size());
      for (final Assignment model : models) {
        for (final Variable lit : lits) {
          Assert.assertTrue(model.positiveLiterals().contains(lit) || model.negativeVariables().contains(lit));
        }
      }
      s.reset();
    }
  }

  @Test
  public void testModelEnumerationWithHandler01() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final SortedSet<Variable> lits = new TreeSet<>();
      final SortedSet<Variable> firstFive = new TreeSet<>();
      for (int j = 0; j < 20; j++) {
        final Variable lit = this.f.variable("x" + j);
        lits.add(lit);
        if (j < 5) {
          firstFive.add(lit);
        }
      }
      s.add(this.f.cc(CType.GE, 1, lits));

      final NumberOfModelsHandler handler = new NumberOfModelsHandler(29);
      final List<Assignment> modelsWithHandler = s.enumerateAllModels(firstFive, lits, handler);
      Assert.assertEquals(29, modelsWithHandler.size());
      for (final Assignment model : modelsWithHandler) {
        for (final Variable lit : lits) {
          Assert.assertTrue(model.positiveLiterals().contains(lit) || model.negativeVariables().contains(lit));
        }
      }
      s.reset();
    }
  }

  @Test
  public void testModelEnumerationWithHandler02() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final SortedSet<Variable> lits = new TreeSet<>();
      final SortedSet<Variable> firstFive = new TreeSet<>();
      for (int j = 0; j < 20; j++) {
        final Variable lit = this.f.variable("x" + j);
        lits.add(lit);
        if (j < 5) {
          firstFive.add(lit);
        }
      }
      s.add(this.f.cc(CType.GE, 1, lits));

      final NumberOfModelsHandler handler = new NumberOfModelsHandler(29);
      final List<Assignment> modelsWithHandler = s.enumerateAllModels(null, Collections.singletonList(firstFive.first()), handler);
      Assert.assertEquals(29, modelsWithHandler.size());
      for (final Assignment model : modelsWithHandler) {
        for (final Variable lit : lits) {
          Assert.assertTrue(model.positiveLiterals().contains(lit) || model.negativeVariables().contains(lit));
        }
      }
      s.reset();
    }
  }

  @Test
  public void testEmptyEnumeration() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      s.add(this.f.falsum());
      final List<Assignment> models = s.enumerateAllModels();
      Assert.assertTrue(models.isEmpty());

      s.reset();
    }
  }

  @Test
  public void testNumberOfModelHandler() {
    for (int i = 0; i < this.solvers.length - 1; i++) {
      final SATSolver s = this.solvers[i];
      final Variable[] lits = new Variable[100];
      for (int j = 0; j < lits.length; j++) {
        lits[j] = this.f.variable("x" + j);
      }
      s.add(this.f.exo(lits));
      NumberOfModelsHandler handler = new NumberOfModelsHandler(100);
      List<Assignment> models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models) {
        Assert.assertEquals(1, m.positiveLiterals().size());
      }
      s.reset();

      s.add(this.f.exo(lits));
      handler = new NumberOfModelsHandler(200);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(100, models.size());
      for (final Assignment m : models) {
        Assert.assertEquals(1, m.positiveLiterals().size());
      }
      s.reset();

      s.add(this.f.exo(lits));
      handler = new NumberOfModelsHandler(50);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(50, models.size());
      for (final Assignment m : models) {
        Assert.assertEquals(1, m.positiveLiterals().size());
      }
      s.reset();

      s.add(this.f.exo(lits));
      handler = new NumberOfModelsHandler(1);
      models = s.enumerateAllModels(lits, handler);
      Assert.assertEquals(1, models.size());
      for (final Assignment m : models) {
        Assert.assertEquals(1, m.positiveLiterals().size());
      }
      s.reset();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalHandler() {
    new NumberOfModelsHandler(0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAddNonCCAsCC() {
    final MiniSat solver = MiniSat.miniSat(this.f);
    solver.addIncrementalCC((PBConstraint) F.PBC3);
  }

  @Test(expected = IllegalStateException.class)
  public void testModelBeforeSolving() {
    final MiniSat solver = MiniSat.miniSat(this.f);
    solver.model();
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCLAddNonCCAsCC() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f, new CleaneLingConfig.Builder().gluered(true).build());
    solver.addIncrementalCC((PBConstraint) F.PBC3);
  }

  @Test(expected = IllegalStateException.class)
  public void testCLModelBeforeSolving() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f);
    solver.model();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCLSatWithLit() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f);
    solver.add(F.AND1);
    solver.sat(new TimeoutSATHandler(10000), F.A);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCLSaveState() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f);
    solver.add(F.AND1);
    solver.saveState();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCLLoadState() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f);
    solver.add(F.AND1);
    solver.loadState(new SolverState(27, new int[3]));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCLSatWithLitCollection() {
    final CleaneLing solver = CleaneLing.minimalistic(this.f);
    solver.add(F.AND1);
    final List<Literal> lits = new ArrayList<>();
    lits.add(F.A);
    lits.add(F.B);
    solver.sat(new TimeoutSATHandler(10000), lits);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCLEnumerateWithWrongConfig() {
    final CleaneLing solver = CleaneLing.full(this.f, new CleaneLingConfig.Builder().plain(false).build());
    solver.add(F.AND1);
    solver.enumerateAllModels();
  }

  @Test
  public void testToString() {
    for (int i = 0; i < this.solvers.length; i++) {
      Assert.assertEquals(this.testStrings[i], this.solvers[i].toString());
    }
  }

  @Test
  public void testPrintMinimalisticCleaneLing() {
    final CleaneLingMinimalisticSolver clms = new CleaneLingMinimalisticSolver(new CleaneLingConfig.Builder().build());
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final PrintStream ps = new PrintStream(baos);
    clms.printSolverState(ps);
    final String expected = String.format("level=0%n" +
            "next=0%n" +
            "ignore=null%n" +
            "empty=null%n" +
            "vars=[]%n" +
            "vals=[]%n" +
            "phases=[]%n" +
            "decisions=LNGDoublePriorityQueue{}%n" +
            "control=[CLFrame{decision=0, level=0, trail=0, mark=false}]%n" +
            "watches=[]%n" +
            "trail=[]%n" +
            "frames=[]%n");
    Assert.assertEquals(expected, baos.toString());
  }

  @Test
  public void testKnownVariables() throws ParserException {
    final PropositionalParser parser = new PropositionalParser(this.f);
    final Formula phi = parser.parse("x1 & x2 & x3 & (x4 | ~x5)");
    final SATSolver minisat = MiniSat.miniSat(this.f);
    final SATSolver minicard = MiniSat.miniCard(this.f);
    final SATSolver cleaneling = CleaneLing.minimalistic(this.f);
    minisat.add(phi);
    minicard.add(phi);
    cleaneling.add(phi);
    final SortedSet<Variable> expected = new TreeSet<>(Arrays.asList(
            this.f.variable("x1"),
            this.f.variable("x2"),
            this.f.variable("x3"),
            this.f.variable("x4"),
            this.f.variable("x5")));
    Assert.assertEquals(expected, minisat.knownVariables());
    Assert.assertEquals(expected, minicard.knownVariables());
    Assert.assertEquals(expected, cleaneling.knownVariables());

    final SolverState state = minisat.saveState();
    final SolverState stateCard = minicard.saveState();
    minisat.add(this.f.variable("x6"));
    minicard.add(this.f.variable("x6"));
    cleaneling.add(this.f.variable("x6"));
    final SortedSet<Variable> expected2 = new TreeSet<>(Arrays.asList(
            this.f.variable("x1"),
            this.f.variable("x2"),
            this.f.variable("x3"),
            this.f.variable("x4"),
            this.f.variable("x5"),
            this.f.variable("x6")));
    Assert.assertEquals(expected2, minisat.knownVariables());
    Assert.assertEquals(expected2, minicard.knownVariables());
    Assert.assertEquals(expected2, cleaneling.knownVariables());

    // load state for minisat
    minisat.loadState(state);
    minicard.loadState(stateCard);
    Assert.assertEquals(expected, minisat.knownVariables());
    Assert.assertEquals(expected, minicard.knownVariables());
  }

  @Test
  public void testAddWithoutUnknown() throws ParserException {
    final PropositionalParser parser = new PropositionalParser(this.f);
    final Formula phi = parser.parse("x1 & (~x2 | x3) & (x4 | ~x5)");
    final SortedSet<Variable> phiVars = new TreeSet<>(Arrays.asList(
            this.f.variable("x1"),
            this.f.variable("x2"),
            this.f.variable("x3"),
            this.f.variable("x4"),
            this.f.variable("x5")));
    final Formula add1 = parser.parse("x1 | x6 | x7");
    final Formula add2 = parser.parse("~x1 | ~x6 | x8");
    final Formula add3 = parser.parse("x2 & ~x3 | x7");
    final Formula add4 = parser.parse("x8 | x9");
    final SATSolver minisat = MiniSat.miniSat(this.f);
    final SATSolver minicard = MiniSat.miniCard(this.f);
    final SATSolver cleaneling = CleaneLing.minimalistic(this.f);
    final SATSolver[] solvers = new SATSolver[]{minisat, minicard, cleaneling};
    for (final SATSolver solver : solvers) {
      solver.add(phi);
      solver.addWithoutUnknown(add1);
      Assert.assertEquals(TRUE, solver.sat());
      Assert.assertEquals(phiVars, solver.model().formula(this.f).variables());
      solver.addWithoutUnknown(add2);
      Assert.assertEquals(TRUE, solver.sat());
      Assert.assertEquals(phiVars, solver.model().formula(this.f).variables());
      if (solver instanceof MiniSat) {
        final SolverState state = solver.saveState();
        solver.addWithoutUnknown(add3);
        Assert.assertEquals(FALSE, solver.sat());
        solver.loadState(state);
        solver.add(add1);
        Assert.assertEquals(TRUE, solver.sat());
        Assert.assertTrue(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7"))));
        solver.loadState(state);
        solver.sat();
        Assert.assertEquals(phiVars, solver.model().formula(this.f).variables());
      } else {
        solver.add(add1);
        Assert.assertEquals(TRUE, solver.sat());
        Assert.assertTrue(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7"))));
        solver.add(this.f.variable("x7"));
        Assert.assertEquals(TRUE, solver.sat());
        Assert.assertTrue(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7"))));
        solver.addWithoutUnknown(add4);
        Assert.assertEquals(FALSE, solver.sat());
      }
    }
  }

  @Test
  public void testUPZeroLiteralsUNSAT() throws ParserException {
    final Formula formula = this.parser.parse("a & (a => b) & (b => c) & (c => ~a)");
    for (final SATSolver solver : this.solvers) {
      solver.reset();
      solver.add(formula);
      solver.sat();
      final SortedSet<Literal> upLiterals = solver.upZeroLiterals();
      assertThat(upLiterals).isNull();
    }
  }

  @Test
  public void testUPZeroLiterals() throws ParserException {
    // Note: The complete unit propagated set of literals on level 0 depends on each solver's added learned clauses during the solving process
    final Map<Formula, SortedSet<Literal>> expectedSubsets = new HashMap<>();
    expectedSubsets.put(this.f.verum(), new TreeSet<Literal>());
    expectedSubsets.put(this.parser.parse("a"), new TreeSet<>(Collections.singletonList(this.f.literal("a", true))));
    expectedSubsets.put(this.parser.parse("a | b"), new TreeSet<Literal>());
    expectedSubsets.put(this.parser.parse("a & b"), new TreeSet<>(Arrays.asList(this.f.literal("a", true), this.f.literal("b", true))));
    expectedSubsets.put(this.parser.parse("a & ~b"), new TreeSet<>(Arrays.asList(this.f.literal("a", true), this.f.literal("b", false))));
    expectedSubsets.put(this.parser.parse("(a | c) & ~b"), new TreeSet<>(Collections.singletonList(this.f.literal("b", false))));
    expectedSubsets.put(this.parser.parse("(b | c) & ~b & (~c | d)"), new TreeSet<>(Arrays.asList(
            this.f.literal("b", false), this.f.literal("c", true), this.f.literal("d", true))));
    for (final SATSolver solver : this.solvers) {
      for (final Formula formula : expectedSubsets.keySet()) {
        solver.reset();
        solver.add(formula);
        final boolean res = solver.sat() == TRUE;
        assertThat(res).isTrue();
        final SortedSet<Literal> upLiterals = solver.upZeroLiterals();
        assertThat(upLiterals).containsAll(expectedSubsets.get(formula));
      }
    }
  }

  @Test
  public void testUPZeroLiteralsDimacsFiles() throws IOException {
    final File testFolder = new File("src/test/resources/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    for (final SATSolver solver : this.solvers) {
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          readCNF(solver, file);
          final boolean res = solver.sat() == TRUE;
          if (res) {
            final SortedSet<Literal> upZeroLiterals = solver.upZeroLiterals();
            final List<Literal> negations = new ArrayList<>(upZeroLiterals.size());
            for (final Literal lit : upZeroLiterals) {
              negations.add(lit.negate());
            }
            solver.add(this.f.or(negations));
            // Test if CNF implies identified unit propagated literals on level zero, i.e., each literal is a backbone literal
            assertThat(solver.sat()).isEqualTo(FALSE);
          }
        }
      }
      solver.reset();
    }
  }

  @Test
  public void testFormulaOnSolver() throws ParserException {
    for (final SATSolver solver : this.solvers) {
      if (solver instanceof MiniSat) {
        final PseudoBooleanParser p = new PseudoBooleanParser(this.f);
        final Set<Formula> formulas = new LinkedHashSet<>();
        formulas.add(p.parse("A | B | C"));
        formulas.add(p.parse("~A | ~B | ~C"));
        formulas.add(p.parse("A | ~B"));
        formulas.add(p.parse("A"));
        solver.add(formulas);
        compareFormulas(formulas, solver.formulaOnSolver());
        formulas.add(p.parse("~A | C"));
        solver.reset();
        solver.add(formulas);
        compareFormulas(formulas, solver.formulaOnSolver());
        final Formula formula = p.parse("C + D + E <= 2");
        formulas.add(formula);
        solver.add(formula);
        compareFormulas(formulas, solver.formulaOnSolver());
      }
    }
  }

  @Test
  public void testSelectionOrderSimple01() throws ParserException {
    for (final SATSolver s : this.solvers) {
      if (s instanceof CleaneLing) {
        continue; // Cleaning does not support selection order
      }
      s.add(this.parser.parse("~(x <=> y)"));

      assertThat(s.satWithSelectionOrder(Arrays.asList(F.X, F.Y))).isEqualTo(TRUE);
      Assignment assignment = s.model();
      assertThat(assignment.literals()).containsExactlyInAnyOrder(F.X, F.NY);

      s.setSolverToUndef();
      assertThat(s.satWithSelectionOrder(Arrays.asList(F.Y, F.X))).isEqualTo(TRUE);
      assignment = s.model();
      assertThat(assignment.literals()).containsExactlyInAnyOrder(F.Y, F.NX);

      s.setSolverToUndef();
      assertThat(s.sat(Collections.singletonList(F.NX))).isEqualTo(TRUE);
      assignment = s.model();
      assertThat(assignment.literals()).containsExactlyInAnyOrder(F.Y, F.NX);

      s.setSolverToUndef();
      assertThat(s.satWithSelectionOrder(Arrays.asList(F.NY, F.NX))).isEqualTo(TRUE);
      assignment = s.model();
      assertThat(assignment.literals()).containsExactlyInAnyOrder(F.X, F.NY);

      s.reset();
    }
  }

  @Ignore("Long running")
  @Test
  public void testDimacsFilesWithSelectionOrder() throws IOException {
    final Map<String, Boolean> expectedResults = new HashMap<>();
    final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/sat/results.txt"));
    while (reader.ready()) {
      final String[] tokens = reader.readLine().split(";");
      expectedResults.put(tokens[0], Boolean.valueOf(tokens[1]));
    }
    final File testFolder = new File("src/test/resources/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    for (final SATSolver solver : this.solvers) {
      if (solver instanceof CleaneLing) {
        continue; // Cleaning does not support selection order
      }
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          readCNF(solver, file);
          final List<Literal> selectionOrder = new ArrayList<>();
          for (final Variable var : FormulaHelper.variables(solver.formulaOnSolver())) {
            if (selectionOrder.size() < 10) {
              selectionOrder.add(var.negate());
            }
          }
          final boolean res = solver.satWithSelectionOrder(selectionOrder) == TRUE;
          Assert.assertEquals(expectedResults.get(fileName), res);
        }
      }
      solver.reset();
    }
  }

  private void compareFormulas(final Collection<Formula> original, final Collection<Formula> solver) {
    final SortedSet<Variable> vars = new TreeSet<>();
    for (final Formula formula : original) {
      vars.addAll(formula.variables());
    }
    final MiniSat miniSat = MiniSat.miniSat(this.f);
    miniSat.add(original);
    final List<Assignment> models1 = miniSat.enumerateAllModels(vars);
    miniSat.reset();
    miniSat.add(solver);
    final List<Assignment> models2 = miniSat.enumerateAllModels(vars);
    assertThat(models1).containsOnlyElementsOf(models2);
  }

  private void testModelsOrderLE(final List<Assignment> models, final List<? extends Literal> selectionOrder) {
    for (int i = 0; i < models.size() - 1; i++) {
      final Assignment a = models.get(i);
      final Assignment b = models.get(i + 1);
      assertThat(compareModel(a, b, selectionOrder)).isNotPositive();
    }
  }

  private void testModelsOrderLT(final List<Assignment> models, final List<? extends Literal> selectionOrder) {
    for (int i = 0; i < models.size() - 1; i++) {
      final Assignment a = models.get(i);
      final Assignment b = models.get(i + 1);
      assertThat(compareModel(a, b, selectionOrder)).isNegative();
    }
  }

  private int compareModel(final Assignment a, final Assignment b, final List<? extends Literal> selectionOrder) {
    final SortedSet<Literal> aLiterals = a.literals();
    final SortedSet<Literal> bLiterals = b.literals();
    for (final Literal lit : selectionOrder) {
      if (aLiterals.contains(lit) && !bLiterals.contains(lit)) {
        return -1;
      } else if (!aLiterals.contains(lit) && bLiterals.contains(lit)) {
        return 1;
      }
    }
    return 0;
  }
}
