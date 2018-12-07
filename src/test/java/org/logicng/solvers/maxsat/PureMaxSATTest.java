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

package org.logicng.solvers.maxsat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.F;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Builder;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 1.3
 * @since 1.0
 */
public class PureMaxSATTest {

  private static final String[] files = new String[]{
          "c5315-bug-gate-0.dimacs.seq.filtered.cnf",
          "c6288-bug-gate-0.dimacs.seq.filtered.cnf",
          "c7552-bug-gate-0.dimacs.seq.filtered.cnf",
          "mot_comb1._red-gate-0.dimacs.seq.filtered.cnf",
          "mot_comb2._red-gate-0.dimacs.seq.filtered.cnf",
          "mot_comb3._red-gate-0.dimacs.seq.filtered.cnf",
          "s15850-bug-onevec-gate-0.dimacs.seq.filtered.cnf"
  };
  private final PrintStream logStream;
  private final FormulaFactory f = new FormulaFactory();

  public PureMaxSATTest() throws FileNotFoundException {
    logStream = new PrintStream("src/test/resources/maxsat/log.txt");
  }

  @Test
  public void testWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[2];
    configs[0] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (final String file : files) {
        final MaxSATSolver solver = MaxSATSolver.wbo(config);
        readCNF(solver, "src/test/resources/maxsat/" + file);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(1, solver.result());
      }
      final MaxSATSolver solver = MaxSATSolver.wbo(config);
      readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
      Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
      Assert.assertEquals(0, solver.result());
    }
  }

  @Test
  public void testIncWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[2];
    configs[0] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (final String file : files) {
        final MaxSATSolver solver = MaxSATSolver.incWBO(config);
        readCNF(solver, "src/test/resources/maxsat/" + file);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(1, solver.result());
      }
      final MaxSATSolver solver = MaxSATSolver.wbo(config);
      readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
      Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
      Assert.assertEquals(0, solver.result());
    }
  }

  @Test
  public void testLinearSU() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[2];
    configs[0] = new Builder().cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (final String file : files) {
        final MaxSATSolver solver = MaxSATSolver.linearSU(config);
        readCNF(solver, "src/test/resources/maxsat/" + file);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(1, solver.result());
      }
      final MaxSATSolver solver = MaxSATSolver.wbo(config);
      readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
      Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
      Assert.assertEquals(0, solver.result());
    }
  }

  @Test
  public void testLinearUS() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(logStream).build();
    configs[2] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (final String file : files) {
        final MaxSATSolver solver = MaxSATSolver.linearUS(config);
        readCNF(solver, "src/test/resources/maxsat/" + file);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(1, solver.result());
      }
      final MaxSATSolver solver = MaxSATSolver.wbo(config);
      readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
      Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
      Assert.assertEquals(0, solver.result());
    }
  }

  @Test
  public void testMSU3() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(logStream).build();
    configs[2] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (final String file : files) {
        final MaxSATSolver solver = MaxSATSolver.msu3(config);
        readCNF(solver, "src/test/resources/maxsat/" + file);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(1, solver.result());
      }
      final MaxSATSolver solver = MaxSATSolver.wbo(config);
      readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
      Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
      Assert.assertEquals(0, solver.result());
    }
  }

  @Test
  public void testSingle() throws IOException {
    final MaxSATSolver solver = MaxSATSolver.incWBO(new MaxSATConfig.Builder().cardinality(CardinalityEncoding.MTOTALIZER)
            .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(logStream).build());
    readCNF(solver, "src/test/resources/maxsat/c-fat200-2.clq.cnf");
    Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
    Assert.assertEquals(26, solver.result());
    final MaxSAT.Stats stats = solver.stats();
    Assert.assertEquals(26, stats.bestSolution());
    Assert.assertEquals(26, stats.unsatCalls());
    Assert.assertEquals(2, stats.satCalls());
    Assert.assertEquals(29.46, stats.averageCoreSize(), 0.1);
    Assert.assertEquals(24449, stats.symmetryClauses());
    Assert.assertEquals("MaxSAT.Stats{best solution=26, #sat calls=2, #unsat calls=26, average core size=29.46, #symmetry clauses=24449}", stats.toString());
  }

  @Test
  public void testAssignment() throws ParserException {
    final MaxSATSolver solver = MaxSATSolver.incWBO(new MaxSATConfig.Builder().cardinality(CardinalityEncoding.MTOTALIZER)
            .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(logStream).build());
    final PropositionalParser p = new PropositionalParser(f);
    solver.addSoftFormula(p.parse("a => b"), 1);
    solver.addSoftFormula(p.parse("b => c"), 1);
    solver.addSoftFormula(p.parse("c => d"), 1);
    solver.addSoftFormula(p.parse("d => e"), 1);
    solver.addSoftFormula(p.parse("a => x"), 1);
    solver.addSoftFormula(p.parse("~e"), 1);
    solver.addSoftFormula(p.parse("~x"), 1);
    solver.addSoftFormula(p.parse("a"), 1);
    Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
    Assert.assertEquals(1, solver.result());
    final Assignment model = solver.model();
    Assert.assertNotNull(model);
    Assert.assertEquals(6, model.size());
    Assert.assertEquals(0, model.positiveLiterals().size());
    Assert.assertEquals(6, model.negativeLiterals().size());
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("a", false)));
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("b", false)));
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("c", false)));
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("d", false)));
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("e", false)));
    Assert.assertTrue(model.negativeLiterals().contains(f.literal("x", false)));
  }

  @Test(expected = IllegalStateException.class)
  public void testIllegalModel() throws ParserException {
    final MaxSATSolver solver = MaxSATSolver.incWBO(new MaxSATConfig.Builder().cardinality(CardinalityEncoding.MTOTALIZER)
            .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(logStream).build());
    final PropositionalParser p = new PropositionalParser(f);
    solver.addSoftFormula(p.parse("a => b"), 1);
    solver.addSoftFormula(p.parse("b => c"), 1);
    solver.addSoftFormula(p.parse("c => d"), 1);
    solver.addSoftFormula(p.parse("d => e"), 1);
    solver.addSoftFormula(p.parse("a => x"), 1);
    solver.addSoftFormula(p.parse("~e"), 1);
    solver.addSoftFormula(p.parse("~x"), 1);
    solver.model();
  }

  @Test
  public void testToString() {
    MaxSATSolver[] solvers = new MaxSATSolver[6];
    solvers[0] = MaxSATSolver.incWBO();
    solvers[1] = MaxSATSolver.linearSU();
    solvers[2] = MaxSATSolver.linearUS();
    solvers[3] = MaxSATSolver.msu3();
    solvers[4] = MaxSATSolver.wbo();
    solvers[5] = MaxSATSolver.wmsu3();

    String expected = "MaxSATSolver{result=OPTIMUM, var2index={a=0, b=1}}";

    for (int i = 0; i < 6; i++) {
      MaxSATSolver s = solvers[i];
      s.addHardFormula(F.OR3);
      s.addSoftFormula(F.A, 1);
      if (i == 2 || i == 3) {
        s.addSoftFormula(F.NA, 1);
      } else {
        s.addSoftFormula(F.NA, 2);
      }
      s.solve();
      Assert.assertEquals(expected, s.toString());
    }
  }

  private void readCNF(final MaxSATSolver solver, final String fileName) throws IOException {
    final BufferedReader reader = new BufferedReader(new FileReader(fileName));
    boolean cont = true;
    while (reader.ready() && cont) {
      final String line = reader.readLine().trim();
      if (line.startsWith("p"))
        cont = false;

    }
    String[] tokens;
    final List<Literal> literals = new ArrayList<>();
    while (reader.ready()) {
      tokens = reader.readLine().split(" ");
      assert tokens.length >= 2;
      assert "0".equals(tokens[tokens.length - 1]);
      literals.clear();
      for (int i = 0; i < tokens.length - 1; i++) {
        if (!tokens[i].isEmpty()) {
          int parsedLit = Integer.parseInt(tokens[i]);
          String var = "v" + Math.abs(parsedLit);
          literals.add(parsedLit > 0 ? f.literal(var, true) : f.literal(var, false));
        }
      }
      solver.addSoftFormula(f.or(literals), 1);
    }
  }
}
