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
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.handlers.TimeoutMaxSATHandler;
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

import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 1.3
 * @since 1.0
 */
public class PartialMaxSATTest {

  private static final String[] files = new String[]{
          "c1355_F176gat-1278gat@1.wcnf",
          "c1355_F1001gat-1048gat@1.wcnf",
          "c1355_F1183gat-1262gat@1.wcnf",
          "c1355_F1229gat@1.wcnf",
          "normalized-s3-3-3-1pb.wcnf",
          "normalized-s3-3-3-2pb.wcnf",
          "normalized-s3-3-3-3pb.wcnf",
          "term1_gr_2pin_w4.shuffled.cnf"
  };
  private static final int[] results = new int[]{
          13, 21, 33, 33, 36, 36, 36, 0
  };
  private final PrintStream logStream;
  private final FormulaFactory f = new FormulaFactory();

  public PartialMaxSATTest() throws FileNotFoundException {
    logStream = new PrintStream("src/test/resources/partialmaxsat/log.txt");
  }

  @Test
  public void testWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[1];
    configs[0] = new MaxSATConfig.Builder().verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.wbo(config);
        readCNF(solver, "src/test/resources/partialmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testIncWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[1];
    configs[0] = new MaxSATConfig.Builder().verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.incWBO(config);
        readCNF(solver, "src/test/resources/partialmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testLinearSU() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[4];
    configs[0] = new MaxSATConfig.Builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    configs[1] = new MaxSATConfig.Builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    configs[2] = new MaxSATConfig.Builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(logStream).build();
    configs[3] = new MaxSATConfig.Builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.linearSU(config);
        readCNF(solver, "src/test/resources/partialmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testLinearUS() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    configs[1] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(logStream).build();
    configs[2] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.linearUS(config);
        readCNF(solver, "src/test/resources/partialmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testMSU3() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    configs[1] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(logStream).build();
    configs[2] = new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.msu3(config);
        readCNF(solver, "src/test/resources/partialmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testTimeoutHandler() throws IOException {
    MaxSATSolver solver = MaxSATSolver.wbo(new MaxSATConfig.Builder().verbosity(SOME).output(logStream).build());
    readCNF(solver, "src/test/resources/partialmaxsat/c1355_F176gat-1278gat@1.wcnf");
    MaxSATHandler handler = new TimeoutMaxSATHandler(1000);
    Assert.assertEquals(MaxSAT.MaxSATResult.UNDEF, solver.solve(handler));
    Assert.assertTrue(handler.lowerBoundApproximation() < 13);

    solver = MaxSATSolver.wbo(new MaxSATConfig.Builder().verbosity(SOME).output(logStream).build());
    readCNF(solver, "src/test/resources/partialmaxsat/c1355_F1229gat@1.wcnf");
    handler = new TimeoutMaxSATHandler(5000);
    Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve(handler));
  }

  @Test
  public void testTimeoutHandlerUB() throws IOException {
    MaxSATSolver solver = MaxSATSolver.linearSU(new MaxSATConfig.Builder().verbosity(SOME).output(logStream).build());
    readCNF(solver, "src/test/resources/partialmaxsat/c1355_F1229gat@1.wcnf");
    MaxSATHandler handler = new TimeoutMaxSATHandler(5000);
    Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve(handler));
    Assert.assertEquals(solver.result(), handler.upperBoundApproximation());
  }

  private void readCNF(final MaxSATSolver solver, final String fileName) throws IOException {
    final BufferedReader reader = new BufferedReader(new FileReader(fileName));
    int hardWeight = 0;
    while (reader.ready()) {
      final String line = reader.readLine();
      if (line.startsWith("p wcnf")) {
        final String[] header = line.split(" ", -1);
        hardWeight = Integer.parseInt(header[4]);
        break;
      }
    }
    String[] tokens;
    final List<Literal> literals = new ArrayList<>();
    while (reader.ready()) {
      tokens = reader.readLine().split(" ");
      assert tokens.length >= 3;
      assert "0".equals(tokens[tokens.length - 1]);
      literals.clear();
      int weight = Integer.parseInt(tokens[0]);
      for (int i = 1; i < tokens.length - 1; i++) {
        if (!tokens[i].isEmpty()) {
          int parsedLit = Integer.parseInt(tokens[i]);
          String var = "v" + Math.abs(parsedLit);
          literals.add(parsedLit > 0 ? f.literal(var, true) : f.literal(var, false));
        }
      }
      if (weight == hardWeight)
        solver.addHardFormula(f.or(literals));
      else {
        solver.addSoftFormula(f.or(literals), weight);
      }
    }
  }
}
