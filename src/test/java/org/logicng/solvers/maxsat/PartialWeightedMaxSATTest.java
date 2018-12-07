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
public class PartialWeightedMaxSATTest {

  private static final String[] files = new String[]{
          "8.wcsp.log.wcnf",
          "54.wcsp.log.wcnf",
          "404.wcsp.log.wcnf",
          "term1_gr_2pin_w4.shuffled.cnf"
  };
  private static final int[] results = new int[]{
          2, 37, 114, 0
  };
  private static final String[] bmoFiles = new String[]{
          "normalized-factor-size=9-P=11-Q=283.opb.wcnf",
          "normalized-factor-size=9-P=11-Q=53.opb.wcnf",
          "normalized-factor-size=9-P=13-Q=179.opb.wcnf",
          "normalized-factor-size=9-P=17-Q=347.opb.wcnf",
          "normalized-factor-size=9-P=17-Q=487.opb.wcnf",
          "normalized-factor-size=9-P=23-Q=293.opb.wcnf"
  };
  private static final int[] bmoResults = new int[]{
          11, 11, 13, 17, 17, 23
  };
  private final PrintStream logStream;
  private final FormulaFactory f = new FormulaFactory();

  public PartialWeightedMaxSATTest() throws FileNotFoundException {
    logStream = new PrintStream("src/test/resources/partialweightedmaxsat/log.txt");
  }

  @Test
  public void testWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(logStream).build();
    configs[2] = new Builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.wbo(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testIncWBO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new Builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(logStream).build();
    configs[2] = new Builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.incWBO(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testLinearSU() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[2];
    configs[0] = new Builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.linearSU(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testWMSU3() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[3];
    configs[0] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    configs[2] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < files.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.wmsu3(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(results[i], solver.result());
      }
    }
  }

  @Test
  public void testWMSU3BMO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[1];
    configs[0] = new Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < bmoFiles.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.wmsu3(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/bmo/" + bmoFiles[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(bmoResults[i], solver.result());
      }
    }
  }

  @Test
  public void testLineaerSUBMO() throws IOException {
    final MaxSATConfig[] configs = new MaxSATConfig[2];
    configs[0] = new Builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(logStream).build();
    configs[1] = new Builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(logStream).build();
    for (final MaxSATConfig config : configs) {
      for (int i = 0; i < bmoFiles.length; i++) {
        final MaxSATSolver solver = MaxSATSolver.linearSU(config);
        readCNF(solver, "src/test/resources/partialweightedmaxsat/bmo/" + bmoFiles[i]);
        Assert.assertEquals(MaxSAT.MaxSATResult.OPTIMUM, solver.solve());
        Assert.assertEquals(bmoResults[i], solver.result());
      }
    }
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
