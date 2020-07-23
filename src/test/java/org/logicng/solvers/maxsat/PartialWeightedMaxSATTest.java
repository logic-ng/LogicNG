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

package org.logicng.solvers.maxsat;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.TimeoutMaxSATHandler;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.testutils.PigeonHoleGenerator;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 2.0.0
 * @since 1.0
 */
public class PartialWeightedMaxSATTest extends TestWithExampleFormulas {

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
        this.logStream = new PrintStream("src/test/resources/partialweightedmaxsat/log.txt");
    }

    @Test
    public void testExceptionalBehaviorForWMSU3() {
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.wmsu3();
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently algorithm WMSU3 does not support unweighted MaxSAT instances.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.wmsu3(MaxSATConfig.builder()
                    .bmo(true)
                    .incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                    .cardinality(CardinalityEncoding.MTOTALIZER)
                    .build());
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 2);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently iterative encoding in WMSU3 only supports the Totalizer encoding.");
    }

    @Test
    public void testWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.wbo(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testIncWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.incWBO(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testLinearSU() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.linearSU(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testWMSU3() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.wmsu3(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testWMSU3BMO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < bmoFiles.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.wmsu3(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/bmo/" + bmoFiles[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(bmoResults[i]);
            }
        }
    }

    @Test
    public void testLineaerSUBMO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < bmoFiles.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.linearSU(config);
                readCNF(solver, "src/test/resources/partialweightedmaxsat/bmo/" + bmoFiles[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(bmoResults[i]);
            }
        }
    }

    @Test
    public void testTimeoutHandlerWBO() {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerIncWBO() {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NORMAL).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.DIVERSIFY).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerLinearSU() {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerWMSU3() {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerWMSU3BMO() {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerLineaerSUBMO() {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            testTimeoutHandler(solver);
        }
    }

    private void testTimeoutHandler(final MaxSATSolver solver) {
        final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(1000L);

        final PigeonHoleGenerator pg = new PigeonHoleGenerator(this.f);
        final Formula formula = pg.generate(10);
        solver.addHardFormula(formula);
        solver.addSoftFormula(this.f.or(formula.variables()), 10);
        MaxSAT.MaxSATResult result = solver.solve(handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.UNDEF);

        solver.reset();
        solver.addHardFormula(this.IMP1);
        solver.addSoftFormula(this.AND1, 10);
        result = solver.solve(handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
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
            final int weight = Integer.parseInt(tokens[0]);
            for (int i = 1; i < tokens.length - 1; i++) {
                if (!tokens[i].isEmpty()) {
                    final int parsedLit = Integer.parseInt(tokens[i]);
                    final String var = "v" + Math.abs(parsedLit);
                    literals.add(parsedLit > 0 ? this.f.literal(var, true) : this.f.literal(var, false));
                }
            }
            if (weight == hardWeight) {
                solver.addHardFormula(this.f.or(literals));
            } else {
                solver.addSoftFormula(this.f.or(literals), weight);
            }
        }
    }
}
