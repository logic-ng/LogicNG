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
import static org.logicng.solvers.maxsat.MaxSATReader.readCnfToSolver;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.TimeoutMaxSATHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.testutils.PigeonHoleGenerator;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 2.4.0
 * @since 1.0
 */
public class PartialMaxSATTest extends TestWithExampleFormulas {

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
        this.logStream = new PrintStream("src/test/resources/partialmaxsat/log.txt");
    }

    @Test
    public void testExceptionalBehaviorForMSU3() {
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.msu3(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 2);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently algorithm MSU3 does not support weighted MaxSAT instances.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.msu3(this.f, MaxSATConfig.builder()
                    .incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                    .cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER)
                    .build());
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently iterative encoding in MSU3 only supports the Totalizer encoding.");
    }

    @Test
    public void testWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
                readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testOLL() throws IOException {
        for (int i = 0; i < files.length; i++) {
            final MaxSATSolver solver = MaxSATSolver.oll(this.f);
            readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(results[i]);
        }
    }

    @Test
    public void testIncWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.incWBO(this.f, config);
                readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testLinearSU() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[4];
        configs[0] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        configs[3] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.linearSU(this.f, config);
                readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testLinearUS() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.linearUS(this.f, config);
                readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testMSU3() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (int i = 0; i < files.length; i++) {
                final MaxSATSolver solver = MaxSATSolver.msu3(this.f, config);
                readCnfToSolver(solver, "src/test/resources/partialmaxsat/" + files[i]);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(results[i]);
            }
        }
    }

    @Test
    public void testTimeoutHandlerWBO() {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerIncWBO() {
        final MaxSATConfig[] configs = new MaxSATConfig[1];
        configs[0] = MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerLinearSU() {
        final MaxSATConfig[] configs = new MaxSATConfig[4];
        configs[0] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(false).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        configs[3] = MaxSATConfig.builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).bmo(true).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerLinearUS() {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            testTimeoutHandler(solver);
        }
    }

    @Test
    public void testTimeoutHandlerMSU3() {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(MaxSATConfig.CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            testTimeoutHandler(solver);
        }
    }

    private void testTimeoutHandler(final MaxSATSolver solver) {
        final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(1000L);

        final PigeonHoleGenerator pg = new PigeonHoleGenerator(this.f);
        final Formula formula = pg.generate(10);
        solver.addHardFormula(formula);
        solver.addSoftFormula(this.f.or(formula.variables()), 1);
        MaxSAT.MaxSATResult result = solver.solve(handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.UNDEF);

        solver.reset();
        solver.addHardFormula(this.IMP1);
        solver.addSoftFormula(this.AND1, 1);
        result = solver.solve(handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
    }

    @Test
    public void testTimeoutHandlerSimple() throws IOException {
        MaxSATSolver solver = MaxSATSolver.wbo(this.f, MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build());
        readCnfToSolver(solver, "src/test/resources/partialmaxsat/c1355_F176gat-1278gat@1.wcnf");
        TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(1000L);
        MaxSAT.MaxSATResult result = solver.solve(handler);
        assertThat(handler.aborted()).isTrue();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.UNDEF);
        assertThat(handler.lowerBoundApproximation()).isLessThan(13);

        solver = MaxSATSolver.wbo(this.f, MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build());
        readCnfToSolver(solver, "src/test/resources/partialmaxsat/c1355_F1229gat@1.wcnf");
        handler = new TimeoutMaxSATHandler(5000L);
        result = solver.solve(handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
    }

    @Test
    public void testTimeoutHandlerUB() throws IOException {
        final MaxSATSolver solver = MaxSATSolver.linearSU(this.f, MaxSATConfig.builder().verbosity(SOME).output(this.logStream).build());
        readCnfToSolver(solver, "src/test/resources/partialmaxsat/c1355_F1229gat@1.wcnf");
        final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(5000L);
        final MaxSAT.MaxSATResult result = solver.solve(handler);
        assertThat(handler.aborted()).isFalse();
        assertThat(result).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
        assertThat(solver.result()).isEqualTo(handler.upperBoundApproximation());
    }

    @Test
    public void testNonClauselSoftConstraints() throws ParserException {
        final MaxSATSolver[] solvers = new MaxSATSolver[2];
        solvers[0] = MaxSATSolver.msu3(this.f);
        solvers[1] = MaxSATSolver.linearUS(this.f);
        for (final MaxSATSolver solver : solvers) {
            solver.addHardFormula(this.f.parse("a & b & c"));
            solver.addSoftFormula(this.f.parse("~a & ~b & ~c"), 1);
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.model().literals()).containsExactlyInAnyOrder(
                    this.f.variable("a"), this.f.variable("b"), this.f.variable("c")
            );
            assertThat(solver.result()).isEqualTo(1);
        }
    }

    @Test
    public void testSoftConstraintsCornerCaseVerum() throws ParserException {
        final MaxSATSolver[] solvers = new MaxSATSolver[2];
        solvers[0] = MaxSATSolver.msu3(this.f);
        solvers[1] = MaxSATSolver.linearUS(this.f);
        for (final MaxSATSolver solver : solvers) {
            solver.addHardFormula(this.f.parse("a & b & c"));
            solver.addSoftFormula(this.f.parse("$true"), 1);
            solver.addSoftFormula(this.f.parse("~a & ~b & ~c"), 1);
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.model().literals()).containsExactlyInAnyOrder(
                    this.f.variable("a"), this.f.variable("b"), this.f.variable("c")
            );
            assertThat(solver.result()).isEqualTo(1);
        }
    }

    @Test
    public void testSoftConstraintsCornerCaseFalsum() throws ParserException {
        final MaxSATSolver[] solvers = new MaxSATSolver[2];
        solvers[0] = MaxSATSolver.msu3(this.f);
        solvers[1] = MaxSATSolver.linearUS(this.f);
        for (final MaxSATSolver solver : solvers) {
            solver.addHardFormula(this.f.parse("a & b & c"));
            solver.addSoftFormula(this.f.parse("$false"), 1);
            solver.addSoftFormula(this.f.parse("~a & ~b & ~c"), 1);
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.model().literals()).containsExactlyInAnyOrder(
                    this.f.variable("a"), this.f.variable("b"), this.f.variable("c")
            );
            assertThat(solver.result()).isEqualTo(2);
        }
    }
}
