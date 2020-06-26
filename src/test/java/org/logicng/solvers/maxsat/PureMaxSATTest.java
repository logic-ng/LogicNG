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

import org.assertj.core.data.Offset;
import org.junit.jupiter.api.Test;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 2.0.0
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
        this.logStream = new PrintStream("src/test/resources/maxsat/log.txt");
    }

    @Test
    public void testWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.wbo(config);
                readCNF(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testIncWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.incWBO(config);
                readCNF(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testLinearSU() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.linearSU(config);
                readCNF(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testLinearUS() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.linearUS(config);
                readCNF(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testMSU3() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE).cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] = MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.msu3(config);
                readCNF(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(config);
            readCNF(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testSingle() throws IOException {
        final MaxSATSolver solver = MaxSATSolver.incWBO(MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
                .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(this.logStream).build());
        readCNF(solver, "src/test/resources/maxsat/c-fat200-2.clq.cnf");
        assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
        assertThat(solver.result()).isEqualTo(26);
        final MaxSAT.Stats stats = solver.stats();
        assertThat(stats.bestSolution()).isEqualTo(26);
        assertThat(stats.unsatCalls()).isEqualTo(26);
        assertThat(stats.satCalls()).isEqualTo(2);
        assertThat(stats.averageCoreSize()).isEqualTo(29.46, Offset.offset(0.01));
        assertThat(stats.symmetryClauses()).isEqualTo(24449);
        assertThat(stats.toString()).isEqualTo("MaxSAT.Stats{best solution=26, #sat calls=2, #unsat calls=26, average core size=29.46, #symmetry clauses=24449}");
    }

    @Test
    public void testAssignment() throws ParserException {
        final MaxSATSolver solver = MaxSATSolver.incWBO(MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
                .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(this.logStream).build());
        final PropositionalParser p = new PropositionalParser(this.f);
        solver.addSoftFormula(p.parse("a => b"), 1);
        solver.addSoftFormula(p.parse("b => c"), 1);
        solver.addSoftFormula(p.parse("c => d"), 1);
        solver.addSoftFormula(p.parse("d => e"), 1);
        solver.addSoftFormula(p.parse("a => x"), 1);
        solver.addSoftFormula(p.parse("~e"), 1);
        solver.addSoftFormula(p.parse("~x"), 1);
        solver.addSoftFormula(p.parse("a"), 1);
        assertThat(solver.solve()).isEqualTo(MaxSAT.MaxSATResult.OPTIMUM);
        assertThat(solver.result()).isEqualTo(1);
        final Assignment model = solver.model();
        assertThat(model.size()).isEqualTo(6);
        assertThat(model.positiveVariables().size()).isEqualTo(0);
        assertThat(model.negativeLiterals().size()).isEqualTo(6);
        assertThat(model.negativeLiterals().contains(this.f.literal("a", false))).isTrue();
        assertThat(model.negativeLiterals().contains(this.f.literal("b", false))).isTrue();
        assertThat(model.negativeLiterals().contains(this.f.literal("c", false))).isTrue();
        assertThat(model.negativeLiterals().contains(this.f.literal("d", false))).isTrue();
        assertThat(model.negativeLiterals().contains(this.f.literal("e", false))).isTrue();
        assertThat(model.negativeLiterals().contains(this.f.literal("x", false))).isTrue();
    }

    @Test
    public void testIllegalModel() throws ParserException {
        final MaxSATSolver solver = MaxSATSolver.incWBO(MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
                .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(this.logStream).build());
        final PropositionalParser p = new PropositionalParser(this.f);
        solver.addSoftFormula(p.parse("a => b"), 1);
        solver.addSoftFormula(p.parse("b => c"), 1);
        solver.addSoftFormula(p.parse("c => d"), 1);
        solver.addSoftFormula(p.parse("d => e"), 1);
        solver.addSoftFormula(p.parse("a => x"), 1);
        solver.addSoftFormula(p.parse("~e"), 1);
        solver.addSoftFormula(p.parse("~x"), 1);
        assertThatThrownBy(solver::model).isInstanceOf(IllegalStateException.class);
    }

    @Test
    public void testToString() {
        final MaxSATSolver[] solvers = new MaxSATSolver[6];
        solvers[0] = MaxSATSolver.incWBO();
        solvers[1] = MaxSATSolver.linearSU();
        solvers[2] = MaxSATSolver.linearUS();
        solvers[3] = MaxSATSolver.msu3();
        solvers[4] = MaxSATSolver.wbo();
        solvers[5] = MaxSATSolver.wmsu3();

        final String expected = "MaxSATSolver{result=OPTIMUM, var2index={a=0, b=1}}";

        for (int i = 0; i < 6; i++) {
            final MaxSATSolver s = solvers[i];
            s.addHardFormula(F.OR3);
            s.addSoftFormula(F.A, 1);
            if (i == 2 || i == 3) {
                s.addSoftFormula(F.NA, 1);
            } else {
                s.addSoftFormula(F.NA, 2);
            }
            s.solve();
            assertThat(s.toString()).isEqualTo(expected);
        }
    }

    private void readCNF(final MaxSATSolver solver, final String fileName) throws IOException {
        final BufferedReader reader = new BufferedReader(new FileReader(fileName));
        boolean cont = true;
        while (reader.ready() && cont) {
            final String line = reader.readLine().trim();
            if (line.startsWith("p")) {
                cont = false;
            }

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
                    final int parsedLit = Integer.parseInt(tokens[i]);
                    final String var = "v" + Math.abs(parsedLit);
                    literals.add(parsedLit > 0 ? this.f.literal(var, true) : this.f.literal(var, false));
                }
            }
            solver.addSoftFormula(this.f.or(literals), 1);
        }
    }
}
