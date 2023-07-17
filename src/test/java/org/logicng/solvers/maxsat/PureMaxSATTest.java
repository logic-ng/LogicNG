// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.solvers.maxsat;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.solvers.maxsat.MaxSATReader.readCnfToSolver;
import static org.logicng.solvers.maxsat.algorithms.MaxSAT.MaxSATResult.OPTIMUM;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity.SOME;

import org.assertj.core.data.Offset;
import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;

/**
 * Unit tests for the MaxSAT solvers.
 * @version 2.4.0
 * @since 1.0
 */
public class PureMaxSATTest extends TestWithExampleFormulas {

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
    public void testExceptionalBehavior() {
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.solve();
            solver.addHardFormula(this.B);
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage(
                        "The MaxSAT solver does currently not support an incremental interface.  Reset the solver.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.solve();
            solver.addSoftFormula(this.B, 1);
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage(
                        "The MaxSAT solver does currently not support an incremental interface.  Reset the solver.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, -1);
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("The weight of a formula must be > 0");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.result();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot get a result as long as the formula is not solved.  Call 'solver' first.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.model();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot get a model as long as the formula is not solved.  Call 'solver' first.");
    }

    @Test
    public void testExceptionalBehaviorForLinearUS() {
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.linearUS(this.f);
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 3);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently LinearUS does not support weighted MaxSAT instances.");
        assertThatThrownBy(() -> {
            final MaxSATSolver solver = MaxSATSolver.linearUS(this.f, MaxSATConfig.builder()
                    .incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                    .cardinality(CardinalityEncoding.MTOTALIZER)
                    .build());
            solver.addHardFormula(this.f.parse("a | b"));
            solver.addSoftFormula(this.A, 1);
            solver.solve();
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Error: Currently iterative encoding in LinearUS only supports the Totalizer encoding.");
    }

    @Test
    public void testCornerCase() throws ParserException {
        final MaxSATSolver solver = MaxSATSolver.incWBO(this.f);
        solver.addHardFormula(this.f.parse("a | b"));
        solver.addHardFormula(this.f.verum());
        solver.addSoftFormula(this.A, 1);
        MaxSAT.MaxSATResult result = solver.solve();
        assertThat(result).isEqualTo(OPTIMUM);
        result = solver.solve();
        assertThat(result).isEqualTo(OPTIMUM);
    }

    @Test
    public void testWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME)
                .output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME)
                .output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
                readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testIncWBO() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(true).verbosity(SOME)
                .output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().weight(MaxSATConfig.WeightStrategy.NONE).symmetry(false).verbosity(SOME)
                .output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.incWBO(this.f, config);
                readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testLinearSU() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[2];
        configs[0] = MaxSATConfig.builder().cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME)
                .output(this.logStream).build();
        configs[1] = MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME)
                .output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.linearSU(this.f, config);
                readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testLinearUS() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE)
                        .cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE)
                        .cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                        .cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.linearUS(this.f, config);
                readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testMSU3() throws IOException {
        final MaxSATConfig[] configs = new MaxSATConfig[3];
        configs[0] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE)
                        .cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[1] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.NONE)
                        .cardinality(CardinalityEncoding.MTOTALIZER).verbosity(SOME).output(this.logStream).build();
        configs[2] =
                MaxSATConfig.builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
                        .cardinality(CardinalityEncoding.TOTALIZER).verbosity(SOME).output(this.logStream).build();
        for (final MaxSATConfig config : configs) {
            for (final String file : files) {
                final MaxSATSolver solver = MaxSATSolver.msu3(this.f, config);
                readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
                assertThat(solver.solve()).isEqualTo(OPTIMUM);
                assertThat(solver.result()).isEqualTo(1);
            }
            final MaxSATSolver solver = MaxSATSolver.wbo(this.f, config);
            readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(0);
        }
    }

    @Test
    public void testOLL() throws IOException {
        for (final String file : files) {
            final MaxSATSolver solver = MaxSATSolver.oll(this.f);
            readCnfToSolver(solver, "src/test/resources/maxsat/" + file);
            assertThat(solver.solve()).isEqualTo(OPTIMUM);
            assertThat(solver.result()).isEqualTo(1);
        }
        final MaxSATSolver solver = MaxSATSolver.oll(this.f);
        readCnfToSolver(solver, "src/test/resources/sat/9symml_gr_rcs_w6.shuffled.cnf");
        assertThat(solver.solve()).isEqualTo(OPTIMUM);
        assertThat(solver.result()).isEqualTo(0);
    }

    @Test
    public void testSingle() throws IOException {
        final MaxSATSolver solver =
                MaxSATSolver.incWBO(this.f, MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
                        .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(this.logStream).build());
        readCnfToSolver(solver, "src/test/resources/maxsat/c-fat200-2.clq.cnf");
        assertThat(solver.solve()).isEqualTo(OPTIMUM);
        assertThat(solver.result()).isEqualTo(26);
        final MaxSAT.Stats stats = solver.stats();
        assertThat(stats.bestSolution()).isEqualTo(26);
        assertThat(stats.unsatCalls()).isEqualTo(26);
        assertThat(stats.satCalls()).isEqualTo(2);
        assertThat(stats.averageCoreSize()).isEqualTo(31.88, Offset.offset(0.01));
        assertThat(stats.symmetryClauses()).isEqualTo(31150);
        assertThat(stats.toString()).isEqualTo(
                "MaxSAT.Stats{best solution=26, #sat calls=2, #unsat calls=26, average core size=31.88, #symmetry clauses=31150}");
    }

    @Test
    public void testAssignment() throws ParserException {
        final MaxSATSolver solver =
                MaxSATSolver.incWBO(this.f, MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
                        .solver(MaxSATConfig.SolverType.GLUCOSE).verbosity(SOME).output(this.logStream).build());
        final PropositionalParser p = new PropositionalParser(this.f);
        solver.addHardFormula(p.parse("y"));
        solver.addHardFormula(p.parse("~z"));
        solver.addSoftFormula(p.parse("a => b"), 1);
        solver.addSoftFormula(p.parse("b => c"), 1);
        solver.addSoftFormula(p.parse("c => d"), 1);
        solver.addSoftFormula(p.parse("d => e"), 1);
        solver.addSoftFormula(p.parse("a => x"), 1);
        solver.addSoftFormula(p.parse("~e"), 1);
        solver.addSoftFormula(p.parse("~x"), 1);
        solver.addSoftFormula(p.parse("a"), 1);
        solver.addSoftFormula(p.parse("~y"), 1);
        solver.addSoftFormula(p.parse("z"), 1);
        assertThat(solver.solve()).isEqualTo(OPTIMUM);
        assertThat(solver.result()).isEqualTo(3);
        final Assignment model = solver.model();
        assertThat(model.size()).isEqualTo(8);
        assertThat(model.positiveVariables()).hasSize(1);
        assertThat(model.positiveVariables()).extracting(Variable::name).containsExactlyInAnyOrder("y");
        assertThat(model.negativeLiterals()).hasSize(7);
        assertThat(model.negativeVariables()).extracting(Variable::name).containsExactlyInAnyOrder("a", "b", "c", "d",
                "e", "x", "z");
    }

    @Test
    public void testIllegalModel() throws ParserException {
        final MaxSATSolver solver =
                MaxSATSolver.incWBO(this.f, MaxSATConfig.builder().cardinality(CardinalityEncoding.MTOTALIZER)
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
        solvers[0] = MaxSATSolver.incWBO(this.f);
        solvers[1] = MaxSATSolver.linearSU(this.f);
        solvers[2] = MaxSATSolver.linearUS(this.f);
        solvers[3] = MaxSATSolver.msu3(this.f);
        solvers[4] = MaxSATSolver.wbo(this.f);
        solvers[5] = MaxSATSolver.wmsu3(this.f);

        final String expected = "MaxSATSolver{result=OPTIMUM, var2index={@SEL_SOFT_0=2, @SEL_SOFT_1=3, a=0, b=1}}";

        for (int i = 0; i < solvers.length; i++) {
            final MaxSATSolver solver = solvers[i];
            solver.addHardFormula(this.OR3);
            solver.addSoftFormula(this.A, 1);
            if (i == 2 || i == 3) {
                solver.addSoftFormula(this.NA, 1);
            } else {
                solver.addSoftFormula(this.NA, 2);
            }
            solver.solve();
            assertThat(solver.toString()).isEqualTo(expected);
        }
    }
}
