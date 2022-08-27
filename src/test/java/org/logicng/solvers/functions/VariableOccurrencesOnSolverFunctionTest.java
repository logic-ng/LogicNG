package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

/**
 * Unit tests for {@link VariableOccurrencesOnSolverFunction}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class VariableOccurrencesOnSolverFunctionTest extends TestWithExampleFormulas {

    protected final Variable E = this.f.variable("e");
    protected final Variable G = this.f.variable("g");
    protected final Variable H = this.f.variable("h");
    protected final Variable I = this.f.variable("i");
    protected final Variable J = this.f.variable("j");

    @Test
    public void testWithEmptySolver() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        assertThat(solver.execute(new VariableOccurrencesOnSolverFunction())).isEmpty();
        final Map<Variable, Integer> countWithRelevantVariables = solver.execute(
                new VariableOccurrencesOnSolverFunction(new HashSet<>(Arrays.asList(this.A, this.B, this.C))));
        assertThat(countWithRelevantVariables).hasSize(3);
        assertThat(countWithRelevantVariables).containsKeys(this.A, this.B, this.C);
        assertThat(countWithRelevantVariables.values()).containsOnly(0);
    }

    @Test
    public void testWithAllVariables() throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(a | b | c) & (~b | c) & (d | ~e) & x & (~a | e) & (a | d | b | g | h) & (~h | i) & y"));
        final Map<Variable, Integer> counts = solver.execute(new VariableOccurrencesOnSolverFunction());
        assertThat(counts).hasSize(10);
        assertThat(counts).containsEntry(this.A, 3);
        assertThat(counts).containsEntry(this.B, 3);
        assertThat(counts).containsEntry(this.C, 2);
        assertThat(counts).containsEntry(this.D, 2);
        assertThat(counts).containsEntry(this.E, 2);
        assertThat(counts).containsEntry(this.G, 1);
        assertThat(counts).containsEntry(this.H, 2);
        assertThat(counts).containsEntry(this.I, 1);
        assertThat(counts).containsEntry(this.X, 1);
        assertThat(counts).containsEntry(this.Y, 1);
    }

    @Test
    public void testWithRelevantVariables() throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("(a | b | c) & (~b | c) & (d | ~e) & x & (~a | e) & (a | d | b | g | h) & (~h | i) & y"));
        final Map<Variable, Integer> counts = solver.execute(
                new VariableOccurrencesOnSolverFunction(new HashSet<>(Arrays.asList(this.A, this.C, this.X, this.J))));
        assertThat(counts).hasSize(4);
        assertThat(counts).containsEntry(this.A, 3);
        assertThat(counts).containsEntry(this.C, 2);
        assertThat(counts).containsEntry(this.X, 1);
        assertThat(counts).containsEntry(this.J, 0);
    }
}
