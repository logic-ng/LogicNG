package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.DimacsReader;
import org.logicng.solvers.MaxSATSolver;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.testutils.PigeonHoleGenerator;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@ExtendWith(MockitoExtension.class)
class TimeoutMaxSATHandlerTest {

    private FormulaFactory f;
    private List<MaxSATSolver> solvers;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.solvers = Arrays.asList(
                MaxSATSolver.incWBO(),
                MaxSATSolver.wbo(),
                MaxSATSolver.linearSU(),
                MaxSATSolver.linearUS(),
                MaxSATSolver.msu3(),
                MaxSATSolver.wmsu3()
        );
    }

    @Test
    public void testTimeoutForLowerBound() throws InterruptedException {
        final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.foundLowerBound(1, new Assignment())).isTrue();
        Thread.sleep(200);
        assertThat(handler.foundLowerBound(1, new Assignment())).isFalse();
    }

    @Test
    public void testTimeoutForUpperBound() throws InterruptedException {
        final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.foundUpperBound(1, new Assignment())).isTrue();
        Thread.sleep(200);
        assertThat(handler.foundUpperBound(1, new Assignment())).isFalse();
    }

    @Test
    public void testThatMethodsAreCalled() throws ParserException {
        for (final MaxSATSolver solver : this.solvers) {
            final int weight = solver.isWeighted() ? 2 : 1;
            solver.addHardFormula(this.f.parse("A&B"));
            solver.addSoftFormula(this.f.parse("~A"), weight);
            solver.addSoftFormula(this.f.parse("~B"), weight);
            final TimeoutMaxSATHandler handler = Mockito.mock(TimeoutMaxSATHandler.class);
            solver.solve(handler);

            verify(handler, times(1)).started();
            verify(handler, atLeast(1)).satHandler();
            verify(handler, times(1)).finishedSolving();
        }
    }

    @Test
    public void testThatSatHandlerIsHandledProperly() throws IOException {
        final List<Formula> formulas = DimacsReader.readCNF("src/test/resources/sat/unsat/pret60_40.cnf", this.f);
        for (final MaxSATSolver solver : this.solvers) {
            final int weight = solver.isWeighted() ? 2 : 1;
            formulas.forEach(c -> solver.addSoftFormula(c, weight));
            final TimeoutSATHandler satHandler = Mockito.mock(TimeoutSATHandler.class);
            final TimeoutMaxSATHandler handler = Mockito.mock(TimeoutMaxSATHandler.class);
            when(handler.satHandler()).thenReturn(satHandler);
            lenient().when(handler.foundLowerBound(anyInt(), any())).thenReturn(true);
            lenient().when(handler.foundUpperBound(anyInt(), any())).thenReturn(true);
            final AtomicInteger count = new AtomicInteger(0);
            when(satHandler.detectedConflict()).thenReturn(true);
            when(satHandler.aborted()).then(invocationOnMock -> count.addAndGet(1) > 2);

            final MaxSAT.MaxSATResult solve = solver.solve(handler);

            assertThat(solve).isEqualTo(MaxSAT.MaxSATResult.UNDEF);

            verify(handler, times(1)).started();
            verify(handler, atLeast(1)).satHandler();
            verify(handler, times(1)).finishedSolving();
            verify(satHandler, times(3)).aborted();
        }
    }

    @Test
    public void testTimeoutHandlerSingleTimeout() throws IOException {
        final List<Formula> formulas = DimacsReader.readCNF("src/test/resources/sat/too_large_gr_rcs_w5.shuffled.cnf", this.f);
        for (final MaxSATSolver solver : this.solvers) {
            final int weight = solver.isWeighted() ? 2 : 1;
            formulas.forEach(c -> solver.addSoftFormula(c, weight));
            final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(10L);

            final MaxSAT.MaxSATResult solve = solver.solve(handler);

            assertThat(handler.aborted()).isTrue();
            assertThat(solve).isEqualTo(MaxSAT.MaxSATResult.UNDEF);
        }
    }

    @Test
    public void testTimeoutHandlerFixedEnd() {
        final Formula ph = new PigeonHoleGenerator(this.f).generate(10);
        for (final MaxSATSolver solver : this.solvers) {
            final int weight = solver.isWeighted() ? 2 : 1;
            ph.forEach(c -> solver.addSoftFormula(c, weight));
            final TimeoutMaxSATHandler handler = new TimeoutMaxSATHandler(System.currentTimeMillis() + 100L, TimeoutHandler.TimerType.FIXED_END);

            final MaxSAT.MaxSATResult solve = solver.solve(handler);

            assertThat(handler.aborted()).isTrue();
            assertThat(solve).isEqualTo(MaxSAT.MaxSATResult.UNDEF);
        }
    }
}
