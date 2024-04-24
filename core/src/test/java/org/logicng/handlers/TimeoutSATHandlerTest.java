package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.testutils.PigeonHoleGenerator;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.atomic.AtomicInteger;

@ExtendWith(MockitoExtension.class)
class TimeoutSATHandlerTest {

    private FormulaFactory f;
    private PigeonHoleGenerator pg;
    private SATSolver[] solvers;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.pg = new PigeonHoleGenerator(this.f);
        this.solvers = new SATSolver[8];
        this.solvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.glucose(this.f, MiniSatConfig.builder().incremental(false).build(),
                GlucoseConfig.builder().build());
        this.solvers[3] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[4] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[5] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        this.solvers[6] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
        this.solvers[7] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
    }

    @Test
    public void testDetectedConflict() throws InterruptedException {
        final TimeoutSATHandler handler = new TimeoutSATHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.detectedConflict()).isTrue();
        Thread.sleep(200);
        assertThat(handler.detectedConflict()).isFalse();
    }

    @Test
    public void testThatMethodsAreCalled() throws ParserException {
        for (final SATSolver solver : this.solvers) {
            solver.add(this.f.parse("(x => y) & (~x => y) & (y => z) & (z => ~y)"));
            final TimeoutSATHandler handler = Mockito.mock(TimeoutSATHandler.class);

            solver.sat(handler);

            verify(handler, times(1)).started();
            verify(handler, atLeast(1)).detectedConflict();
            verify(handler, times(1)).finishedSolving();
        }
    }

    @Test
    public void testThatDetectedConflictIsHandledProperly() {
        for (final SATSolver solver : this.solvers) {
            solver.add(pg.generate(10));
            final TimeoutSATHandler handler = Mockito.mock(TimeoutSATHandler.class);
            final AtomicInteger count = new AtomicInteger(0);
            when(handler.detectedConflict()).then(invocationOnMock -> count.addAndGet(1) < 5);

            final Tristate result = solver.sat(handler);

            assertThat(result).isEqualTo(Tristate.UNDEF);

            verify(handler, times(1)).started();
            verify(handler, times(5)).detectedConflict();
            verify(handler, times(1)).finishedSolving();
        }
    }

    @Test
    public void testTimeoutHandlerSingleTimeout() {
        for (final SATSolver solver : this.solvers) {
            solver.add(pg.generate(10));
            final TimeoutSATHandler handler = new TimeoutSATHandler(100L);

            final Tristate result = solver.sat(handler);

            assertThat(handler.aborted).isTrue();
            assertThat(result).isEqualTo(Tristate.UNDEF);
        }
    }

    @Test
    public void testTimeoutHandlerFixedEnd() {
        for (final SATSolver solver : this.solvers) {
            solver.add(pg.generate(10));
            final TimeoutSATHandler handler = new TimeoutSATHandler(System.currentTimeMillis() + 100L, TimeoutHandler.TimerType.FIXED_END);

            final Tristate result = solver.sat(handler);

            assertThat(handler.aborted).isTrue();
            assertThat(result).isEqualTo(Tristate.UNDEF);
        }
    }
}
