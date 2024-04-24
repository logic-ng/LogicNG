package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
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
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.testutils.PigeonHoleGenerator;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@ExtendWith(MockitoExtension.class)
class TimeoutModelEnumerationHandlerTest {

    private FormulaFactory f;
    private PigeonHoleGenerator pg;
    private SATSolver[] solvers;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.pg = new PigeonHoleGenerator(f);
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
    public void testFoundModel() throws InterruptedException {
        final TimeoutModelEnumerationHandler handler = new TimeoutModelEnumerationHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.foundModel(new Assignment())).isTrue();
        Thread.sleep(200);
        assertThat(handler.foundModel(new Assignment())).isFalse();
    }

    @Test
    public void testThatMethodsAreCalled() throws ParserException {
        final Formula formula = f.parse("A & B | C");
        for (final SATSolver solver : this.solvers) {
            solver.add(formula);
            final TimeoutModelEnumerationHandler handler = Mockito.mock(TimeoutModelEnumerationHandler.class);
            final ModelEnumerationFunction me = ModelEnumerationFunction.builder().handler(handler).variables(formula.variables()).build();

            solver.execute(me);

            verify(handler, times(1)).started();
            verify(handler, times(1)).satHandler();
        }
    }

    @Test
    public void testThatSatHandlerIsHandledProperly() {
        final Formula formula = pg.generate(10).negate();
        for (final SATSolver solver : this.solvers) {
            solver.add(formula);
            final TimeoutSATHandler satHandler = Mockito.mock(TimeoutSATHandler.class);
            final TimeoutModelEnumerationHandler handler = Mockito.mock(TimeoutModelEnumerationHandler.class);
            final AtomicInteger count = new AtomicInteger(0);
            when(handler.satHandler()).then(invocationOnMock -> {
                count.addAndGet(1);
                return satHandler;
            });
            when(handler.foundModel(any())).thenReturn(true);
            when(handler.aborted()).then(invocationOnMock -> count.get() > 5);
            lenient().when(satHandler.detectedConflict()).thenReturn(true);
            final ModelEnumerationFunction me = ModelEnumerationFunction.builder().handler(handler).variables(formula.variables()).build();

            solver.execute(me);

            verify(handler, times(1)).started();
            verify(handler, times(6)).satHandler();
        }
    }

    @Test
    public void testTimeoutHandlerSingleTimeout() {
        final Formula formula = pg.generate(10).negate();
        for (final SATSolver solver : this.solvers) {
            solver.add(formula);
            final TimeoutModelEnumerationHandler handler = new TimeoutModelEnumerationHandler(100L);
            final ModelEnumerationFunction me = ModelEnumerationFunction.builder().handler(handler).variables(formula.variables()).build();

            final List<Assignment> result = solver.execute(me);

            assertThat(handler.aborted).isTrue();
            assertThat(result).isNotNull(); // assignments found so far are returned
        }
    }

    @Test
    public void testTimeoutHandlerFixedEnd() {
        final Formula formula = pg.generate(10).negate();
        for (final SATSolver solver : this.solvers) {
            solver.add(formula);
            final TimeoutModelEnumerationHandler handler = new TimeoutModelEnumerationHandler(System.currentTimeMillis() + 100L, TimeoutHandler.TimerType.FIXED_END);
            final ModelEnumerationFunction me = ModelEnumerationFunction.builder().handler(handler).variables(formula.variables()).build();

            final List<Assignment> result = solver.execute(me);

            assertThat(handler.aborted).isTrue();
            assertThat(result).isNotNull(); // assignments found so far are returned
        }
    }
}
