package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.DimacsReader;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.OptimizationFunction;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaHelper;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class TimeoutOptimizationHandlerTest {

    private FormulaFactory f;
    private SATSolver[] solvers;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
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
    public void testTimeoutFoundBetterBound() throws InterruptedException {
        final TimeoutOptimizationHandler handler = new TimeoutOptimizationHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.foundBetterBound(Assignment::new)).isTrue();
        Thread.sleep(200);
        assertThat(handler.foundBetterBound(Assignment::new)).isFalse();
    }

    @Test
    public void testThatMethodsAreCalled() throws ParserException {
        final Formula formula = f.parse("a & b & (~a => b)");
        for (final SATSolver solver : this.solvers) {
            solver.add(formula);
            final TimeoutOptimizationHandler handler = Mockito.mock(TimeoutOptimizationHandler.class);

            solver.execute(OptimizationFunction.builder()
                    .handler(handler)
                    .literals(formula.variables())
                    .maximize().build());

            verify(handler, times(1)).started();
            verify(handler, atLeast(1)).satHandler();
        }
    }

    @Test
    public void testTimeoutHandlerSingleTimeout() throws IOException {
        final List<Formula> formulas = DimacsReader.readCNF("src/test/resources/sat/too_large_gr_rcs_w5.shuffled.cnf", this.f);
        for (final SATSolver solver : this.solvers) {
            solver.add(formulas);
            final TimeoutOptimizationHandler handler = new TimeoutOptimizationHandler(100L);

            final Assignment result = solver.execute(OptimizationFunction.builder()
                    .handler(handler)
                    .literals(FormulaHelper.variables(formulas))
                    .maximize().build());

            assertThat(result).isNull();
        }
    }

    @Test
    public void testTimeoutHandlerFixedEnd() throws IOException {
        final List<Formula> formulas = DimacsReader.readCNF("src/test/resources/sat/too_large_gr_rcs_w5.shuffled.cnf", this.f);
        for (final SATSolver solver : this.solvers) {
            solver.add(formulas);
            final TimeoutOptimizationHandler handler = new TimeoutOptimizationHandler(100L, TimeoutHandler.TimerType.FIXED_END);

            final Assignment result = solver.execute(OptimizationFunction.builder()
                    .handler(handler)
                    .literals(FormulaHelper.variables(formulas))
                    .maximize().build());

            assertThat(result).isNull();
        }
    }
}
