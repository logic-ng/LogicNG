package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrdering;
import org.logicng.knowledgecompilation.bdds.orderings.VariableOrderingProvider;
import org.logicng.testutils.PigeonHoleGenerator;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.atomic.AtomicInteger;

@ExtendWith(MockitoExtension.class)
class TimeoutBDDHandlerTest {

    private FormulaFactory f;
    private PigeonHoleGenerator pg;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.pg = new PigeonHoleGenerator(this.f);
    }

    @Test
    public void testNewRefAdded() throws InterruptedException {
        final TimeoutBDDHandler handler = new TimeoutBDDHandler(100, TimeoutHandler.TimerType.SINGLE_TIMEOUT);
        handler.started();
        assertThat(handler.newRefAdded()).isTrue();
        Thread.sleep(200);
        assertThat(handler.newRefAdded()).isFalse();
    }

    @Test
    public void testThatMethodsAreCalled() throws ParserException {
        final Formula formula = f.parse("(A => ~B) & ((A & C) | (D & ~C)) & (A | Y | X)");
        final VariableOrderingProvider provider = VariableOrdering.BFS.provider();
        final BDDKernel kernel = new BDDKernel(this.f, provider.getOrder(formula), 100, 100);
        final TimeoutBDDHandler handler = Mockito.mock(TimeoutBDDHandler.class);

        BDDFactory.build(formula, kernel, handler);

        verify(handler, times(1)).started();
        verify(handler, atLeast(1)).newRefAdded();
    }

    @Test
    public void testThatNewRefAddedHandledProperly() throws ParserException {
        final Formula formula = f.parse("(A => ~B) & ((A & C) | ~(D & ~C)) & (A | Y | X)");
        final VariableOrderingProvider provider = VariableOrdering.BFS.provider();
        final BDDKernel kernel = new BDDKernel(this.f, provider.getOrder(formula), 100, 100);
        final TimeoutBDDHandler handler = Mockito.mock(TimeoutBDDHandler.class);
        final AtomicInteger count = new AtomicInteger(0);
        when(handler.newRefAdded()).then(invocationOnMock -> count.addAndGet(1) < 5);

        final BDD result = BDDFactory.build(formula, kernel, handler);

        assertThat(result).isEqualTo(new BDD(BDDKernel.BDD_ABORT, kernel));

        verify(handler, times(1)).started();
        verify(handler, times(5)).newRefAdded();
    }

    @Test
    public void testTimeoutHandlerSingleTimeout() {
        final Formula formula = pg.generate(10);
        final VariableOrderingProvider provider = VariableOrdering.BFS.provider();
        final BDDKernel kernel = new BDDKernel(this.f, provider.getOrder(formula), 100, 100);
        final TimeoutBDDHandler handler = new TimeoutBDDHandler(100L);

        final BDD result = BDDFactory.build(formula, kernel, handler);

        assertThat(handler.aborted).isTrue();
        assertThat(result).isEqualTo(new BDD(BDDKernel.BDD_ABORT, kernel));
    }

    @Test
    public void testTimeoutHandlerFixedEnd() {
        final Formula formula = pg.generate(10);
        final VariableOrderingProvider provider = VariableOrdering.BFS.provider();
        final BDDKernel kernel = new BDDKernel(this.f, provider.getOrder(formula), 100, 100);
        final TimeoutBDDHandler handler = new TimeoutBDDHandler(System.currentTimeMillis() + 100L, TimeoutHandler.TimerType.FIXED_END);

        final BDD result = BDDFactory.build(formula, kernel, handler);

        assertThat(handler.aborted).isTrue();
        assertThat(result).isEqualTo(new BDD(BDDKernel.BDD_ABORT, kernel));
    }
}
