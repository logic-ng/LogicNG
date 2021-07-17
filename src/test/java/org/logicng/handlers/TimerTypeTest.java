package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;

/**
 * Unit tests for {@link TimeoutHandler.TimerType}.
 * @version 2.1.0
 * @since 2.1.0
 */
public class TimerTypeTest {

    @Test
    public void testSingleTimeout() throws InterruptedException {
        for (final TimeoutHandler handler : mkHandlers(100L, TimeoutHandler.TimerType.SINGLE_TIMEOUT)) {
            handler.started();
            assertThat(handler.timeLimitExceeded()).isFalse();
            Thread.sleep(200L);
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    @Test
    public void testRestartingTimeout() throws InterruptedException {
        for (final TimeoutHandler handler : mkHandlers(100L, TimeoutHandler.TimerType.RESTARTING_TIMEOUT)) {
            handler.started();
            assertThat(handler.timeLimitExceeded()).isFalse();
            Thread.sleep(200L);
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isFalse();
            Thread.sleep(200L);
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    @Test
    public void testFixedEnd() throws InterruptedException {
        for (final Supplier<TimeoutHandler> handlerSupplier : mkFixedEndHandlers(100L)) {
            final TimeoutHandler handler = handlerSupplier.get();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isFalse();
            Thread.sleep(200L);
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    @Test
    public void testFixedEndWithDelayedStartedCall() throws InterruptedException {
        for (final Supplier<TimeoutHandler> handlerSupplier : mkFixedEndHandlers(100L)) {
            final TimeoutHandler handler = handlerSupplier.get();
            Thread.sleep(200L);
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    @Test
    public void testFixedEndWithZeroTime() {
        for (final Supplier<TimeoutHandler> handlerSupplier : mkFixedEndHandlers(0L)) {
            final TimeoutHandler handler = handlerSupplier.get();
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    @Test
    public void testFixedEndWithPastPointInTime() {
        for (final Supplier<TimeoutHandler> handlerSupplier : mkFixedEndHandlers(-100L)) {
            final TimeoutHandler handler = handlerSupplier.get();
            assertThat(handler.timeLimitExceeded()).isTrue();
            handler.started();
            assertThat(handler.timeLimitExceeded()).isTrue();
        }
    }

    private static List<TimeoutHandler> mkHandlers(final long timeout, final TimeoutHandler.TimerType type) {
        return Arrays.asList(
                new TimeoutBDDHandler(timeout, type),
                new TimeoutMaxSATHandler(timeout, type),
                new TimeoutModelEnumerationHandler(timeout, type),
                new TimeoutOptimizationHandler(timeout, type),
                new TimeoutSATHandler(timeout, type));
    }

    /**
     * Constructs fixed timeout handlers by using suppliers, since the creation of the fixed timeout handler affects the duration of the timeout.
     * @param delta the delta in milliseconds added to the current point in time when the handler is created
     * @return the fixed timeout handlers as suppliers
     */
    private static List<Supplier<TimeoutHandler>> mkFixedEndHandlers(final long delta) {
        return Arrays.asList(
                () -> new TimeoutBDDHandler(System.currentTimeMillis() + delta, TimeoutHandler.TimerType.FIXED_END),
                () -> new TimeoutMaxSATHandler(System.currentTimeMillis() + delta, TimeoutHandler.TimerType.FIXED_END),
                () -> new TimeoutModelEnumerationHandler(System.currentTimeMillis() + delta, TimeoutHandler.TimerType.FIXED_END),
                () -> new TimeoutOptimizationHandler(System.currentTimeMillis() + delta, TimeoutHandler.TimerType.FIXED_END),
                () -> new TimeoutSATHandler(System.currentTimeMillis() + delta, TimeoutHandler.TimerType.FIXED_END));
    }
}
