package org.logicng.handlers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.util.CollectionHelper.union;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.ModelCountingFunction;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.DefaultAdvancedModelEnumerationStrategy;

import java.math.BigInteger;
import java.util.SortedSet;
import java.util.TreeSet;

public class AdvancedModelEnumerationHandlerTest {

    FormulaFactory f;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
    }

    @Test
    public void testTimeoutHandler() throws ParserException, InterruptedException {
        final MiniSat solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A | B | C | D | E | F | G | H | I | J | K | L | N | M | O | P | Q | R | S | T | U | V | W"));
        final TimeoutAdvancedModelEnumerationHandler handler = new TimeoutAdvancedModelEnumerationHandler(100);
        final ModelCountingFunction enumeration = ModelCountingFunction.builder().configuration(AdvancedModelEnumerationConfig.builder().handler(handler).build()).build();

        Thread.sleep(150);
        assertThat(handler.aborted()).isFalse();

        final long start = System.currentTimeMillis();
        solver.execute(enumeration);
        final long finish = System.currentTimeMillis();
        final long timeElapsed = finish - start;

        // Should be very unlikely that the formula can be fully enumerated in 100ms.
        // Thus, we expect the handler to stop the execution.
        assertThat(handler.aborted()).isTrue();
        assertThat(timeElapsed).isGreaterThanOrEqualTo(100L);
    }

    @Test
    public void testNumberOfModelsHandler() throws ParserException {
        final Formula formula = this.f.parse("A | B | C | D | E | F | G | H | I | J | K | L | N | M | O | P | Q | R | S | T | U | V | W");
        final SortedSet<Variable> vars = union(formula.variables(), this.f.variables("X", "Y", "Z"), TreeSet::new);
        for (int i = 1; i <= 1000; i += 7) {
            final MiniSat solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            final AdvancedNumberOfModelsHandler handler = new AdvancedNumberOfModelsHandler(i);
            final ModelCountingFunction enumeration = ModelCountingFunction.builder()
                    .variables(vars).configuration(AdvancedModelEnumerationConfig.builder().handler(handler).strategy(
                                    DefaultAdvancedModelEnumerationStrategy.builder().maxNumberOfModels(200).build()
                            ).build()
                    ).build();
            final BigInteger numberOfModels = solver.execute(enumeration);
            assertThat(handler.aborted()).isTrue();
            assertThat(numberOfModels.longValueExact()).isLessThanOrEqualTo(i);
        }
    }
}
