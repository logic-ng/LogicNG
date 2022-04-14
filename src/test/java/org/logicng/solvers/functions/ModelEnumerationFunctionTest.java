package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.List;

/**
 * Units tests for {@link ModelEnumerationFunction}.
 * @version 2.3.0
 * @since 2.3.0
 */
public class ModelEnumerationFunctionTest {

    private final FormulaFactory f;

    public ModelEnumerationFunctionTest() {
        this.f = new FormulaFactory();
    }

    @Test
    public void testModelEnumerationSimple() throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder().build());
        assertThat(models).containsExactlyInAnyOrder(
                new Assignment(this.f.variable("A"), this.f.variable("B"), this.f.variable("C")),
                new Assignment(this.f.variable("A"), this.f.variable("B"), this.f.literal("C", false)),
                new Assignment(this.f.variable("A"), this.f.literal("B", false), this.f.variable("C"))
        );
    }

    @Test
    public void testFastEvaluable() throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A & (B | C)"));
        List<Assignment> models = solver.execute(ModelEnumerationFunction.builder().build());
        assertThat(models).extracting(Assignment::fastEvaluable).containsOnly(false);
        models = solver.execute(ModelEnumerationFunction.builder().fastEvaluable(false).build());
        assertThat(models).extracting(Assignment::fastEvaluable).containsOnly(false);
        models = solver.execute(ModelEnumerationFunction.builder().fastEvaluable(true).build());
        assertThat(models).extracting(Assignment::fastEvaluable).containsOnly(true);
    }

    @Test
    public void testModelEnumerationWithSplit() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(13).build());
        final Formula formula = randomizer.formula(4);
        solver.add(formula);
        final long t1 = System.currentTimeMillis();
        final List<Assignment> modelsWithSplit = solver.execute(
                ModelEnumerationFunction.builder().variables(f.variable("v01"), f.variable("v02"), f.variable("v03")).computeWithSplit(true).build());
        System.out.println(modelsWithSplit);

        final long t2 = System.currentTimeMillis();
        final List<Assignment> modelsWithoutSplit = solver.execute(
                ModelEnumerationFunction.builder().variables(f.variable("v01"), f.variable("v02"), f.variable("v03")).computeWithSplit(false).build());
        final long t3 = System.currentTimeMillis();

        System.out.println(modelsWithoutSplit);

        System.out.println("time with split: " + (t2 - t1));
        System.out.println("time without split: " + (t3 - t2));
        assertThat(modelsWithSplit).containsAll(modelsWithoutSplit);
        assertThat(modelsWithoutSplit).containsAll(modelsWithSplit);
    }

    @Test
    public void testModelEnumerationSplit() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(13).build());
        final Formula formula = randomizer.formula(5);
        solver.add(formula);
        final long t1 = System.currentTimeMillis();
        final List<Assignment> modelsWithSplit = solver.execute(ModelEnumerationFunction.builder().computeWithSplit(true).build());
        System.out.println(modelsWithSplit);
        final long t2 = System.currentTimeMillis();
        System.out.println("time with split: " + (t2 - t1));
    }

    @Test
    public void testModelEnumerationNoSplit() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(10).build());
        final Formula formula = randomizer.formula(5);
        solver.add(formula);
        final long t1 = System.currentTimeMillis();
        final List<Assignment> modelsWithSplit = solver.execute(ModelEnumerationFunction.builder().computeWithSplit(false).build());
        final long t2 = System.currentTimeMillis();

        System.out.println("time without split: " + (t2 - t1));
    }
}
