package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.functions.FormulaDepthFunction;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
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
    public void testModelEnumerationWithVariables() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(10).build());
        final Formula formula = randomizer.formula(5);
        solver.add(formula);
        final long t1 = System.currentTimeMillis();
        final List<Assignment> modelsWithSplit = solver.execute(
                ModelEnumerationFunction.builder().variables(f.variable("v01"), f.variable("v02"), f.variable("v03")).enumerateWithSplit(true).build());
        System.out.println(modelsWithSplit);

        final long t2 = System.currentTimeMillis();
        final List<Assignment> modelsWithoutSplit = solver.execute(
                ModelEnumerationFunction.builder().variables(f.variable("v01"), f.variable("v02"), f.variable("v03")).enumerateWithSplit(false).build());
        final long t3 = System.currentTimeMillis();

        System.out.println(modelsWithoutSplit);

        System.out.println("time with split: " + (t2 - t1));
        System.out.println("time without split: " + (t3 - t2));
        assertThat(modelsWithSplit).containsAll(modelsWithoutSplit);
        assertThat(modelsWithoutSplit).containsAll(modelsWithSplit);
    }


    @Test
    public void testNewImplementation() {
        for (int i = 1; i <= 1000; i++) {
            // given
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(2);
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            // when
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 = solver.execute(ModelEnumerationFunction.builder().build());

            final long t2 = System.currentTimeMillis();
            final List<Assignment> models2 = solver.execute(ModelEnumerationFunction.builder().enumerateWithSplit(true).build());

            final long t3 = System.currentTimeMillis();
            final List<Assignment> models3 = solver.execute(ModelEnumerationFunction.builder().computeWithComponents(true).build());

            final long t4 = System.currentTimeMillis();
            final List<Assignment> models4 = solver.execute(ModelEnumerationFunction.builder().computeWithComponents(true).enumerateWithSplit(true).build());

            final long t5 = System.currentTimeMillis();

            System.out.println("Time no split, no components: " + (t2 - t1));
            System.out.println("Time split, no components: " + (t3 - t2));
            System.out.println("Time no split, components: " + (t4 - t3));
            System.out.println("Time split, component: " + (t5 - t4));
            // System.out.println("Models 1: " + models1);
            // System.out.println("Models 2: " + models2);
            // System.out.println("Models 3: " + models3);
            // System.out.println("Models 4: " + models4);


            // then
            assertThat(models1.containsAll(models2)).isTrue();
            assertThat(models1.containsAll(models3)).isTrue();
            assertThat(models1.containsAll(models4)).isTrue();

            assertThat(models2.containsAll(models1)).isTrue();
            assertThat(models2.containsAll(models3)).isTrue();
            assertThat(models2.containsAll(models4)).isTrue();

            assertThat(models3.containsAll(models1)).isTrue();
            assertThat(models3.containsAll(models2)).isTrue();
            assertThat(models3.containsAll(models4)).isTrue();

            assertThat(models4.containsAll(models1)).isTrue();
            assertThat(models4.containsAll(models2)).isTrue();
            assertThat(models4.containsAll(models3)).isTrue();
        }
    }


    @Test
    public void compareSplitCriteria() {
        for (int i = 1; i <= 30; i++) {

            // given
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            // when
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 = solver.execute(ModelEnumerationFunction.builder().computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.RANDOM).build());

            final long t2 = System.currentTimeMillis();
            System.out.println("Time random split: " + (t2 - t1));

            final List<Assignment> models2 = solver.execute(ModelEnumerationFunction.builder().computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.LEAST_COMMON_VARS).build());

            final long t3 = System.currentTimeMillis();
            System.out.println("Time prefer least common vars: " + (t3 - t2));

            final List<Assignment> models3 = solver.execute(ModelEnumerationFunction.builder().computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.MOST_COMMON_VARS).build());

            final long t4 = System.currentTimeMillis();
            System.out.println("Time prefer most common vars: " + (t4 - t3));

            // then
            assertThat(models1.containsAll(models2)).isTrue();
            assertThat(models1.containsAll(models3)).isTrue();

            assertThat(models2.containsAll(models1)).isTrue();
            assertThat(models2.containsAll(models3)).isTrue();

            assertThat(models3.containsAll(models1)).isTrue();
            assertThat(models3.containsAll(models2)).isTrue();
        }
    }

    @Test
    public void getBestSplitAlgorithm() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("performanceSplitCriteria_maxDepth4.csv"));
        fw.write("seed;depth;#vars;#combinations;time random;random aborted;time least common vars;lc aborted;time most common vars;mc aborted;formula");
        fw.newLine();
        for (int i = 1; i <= 50; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(4);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler3 = new NumberOfModelsHandler(2500000);

            final long t1 = System.currentTimeMillis();
            final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder()
                    .handler(handler1)
                    .computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.RANDOM).build());

            final long t2 = System.currentTimeMillis();
            final long timeRandom = t2 - t1;
            System.out.println("Number of combis: " + models.size());

            if (models.size() < 1000) {
                continue;
            }

            System.out.println("Time random split: " + timeRandom);

            solver.execute(ModelEnumerationFunction.builder().handler(handler2)
                    .computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.LEAST_COMMON_VARS).build());
            final long t3 = System.currentTimeMillis();
            final long timeLeastCommon = t3 - t2;
            System.out.println("Time prefer least common vars: " + timeLeastCommon);

            solver.execute(ModelEnumerationFunction.builder().handler(handler3).computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.MOST_COMMON_VARS).build());

            final long t4 = System.currentTimeMillis();
            final long timeMostCommon = t4 - t3;
            System.out.println("Time prefer most common vars: " + timeMostCommon);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString = String.format("%d;%d;%d;%d;%d;%b;%d;%b;%d;%b;%s", i, depth, numberOfVars, models.size(), timeRandom, handler1.aborted(),
                    timeLeastCommon, handler2.aborted(), timeMostCommon, handler3.aborted(), formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }

    @Test
    public void testLeastCommonSplitAlgorithm() {
        for (int i = 1; i <= 1; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(4);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final long t1 = System.currentTimeMillis();

            solver.execute(ModelEnumerationFunction.builder().handler(handler1)
                    .computeWithComponents(true).enumerateWithSplit(true)
                    .splitCriterion(ModelEnumerationFunction.SPLIT_CRITERION.LEAST_COMMON_VARS).build());
            final long t2 = System.currentTimeMillis();
            final long timeLeastCommon = t2 - t1;
            System.out.println("Time prefer least common vars: " + timeLeastCommon);
        }

    }

    @Test
    public void compareComputationsWithSplitAndComponents() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("performanceSplitAndComponentsPart2.csv"));
        fw.write(
                "seed;depth;#vars;time no split, no components;aborted?;time split, no components;aborted?;time no split, components;aborted?;time split, " +
                        "components;aborted?;formula");
        fw.newLine();
        for (int i = 1; i <= 50; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler3 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler4 = new NumberOfModelsHandler(2500000);

            final long t1 = System.currentTimeMillis();
            final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder().handler(handler1).build());
            if (models.size() < 1000) {
                continue;
            }

            final long t2 = System.currentTimeMillis();
            final long timeNoSplitNoComponents = t2 - t1;
            System.out.println("Time no split, no components: " + timeNoSplitNoComponents);

            solver.execute(ModelEnumerationFunction.builder().handler(handler2).enumerateWithSplit(true).build());
            final long t3 = System.currentTimeMillis();
            final long timeSplitNoComponents = t3 - t2;
            System.out.println("Time split, no components: " + timeSplitNoComponents);

            solver.execute(ModelEnumerationFunction.builder().handler(handler3).computeWithComponents(true).build());
            final long t4 = System.currentTimeMillis();
            final long timeNoSplitComponents = t4 - t3;
            System.out.println("Time no split, components: " + timeNoSplitComponents);

            solver.execute(ModelEnumerationFunction.builder().handler(handler4).computeWithComponents(true).enumerateWithSplit(true).build());
            final long t5 = System.currentTimeMillis();
            final long timeSplitComponents = t5 - t4;
            System.out.println("Time split, component: " + timeSplitComponents);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%b;%d;%b;%d;%b;%d;%b;%s", i, depth, numberOfVars, timeNoSplitNoComponents, handler1.aborted(),
                            timeSplitNoComponents, handler2.aborted(), timeNoSplitComponents, handler3.aborted(), timeSplitComponents, handler4.aborted(),
                            formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }


}
