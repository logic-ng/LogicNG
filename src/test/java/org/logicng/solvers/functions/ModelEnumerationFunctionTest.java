package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.functions.FormulaDepthFunction;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.splitVariableProvider.LeastCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.MostCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.RandomSplitVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.SplitVariableProvider;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

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
    public void testMeWithSplitAllProviders() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("testMeWithSplit.csv"));
        fw.write("seed;depth;#vars;#combinations;time no split;time split lc (ms);time split mc (ms);time split random (ms);time split fixed (ms);formula");
        fw.newLine();
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 12) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // least common vars
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).build());
            final long t1a = System.currentTimeMillis();
            if (models1.size() > 500000 || models1.size() < 10000) {
                continue;
            }
            final long t2 = System.currentTimeMillis();

            // most common vars
            final List<Assignment> models2 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new MostCommonVariableProvider(this.f)).build());
            final long t3 = System.currentTimeMillis();

            // random vars
            final List<Assignment> models3 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new RandomSplitVariableProvider(this.f)).build());
            final long t4 = System.currentTimeMillis();

            // fixed number of vars. Here replaced by random split vars.
            // final List<Variable> varsFormula = new ArrayList<>(formula.variables());
            // final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 2);
            // final SortedSet<Variable> splitVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));
            final SplitVariableProvider splitVariableProvider = new RandomSplitVariableProvider(this.f);
            final long t5 = System.currentTimeMillis();
            final List<Assignment> models4 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(splitVariableProvider).build());
            final long t6 = System.currentTimeMillis();

            // no split
            final List<Assignment> models5 = solver.execute(ModelEnumerationFunction.builder().build());
            final long t7 = System.currentTimeMillis();

            final long timeLc = t1a - t1;
            final long timeMc = t3 - t2;
            final long timeRandom = t4 - t3;
            final long timeFixed = t6 - t5;
            final long timeNoSplit = t7 - t6;

            System.out.println("\nSeed: " + i);
            System.out.println("Number of combinations: " + models1.size());
            System.out.println("Time least common vars: " + timeLc);
            System.out.println("Time most common vars: " + timeMc);
            System.out.println("Time random vars: " + timeRandom);
            System.out.println("Time fixed number of vars: " + timeFixed);
            System.out.println("Time no split: " + timeNoSplit);

            assertThat(models1.size()).isEqualTo(models5.size());
            assertThat(models2.size()).isEqualTo(models5.size());
            assertThat(models3.size()).isEqualTo(models5.size());
            assertThat(models4.size()).isEqualTo(models5.size());

            assertThat(models5).containsExactlyInAnyOrderElementsOf(models1);
            assertThat(models5).containsExactlyInAnyOrderElementsOf(models2);
            assertThat(models5).containsExactlyInAnyOrderElementsOf(models3);
            assertThat(models5).containsExactlyInAnyOrderElementsOf(models4);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, models1.size(), timeNoSplit, timeLc, timeMc, timeRandom,
                            timeFixed, formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }

    @Test
    public void testPme() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("pme.csv"));
        fw.write("seed;depth;#vars formula;# vars pme;#combinations;time no split (ms);time split (ms);formula");
        fw.newLine();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final SolverState initialState = solver.saveState();
        for (int i = 1; i <= 100; i++) {
            solver.loadState(initialState);
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(6);
            solver.add(formula);

            final List<Variable> varsFormula = new ArrayList<>(formula.variables());
            final int numberOfVars = formula.variables().size();
            final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 2) + 2;
            final SortedSet<Variable> pmeVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));

            // when
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 = solver.execute(ModelEnumerationFunction.builder().variables(pmeVars).build());
            final long t1a = System.currentTimeMillis();
            if (models1.size() < 100) {
                continue;
            }

            final long timeNoSplit = t1a - t1;
            System.out.println("\nSeed: " + i);
            System.out.println("Number of combinations no split: " + models1.size());
            System.out.println("Time no split: " + timeNoSplit);

            final long t2 = System.currentTimeMillis();
            final List<Assignment> models2 =
                    solver.execute(
                            ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).variables(pmeVars).build());
            final long t3 = System.currentTimeMillis();
            final long timeSplit = t3 - t2;

            System.out.println("Time split: " + timeSplit);
            assertThat(models1.size()).isEqualTo(models2.size());
            assertThat(models1).containsExactlyInAnyOrderElementsOf(models2);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, pmeVars.size(), models2.size(), timeNoSplit, timeSplit, formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }


    @Test
    public void testAdditionalVariables() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("additionalVars.csv"));
        fw.write("seed;depth;#vars formula;# vars pme;#combinations;time no split (ms);time split (ms);formula");
        fw.newLine();
        final SATSolver solver = MiniSat.miniSat(this.f);

        for (int i = 1; i <= 1000; i++) {
            // given
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(10);
            solver.add(formula);

            final List<Variable> varsFormula = new ArrayList<>(formula.variables());
            final int numberOfVars = formula.variables().size();
            final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 3) + 2;
            final SortedSet<Variable> pmeVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));

            final int additionalVarsStart = 2 * minNumberOfVars;
            final SortedSet<Variable> additionalVars = new TreeSet<>(varsFormula.subList(additionalVarsStart, varsFormula.size()));

            // when
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 = solver.execute(ModelEnumerationFunction.builder().variables(pmeVars).additionalVariables(additionalVars).build());
            final long t1a = System.currentTimeMillis();

            final long timeNoSplit = t1a - t1;
            System.out.println("\nSeed: " + i);
            if (models1.size() < 10) {
                continue;
            }

            System.out.println("Number of combinations: " + models1.size());
            System.out.println("Time no split: " + timeNoSplit);

            final long t2 = System.currentTimeMillis();
            final List<Assignment> models2 =
                    solver.execute(
                            ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f, 3, 50, 70)).variables(pmeVars)
                                    .additionalVariables(additionalVars).build());

            final long t3 = System.currentTimeMillis();
            final long timeSplit = t3 - t2;

            System.out.println("Time split: " + timeSplit);

            final List<Assignment> updatedModels1 = restrictAssignmentsToPmeVars(pmeVars, models1);
            final List<Assignment> updatedModels2 = restrictAssignmentsToPmeVars(pmeVars, models2);

            assertThat(models1.size()).isEqualTo(models2.size());
            assertThat(updatedModels1).containsExactlyInAnyOrderElementsOf(updatedModels2);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, pmeVars.size(), models1.size(), timeNoSplit, timeSplit, formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }

    private List<Assignment> restrictAssignmentsToPmeVars(final SortedSet<Variable> pmeVars, final List<Assignment> models) {
        final List<Assignment> updatedModels = new ArrayList<>();
        for (final Assignment assignment : models) {
            final Assignment updatedAssignment = new Assignment();
            for (final Literal literal : assignment.literals()) {
                if (pmeVars.contains(literal.variable())) {
                    updatedAssignment.addLiteral(literal);
                }
            }
            updatedModels.add(updatedAssignment);
        }
        return updatedModels;
    }

    @Test
    public void testOneSplitProvider() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("Recursive.csv"));
        fw.write("seed;depth;#vars;#combinations;time original (ms);aborted?; time recursive (ms);aborted?;formula");
        fw.newLine();
        final SATSolver solver = MiniSat.miniSat(this.f);
        for (int i = 1; i <= 10000; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            solver.add(formula);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);

            final long time0 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler1).build());
            final long time1 = System.currentTimeMillis();

            if (models1.size() < 10000) {
                solver.reset();
                continue;
            }
            System.out.println("\nSeed: " + i);

            final long time20 = System.currentTimeMillis();
            final List<Assignment> models2 =
                    solver.execute(ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler2)
                            .build());
            final long time2 = System.currentTimeMillis();

            if (!handler1.aborted() && !handler2.aborted()) {
                assertThat(models1.size()).isEqualTo(models2.size());
            }
            final long timeStandard = time1 - time0;
            final long timeRecursive = time2 - time20;

            System.out.println("Time standard: " + (time1 - time0));
            System.out.println("Time recursive: " + (time2 - time1));
            System.out.println("models: " + models1.size());

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%b;%d;%b;%s", i, depth, numberOfVars, models1.size(), timeStandard, handler1.aborted(), timeRecursive,
                            handler2.aborted(), formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
            solver.reset();
        }
    }

    @Test
    public void testRecursives() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("Recursives.csv"));
        fw.write(
                "seed;depth;#vars;#combinations;time original (ms);aborted?;time recursive 500 (ms);aborted?;recursive?;time recursive 1000 (ms);aborted?;" +
                        "recursive?;time recursive 1500 (ms);aborted?;recursive?");
        fw.newLine();
        final SATSolver solver = MiniSat.miniSat(this.f);
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            solver.add(formula);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler3 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler4 = new NumberOfModelsHandler(2500000);

            // original
            final long time0 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler1).build());
            final long time1 = System.currentTimeMillis();

            if (models1.size() < 10000) {
                solver.reset();
                continue;
            }
            System.out.println("\nSeed: " + i);

            // recursive 500
            final ModelEnumerationFunctionRecursive recursive500 =
                    ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler2)
                            .maxNumberOfVarsForSplit(500).build();
            final long time20 = System.currentTimeMillis();
            final List<Assignment> models2 = solver.execute(recursive500);
            final long time2 = System.currentTimeMillis();

            // recursive 1000
            final ModelEnumerationFunctionRecursive recursive1000 =
                    ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler3)
                            .maxNumberOfVarsForSplit(1000).build();
            final long time30 = System.currentTimeMillis();
            final List<Assignment> models3 = solver.execute(recursive1000);
            final long time3 = System.currentTimeMillis();

            // recursive 1500
            final ModelEnumerationFunctionRecursive recursive1500 =
                    ModelEnumerationFunctionRecursive.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f)).handler(handler4)
                            .maxNumberOfVarsForSplit(1500).build();
            final long time40 = System.currentTimeMillis();
            final List<Assignment> models4 = solver.execute(recursive1500);
            final long time4 = System.currentTimeMillis();

            if (!handler1.aborted() && !handler2.aborted()) {
                assertThat(models1.size()).isEqualTo(models2.size());
            }
            final long timeStandard = time1 - time0;
            final long timeRecursive500 = time2 - time20;
            final long timeRecursive1000 = time3 - time30;
            final long timeRecursive1500 = time4 - time40;

            System.out.println("Time standard: " + timeStandard);
            System.out.println("Time recursive 500: " + timeRecursive500);
            System.out.println("Time recursive 1000: " + timeRecursive1000);
            System.out.println("Time recursive 1500: " + timeRecursive1500);

            System.out.println("models: " + models1.size());

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%b;%d;%b;%b;%d;%b;%b;%d;%b;%b", i, depth, numberOfVars, models1.size(),
                            timeStandard, handler1.aborted(),
                            timeRecursive500, handler2.aborted(), recursive500.isRecursive,
                            timeRecursive1000, handler3.aborted(), recursive1000.isRecursive,
                            timeRecursive1500, handler4.aborted(), recursive1500.isRecursive);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
            solver.reset();
        }
    }


    @Test
    public void performanceTestMultipleSplits() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("PerformanceDoubleSplit.csv"));
        fw.write("seed;depth;#vars;#combinations;time no double split (ms);aborted?;time double split (ms);aborted?;formula");
        fw.newLine();
        for (int i = 1; i <= 100; i++) {

            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);

            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().handler(handler1).splitVariableProvider(new LeastCommonVariableProvider(this.f)).build());
            if (models1.size() > 500000) {
                continue;
            }

            final long t2 = System.currentTimeMillis();
            final long timeSplit = t2 - t1;
            System.out.println(models1.size());
            System.out.println("Time normal split: " + timeSplit);
            final List<Assignment> models2 = solver.execute(
                    ModelEnumerationFunction.builder().handler(handler2).splitVariableProvider(new LeastCommonVariableProvider(this.f)).multipleSplits(true)
                            .build());
            final long t3 = System.currentTimeMillis();
            final long timeNoSplit = t3 - t2;

            System.out.println("Time multiple splits: " + timeNoSplit);
            assertThat(models1.size()).isEqualTo(models2.size());
            assertThat(models1).containsExactlyInAnyOrderElementsOf(models2);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%b;%d;%b;%s", i, depth, numberOfVars, models1.size(), timeNoSplit, handler1.aborted(), timeSplit,
                            handler2.aborted(), formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }


    @Test
    public void performanceTestOneSplitProvider() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("MePerformance.csv"));
        fw.write("seed;depth;#vars;#combinations;time no split (ms);aborted?;time split (ms);aborted?;formula");
        fw.newLine();
        for (int i = 1; i <= 100; i++) {

            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i);

            final ModelEnumerationHandler handler1 = new NumberOfModelsHandler(2500000);
            final ModelEnumerationHandler handler2 = new NumberOfModelsHandler(2500000);

            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().handler(handler1).splitVariableProvider(new LeastCommonVariableProvider(this.f)).build());
            if (models1.size() > 500000) {
                continue;
            }

            final long t2 = System.currentTimeMillis();
            final long timeSplit = t2 - t1;
            System.out.println(models1.size());
            System.out.println("Time with split: " + timeSplit);
            final List<Assignment> models2 = solver.execute(ModelEnumerationFunction.builder().handler(handler2).build());
            final long t3 = System.currentTimeMillis();
            final long timeNoSplit = t3 - t2;

            System.out.println("Time without split: " + timeNoSplit);
            assertThat(models1.size()).isEqualTo(models2.size());
            assertThat(models1).containsExactlyInAnyOrderElementsOf(models2);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%b;%d;%b;%s", i, depth, numberOfVars, models1.size(), timeNoSplit, handler1.aborted(), timeSplit,
                            handler2.aborted(), formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }


    @Test
    public void fineTuneSingleSplitProvider() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("fineTuningProvider.csv"));
        fw.write("seed;depth;#vars;#combinations;time 40;time 30;time 50;formula");
        fw.newLine();
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 12) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            // parameter 1
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 =
                    solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f, 12, 50, 65)).build());
            final long t1a = System.currentTimeMillis();
            if (models1.size() > 100000 || models1.size() < 10000) {
                continue;
            }
            final long t2 = System.currentTimeMillis();

            // parameter 2
            solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f, 12, 50, 70)).build());
            final long t3 = System.currentTimeMillis();

            // parameter 3
            solver.execute(ModelEnumerationFunction.builder().splitVariableProvider(new LeastCommonVariableProvider(this.f, 12, 50, 75)).build());
            final long t4 = System.currentTimeMillis();

            final long timeMc10 = t1a - t1;
            final long timeMc30 = t3 - t2;
            final long timeMc50 = t4 - t3;

            System.out.println("\nSeed: " + i);
            System.out.println("Number combinations: " + models1.size());
            System.out.println("Time timeMc40: " + timeMc10);
            System.out.println("Time timeMc30: " + timeMc30);
            System.out.println("Time timeMc50: " + timeMc50);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, models1.size(), timeMc10, timeMc30, timeMc50, formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }
}
