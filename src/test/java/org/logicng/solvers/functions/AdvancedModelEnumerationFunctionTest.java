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
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.splitVariableProvider.LeastCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.MostCommonVariableProvider;
import org.logicng.solvers.functions.splitVariableProvider.RandomSplitVariableProvider;
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
 * Units tests for {@link AdvancedModelEnumerationFunction}.
 * @version 2.3.0
 * @since 2.3.0
 */
public class AdvancedModelEnumerationFunctionTest {

    private final FormulaFactory f;

    public AdvancedModelEnumerationFunctionTest() {
        this.f = new FormulaFactory();
    }

    @Test
    public void computeWithComponentsWithSplit() {
        for (int i = 1; i <= 100; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 20) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);

            final List<Assignment> models = solver.execute(AdvancedModelEnumerationFunction.builder().computeWithComponents(true).build());
            System.out.println("\nSeed: " + i + ", models: " + models.size());

        }
    }

    @Test
    public void testNewImplementation() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("advancedME.csv"));
        fw.write("seed;depth;#vars;#combinations;time classic;time advanced no components;time advanced with components;formula");
        fw.newLine();
        for (int i = 1; i <= 1000; i++) {
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(3);
            final int numberOfVars = formula.variables().size();
            if (numberOfVars < 10) {
                continue;
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(formula);
            System.out.println("\nSeed: " + i + ", formula: " + formula);

            final long t1 = System.currentTimeMillis();

            // advanced enumeration, no components
            final List<Assignment> modelsAdvNoComp = solver.execute(AdvancedModelEnumerationFunction.builder().build());
            if (modelsAdvNoComp.size() < 100 || modelsAdvNoComp.size() > 100000) {
                continue;
            }

            final long t2 = System.currentTimeMillis();
            final long timeAdvancedNoComponents = t2 - t1;
            System.out.println("Time advanced enumeration no components: " + timeAdvancedNoComponents);

            // classic enumeration
            final List<Assignment> modelsClassic = solver.execute(ModelEnumerationFunction.builder().build());

            final long t3 = System.currentTimeMillis();
            final long timeClassic = t3 - t2;
            System.out.println("Time classic enumeration: " + timeClassic);

            // advanced enumeration, with components
            final List<Assignment> modelsAdvWithComp = solver.execute(AdvancedModelEnumerationFunction.builder().computeWithComponents(true).build());
            final long t4 = System.currentTimeMillis();
            final long timeAdvancedWithComponents = t4 - t3;
            System.out.println("Time advanced enumeration with components: " + timeAdvancedWithComponents);

            assertThat(modelsClassic.size()).isEqualTo(modelsAdvNoComp.size());
            assertThat(modelsClassic.size()).isEqualTo(modelsAdvWithComp.size());
            assertThat(modelsClassic).containsExactlyInAnyOrderElementsOf(modelsAdvNoComp);
            assertThat(modelsClassic).containsExactlyInAnyOrderElementsOf(modelsAdvWithComp);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString =
                    String.format("%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, modelsClassic.size(), timeClassic, timeAdvancedNoComponents,
                            timeAdvancedWithComponents, formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }


    @Test
    public void testPme() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final SolverState initialState = solver.saveState();
        for (int i = 1; i <= 1000; i++) {
            solver.loadState(initialState);
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(5);
            solver.add(formula);
            final long t1 = System.currentTimeMillis();
            final List<Assignment> modelsWithComponents = solver.execute(
                    AdvancedModelEnumerationFunction.builder().computeWithComponents(true)
                            .variables(f.variable("v01"), f.variable("v02"), f.variable("v03"), f.variable("v04"), f.variable("v05"), f.variable("v06"),
                                    f.variable("v07"), f.variable("v08"), f.variable("v09"),
                                    f.variable("v10"), f.variable("v11"), f.variable("v12")).build());
            if (modelsWithComponents.size() < 100) {
                continue;
            }
            System.out.println("\nSeed: " + i + ", number of models: " + modelsWithComponents.size());
            final long t2 = System.currentTimeMillis();
            final List<Assignment> classicModels = solver.execute(
                    AdvancedModelEnumerationFunction.builder()
                            .variables(f.variable("v01"), f.variable("v02"), f.variable("v03"), f.variable("v04"), f.variable("v05"), f.variable("v06"),
                                    f.variable("v07"), f.variable("v08"), f.variable("v09"),
                                    f.variable("v10"), f.variable("v11"), f.variable("v12")).build());
            final long t3 = System.currentTimeMillis();

            assertThat(modelsWithComponents.size()).isEqualTo(classicModels.size());
            assertThat(modelsWithComponents).containsExactlyInAnyOrderElementsOf(classicModels);
            System.out.println("time with components: " + (t2 - t1));
            System.out.println("time without components: " + (t3 - t2));
        }
    }


    @Test
    public void testAdditionalVariables() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("AmeAdditionalVars.csv"));
        fw.write("seed;depth;#vars formula;# vars pme;#combinations;time no components (ms);time with components (ms);formula");
        fw.newLine();
        final SATSolver solver = MiniSat.miniSat(this.f);
        final SolverState initialState = solver.saveState();
        for (int i = 1; i <= 1000; i++) {
            solver.loadState(initialState);
            // given
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().seed(i).build());
            final Formula formula = randomizer.formula(8);
            solver.add(formula);

            final List<Variable> varsFormula = new ArrayList<>(formula.variables());
            final int numberOfVars = formula.variables().size();
            final int minNumberOfVars = (int) Math.ceil(numberOfVars / (double) 3) + 2;
            final SortedSet<Variable> pmeVars = new TreeSet<>(varsFormula.subList(0, minNumberOfVars));

            final int additionalVarsStart = 2 * minNumberOfVars;
            final SortedSet<Variable> additionalVars = new TreeSet<>(varsFormula.subList(additionalVarsStart, varsFormula.size()));

            // when
            final long t1 = System.currentTimeMillis();
            final List<Assignment> models1 = solver.execute(AdvancedModelEnumerationFunction.builder()
                    .splitVariableProvider(new LeastCommonVariableProvider(this.f, 3, 50, 70))
                    .variables(pmeVars)
                    .additionalVariables(additionalVars).build());
            final long t1a = System.currentTimeMillis();

            final long timeNoComponents = t1a - t1;
            if (models1.size() < 10) {
                continue;
            }
            System.out.println("\nSeed: " + i);
            System.out.println("Number of combinations: " + models1.size());
            System.out.println("Time no components: " + timeNoComponents);

            final long t2 = System.currentTimeMillis();
            final List<Assignment> models2 = solver.execute(AdvancedModelEnumerationFunction.builder()
                    .computeWithComponents(true)
                    .splitVariableProvider(new LeastCommonVariableProvider(this.f, 3, 50, 70))
                    .variables(pmeVars)
                    .additionalVariables(additionalVars).build());

            final long t3 = System.currentTimeMillis();
            final long timeWithComponents = t3 - t2;

            System.out.println("Time with components: " + timeWithComponents);

            final List<Assignment> updatedModels1 = restrictAssignmentsToPmeVars(pmeVars, models1);
            final List<Assignment> updatedModels2 = restrictAssignmentsToPmeVars(pmeVars, models2);

            // then
            assertThat(models1.size()).isEqualTo(models2.size());
            assertThat(updatedModels1).containsExactlyInAnyOrderElementsOf(updatedModels2);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString = String.format("%d;%d;%d;%d;%d;%d;%d;%s", i, depth, numberOfVars, pmeVars.size(), models2.size(), timeNoComponents,
                    timeWithComponents, formula);
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
    public void testPerformanceSplitProvider() throws IOException {
        final BufferedWriter fw = new BufferedWriter(new FileWriter("AmePerformance.csv"));
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

            // random split provider
            final List<Assignment> models = solver.execute(AdvancedModelEnumerationFunction.builder()
                    .splitVariableProvider(new RandomSplitVariableProvider(this.f))
                    .handler(handler1)
                    .computeWithComponents(true).build());
            final long t2 = System.currentTimeMillis();
            final long timeRandom = t2 - t1;
            System.out.println("Number of combis: " + models.size());
            if (models.size() < 1000) {
                continue;
            }
            System.out.println("Time random split: " + timeRandom);

            // least common split provider
            solver.execute(AdvancedModelEnumerationFunction.builder()
                    .splitVariableProvider(new LeastCommonVariableProvider(this.f))
                    .handler(handler2)
                    .computeWithComponents(true).build());
            final long t3 = System.currentTimeMillis();
            final long timeLeastCommon = t3 - t2;
            System.out.println("Time least common vars: " + timeLeastCommon);

            // most common split provider
            solver.execute(AdvancedModelEnumerationFunction.builder()
                    .splitVariableProvider(new MostCommonVariableProvider(this.f))
                    .handler(handler3)
                    .computeWithComponents(true).build());
            final long t4 = System.currentTimeMillis();
            final long timeMostCommon = t4 - t3;
            System.out.println("Time most common vars: " + timeMostCommon);

            final int depth = formula.apply(new FormulaDepthFunction());
            final String resultString = String.format("%d;%d;%d;%d;%d;%b;%d;%b;%d;%b;%s", i, depth, numberOfVars, models.size(), timeRandom,
                    handler1.aborted(), timeLeastCommon, handler2.aborted(), timeMostCommon, handler3.aborted(), formula);
            fw.write(resultString);
            fw.newLine();
            fw.flush();
        }
    }
}
