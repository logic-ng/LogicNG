package org.logicng.knowledgecompilation.dnnf;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfModelEnumerationFunction;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;
import org.logicng.util.CollectionHelper;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class DnnfModelEnumerationTest extends TestWithExampleFormulas {

    @Test
    public void testCornerCases() throws ParserException, IOException {
        final Formula contradiction = this.f.parse("(A | B) & ~A & ~B");
        compareModels(this.f.verum(), Collections.emptyList(), 1);
        compareModels(this.A, Collections.emptyList(), 1);
        compareModels(this.AND1, Collections.emptyList(), 1);
        compareModels(contradiction, Collections.emptyList(), 0);
        compareModels(this.f.falsum(), Collections.emptyList(), 0);

        compareModels(this.A, Collections.singletonList(this.A), 1);
        compareModels(this.AND1, Collections.singletonList(this.A), 1);
        compareModels(this.NA, Collections.singletonList(this.A), 1);
        compareModels(this.AND2, Collections.singletonList(this.A), 1);
        compareModels(contradiction, Collections.singletonList(this.A), 0);

        // Dnnf Model Enumeration adds unknown variables negatively, Solver Model enumeration ignores them
        compareModelsDnnfOnly(this.f.verum(), Collections.singletonList(this.A), 1);
        compareModelsDnnfOnly(this.A, Collections.singletonList(this.C), 1);
        compareModelsDnnfOnly(this.AND1, Collections.singletonList(this.C), 1);
        compareModelsDnnfOnly(this.NA, Collections.singletonList(this.C), 1);
        compareModelsDnnfOnly(this.AND2, Collections.singletonList(this.C), 1);
        compareModelsDnnfOnly(contradiction, Collections.singletonList(this.C), 0);
        compareModelsDnnfOnly(this.f.falsum(), Collections.singletonList(this.A), 0);
    }

    @Test
    @RandomTag
    @LongRunningTag
    public void randomTests() {
        for (int i = 0; i < 100; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(12)
                    .weightAmo(5)
                    .weightExo(5)
                    .seed((i + 1) * 42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);

            Formula formula;
            do {
                formula = f.and(IntStream.range(1, 5).mapToObj(j -> randomizer.formula(4)).collect(Collectors.toList()));
            } while (!hasMoreThanOneModel(formula));
            compareModels(formula, formula.variables());
        }
    }

    private void compareModels(final Formula formula, final Collection<Variable> vars) {
        compareModels(formula, vars, null);
    }

    private void compareModels(final Formula formula, final Collection<Variable> vars, final Integer expectedSize) {
        final List<Assignment> solverModels = enumerateWithSolver(formula, vars);
        final List<Assignment> dnnfModels = enumerateWithDnnf(formula, vars);
        if (expectedSize != null) {
            assertThat(dnnfModels).hasSize(expectedSize);
        }
        assertThat(dnnfModels).containsExactlyInAnyOrderElementsOf(solverModels);
    }

    private void compareModelsDnnfOnly(final Formula formula, final List<Variable> vars, final Integer expectedSize) {
        final List<Assignment> dnnfModels = enumerateWithDnnf(formula, vars);
        assertThat(dnnfModels).hasSize(expectedSize);
    }

    private List<Assignment> enumerateWithSolver(final Formula formula, final Collection<Variable> vars) {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(formula);
        final List<Assignment> result = solver.execute(ModelEnumerationFunction.builder().variables(vars).build());
        final SortedSet<Variable> missingVars = CollectionHelper.difference(vars, solver.knownVariables(), TreeSet::new);
        return missingVars.isEmpty() ? result : fixSolverModels(result, missingVars);
    }

    private List<Assignment> enumerateWithDnnf(final Formula formula, final Collection<Variable> vars) {
        final DnnfFactory dnnfFactory = new DnnfFactory();
        final Dnnf dnnf = dnnfFactory.compile(formula);
        return dnnf.execute(new DnnfModelEnumerationFunction(vars));
    }

    private boolean hasMoreThanOneModel(final Formula formula) {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(formula);
        final List<Assignment> result = solver.execute(ModelEnumerationFunction.builder().variables(formula.variables()).handler(new NumberOfModelsHandler(3)).build());
        return result.size() >= 2;
    }

    private List<Assignment> fixSolverModels(final List<Assignment> assignments, final SortedSet<Variable> missingVars) {
        final List<Assignment> result = new ArrayList<>();
        for (final List<Literal> additional : getCartesianProduct(missingVars)) {
            for (final Assignment assignment : assignments) {
                final Assignment newAssignment = new Assignment(assignment.literals());
                additional.forEach(newAssignment::addLiteral);
                result.add(newAssignment);
            }
        }
        return result;
    }

    static List<List<Literal>> getCartesianProduct(final SortedSet<Variable> variables) {
        List<List<Literal>> result = singletonList(emptyList());
        for (final Variable var : variables) {
            final List<List<Literal>> extended = new ArrayList<>(result.size() * 2);
            for (final List<Literal> literals : result) {
                extended.add(extendedByLiteral(literals, var));
                extended.add(extendedByLiteral(literals, var.negate()));
            }
            result = extended;
        }
        return result;
    }

    private static List<Literal> extendedByLiteral(final List<Literal> literals, final Literal lit) {
        final ArrayList<Literal> extended = new ArrayList<>(literals);
        extended.add(lit);
        return extended;
    }
}
