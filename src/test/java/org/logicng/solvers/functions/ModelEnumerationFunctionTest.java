package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.util.FormulaHelper.variables;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaFactoryConfig;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.List;

/**
 * Units tests for {@link ModelEnumerationFunction}.
 * @version 2.4.2
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
    public void testVariableRemovedBySimplificationOccursInModels() throws ParserException {
        final FormulaFactory f = new FormulaFactory(FormulaFactoryConfig.builder().simplifyComplementaryOperands(true).build());
        final SATSolver solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        final Variable a = f.variable("A");
        final Variable b = f.variable("B");
        final Formula formula = this.f.parse("A & B => A");
        solver.add(formula); // during NNF conversion, used by the PG transformation, the formula simplifies to verum when added to the solver
        final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder().variables(formula.variables()).build());
        assertThat(models).hasSize(4);
        for (final Assignment model : models) {
            assertThat(variables(model.literals())).containsExactlyInAnyOrder(a, b);
        }
    }

    @Test
    public void testUnknownVariableNotOccurringInModel() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        final Variable a = this.f.variable("A");
        solver.add(a);
        final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder().variables(this.f.variables("A", "X")).build());
        assertThat(models).hasSize(1);
        assertThat(models.get(0).literals()).containsExactly(a);
    }
}
