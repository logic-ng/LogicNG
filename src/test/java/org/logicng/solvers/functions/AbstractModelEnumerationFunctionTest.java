package org.logicng.solvers.functions;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.datastructures.Model;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.modelenumeration.AdvancedModelEnumerationConfig;
import org.logicng.solvers.functions.modelenumeration.DefaultAdvancedModelEnumerationStrategy;
import org.logicng.solvers.functions.modelenumeration.TestVariableProvider;

import java.util.List;

public class AbstractModelEnumerationFunctionTest {

    private FormulaFactory f;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
    }
    
    @Test
    @RandomTag
    public void testEmptySplitVariables() throws ParserException {
        final Formula formula = this.f.parse("A | B | C | D | E");

        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(formula);

        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new TestVariableProvider.EmptyVariableProvider()).maxNumberOfModels(5).build()).build();
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(models.size()).isEqualTo(31);
    }

    @Test
    @RandomTag
    public void testNullSplitVariables() throws ParserException {
        final Formula formula = this.f.parse("A | B | C | D | E");

        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(formula);

        final AdvancedModelEnumerationConfig config =
                AdvancedModelEnumerationConfig.builder().strategy(DefaultAdvancedModelEnumerationStrategy.builder().splitVariableProvider(new TestVariableProvider.NullVariableProvider()).maxNumberOfModels(5).build()).build();
        final List<Model> models = solver.execute(AdvancedModelEnumerationFunction.builder().configuration(config).build());
        assertThat(models.size()).isEqualTo(31);
    }
}
