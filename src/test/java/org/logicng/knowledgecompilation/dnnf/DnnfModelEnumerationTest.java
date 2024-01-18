package org.logicng.knowledgecompilation.dnnf;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.DimacsReader;
import org.logicng.knowledgecompilation.dnnf.datastructures.Dnnf;
import org.logicng.knowledgecompilation.dnnf.functions.DnnfModelEnumerationFunction;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class DnnfModelEnumerationTest {

    private final FormulaFactory f = new FormulaFactory();

    @Test
    public void test() throws ParserException, IOException {
        final Formula formula = this.f.and(DimacsReader.readCNF("src/test/resources/dnnf/both_bdd_dnnf_1.cnf", this.f));
        final Random random = new Random(42);
        for (int i = 0; i < 10; i++) {
            final List<Variable> shuffledVars = new ArrayList<>(formula.variables());
            Collections.shuffle(shuffledVars, random);
            final List<Variable> vars = shuffledVars.subList(0, 30);
            final long startDnnfComp = System.currentTimeMillis();
            final DnnfFactory dnnfFactory = new DnnfFactory();
            final Dnnf dnnf = dnnfFactory.compile(formula);
            final long endDnnfComp = System.currentTimeMillis();
            System.out.printf("Dnnf   Compilation: %5dms%n", endDnnfComp - startDnnfComp);
            final List<Assignment> dnnfModels = dnnf.execute(new DnnfModelEnumerationFunction(vars));
            final long endDnnfEnum = System.currentTimeMillis();
            System.out.printf("Dnnf   Enumeration: %5dms%n", endDnnfEnum - endDnnfComp);
            final List<Assignment> solverModels = enumerateWithSolver(formula, vars);
            final long endSolverEnum = System.currentTimeMillis();
            System.out.printf("Solver Enumeration: %5dms%n", endSolverEnum - endDnnfEnum);
//            assertThat(dnnfModels).containsExactlyInAnyOrderElementsOf(solverModels);
            assertThat(dnnfModels).hasSameSizeAs(solverModels);
            System.out.println("Models: " + dnnfModels.size());
            System.out.println();
        }
    }

    private List<Assignment> enumerateWithSolver(final Formula formula, final List<Variable> vars) {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(formula);
        return solver.execute(ModelEnumerationFunction.builder().variables(vars).build());
    }
}
