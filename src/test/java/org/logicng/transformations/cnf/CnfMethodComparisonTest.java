package org.logicng.transformations.cnf;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LongRunningTag;
import org.logicng.backbones.Backbone;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class CnfMethodComparisonTest {

    @Test
    public void compareFullBackbonesOnLargeFormulas() throws IOException, ParserException {
        compareBackbone("src/test/resources/formulas/formula1.txt");
        compareBackbone("src/test/resources/formulas/large_formula.txt");
        compareBackbone("src/test/resources/formulas/small_formulas.txt");
    }

    @Test
    @LongRunningTag
    public void compareBackbonesForVariablesOnLargeFormulas() throws IOException, ParserException {
        compareBackbonePerVariable("src/test/resources/formulas/formula1.txt");
        compareBackbonePerVariable("src/test/resources/formulas/large_formula.txt");
        compareBackbonePerVariable("src/test/resources/formulas/small_formulas.txt");
    }

    private void compareBackbone(final String fileName) throws IOException, ParserException {
        final Backbone backboneFactory = computeBackbone(fileName, MiniSatConfig.CNFMethod.FACTORY_CNF);
        final Backbone backbonePg = computeBackbone(fileName, MiniSatConfig.CNFMethod.PG_ON_SOLVER);
        final Backbone backboneDirectPg = computeBackbone(fileName, MiniSatConfig.CNFMethod.DIRECT_PG_ON_SOLVER);
        assertThat(backboneFactory).isEqualTo(backbonePg);
        assertThat(backboneFactory).isEqualTo(backboneDirectPg);
    }

    private Backbone computeBackbone(final String fileName, final MiniSatConfig.CNFMethod cnfMethod) throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula(fileName, f);
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(cnfMethod).build());
        solver.add(formula);
        return solver.backbone(formula.variables());
    }

    private void compareBackbonePerVariable(final String fileName) throws IOException, ParserException {
        final Map<Variable, Backbone> backboneFactory = computeBackbonePerVariable(fileName, MiniSatConfig.CNFMethod.FACTORY_CNF);
        final Map<Variable, Backbone> backbonePg = computeBackbonePerVariable(fileName, MiniSatConfig.CNFMethod.PG_ON_SOLVER);
        final Map<Variable, Backbone> backboneDirectPg = computeBackbonePerVariable(fileName, MiniSatConfig.CNFMethod.DIRECT_PG_ON_SOLVER);
        assertThat(backboneFactory).isEqualTo(backbonePg);
        assertThat(backboneFactory).isEqualTo(backboneDirectPg);
    }

    private Map<Variable, Backbone> computeBackbonePerVariable(final String fileName, final MiniSatConfig.CNFMethod cnfMethod)
            throws IOException, ParserException {
        final long start = System.currentTimeMillis();
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula(fileName, f);
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(cnfMethod).build());
        solver.add(formula);
        final SolverState solverState = solver.saveState();
        final Map<Variable, Backbone> result = new TreeMap<>();
        int counter = 1000;
        for (final Variable variable : formula.variables()) {
            if (counter-- > 0) {
                solver.add(variable);
                if (solver.sat() == Tristate.TRUE) {
                    final Backbone backbone = solver.backbone(formula.variables());
                    result.put(variable, backbone);
                }
                solver.loadState(solverState);
            }
        }
        final long stop = System.currentTimeMillis();
        System.out.println(fileName + " " + cnfMethod + ": " + (stop - start) + " ms.");
        return result;
    }

}
