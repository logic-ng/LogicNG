package org.logicng.explanations.backbones;

import org.junit.Ignore;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import java.io.IOException;
import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class MiniSatBackboneTest {

    @Test
    public void testMiniSatBackboneConfig() {
        MiniSatBackboneConfig config = new MiniSatBackboneConfig.Builder().build();
        assertThat(config.toString()).isEqualTo("MiniSatBackboneConfig{\n" +
                "initialLBCheckForUPZeroLiterals=true\n" +
                "initialUBCheckForRotatableLiterals=true\n" +
                "checkForUPZeroLiterals=true\n" +
                "checkForComplementModelLiterals=true\n" +
                "checkForRotatableLiterals=true\n" +
                "}\n");

        config = new MiniSatBackboneConfig.Builder().checkForComplementModelLiterals(false)
                .checkForRotatableLiterals(false).initialUBCheckForRotatableLiterals(false).build();
        assertThat(config.toString()).isEqualTo("MiniSatBackboneConfig{\n" +
                "initialLBCheckForUPZeroLiterals=true\n" +
                "initialUBCheckForRotatableLiterals=false\n" +
                "checkForUPZeroLiterals=true\n" +
                "checkForComplementModelLiterals=false\n" +
                "checkForRotatableLiterals=false\n" +
                "}\n");

        config = new MiniSatBackboneConfig.Builder().checkForComplementModelLiterals(true).build();
        assertThat(config.toString()).isEqualTo("MiniSatBackboneConfig{\n" +
                "initialLBCheckForUPZeroLiterals=true\n" +
                "initialUBCheckForRotatableLiterals=true\n" +
                "checkForUPZeroLiterals=true\n" +
                "checkForComplementModelLiterals=true\n" +
                "checkForRotatableLiterals=true\n" +
                "}\n");
    }


    @Test
    public void testSimpleBackbones() {
        final FormulaFactory f = new FormulaFactory();
        MiniSatBackbone solver = new MiniSatBackbone(f);

        Literal x = f.literal("x", true);
        Literal y = f.literal("y", true);
        Literal z = f.literal("z", true);
        Literal u = f.literal("u", true);
        Literal v = f.literal("v", true);

        Collection<Variable> variables = new ArrayList<>(Arrays.asList(f.variable("x"), f.variable("y"),
                f.variable("z"), f.variable("u"), f.variable("v")));

        Formula formula = f.verum();
        int[] before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), Collections.<Variable>emptyList())).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x;
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Collections.singletonList(x))
        );
        solver.loadState(before);

        formula = f.and(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.or(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x.negate();
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Collections.singleton(x.negate()))
        );
        solver.loadState(before);

        formula = f.or(f.and(x, y, z), f.and(x, y, u), f.and(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Collections.singleton(x))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y, z), f.or(x, y, u), f.or(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = f.and(f.or(x.negate(), y), x);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(x.negate(), y));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Collections.singleton(y))
        );
        solver.loadState(before);

        formula = f.and(f.and(f.or(x.negate(), y), x.negate()), f.and(z, f.or(x, y)));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Arrays.asList(x.negate(), y, z))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(u, v), z);
        solver.add(formula);
        assertThat(solver.compute(Collections.<Formula>emptyList(), variables)).isEqualTo(
                new TreeSet<>(Collections.singleton(z))
        );
    }

    @Test
    public void testSmallFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
        MiniSatBackbone backboneSolver = new MiniSatBackbone(f);
        backboneSolver.add(formula);
        SortedSet<Literal> backbone = backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        assertThat(verifyBackbone(backbone, formula, formula.variables())).isTrue();
    }

    @Test
    public void testLargeFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        MiniSatBackbone backboneSolver = new MiniSatBackbone(f);
        backboneSolver.add(formula);
        SortedSet<Literal> backbone = backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        assertThat(verifyBackbone(backbone, formula, formula.variables())).isTrue();
    }

    private boolean verifyBackbone(final Set<Literal> backbone, final Formula formula, final Collection<Variable> variables) {
        final SATSolver solver = MiniSat.miniSat(formula.factory());
        solver.add(formula);
        for (final Literal bbVar : backbone) {
            if (solver.sat(bbVar.negate()) == Tristate.TRUE) {
                return false;
            }
        }
        for (final Variable variable : variables) {
            if (!backbone.contains(formula.factory().literal(variable.name(), true)) &&
                !backbone.contains(formula.factory().literal(variable.name(), false))) {
                if (solver.sat(variable) == Tristate.FALSE) {
                    return false;
                }
                if (solver.sat(variable.negate()) == Tristate.FALSE) {
                    return false;
                }
            }
        }
        return true;
    }

    @Ignore
    @Test
    public void benchmarkLargeFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);

        MiniSatBackboneConfig config = new MiniSatBackboneConfig.Builder().checkForComplementModelLiterals(false).build();
        MiniSatBackbone backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        long start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        long end = System.currentTimeMillis();
        System.out.println("\nlarge formula with checkForComplementModelLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().checkForRotatableLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nlarge formula with checkForRotatables disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().initialUBCheckForRotatableLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nlarge formula with initialUBCheckForRotatableLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().checkForUPZeroLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nlarge formula with checkForUPZeroLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().initialLBCheckForUPZeroLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nlarge formula with initialLBCheckForUPZeroLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nlarge formula with all options enabled:");
        System.out.println("running time : " + (end - start) + " ms");

    }

    @Ignore
    @Test
    public void benchmarkSmallFormulas() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);

        MiniSatBackboneConfig config = new MiniSatBackboneConfig.Builder().checkForComplementModelLiterals(false).build();
        MiniSatBackbone backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        long start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        long end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with checkForComplementModelLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().checkForRotatableLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with checkForRotatables disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().initialUBCheckForRotatableLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with initialUBCheckForRotatableLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().checkForUPZeroLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with checkForUPZeroLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().initialLBCheckForUPZeroLiterals(false).build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with initialLBCheckForUPZeroLiterals disabled:");
        System.out.println("running time : " + (end - start) + " ms");

        config = new MiniSatBackboneConfig.Builder().build();
        backboneSolver = new MiniSatBackbone(f, config);
        backboneSolver.add(formula);
        start = System.currentTimeMillis();
        backboneSolver.compute(Collections.<Formula>emptyList(), formula.variables());
        end = System.currentTimeMillis();
        System.out.println("\nsmall formulas with all options enabled:");
        System.out.println("running time : " + (end - start) + " ms");
    }
}
