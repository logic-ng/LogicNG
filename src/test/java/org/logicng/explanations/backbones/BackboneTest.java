package org.logicng.explanations.backbones;

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

public class BackboneTest {

    @Test
    public void testBackboneConfig() {
        BackboneConfig config = new BackboneConfig.Builder().build();
        assertThat(config.toString()).isEqualTo("BackboneConfig{\n" +
                "initialUBCheckForRotatableLiterals=true\n" +
                "checkForComplementModelLiterals=true\n" +
                "checkForRotatableLiterals=true\n" +
                "}\n");

        config = new BackboneConfig.Builder().checkForComplementModelLiterals(false)
                .checkForRotatableLiterals(false).initialUBCheckForRotatableLiterals(false).build();
        assertThat(config.toString()).isEqualTo("BackboneConfig{\n" +
                "initialUBCheckForRotatableLiterals=false\n" +
                "checkForComplementModelLiterals=false\n" +
                "checkForRotatableLiterals=false\n" +
                "}\n");

        config = new BackboneConfig.Builder().checkForComplementModelLiterals(true).build();
        assertThat(config.toString()).isEqualTo("BackboneConfig{\n" +
                "initialUBCheckForRotatableLiterals=true\n" +
                "checkForComplementModelLiterals=true\n" +
                "checkForRotatableLiterals=true\n" +
                "}\n");
    }

    @Test
    public void testBackboneGeneration() {
        final FormulaFactory f = new FormulaFactory();

        final Variable x = f.variable("x");
        final Variable y = f.variable("y");
        final Variable z = f.variable("z");

        final Formula formula1 = f.and(x.negate(), y);
        final Formula formula2 = f.or(x, z.negate());
        final Collection<Formula> collection = new ArrayList<>(Arrays.asList(formula1, formula2));

        assertThat(BackboneGeneration.compute(formula1).getCompleteBackbone()).containsExactly(x.negate(), y);
        assertThat(BackboneGeneration.compute(formula1, BackboneType.ONLY_NEGATIVE).getCompleteBackbone()).containsExactly(x.negate());
        assertThat(BackboneGeneration.compute(formula1, new ArrayList<>(Arrays.asList(x, z))).getCompleteBackbone()).containsExactly(x.negate());
        assertThat(BackboneGeneration.compute(formula1, new ArrayList<>(Arrays.asList(x, z)), BackboneType.ONLY_NEGATIVE).getCompleteBackbone()).containsExactly(x.negate());
        assertThat(BackboneGeneration.compute(collection).getCompleteBackbone()).containsExactly(x.negate(), y, z.negate());
        assertThat(BackboneGeneration.compute(collection, BackboneType.ONLY_NEGATIVE).getCompleteBackbone()).containsExactly(x.negate(), z.negate());
        assertThat(BackboneGeneration.compute(collection, new ArrayList<>(Arrays.asList(x, y))).getCompleteBackbone()).containsExactly(x.negate(), y);
        assertThat(BackboneGeneration.compute(collection, new ArrayList<>(Arrays.asList(x, y)), BackboneType.ONLY_NEGATIVE).getCompleteBackbone()).containsExactly(x.negate());

        assertThat(BackboneGeneration.computePositive(formula1).getCompleteBackbone()).containsExactly(y);
        assertThat(BackboneGeneration.computePositive(formula1, new ArrayList<>(Arrays.asList(x, z))).getCompleteBackbone()).isEmpty();
        assertThat(BackboneGeneration.computePositive(collection).getCompleteBackbone()).containsExactly(y);
        assertThat(BackboneGeneration.computePositive(collection, new ArrayList<>(Arrays.asList(x, y))).getCompleteBackbone()).containsExactly(y);

        assertThat(BackboneGeneration.computeNegative(formula1).getCompleteBackbone()).containsExactly(x.negate());
        assertThat(BackboneGeneration.computeNegative(formula1, new ArrayList<>(Arrays.asList(x, z))).getCompleteBackbone()).containsExactly(x.negate());
        assertThat(BackboneGeneration.computeNegative(collection).getCompleteBackbone()).containsExactly(x.negate(), z.negate());
        assertThat(BackboneGeneration.computeNegative(collection, new ArrayList<>(Arrays.asList(x, y))).getCompleteBackbone()).containsExactly(x.negate());

        final BackboneConfig config = new BackboneConfig.Builder().checkForRotatableLiterals(false).build();
        BackboneGeneration.setConfig(config);
        assertThat(BackboneGeneration.compute(formula1).getCompleteBackbone()).containsExactly(x.negate(), y);
    }


    @Test
    public void testSimpleBackbones() {
        final FormulaFactory f = new FormulaFactory();
        final MiniSatBackbone solver = new MiniSatBackbone();

        final Literal x = f.literal("x", true);
        final Literal y = f.literal("y", true);
        final Literal z = f.literal("z", true);
        final Literal u = f.literal("u", true);
        final Literal v = f.literal("v", true);

        final Collection<Variable> variables = new ArrayList<>(Arrays.asList(f.variable("x"), f.variable("y"),
                f.variable("z"), f.variable("u"), f.variable("v")));

        Formula formula = f.verum();
        int[] before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(Collections.<Variable>emptyList(), BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x;
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singletonList(x))
        );
        solver.loadState(before);

        formula = f.and(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.or(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x.negate();
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(x.negate()))
        );
        solver.loadState(before);

        formula = f.or(f.and(x, y, z), f.and(x, y, u), f.and(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(x))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y, z), f.or(x, y, u), f.or(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = f.and(f.or(x.negate(), y), x);
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(x.negate(), y));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(y))
        );
        solver.loadState(before);

        formula = f.and(f.and(f.or(x.negate(), y), x.negate()), f.and(z, f.or(x, y)));
        before = solver.saveState();
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x.negate(), y, z))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(u, v), z);
        solver.add(formula);
        assertThat(solver.compute(variables, BackboneType.POSITIVE_AND_NEGATIVE).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(z))
        );
    }

    @Test
    public void testSmallFormulas() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
        final MiniSatBackbone backboneSolver = new MiniSatBackbone();
        backboneSolver.add(formula);
        final Backbone backbone = backboneSolver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        assertThat(verifyBackbone(backbone, formula, formula.variables())).isTrue();
    }

    @Test
    public void testLargeFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        final MiniSatBackbone backboneSolver = new MiniSatBackbone();
        backboneSolver.add(formula);
        final Backbone backbone = backboneSolver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        assertThat(verifyBackbone(backbone, formula, formula.variables())).isTrue();
    }

    private boolean verifyBackbone(final Backbone backbone, final Formula formula, final Collection<Variable> variables) {
        final SATSolver solver = MiniSat.miniSat(formula.factory());
        solver.add(formula);
        for (final Variable bbVar : backbone.getPositiveBackbone()) {
            if (solver.sat(bbVar.negate()) == Tristate.TRUE) {
                return false;
            }
        }
        for (final Variable bbVar : backbone.getNegativeBackbone()) {
            if (solver.sat(bbVar) == Tristate.TRUE) {
                return false;
            }
        }
        for (final Variable variable : variables) {
            if (!backbone.getPositiveBackbone().contains(variable) && !backbone.getNegativeBackbone().contains(variable)) {
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

    @Test
    public void testBackboneType() {
        final FormulaFactory f = new FormulaFactory();
        final MiniSatBackbone solver = new MiniSatBackbone();

        final Literal x = f.literal("x", true);
        final Literal y = f.literal("y", true);
        final Literal z = f.literal("z", true);

        Formula formula = f.not(x);
        int[] before = solver.saveState();
        solver.add(formula);
        Backbone backbone = solver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        Backbone backbonePositive = solver.compute(formula.variables(), BackboneType.ONLY_POSITIVE);
        Backbone backboneNegative = solver.compute(formula.variables(), BackboneType.ONLY_NEGATIVE);
        assertThat(backbone.getCompleteBackbone()).containsExactly(x.negate());
        assertThat(backbonePositive.getCompleteBackbone()).isEmpty();
        assertThat(backboneNegative.getCompleteBackbone()).containsExactly(x.negate());
        SortedSet<Literal> combinedPosNegBackbone = new TreeSet<>(backbonePositive.getCompleteBackbone());
        combinedPosNegBackbone.addAll(backboneNegative.getCompleteBackbone());
        assertThat(backbone.getCompleteBackbone()).isEqualTo(combinedPosNegBackbone);
        solver.loadState(before);

        formula = f.and(f.or(x, y.negate()), x.negate());
        before = solver.saveState();
        solver.add(formula);
        backbone = solver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        backbonePositive = solver.compute(formula.variables(), BackboneType.ONLY_POSITIVE);
        backboneNegative = solver.compute(formula.variables(), BackboneType.ONLY_NEGATIVE);
        assertThat(backbone.getCompleteBackbone()).containsExactly(x.negate(), y.negate());
        assertThat(backbonePositive.getCompleteBackbone()).isEmpty();
        assertThat(backboneNegative.getCompleteBackbone()).containsExactly(x.negate(), y.negate());
        combinedPosNegBackbone = new TreeSet<>(backbonePositive.getCompleteBackbone());
        combinedPosNegBackbone.addAll(backboneNegative.getCompleteBackbone());
        assertThat(backbone.getCompleteBackbone()).isEqualTo(combinedPosNegBackbone);
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(x.negate(), y));
        before = solver.saveState();
        solver.add(formula);
        backbone = solver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        backbonePositive = solver.compute(formula.variables(), BackboneType.ONLY_POSITIVE);
        backboneNegative = solver.compute(formula.variables(), BackboneType.ONLY_NEGATIVE);
        assertThat(backbone.getCompleteBackbone()).containsExactly(y);
        assertThat(backbonePositive.getCompleteBackbone()).containsExactly(y);
        assertThat(backboneNegative.getCompleteBackbone()).isEmpty();
        combinedPosNegBackbone = new TreeSet<>(backbonePositive.getCompleteBackbone());
        combinedPosNegBackbone.addAll(backboneNegative.getCompleteBackbone());
        assertThat(backbone.getCompleteBackbone()).isEqualTo(combinedPosNegBackbone);
        solver.loadState(before);

        formula = f.and(f.and(f.or(x.negate(), y), x.negate()), f.and(z, f.or(x, y)));
        before = solver.saveState();
        solver.add(formula);
        backbone = solver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
        backbonePositive = solver.compute(formula.variables(), BackboneType.ONLY_POSITIVE);
        backboneNegative = solver.compute(formula.variables(), BackboneType.ONLY_NEGATIVE);
        assertThat(backbone.getCompleteBackbone()).containsExactly(x.negate(), y, z);
        assertThat(backbonePositive.getCompleteBackbone()).containsExactly(y, z);
        assertThat(backboneNegative.getCompleteBackbone()).containsExactly(x.negate());
        combinedPosNegBackbone = new TreeSet<>(backbonePositive.getCompleteBackbone());
        combinedPosNegBackbone.addAll(backboneNegative.getCompleteBackbone());
        assertThat(backbone.getCompleteBackbone()).isEqualTo(combinedPosNegBackbone);
        solver.loadState(before);
    }

    @Test
    public void testDifferentConfigurations() throws IOException, ParserException {
        final List<BackboneConfig> configs = new ArrayList<>();
        configs.add(new BackboneConfig.Builder().checkForComplementModelLiterals(false).build());
        configs.add(new BackboneConfig.Builder().checkForRotatableLiterals(false).build());
        configs.add(new BackboneConfig.Builder().initialUBCheckForRotatableLiterals(false).build());

        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        MiniSatBackbone backboneSolver = new MiniSatBackbone(new BackboneConfig.Builder().build());
        backboneSolver.add(formula);
        final Backbone backbone = backboneSolver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);

        for (final BackboneConfig config : configs) {
            backboneSolver = new MiniSatBackbone(config);
            backboneSolver.add(formula);
            assertThat(backboneSolver.compute(formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE)).isEqualTo(backbone);
        }
    }
}
