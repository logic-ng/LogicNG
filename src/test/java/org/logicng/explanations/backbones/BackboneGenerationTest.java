package org.logicng.explanations.backbones;

import org.junit.Ignore;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class BackboneGenerationTest {

    @Test
    public void testBackbone() {

        Backbone backbone = new Backbone();
        assertThat(backbone.isEmpty()).isTrue();
        assertThat(backbone.getPositiveBackbone()).isEmpty();
        assertThat(backbone.getNegativeBackbone()).isEmpty();
        assertThat(backbone.getCompleteBackbone()).isEmpty();

        FormulaFactory f = new FormulaFactory();
        Literal x = f.literal("x", true);
        Literal y = f.literal("y", true);
        Literal z = f.literal("z", false);
        Literal u = f.literal("u", false);

        backbone.add(x);
        assertThat(backbone.isEmpty()).isFalse();
        assertThat(backbone.getPositiveBackbone()).isEqualTo(new TreeSet<>(Collections.singletonList(x)));
        assertThat(backbone.getNegativeBackbone()).isEmpty();
        assertThat(backbone.getCompleteBackbone()).isEqualTo(new TreeSet<>(Collections.singletonList(x)));

        backbone.add(z);
        assertThat(backbone.isEmpty()).isFalse();
        assertThat(backbone.getPositiveBackbone()).isEqualTo(new TreeSet<>(Collections.singletonList(x.variable())));
        assertThat(backbone.getNegativeBackbone()).isEqualTo(new TreeSet<>(Collections.singletonList(z.variable())));
        assertThat(backbone.getCompleteBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(x, z)));

        backbone.add(Arrays.asList(y, u));
        assertThat(backbone.isEmpty()).isFalse();
        assertThat(backbone.getPositiveBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(x.variable(), y.variable())));
        assertThat(backbone.getNegativeBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(z.variable(), u.variable())));
        assertThat(backbone.getCompleteBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(x, y, z, u)));

        backbone = new Backbone(Arrays.asList(x, y, z));
        assertThat(backbone.isEmpty()).isFalse();
        assertThat(backbone.getPositiveBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(x.variable(), y.variable())));
        assertThat(backbone.getNegativeBackbone()).isEqualTo(new TreeSet<>(Collections.singletonList(z.variable())));
        assertThat(backbone.getCompleteBackbone()).isEqualTo(new TreeSet<>(Arrays.asList(x, y, z)));
    }


    @Test
    public void testBackboneConfig() {
        BackboneConfig config = new BackboneConfig.Builder().build();
        assertThat(config.algorithm == BackboneConfig.Algorithm.ITERATIVE_PLAIN).isTrue();
        assertThat(config.toString()).isEqualTo("BackboneConfig{\nalgorithm=ITERATIVE_PLAIN\n}\n");

        config = new BackboneConfig.Builder().algorithm(BackboneConfig.Algorithm.CORE).build();
        assertThat(config.algorithm == BackboneConfig.Algorithm.CORE).isTrue();
        assertThat(config.toString()).isEqualTo("BackboneConfig{\nalgorithm=CORE\n}\n");
    }


    @Test
    public void testBackboneGeneration() {

        BackboneGeneration backboneGeneration = new BackboneGeneration();
        final FormulaFactory f = new FormulaFactory();
        final MiniSatConfig miniSatConfig = new MiniSatConfig.Builder().build();
        final SATSolver solver = MiniSat.miniSat(f, miniSatConfig);
        Formula formula1 = f.and(f.literal("x", true), f.literal("y", false));
        solver.add(formula1);
        assertThat(backboneGeneration.computeBackbone(solver, formula1.variables()).getCompleteBackbone())
                .containsExactlyElementsOf(
                        backboneGeneration.computeBackbone(formula1).getCompleteBackbone());

        backboneGeneration = new BackboneGeneration(f);
        assertThat(backboneGeneration.computeBackbone(solver, formula1.variables()).getCompleteBackbone())
                .containsExactlyElementsOf(
                        backboneGeneration.computeBackbone(formula1, formula1.variables()).getCompleteBackbone());

        final BackboneConfig config = new BackboneConfig.Builder().build();
        Formula formula2 = f.or(f.literal("x", false), f.literal("y", true));
        Formula formula3 = f.literal("z", true);
        solver.add(formula2);
        solver.add(formula3);
        List<Formula> formulas = Arrays.asList(formula1, formula2, formula3);
        SortedSet<Variable> variables = new TreeSet<>();
        variables.addAll(formula1.variables());
        variables.addAll(formula2.variables());
        variables.addAll(formula3.variables());
        backboneGeneration = new BackboneGeneration(config);
        assertThat(backboneGeneration.computeBackbone(solver, variables).getCompleteBackbone())
                .containsExactlyElementsOf(
                        backboneGeneration.computeBackbone(formulas).getCompleteBackbone());
        assertThat(backboneGeneration.computeBackbone(solver, variables).getCompleteBackbone())
                .containsExactlyElementsOf(
                        backboneGeneration.computeBackbone(formulas, variables).getCompleteBackbone());
    }


    @Test
    public void testSimpleBackbones() {
        List<BackboneConfig.Algorithm> algorithms = new ArrayList<>(Arrays.asList(
                BackboneConfig.Algorithm.ENUMERATION,
                BackboneConfig.Algorithm.ITERATIVE_PLAIN,
                BackboneConfig.Algorithm.ITERATIVE_ONE_TEST,
                BackboneConfig.Algorithm.ITERATIVE_PLUS,
                BackboneConfig.Algorithm.CHUNKING,
                BackboneConfig.Algorithm.CORE,
                BackboneConfig.Algorithm.CORE_CHUNKING
        ));

        for (BackboneConfig.Algorithm algorithm : algorithms) {
            testSimpleBackbonesHelper(algorithm);
        }
    }

    private void testSimpleBackbonesHelper(BackboneConfig.Algorithm algorithm) {

        final BackboneConfig config = new BackboneConfig.Builder().algorithm(algorithm).build();
        final FormulaFactory f = new FormulaFactory();
        final MiniSatConfig miniSatConfig = new MiniSatConfig.Builder().proofGeneration(true).build();
        final SATSolver solver = MiniSat.miniSat(f, miniSatConfig);
        final BackboneGeneration backboneGeneration = new BackboneGeneration(config);

        Literal x = f.literal("x", true);
        Literal y = f.literal("y", true);
        Literal z = f.literal("z", true);
        Literal u = f.literal("u", true);
        Literal v = f.literal("v", true);

        Formula formula = f.verum();
        SolverState before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x;
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singletonList(x))
        );
        solver.loadState(before);

        formula = f.and(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.or(x, y);
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = x.negate();
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(x.negate()))
        );
        solver.loadState(before);

        formula = f.or(f.and(x, y, z), f.and(x, y, u), f.and(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(x))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y, z), f.or(x, y, u), f.or(x, u, z));
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>()
        );
        solver.loadState(before);

        formula = f.and(f.or(x.negate(), y), x);
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x, y))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(x.negate(), y));
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(y))
        );
        solver.loadState(before);

        formula = f.and(f.and(f.or(x.negate(), y), x.negate()), f.and(z, f.or(x, y)));
        before = solver.saveState();
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Arrays.asList(x.negate(), y, z))
        );
        solver.loadState(before);

        formula = f.and(f.or(x, y), f.or(u, v), z);
        solver.add(formula);
        assertThat(backboneGeneration.computeBackbone(solver, formula.variables()).getCompleteBackbone()).isEqualTo(
                new TreeSet<>(Collections.singleton(z))
        );
    }


    @Test
    public void testEqualResultsOfDifferentAlgorithms() throws IOException, ParserException {
        List<BackboneConfig.Algorithm> algorithms = new ArrayList<>(Arrays.asList(
                BackboneConfig.Algorithm.ENUMERATION,
                BackboneConfig.Algorithm.ITERATIVE_PLUS,
                BackboneConfig.Algorithm.CHUNKING,
                BackboneConfig.Algorithm.ITERATIVE_PLAIN,
                BackboneConfig.Algorithm.ITERATIVE_ONE_TEST,
                BackboneConfig.Algorithm.CORE,
                BackboneConfig.Algorithm.CORE_CHUNKING
        ));

        final FormulaFactory f = new FormulaFactory();

        Literal x = f.literal("x", true);
        Literal y = f.literal("y", true);
        Literal z = f.literal("z", true);
        Literal u = f.literal("u", true);
        Literal v = f.literal("v", true);

        List<Formula> formulas = new ArrayList<>(Arrays.asList(
                f.verum(),
                x,
                f.and(x, y),
                f.or(x, y),
                x.negate(),
                f.or(f.and(x, y, z), f.and(x, y, u), f.and(x, u, z)),
                f.and(f.or(x, y, z), f.or(x, y, u), f.or(x, u, z)),
                f.and(f.or(x.negate(), y), x),
                f.and(f.or(x, y), f.or(x.negate(), y)),
                f.and(f.and(f.or(x.negate(), y), x.negate()), f.and(z, f.or(x, y))),
                f.and(f.or(x, y), f.or(u, v), z)
//                FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f)
        ));

//        List<Formula> formulas = new ArrayList<>();
//        final PropositionalParser p = new PropositionalParser(f);
//        final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/formulas/small_formulas.txt"));
//        while (reader.ready()) {
//            formulas.add(p.parse(reader.readLine()));
//        }

        for (Formula formula : formulas) {
            List<Backbone> backbones = new ArrayList<>();
            for (BackboneConfig.Algorithm algorithm : algorithms) {
                final BackboneConfig config = new BackboneConfig.Builder().algorithm(algorithm).build();
                final BackboneGeneration backboneGeneration = new BackboneGeneration(config);
                backbones.add(backboneGeneration.computeBackbone(formula));
            }
            Backbone backbone1 = backbones.get(0);
            SortedSet<Literal> completeBackbone1 = backbone1.getCompleteBackbone();
            for (Backbone backbone : backbones) {
                if (backbone != backbone1) {
                    assertThat(backbone.getCompleteBackbone()).containsExactlyElementsOf(completeBackbone1);
                }
            }
        }
    }


//    First Profiling Tests


    @Ignore
    @Test
    public void testLargeFormula() throws IOException, ParserException {
        List<BackboneConfig.Algorithm> algorithms = new ArrayList<>(Arrays.asList(
//                BackboneConfig.Algorithm.ENUMERATION -- takes too long, leave out.
                BackboneConfig.Algorithm.ITERATIVE_PLUS,
                BackboneConfig.Algorithm.CHUNKING,
                BackboneConfig.Algorithm.ITERATIVE_PLAIN,
                BackboneConfig.Algorithm.ITERATIVE_ONE_TEST,
                BackboneConfig.Algorithm.CORE,
                BackboneConfig.Algorithm.CORE_CHUNKING
        ));

        for (BackboneConfig.Algorithm algorithm : algorithms) {
            final long start = System.nanoTime();
            testLargeFormulaHelper(algorithm);
            final long end = System.nanoTime();
            System.out.printf("%s took %d ms\n", algorithm, (end-start)/1000000);
        }
    }

    private void testLargeFormulaHelper(BackboneConfig.Algorithm algorithm) throws IOException, ParserException {
        final BackboneConfig config = new BackboneConfig.Builder().algorithm(algorithm).build();
        final BackboneGeneration backboneGeneration = new BackboneGeneration(config);
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        assertThat(verifyBackbone(backboneGeneration.computeBackbone(formula), formula, formula.variables())).isTrue();
    }


    @Ignore
    @Test
    public void testSmallFormulas() throws IOException, ParserException {
        List<BackboneConfig.Algorithm> algorithms = new ArrayList<>(Arrays.asList(
//                BackboneConfig.Algorithm.ENUMERATION -- takes too long, leave out.
                BackboneConfig.Algorithm.ITERATIVE_PLUS,
                BackboneConfig.Algorithm.CHUNKING,
                BackboneConfig.Algorithm.ITERATIVE_PLAIN,
                BackboneConfig.Algorithm.ITERATIVE_ONE_TEST,
                BackboneConfig.Algorithm.CORE,
                BackboneConfig.Algorithm.CORE_CHUNKING
        ));

        for (BackboneConfig.Algorithm algorithm : algorithms) {
            final long start = System.nanoTime();
            testSmallFormulasHelper(algorithm);
            final long end = System.nanoTime();
            System.out.printf("%s took %d ms\n", algorithm, (end-start)/1000000);
        }
    }

    private void testSmallFormulasHelper(BackboneConfig.Algorithm algorithm) throws IOException, ParserException {
        final BackboneConfig config = new BackboneConfig.Builder().algorithm(algorithm).build();
        final FormulaFactory f = new FormulaFactory();
        final BackboneGeneration backboneGeneration = new BackboneGeneration(config);
        final PropositionalParser p = new PropositionalParser(f);
        final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/formulas/small_formulas.txt"));
        while (reader.ready()) {
            final Formula formula = p.parse(reader.readLine());
            Backbone backbone = backboneGeneration.computeBackbone(formula);
//            assertThat(verifyBackbone(backbone, formula, formula.variables())).isTrue();
        }
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

}
