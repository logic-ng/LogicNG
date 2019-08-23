package org.logicng.solvers;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.Pair;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Tests for generating backbones on solvers.
 * @version 1.6.0
 * @since 1.6.0
 */
@RunWith(Parameterized.class)
public class BackboneOnSolverTest {

  private static final FormulaFactory f = new FormulaFactory();

  private final MiniSat solver;

  @Parameterized.Parameters(name = "{1}")
  public static Collection<Object[]> solvers() {
    final MiniSatConfig configNoPG1 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configNoPG2 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(true).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configNoPG3 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configNoPG4 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(true).build();
    final MiniSatConfig configNoPG5 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).bbCheckForRotatableLiterals(true).bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(true).build();
    final MiniSatConfig configPG1 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configPG2 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(true).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configPG3 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(false).build();
    final MiniSatConfig configPG4 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(false).bbCheckForComplementModelLiterals(false).bbInitialUBCheckForRotatableLiterals(true).build();
    final MiniSatConfig configPG5 = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).bbCheckForRotatableLiterals(true).bbCheckForComplementModelLiterals(true).bbInitialUBCheckForRotatableLiterals(true).build();
    final List<Pair<MiniSatConfig, String>> configs = Arrays.asList(
            new Pair<>(configNoPG1, "FF CNF -ROT -COMP -UB"),
            new Pair<>(configNoPG2, "FF CNF +ROT -COMP -UB"),
            new Pair<>(configNoPG3, "FF CNF -ROT +COMP -UB"),
            new Pair<>(configNoPG4, "FF CNF -ROT -COMP +UB"),
            new Pair<>(configNoPG5, "FF CNF +ROT +COMP +UB"),
            new Pair<>(configPG1, "PG CNF -ROT -COMP -UB"),
            new Pair<>(configPG2, "PG CNF +ROT -COMP -UB"),
            new Pair<>(configPG3, "PG CNF -ROT +COMP -UB"),
            new Pair<>(configPG4, "PG CNF -ROT -COMP +UB"),
            new Pair<>(configPG5, "PG CNF +ROT +COMP +UB")
    );
    final List<Object[]> solvers = new ArrayList<>();
    for (final Pair<MiniSatConfig, String> config : configs) {
      solvers.add(new Object[]{MiniSat.miniSat(f, config.first()), "MiniSat (" + config.second() + ")"});
      solvers.add(new Object[]{MiniSat.miniCard(f, config.first()), "MiniCard (" + config.second() + ")"});
      solvers.add(new Object[]{MiniSat.glucose(f, config.first(), new GlucoseConfig.Builder().build()), "Glucose (" + config.second() + ")"});
    }
    return solvers;
  }

  public BackboneOnSolverTest(final MiniSat solver, final String description) {
    this.solver = solver;
  }

  @Test
  public void testConstants() {
    this.solver.reset();
    SolverState state = null;
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      state = this.solver.saveState();
    }
    this.solver.add(f.falsum());
    Backbone backbone = this.solver.backbone(v("a b c"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isFalse();
    assertThat(backbone.getCompleteBackbone()).isEmpty();
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      this.solver.loadState(state);
    } else {
      this.solver.reset();
    }
    this.solver.add(f.verum());
    backbone = this.solver.backbone(v("a b c"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isTrue();
    assertThat(backbone.getCompleteBackbone()).isEmpty();
  }

  @Test
  public void testSimpleBackbones() throws ParserException {
    this.solver.reset();
    SolverState state = null;
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      state = this.solver.saveState();
    }
    this.solver.add(f.parse("a & b & ~c"));
    Backbone backbone = this.solver.backbone(v("a b c"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isTrue();
    assertThat(backbone.getCompleteBackbone()).containsExactly(f.variable("a"), f.variable("b"), f.literal("c", false));
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      this.solver.loadState(state);
    } else {
      this.solver.reset();
    }
    this.solver.add(f.parse("~a & ~b & c"));
    backbone = this.solver.backbone(v("a c"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isTrue();
    assertThat(backbone.getCompleteBackbone()).containsExactly(f.literal("a", false), f.variable("c"));
  }

  @Test
  public void testSimpleFormulas() throws ParserException {
    this.solver.reset();
    this.solver.add(f.parse("(a => c | d) & (b => d | ~e) & (a | b)"));
    Backbone backbone = this.solver.backbone(v("a b c d e f"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isTrue();
    assertThat(backbone.getCompleteBackbone()).isEmpty();
    this.solver.add(f.parse("a => b"));
    this.solver.add(f.parse("b => a"));
    this.solver.add(f.parse("~d"));
    backbone = this.solver.backbone(v("a b c d e f g h"), BackboneType.POSITIVE_AND_NEGATIVE);
    assertThat(backbone.isSat()).isTrue();
    assertThat(backbone.getCompleteBackbone()).containsExactly(f.variable("a"), f.variable("b"), f.variable("c"),
            f.literal("d", false), f.literal("e", false));
  }

  @Test
  public void testRealFormulaIncremental1() throws IOException, ParserException {
    this.solver.reset();
    final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
    this.solver.add(formula);
    final List<String> expectedBackbones = new ArrayList<>();
    final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/backbones/backbone_large_formula.txt"));
    while (reader.ready()) {
      expectedBackbones.add(reader.readLine());
    }
    reader.close();
    Backbone backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(0)));
    this.solver.add(f.variable("v411"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(1)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v385"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(2)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v275"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(3)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v188"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(4)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v103"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(5)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v404"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEmpty();
    assertThat(backbone.isSat()).isFalse();
  }

  @Ignore("Long Running")
  @Test
  public void testRealFormulaIncremental2() throws IOException, ParserException {
    this.solver.reset();
    final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
    this.solver.add(formula);
    final List<String> expectedBackbones = new ArrayList<>();
    final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/backbones/backbone_small_formulas.txt"));
    while (reader.ready()) {
      expectedBackbones.add(reader.readLine());
    }
    reader.close();
    Backbone backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(0)));
    this.solver.add(f.variable("v2609"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(1)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v2416"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(2)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v2048"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(3)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v39"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(4)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v1663"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(5)));
    assertThat(backbone.isSat()).isTrue();
    this.solver.add(f.variable("v2238"));
    backbone = this.solver.backbone(formula.variables());
    assertThat(backbone.getCompleteBackbone()).isEmpty();
    assertThat(backbone.isSat()).isFalse();
  }

  @Test
  public void testRealFormulaIncrementalDecremental1() throws IOException, ParserException {
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      this.solver.reset();
      final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
      this.solver.add(formula);
      final SolverState state = this.solver.saveState();
      final List<String> expectedBackbones = new ArrayList<>();
      final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/backbones/backbone_large_formula.txt"));
      while (reader.ready()) {
        expectedBackbones.add(reader.readLine());
      }
      reader.close();
      Backbone backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(0)));
      this.solver.add(f.variable("v411"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(1)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v411 & v385"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(2)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v411 & v385 & v275"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(3)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v411 & v385 & v275 & v188"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(4)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v411 & v385 & v275 & v188 & v103"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(5)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v411 & v385 & v275 & v188 & v103 & v404"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEmpty();
      assertThat(backbone.isSat()).isFalse();
    }
  }

  @Ignore("Long Running")
  @Test
  public void testRealFormulaIncrementalDecremental2() throws IOException, ParserException {
    if (this.solver.underlyingSolver() instanceof MiniSat2Solver) {
      this.solver.reset();
      final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
      this.solver.add(formula);
      final SolverState state = this.solver.saveState();
      final List<String> expectedBackbones = new ArrayList<>();
      final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/backbones/backbone_small_formulas.txt"));
      while (reader.ready()) {
        expectedBackbones.add(reader.readLine());
      }
      reader.close();
      Backbone backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(0)));
      this.solver.add(f.variable("v2609"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(1)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v2609 & v2416"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(2)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v2609 & v2416 & v2048"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(3)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v2609 & v2416 & v2048 & v39"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(4)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v2609 & v2416 & v2048 & v39 & v1663"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEqualTo(parseBackbone(expectedBackbones.get(5)));
      assertThat(backbone.isSat()).isTrue();

      this.solver.loadState(state);
      this.solver.add(f.parse("v2609 & v2416 & v2048 & v39 & v1663 & v2238"));
      backbone = this.solver.backbone(formula.variables());
      assertThat(backbone.getCompleteBackbone()).isEmpty();
      assertThat(backbone.isSat()).isFalse();
    }
  }

  private SortedSet<Variable> v(final String s) {
    final SortedSet<Variable> vars = new TreeSet<>();
    for (final String name : s.split(" ")) {
      vars.add(f.variable(name));
    }
    return vars;
  }

  private SortedSet<Literal> parseBackbone(final String string) throws ParserException {
    final SortedSet<Literal> literals = new TreeSet<>();
    for (final String lit : string.split(" ")) {
      literals.add((Literal) f.parse(lit));
    }
    return literals;
  }
}
