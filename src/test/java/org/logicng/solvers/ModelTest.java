package org.logicng.solvers;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.transformations.cnf.TseitinTransformation;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Test model generation and model enumeration on solvers.
 * @version 1.6.0
 * @since 1.6.0
 */
@RunWith(Parameterized.class)
public class ModelTest {

  private static final FormulaFactory f = new FormulaFactory();

  private final MiniSat solver;

  @Parameterized.Parameters(name = "{1}")
  public static Collection<Object[]> solvers() {
    final MiniSatConfig configNoPGAux = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).auxiliaryVariablesInModels(true).build();
    final MiniSatConfig configNoPGNoAux = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).auxiliaryVariablesInModels(false).build();
    final MiniSatConfig configPGAux = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(true).build();
    final MiniSatConfig configPGNoAux = new MiniSatConfig.Builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(false).build();
    final List<Pair<MiniSatConfig, String>> configs = Arrays.asList(
            new Pair<>(configNoPGAux, "FF CNF, +AUX"),
            new Pair<>(configNoPGNoAux, "FF CNF, -AUX"),
            new Pair<>(configPGAux, "PG CNF, +AUX"),
            new Pair<>(configPGNoAux, "PG CNF, -AUX")
    );
    final List<Object[]> solvers = new ArrayList<>();
    for (final Pair<MiniSatConfig, String> config : configs) {
      solvers.add(new Object[]{MiniSat.miniSat(f, config.first()), "MiniSat (" + config.second() + ")"});
      solvers.add(new Object[]{MiniSat.miniCard(f, config.first()), "MiniCard (" + config.second() + ")"});
      solvers.add(new Object[]{MiniSat.glucose(f, config.first(), new GlucoseConfig.Builder().build()), "Glucose (" + config.second() + ")"});
    }
    return solvers;
  }

  public ModelTest(final MiniSat solver, final String description) {
    this.solver = solver;
  }

  @Test
  public void testNoModel() throws ParserException {
    this.solver.reset();
    this.solver.add(f.falsum());
    this.solver.sat();
    assertThat(this.solver.model()).isNull();
    this.solver.reset();
    this.solver.add(f.parse("A & ~A"));
    this.solver.sat();
    assertThat(this.solver.model()).isNull();
    this.solver.reset();
    this.solver.add(f.parse("(A => (B & C)) & A & C & (C <=> ~B)"));
    this.solver.sat();
    assertThat(this.solver.model()).isNull();
  }

  @Test
  public void testEmptyModel() {
    this.solver.reset();
    this.solver.add(f.verum());
    this.solver.sat();
    final Assignment model = this.solver.model();
    assertThat(model.literals()).isEmpty();
    assertThat(model.blockingClause(f)).isEqualTo(f.falsum());
    assertThat(this.solver.enumerateAllModels()).hasSize(1);
  }

  @Test
  public void testSimpleModel() {
    this.solver.reset();
    this.solver.add(f.literal("A", true));
    this.solver.sat();
    Assignment model = this.solver.model();
    assertThat(model.literals()).containsExactly(f.literal("A", true));
    assertThat(this.solver.enumerateAllModels()).hasSize(1);
    this.solver.reset();
    this.solver.add(f.literal("A", false));
    this.solver.sat();
    model = this.solver.model();
    assertThat(model.literals()).containsExactly(f.literal("A", false));
    assertThat(this.solver.enumerateAllModels()).hasSize(1);
  }

  @Test
  public void testCNFFormula() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A|B|C) & (~A|~B|~C) & (A|~B|~C) & (~A|~B|C)");
    this.solver.add(formula);
    this.solver.sat();
    final Assignment model = this.solver.model();
    assertThat(formula.evaluate(model)).isTrue();
    assertThat(this.solver.enumerateAllModels()).hasSize(4);
    for (final Assignment assignment : this.solver.enumerateAllModels()) {
      assertThat(formula.evaluate(assignment)).isTrue();
    }
  }

  @Test
  public void testCNFWithAuxiliaryVars() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    final Formula cnf = formula.transform(new TseitinTransformation(0));
    this.solver.add(cnf);
    this.solver.sat();
    final Assignment model = this.solver.model();
    assertThat(formula.evaluate(model)).isTrue();
    final List<Assignment> allModels = this.solver.enumerateAllModels();
    assertThat(allModels).hasSize(4);
    if (this.solver.getConfig().isAuxiliaryVariablesInModels()) {
      assertThat(model.formula(f).variables()).isEqualTo(cnf.variables());
      for (final Assignment assignment : allModels) {
        assertThat(formula.evaluate(assignment)).isTrue();
        assertThat(assignment.formula(f).variables()).isEqualTo(cnf.variables());
      }
    } else {
      assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
      for (final Assignment assignment : allModels) {
        assertThat(formula.evaluate(assignment)).isTrue();
        assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
      }
    }
  }

  @Test
  public void testCNFWithAuxiliaryVarsRestrictedToOriginal() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    final Formula cnf = formula.transform(new TseitinTransformation(0));
    this.solver.add(cnf);
    this.solver.sat();
    final Assignment model = this.solver.model(formula.variables());
    assertThat(formula.evaluate(model)).isTrue();
    final List<Assignment> allModels = this.solver.enumerateAllModels(formula.variables());
    assertThat(allModels).hasSize(4);
    assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
    for (final Assignment assignment : allModels) {
      assertThat(formula.evaluate(assignment)).isTrue();
      assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
    }
  }

  @Test
  public void testNonCNFAllVars() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    this.solver.add(formula);
    this.solver.sat();
    final Assignment model = this.solver.model();
    assertThat(formula.evaluate(model)).isTrue();
    final List<Assignment> allModels = this.solver.enumerateAllModels();
    if (!this.solver.getConfig().isAuxiliaryVariablesInModels() || this.solver.getConfig().getCnfMethod() == MiniSatConfig.CNFMethod.FACTORY_CNF) {
      assertThat(allModels).hasSize(4);
      for (final Assignment assignment : allModels) {
        assertThat(formula.evaluate(assignment)).isTrue();
        assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
      }
    } else {
      assertThat(allModels).hasSize(6);
      for (final Assignment assignment : allModels) {
        System.out.println(assignment);
        assertThat(formula.evaluate(assignment)).isTrue();
        assertThat(formula.variables()).isSubsetOf(assignment.formula(f).variables());
      }
    }
  }

  @Test
  public void testNonCNFOnlyFormulaVars() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    this.solver.add(formula);
    this.solver.sat();
    final Assignment model = this.solver.model(formula.variables());
    assertThat(formula.evaluate(model)).isTrue();
    assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
    final List<Assignment> allModels = this.solver.enumerateAllModels(formula.variables());
    assertThat(allModels).hasSize(4);
    for (final Assignment assignment : allModels) {
      assertThat(formula.evaluate(assignment)).isTrue();
      assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
    }
  }

  @Test
  public void testNonCNFRestrictedVars() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    final MiniSat miniSat = MiniSat.miniSat(f);
    miniSat.add(formula);
    this.solver.add(formula);
    this.solver.sat();
    final SortedSet<Variable> relevantVariables = new TreeSet<>(Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C")));
    final Assignment model = this.solver.model(relevantVariables);
    assertThat(miniSat.sat(model.literals())).isEqualTo(Tristate.TRUE);
    assertThat(model.formula(f).variables()).isEqualTo(relevantVariables);
    final List<Assignment> allModels = this.solver.enumerateAllModels(relevantVariables);
    assertThat(allModels).hasSize(2);
    for (final Assignment assignment : allModels) {
      assertThat(miniSat.sat(assignment.literals())).isEqualTo(Tristate.TRUE);
      assertThat(assignment.formula(f).variables()).isEqualTo(relevantVariables);
    }
  }

  @Test
  public void testNonCNFRestrictedAndAdditionalVars() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    final MiniSat miniSat = MiniSat.miniSat(f);
    miniSat.add(formula);
    this.solver.add(formula);
    this.solver.sat();
    final SortedSet<Variable> relevantVariables = new TreeSet<>(Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C")));
    final SortedSet<Variable> additionalVariables = new TreeSet<>(Arrays.asList(f.variable("D"), f.variable("X"), f.variable("Y")));
    final SortedSet<Variable> allVariables = new TreeSet<>(relevantVariables);
    allVariables.add(f.variable("D"));
    final Assignment model = this.solver.model(additionalVariables);
    assertThat(miniSat.sat(model.literals())).isEqualTo(Tristate.TRUE);
    assertThat(model.formula(f).variables()).containsExactly(f.variable("D"));
    final List<Assignment> allModels = this.solver.enumerateAllModels(relevantVariables, additionalVariables);
    assertThat(allModels).hasSize(2);
    for (final Assignment assignment : allModels) {
      assertThat(miniSat.sat(assignment.literals())).isEqualTo(Tristate.TRUE);
      assertThat(assignment.formula(f).variables()).isEqualTo(allVariables);
    }
  }

  @Test(expected = IllegalStateException.class)
  public void testUnsolvedFormula() throws ParserException {
    this.solver.reset();
    final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
    this.solver.add(formula);
    this.solver.model();
  }
}
